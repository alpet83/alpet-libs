unit NBBuff; { Модуль класса неблокируемого буфера данных}

interface
uses Windows, SysUtils, Classes, Misc, FastSync, SyncObjs, Math, WinHeap, MemStat;

{   Технология допускает одновременное чтение и запись данных, но
  исключает конкурентный доступ к записи (синхронизация есть).
  Попытка единовременного чтения разными потоками даст плачевный результат.

}

const
    MST_READWRITE = 0;
    MST_OPTIMIZE = 1;
    MST_RESIZE = 2;
    MPAGE_SIZE = 64 * 1024; // 64kb - должно быть строго для использования LoWord/HiWord

type
     TMemPage = packed array [$0000..MPAGE_SIZE - 1] of BYTE;
     PMemPage = ^TMemPage;

     TByteVector = packed array [0..MAXINT div 2 - 1] of BYTE;
     PByteVector = ^TByteVector;

     TNBBuff = class
     private
      FHeap: TWindowsHeap;
      procedure AddFreePage;
      function  AllocatePage: PMemPage;
      procedure ReleasePage (pg: PMemPage);
      procedure Optimize;
     protected
      FPages: TList;
      iFreeFirst: Integer;  // начала списка неиспользуемых страниц
      last_usage: Integer;   // последнее количество данных помещенное в буфер 
      big_buff_signal: Integer;
      evt_rd: TEvent;  // событие полного вычитывания
      evt_wx: TEvent;  // событие записи
      OptState: Integer;
      ReadPos, WritePos: Integer;
      sc_write: TCritSection;
     public
      wcount: Integer;
      nDataNeed: Integer;
      write_owner: DWORD;
      read_owner: DWORD;


      constructor       Create;
      destructor        Destroy; override;
      { methods }


      function          GetCount: Integer;
      function          Read (pData: Pointer; nBytes: Integer; peek_mode: Boolean = FALSE): Integer;
      function          ReadStart(var nBytes: Integer): Pointer;   // постраничное считывание
      procedure         ReadComplete (nBytes: Integer);

      procedure         LockWrite(const ctx_msg: String);
      procedure         UnlockWrite();

      procedure         Write (pData: Pointer; nBytes: Integer);
      function          WriteStart (var nBytes: Integer): Pointer;
      procedure         WriteComplete (nBytes: Integer);

      function          NeedOptimize: Boolean;
      function          WaitDataArrive (data_need, timeOut: DWORD): DWORD;
      function          WaitReadAll (timeOut: DWORD): DWORD;
     end; // TNBBuff

implementation

const
  ALLOW_OPT = 0;
  BLOCK_OPT = 1;
  CHECK_OPT = 2;
  NOWPF_OPT = 3;



{ TNBBuff }

constructor TNBBuff.Create;
begin
 sc_write := TCritSection.Create ('TNBuff.sc_write');
 FHeap := TWindowsHeap.Create(ClassName + '.FHeap');
 FPages := TList.Create;
 FPages.Capacity := 16384;
 evt_rd := TEvent.Create (nil, TRUE, FALSE, '');
 evt_wx := TEvent.Create (nil, TRUE, FALSE, '');
 AddFreePage;
 // SetSize (start_size);

end;

destructor TNBBuff.Destroy;
var n: Integer;
    l: Boolean;
begin
 l := sc_write.TryLock('TNBBuff.Destroy', 1500);
 ODS('[~T]. #MEMUSAGE: TNBBuff.FPages.Capacity = ' + IntToStr(FPages.Capacity));

 for n := FPages.Count - 1 downto 0 do
  begin
   if FPages[n] <> nil then
       begin
        ReleasePage (FPages[n]);
        FPages.Delete(n);
       end
      else break;
  end;

 FPages.Free;
 // освобождение кучи целиком
 FHeap.Free;
 if l then sc_write.Unlock;
 sc_write.Free;
 evt_rd.Free;
 evt_wx.Free;
end;

function TNBBuff.GetCount: Integer;
begin
 result := WritePos - ReadPos;
end;

procedure TNBBuff.LockWrite(const ctx_msg: String);
begin
 sc_write.Lock ( TSafeStr(ctx_msg), 50);
end;

function TNBBuff.NeedOptimize: Boolean;
begin
 result := WritePos > 128 * 1048576; // 128 mb virtual space
end; // NeedOptimize

procedure TNBBuff.Optimize;
var
   ost: Integer;
   n, iFirstPage: Integer;
   newPages: TList;
begin
 // может получится что оптимизация не возможна!
 if (FPages.Count > 0) and (FPages[0] <> nil) or (GetCount > 0) then exit;
 

 repeat
  ost := InterlockedCompareExchange(OptState, NOWPF_OPT, ALLOW_OPT);  // lock opt-check
  if (ost <> ALLOW_OPT) then // если сейчас проверка или заблокированное состояние
     SleepEx(1, TRUE);
 until (ost = ALLOW_OPT);

 iFirstPage := HiWord(ReadPos);
 // поиск первой ненулевой страницы в списке
 // DEMO: nil, nil, PG[2], PG[3] ...
 while (iFirstPage > 0) and (FPages[iFirstPage - 1] <> nil) do Dec (iFirstPage);

 if iFirstPage > 0 then
  begin
   // копирование страниц в новый список
   newPages := TList.Create;
   newPages.Capacity := 16384;
   for n := iFirstPage to FPages.Count - 1 do
       newPages.Add (FPages[n]);
   // замена списка новым
   FPages.Clear;
   FPages.Free;
   FPages := newPages;
   ODS('[~T].~C0A #OPT: TNBBuff.Optimize removing ' + IntToStr(iFirstPage) + ' void pages ~C07');
   // вычитание выбывших страниц
   InterlockedExchangeAdd (ReadPos, - iFirstPage * MPAGE_SIZE);
   InterlockedExchangeAdd (WritePos, - iFirstPage * MPAGE_SIZE);
   iFreeFirst := 0;
  end; 
 OptState := ALLOW_OPT;
end; // Optimize

function TNBBuff.Read;
var
   ost, nPage: Integer;
   pSrc: PMemPage;
   pDst: PByteVector absolute pData;
   page_ofs: Integer;
   src_ofst: Integer;
   dst_ofst: Integer;
   copy_cnt: Integer;

begin
 repeat
  ost := InterlockedCompareExchange(OptState, CHECK_OPT, ALLOW_OPT);  // lock opt-check
  if (ost <> ALLOW_OPT) then // now optimizing
     SleepEx(1, TRUE);
 until (ost = ALLOW_OPT);
 nBytes := Min(GetCount, nBytes);
 result := nBytes;
 OptState := BLOCK_OPT;      // block optimization
 page_ofs := 0;

 try
   dst_ofst := 0;
   src_ofst := ReadPos;
   try
     repeat
      nPage := HiWord (src_ofst);
      page_ofs := LoWord (src_ofst);
      copy_cnt := Min(MPAGE_SIZE - page_ofs, nBytes);
      pSrc := FPages[nPage];
      if (page_ofs >= $10000)  then
        asm
         int 3
        end;

      SafeMove (pSrc^[page_ofs], pDst^[dst_ofst], copy_cnt);
      Inc(src_ofst, copy_cnt);
      Inc(dst_ofst, copy_cnt);
      Dec (nBytes, copy_cnt);
     until (nBytes <= 0);
   except
    on E: ERangeError do
       PrintError(
         Format('TNBBuff.Read, range-error: page_ofs = %d, dst_ofst = %d', [page_ofs, dst_ofst])
                               );
   end;

   if not peek_mode then
      ReadPos := src_ofst;
   if GetCount = 0 then
      evt_rd.SetEvent;
 finally
  OptState := ALLOW_OPT;        // allow optimization
 end;
end;  // Read



function TNBBuff.ReadStart(var nBytes: Integer): Pointer;
var
   nPage: Integer;
   pSrc: PMemPage;
   page_ofs: Integer;
   src_ofst: Integer;

begin
 nBytes := Min(GetCount, nBytes);
 src_ofst := ReadPos;
 // ------------ //
 nPage := HiWord (src_ofst);
 page_ofs := LoWord (src_ofst);
 nBytes := Min(MPAGE_SIZE - page_ofs, nBytes);
 pSrc := FPages[nPage];
 result := @pSrc[page_ofs];
end; // ReadPage




function TNBBuff.WaitDataArrive (data_need, timeOut: DWORD): DWORD;
begin
 if (data_need > 0) and ( Integer(data_need) <= GetCount) then
  begin
   result := 0;
   exit;
  end;
 nDataNeed := data_need;
 result := WaitForSingleObject (evt_wx.Handle, timeOut);
 if result = WAIT_OBJECT_0 then evt_wx.ResetEvent;
end;

function TNBBuff.WaitReadAll(timeOut: DWORD): DWORD;
begin
 result := WaitForSingleObject (evt_rd.Handle, timeOut);
 evt_rd.ResetEvent;
end;


procedure TNBBuff.AddFreePage;
var
   iFreeLast: Integer;
   nFreeCount: Integer;
   p: Pointer;
begin
 iFreeLast := HiWord (ReadPos) - 1; // последняя из свободных страниц памяти
 nFreeCount := 0;
 if iFreeFirst <= iFreeLast then
  begin
   if (iFreeLast - iFreeFirst) * MPAGE_SIZE > last_usage * 4 then
      Inc(big_buff_signal)
   else
      big_buff_signal := 0;

   if (big_buff_signal > 500) then
   while (iFreeFirst + 16 < iFreeLast) do
    begin
     ReleasePage (FPages[iFreeFirst]);
     FPages[iFreeFirst] := nil;
     Inc (iFreeFirst);
     Inc (nFreeCount);
    end;
   p := FPages[iFreeFirst];
   FPages[iFreeFirst] := nil;
   Inc (iFreeFirst);
   FPages.Add(p);
   if (nFreeCount > 0) then
       ODS ('[~T].~C0F #OPTMEM: Удалено страниц памяти = ~C0D' + IntToStr(nFreeCount) + '~C07');
  end
 else
  begin
   p := AllocatePage ();
   FPages.Add (p);
  end;
  FillChar (FPages.Last^, MPAGE_SIZE, 0);
end; // AddFreePage

procedure TNBBuff.Write(pData: Pointer; nBytes: Integer);
var
   pSrc: PByteArray absolute pData;
   pDst: PMemPage;
   src_ofst: Integer;
   page_ofs: Integer;
   copy_cnt: Integer;
   target_cnt: Integer;
   nPage: Integer;
   bLock: Boolean;
begin
 target_cnt := GetCount + nBytes;
 Assert (Assigned (self), 'self unassigned');
 bLock := GetCurrentThreadId <> write_owner;
 if bLock then LockWrite ('.Write');
 try
  if NeedOptimize then  Optimize;
  last_usage := target_cnt;

  // увеличение пула страниц
  while (HiWord(WritePos + nBytes) >= FPages.Count) do
         AddFreePage;

  src_ofst := 0;
  repeat
   nPage := HiWord (WritePos);
   page_ofs := LoWord(WritePos);
   pDst := FPages [nPage];
   ASSERT (Assigned (pDst));
   copy_cnt := Min (MPAGE_SIZE - page_ofs, nBytes);
   SafeMove(pSrc^[src_ofst], pDst^[page_ofs], copy_cnt);

   Inc (src_ofst, copy_cnt);
   Inc (WritePos, copy_cnt); // ReallyWrite
   Dec (nBytes, copy_cnt);

  until nBytes <= 0;

 finally
  if bLock then UnlockWrite;
 end;

 ASSERT ( WritePos < 1 shl 30, 'Исчерпан пропускной лимит в 1Гб, пропущена оптимизация');


 if (nDataNeed > 0) then
  begin
   if (target_cnt <= nDataNeed) then
       Dec (nDataNeed, target_cnt)
   else
      nDataNeed := 0;
   if nDataNeed = 0 then evt_wx.SetEvent;
  end; 
end; // Write


procedure TNBBuff.ReadComplete(nBytes: Integer);
begin
 Inc (ReadPos, nBytes);
 if GetCount = 0 then
    evt_rd.SetEvent;
end;

procedure TNBBuff.WriteComplete(nBytes: Integer);
begin
 //
 Inc (WritePos, nBytes);
 sc_write.Unlock;
end;

function TNBBuff.WriteStart(var nBytes: Integer): Pointer;
var
   pDst: PMemPage;
   page_ofs: Integer;
   nPage: Integer;
begin
 // Assert (Assigned (self), 'self unassigned');
 sc_write.Lock('.Write');
 // увеличение пула страниц
 while (HiWord(WritePos + nBytes) >= FPages.Count) do
        AddFreePage;

 nPage := HiWord (WritePos);
 page_ofs := LoWord(WritePos);
 pDst := FPages [nPage];
 // ASSERT (Assigned (pDst));
 nBytes := Min (MPAGE_SIZE - page_ofs, nBytes);
 result := @pDst[page_ofs];
end;

function TNBBuff.AllocatePage: PMemPage;
begin
 // result := GlobalAllocPtr (2, MPAGE_SIZE);
 GetCollector.LogAlloc(ClassName + '.pages', MPAGE_SIZE);
 result := FHeap.AllocMem(MPAGE_SIZE);
end;

procedure TNBBuff.ReleasePage(pg: PMemPage);
begin
 GetCollector.LogFree(ClassName + '.pages', MPAGE_SIZE);
 FHeap.ReleaseMem(pg);
 // GlobalFreePtr (pg);
end;

procedure TNBBuff.UnlockWrite;
begin
 sc_write.Unlock;
end;

end.
