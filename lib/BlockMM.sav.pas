unit BlockMM;

interface
uses Windows, BaseLib;


const
   MIB = 1048576;

function BlockAlloc(p: Pointer; dwSize, flAllocationType: Cardinal): Pointer;
function BlockFree (p: Pointer; dwSize: Cardinal): Boolean;
function BlockInfo (lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD; stdcall;


procedure con_log(const msg: WideString; flags: DWORD); stdcall;

procedure FreeBuffer;


var
   log_proc: Procedure (const msg: WideString; flags: Cardinal = 255); stdcall = con_log;
   alloc_sum: Int64 = 0;
   freed_sum: Int64 = 0;
   alloc_cnt: Integer = 0;


implementation

const
   BUFF_SIZE = 64 * MIB;
   PAGE_SIZE = 4096;  // boundary
   PAGE_MASK = PAGE_SIZE - 1;
   PAGE_COUNT = BUFF_SIZE div PAGE_SIZE;
   PAGE_RESV = 1;
   PAGE_COMT = 2;
   PAGE_USED = PAGE_RESV or PAGE_COMT;
   PAGE_LAST = 128;



type
   TPageInfo = packed record
    flags: WORD;
    fwd: WORD;
   end;

   TByteVector = array [0..BUFF_SIZE + PAGE_SIZE - 1] of BYTE;
   PByteVector = ^TByteVector;



   PPageInfo = ^TPageInfo;

var
   // _buffer: TByteVector; // cycled buffer
   buffer: PByteVector;
   enable_con: Boolean = FALSE;


   // pages: array [0..PAGE_COUNT - 1] of TPageInfo;
   pages: array [0..PAGE_COUNT - 1] of BYTE; // used/unused

   ptr_first, ptr_last: Cardinal;

   sc_lock: TRTLCriticalSection;

procedure con_log(const msg: WideString; flags: DWORD); stdcall;
begin
 if enable_con then WriteLn(msg);
 OutputDebugString (PChar (msg));
end;


procedure InitPages;

begin
 FillChar (pages, sizeof(pages), 0);
end;

function FindFreeBlockFirst (dwSize: DWORD): Integer;
var
   n, best, pcnt, free: Integer;
   p: PPageInfo;

begin
 result := -1;
 pcnt := dwSize div PAGE_SIZE;
 if pcnt <= 0 then exit;
 n := 0;
 free := 0;
 best := 0;
 while (n < PAGE_COUNT) do
  begin
   if pages[n] and PAGE_USED = 0 then
      Inc (free)
   else
     begin
      free := 0;
      best := n + 1;
     end;
   if free >= pcnt then
    begin
     result := best;
     break;
    end;
   Inc (n);
  end;
end; // FindFreeBlock

function FindFreeBlockLast (dwSize: DWORD): Integer;
var
   n, best, pcnt, free: Integer;
   p: PPageInfo;

begin
 result := -1;
 pcnt := dwSize div PAGE_SIZE;
 if pcnt <= 0 then exit;
 n := PAGE_COUNT - 1;
 free := 0;
 best := PAGE_COUNT;
 while (n >= 0) do
  begin
   if pages[n] and PAGE_USED = 0 then
     begin
      Inc (free);
      Dec (best);
     end
   else
     begin
      free := 0;
      best := n;
     end;
   if free >= pcnt then
    begin
     result := best;
     break;
    end;
   Dec (n);
  end;
end; // FindFreeBlock

function CheckIsFree(idx, pcnt: Integer; atype: Byte): Boolean;
var
   i, last: Integer;
begin
 result := FALSE;
 last := idx + pcnt - 1;
 for i := idx to last do
  if pages[i] and atype <> 0 then exit;

 result := TRUE;
end;

function itos(i: Int64): ShortString;
begin
 Str (i, result);
end;

function AlignOffset(ofs: DWORD; psz: PDWORD = nil): DWORD; inline;
var
   rest: DWORD;

begin
 rest := PAGE_SIZE - ofs and PAGE_MASK;
 if rest < PAGE_SIZE then
   begin
    ofs := ofs + rest;
    if psz <> nil then psz^ := psz^ - rest;
   end;
 result := ofs;
end; // AlignOffset

function AlignPtr(p: Pointer): Pointer; inline;
begin
 result := Pointer (AlignOffset (DWORD(p)) );
end;

function InBounds(p: Pointer; dwSize: DWORD): Boolean; inline;
begin
 result := ( ptr_first <= DWORD (p) ) and ( DWORD (p) + dwSize <= ptr_last );
end;

function BlockAlloc(p: Pointer; dwSize, flAllocationType: Cardinal): Pointer;
var
   rest, ofs: DWORD;
   atype: Byte;
   i, idx, pcnt, next: Integer;
   tmp: array [0..15] of CHAR;
   // np: PPageInfo;
begin
 result := nil;

 if (p <> nil) and (not InBounds (p, 0)) then
   begin
    Int2Hex ( Integer(p), tmp );
    log_proc('[~T].~C0C #ERROR: trying outbound alloc 1~C07');
    log_proc(tmp);

    exit;
   end;

 EnterCriticalSection (sc_lock);
 try

  if p <> nil then
   begin
    // аллокация блока в хвост, возможна по неровному адресу
    ofs := DWORD(p);
    ofs := AlignOffset (ofs, @dwSize);
   end;

  rest := dwSize and PAGE_MASK;
  if rest > 0 then
     dwSize := dwSize + PAGE_SIZE - rest;

  pcnt := dwSize div PAGE_SIZE;


  atype := 0;
  if flAllocationType and MEM_COMMIT  <> 0 then atype := atype or PAGE_COMT;
  if flAllocationType and MEM_RESERVE <> 0 then atype := atype or PAGE_RESV;

  if p = nil then
   begin
    if flAllocationType and MEM_TOP_DOWN <> 0 then
     idx := FindFreeBlockLast (dwSize)
    else
     idx := FindFreeBlockFirst (dwSize);
   end
  else
   begin
    // попытка аллокации по заданному адресу
    if not InBounds (p, dwSize) then
      begin
       log_proc('[~T].~C0C #ERROR: trying outbound alloc 2~C07');
       exit;
      end;

    ofs := DWORD(p) - ptr_first;
    idx := ofs div PAGE_SIZE;

    if not CheckIsFree (idx, pcnt, atype) then exit;
   end;




  if idx < 0 then
    begin
     log_proc('[~T].~C0C #ERROR: no more free blocks, alloc_sum(KiB) =~C07 ' + itos (alloc_sum div 1024));
     exit;
    end;

  next := (idx + pcnt);  // следующая страница, не входящяя в блок

  ofs := DWORD(idx) * PAGE_SIZE;
  result := Pointer (ptr_first + ofs);


  for i := idx to next - 2 do
      pages[i] := atype;

  pages [next - 1] := atype or PAGE_LAST;

  if atype and PAGE_COMT <> 0 then
    begin
     Inc (alloc_sum, dwSize);
     Inc (alloc_cnt);
    end;


  {if dwSize > 200 * 1024 then
     log_proc ('[~T]. allocated block at~C0D ' + PtrToStr(result) + '~C07, size =~C0D ' + itos(dwSize div 1024) + '~C07');}
 finally
  LeaveCriticalSection (sc_lock);
 end;
end; // BlockAlloc

function BlockInfo (lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD; stdcall;
var
   i, idx, pcnt: Integer;
   ofs: DWORD;
   atp, ptp: BYTE;
   p: Pointer;
begin
 result := 0;
 if not InBounds (lpAddress, 0) then exit;

 EnterCriticalSection (sc_lock);
 try

   lpBuffer.BaseAddress := AlignPtr(lpAddress);
   lpBuffer.AllocationBase := buffer;
   lpBuffer.RegionSize := 0;
   lpBuffer.State := 0;
   lpBuffer.Type_9 := 0;

   p := lpBuffer.BaseAddress;

   ofs := DWORD(p) - ptr_first;
   idx := Integer(ofs) div PAGE_SIZE;

   pcnt := 0;
   atp := 0;
   ptp := 0;

   for i := idx to PAGE_COUNT - 1 do
    begin
     atp := pages[i];
     if (i > idx) and (atp and $7F <> ptp) then break; // region changed

     ptp := atp;
     Inc (pcnt);

     if atp and PAGE_LAST <> 0 then break;

    end;


   if atp and PAGE_COMT <> 0 then
      lpBuffer.State := MEM_COMMIT;

   if atp and PAGE_RESV <> 0 then
      lpBuffer.State := lpBuffer.State or MEM_RESERVE;


   if lpBuffer.State = 0 then
    begin
     lpBuffer.State := MEM_FREE;
     lpBuffer.Protect := PAGE_NOACCESS;
    end
   else
    begin
     lpBuffer.Protect := PAGE_EXECUTE_READWRITE;
    end;

 finally
  LeaveCriticalSection (sc_lock);
 end;

end;


function BlockFree (p: Pointer; dwSize: Cardinal): Boolean;
var
   i, idx, freed: Integer;
   f, ofs: DWORD;
begin
 result := InBounds (p, dwSize);
 if not result then exit;



 EnterCriticalSection (sc_lock);
 try
   ofs := DWORD (p) - ptr_first;
   idx := Integer (ofs div PAGE_SIZE);

   freed := 0;

   for i := idx to PAGE_COUNT - 1 do
    begin
     f := pages [i];
     pages [i] := 0;
     Inc (freed, PAGE_SIZE);

     if (f = 0) or ( f and PAGE_LAST <> 0 ) then break;
    end;

   if freed > 0 then
    begin
     Windows.Beep (1000, 50);
     Inc (freed_sum, freed);
    end;
 finally
  LeaveCriticalSection (sc_lock);
 end;

end;



procedure FailExit;
var
   s: String;
begin
 s := 'GetLastError = ' + itos(GetLastError);
 OutputDebugString (PChar(s));
 ExitProcess (0);
end;

procedure FreeBuffer;
begin
 if buffer = nil then exit;
 if Windows.VirtualFree (buffer, 0, MEM_RELEASE) then
  begin
   Windows.Beep (300, 500);
   Windows.Beep (1000, 500);
   buffer := nil;
  end
 else
  begin
   FailExit;
  end;
end;

initialization

 if not IsConsole then
     AllocConsole;
 InitializeCriticalSection (sc_lock);

 buffer := Windows.VirtualAlloc(nil, BUFF_SIZE, MEM_COMMIT or MEM_RESERVE, PAGE_READWRITE);

{ ptr_first := Cardinal (buffer);
 ptr_first := AlignOffset (ptr_first);
 buffer := PByteVector (ptr_first);
 ptr_last := ptr_first + sizeof(buffer);

 // InitPages; }


finalization
 log_proc := nil;
 DeleteCriticalSection (sc_lock);
end.
