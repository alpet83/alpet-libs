unit UniArray;

interface
uses Windows, SysUtils, Classes, StrClasses, ContNrs, FastSync, MemStat, WinHeap, Algs, ArrayTypes;

{.$DEFINE SHOW_MEM_STAT}

const
     DATA_MEM_QUOTA = 1500 * 1024 * 1024;


type



    TDataMap = class;
    TDataPool = class;

    TMatrixArray = class (TMemoryStream)
    private
       // m_share: TStateMutex;
     sc_share: TCritSection;
     FTags: TStringTags;
     procedure SetReservCnt(const Value: Integer);
     function GetTags: TStringTags;
     procedure SetHeap(const Value: TWindowsHeap);
     function  GetTop: Integer; inline;         inline;
     function  GetBottom: Integer;              inline;
     function  GetLeft: Integer;                inline;
     function  GetRight: Integer;               inline;
     procedure SetBottom(const Value: Integer); inline;
     procedure SetLeft(const Value: Integer);   inline;
     procedure SetRight(const Value: Integer);  inline;
     procedure SetTop(const Value: Integer);    inline;

    protected
      FMakeInfo: String;
       FRowSize: Integer;
     FRowsCount: Integer;
      FRefCount: Integer;
     FReservCnt: Integer;    // сколько резервировать до нулевого элемента
     FProtected: Boolean;    // TODO: multithread access protection using
    FExtensible: Boolean;
     FIsInteger: Boolean;
       FIsFloat: Boolean;
    FValidRange: TIndexRange;
     FRectRange: TRect;
      pwContext: PChar;      // контекст обработки класса
      grow_step: Integer;
      LastSizePt: Integer;
         FOwner: TDataMap;
     FOwnerList: TObject;
          FName: String;
            FHeap: TWindowsHeap;
       bUseHeap: Boolean;
       bOwnedHeap: Boolean;
      FDataPool: TDataPool;
      FUnused: Boolean;
      FZeroInit: Boolean;

     function                   AddRow: Pointer;
     function                   GetRow (nRow: Integer): Pointer;

     function                   GetFirstRow: Pointer;
     function                   GetLastRow: Pointer;
     function                   Realloc(var NewCapacity: Longint): Pointer; override;
     procedure                  OnSizeChanging (newSize: Integer); virtual;
    public



     // iFirstValid, iLastValid: Integer;          // ограничение по "нормальным" данным

     Flags: DWORD;
     iRefOffset: Integer;       // опорное смещение, для совмещения индексных пространств массивов.
     iOrigin: Integer;
     indexList: array [0..7] of Integer; // для асинхронного чтения разными потребителями данных


     mem_stat_id: String;

     { props }

     property                   Capacity;
     property                   Count: Integer read FRowsCount; // Rows Count!

     property                   Extensible: Boolean read FExtensible write FExtensible;

     property                   GrowStep: Integer read grow_step write grow_step;

     property                   Heap: TWindowsHeap read FHeap write SetHeap;
     property                   IsInteger: Boolean read FIsInteger;
     property                   IsFloat: Boolean read FIsFloat;

     property                   FirstRow: Pointer read GetFirstRow;
     property                   LastRow: Pointer read GetLastRow;

     property                   Name: String read FName write FName;

     property                   OwnerList: TObject read FOwnerList write FOwnerList;
     property                   OwnerMap: TDataMap read FOwner write FOwner;
     property                   RefCount: Integer read FRefCount;
     property                   ReservedCount: Integer read FReservCnt write SetReservCnt;
     property                   RowSize: Integer read FRowSize;
     property                   Rows [Index: Integer]: Pointer read GetRow;
     property                   Tags: TStringTags read GetTags;

     property                   Unused: Boolean read FUnused;
     { Ranges }

     property                   iLeft: Integer read GetLeft write SetLeft;
     property                   iTop : Integer read GetTop write SetTop;
     property                   iRight: Integer read GetRight write SetRight;
     property                   iBottom: Integer read GetBottom write SetBottom;

     property                   RectRange: TRect read FRectRange write FRectRange;

     // // для графических/векторных применений

     property                   iFirstValid: Integer read FValidRange.iFirst write FValidRange.iFirst;
     property                   iLastValid:  Integer read FValidRange.iLast write FValidRange.iLast;
     property                   ValidRange: TIndexRange read FValidRange write FValidRange;


     property                   ZeroInit: Boolean read FZeroInit write FZeroInit;

     // { check index }
     { C & D }

     constructor                Create; overload; virtual;
     constructor                Create (nRowSize: Integer; AOwner: TDataMap = nil; bMakeHeap: Boolean = FALSE); overload; virtual;
     destructor                 Destroy;  override;

     { methods }

     function                   AddRef: Integer;

     function                   AddRows (cntRows: Integer): Pointer;

     function                   CheckIndex(nIndex: Integer; bAssert: Boolean = FALSE): Boolean;

     procedure                  Clear; virtual;

     procedure                  CorrectCount(new_count: Integer);
     procedure                  CopyItems (src: TMatrixArray; iFirst, iLast: Integer);
     procedure                  DeleteItems (iFirst, nCount: Integer); virtual;

     procedure                  FillData (v: BYTE = 0);
     procedure                  ImportIntItems (src: TMatrixArray; iFirst, iLast: Integer);

     procedure                  LimitData (cnt: Integer = -1);
     procedure                  Lock;

     procedure                  MoveItems (iFrom, nCount, nOffset: Integer);

     function                   ObjectId: String; virtual;
     procedure                  OptimizeMemUsage; virtual;

     function                   Pop: Pointer;

     procedure                  Sort(Compare: TListSortCompare); virtual;

     function                   Release: Integer;
     procedure                  SetBounds (nFirst, nLast, nset: Integer);
     procedure                  SetSize (NewSize: Longint); override;
     procedure                  SafeLoad (src: TMatrixArray);

     procedure                  Unlock;
     function                   ValidCount: Integer;
    end; { TMatrixArray }


    TMatrixArrayT < TItemType, PItemType, PListType > = class (TMatrixArray)
    private
     function GetItem (index: Integer): PItemType; inline;
     function GetItems: PListType; inline;

    public
     property          RawItems: PListType read GetItems;
     property          Items[index: Integer]: PItemType read GetItem; default;

     { C & D }
     constructor       Create; override;
     { methods }
     function          FirstItem: PItemType; inline;
     function          LastItem: PItemType; inline;
    end;

    TObjectListT < TClassName > = class (TObjectList)
    private

    protected
     function  GetObject(index: Integer): TClassName; inline;
    public

     property     Objects[index: Integer]: TClassName read GetObject;

    end;



    TArray32  = class (TMatrixArray)
    private

    protected
     function                   GetSingleData: PSingleArray; inline;
     function                   GetIntData: PIntArray;       inline;
     function                   GetPtrData: PPtrArray;       inline;
    public
     property                   FloatItems: PSingleArray read GetSingleData;
     property                   IntItems: PIntArray read GetIntData;
     property                   PtrItems: PPtrArray read GetPtrData;

     { c & d }
     constructor                Create; override;
     { methods }
     function                   AddFloat (f: Single): Integer;
     function                   AddInt (i: Integer): Integer;
     function                   AddPtr (p: Pointer): Integer;

     function                   LastFloatVal: Single;
     function                   LastIntVal: Integer;
    end;

    TArray64 = class (TMatrixArray)
    protected
     function                   GetDoubleData: PDoubleArray; inline;
     function                   GetIntData: PInt64Array; inline;
    public

     property                   FloatItems: PDoubleArray read GetDoubleData;
     property                   IntItems: PInt64Array read GetIntData;
     { c & d }
     constructor                Create; override;
     { methods }
     function                   AddFloat (f: Double): Integer;
     function                   AddInt (i: Int64): Integer;
    end; // TArray64


   TCustomPacket = class (TMatrixArray)
   protected

   public
    { C & D }
    constructor         Create; override;

    { Methods }



    procedure           PutAnsi (at: DWORD; const s: String; cntb: Integer);
    procedure           PutBYTE (at: DWORD; value: BYTE);
    procedure           PutWORD (at: DWORD; value: WORD);
    procedure           PutDWORD (at, value: DWORD);
    procedure           PutSingle (at: DWORD; value: Single);
    procedure           PutDouble (at: DWORD; value: Double);
    function            ResvPlace (at, cntb: DWORD): Pointer;

   end; // TCustomPacket



    TDataMap = class (TStrMap)  // карта массивов, для хранения произвольной информации в параллельном виде
    protected
     FArrayObjects: TObjectList;     // объекты, порожденые от класса TMatrixArray
     FDataPool: TDataPool;

     function                   GetArray (const sName: String): TMatrixArray;
     function                   GetArray32 (const sName: String): TArray32;
     function                   GetArray64 (const sName: String): TArray64;
     function                   GetObjectByName (const sName: String): TObject;
     procedure                  OnArrayFree (aobj: TMatrixArray);

    public
     maxItems: Integer;  // рекомендательное значение
     DataRef: TObject;   // объект источник к примеру
     preCreateItems: Integer;

     property                   Arrays [const sIndex: String]: TMatrixArray read GetArray;
     property                   Arrays32 [const sIndex: String]: TArray32 read GetArray32; default;
     property                   Arrays64 [const sIndex: String]: TArray64 read GetArray64;

     property                   NamedObjects [const sIndex: String]: TObject read GetObjectByName;

     property                   DataPool: TDataPool read FDataPool write FDataPool;

     { C & D }
     constructor                Create (AOwner: TObject = nil);
     destructor                 Destroy; override;
     { Methods }
     // для добавление внешних объектов, использовать AddObject напрямую требуется
     function                   AddArray (const sName: String; nRowSize: Integer): TMatrixArray;
     function                   AddArray32 (const sName: String; is_float: Boolean = TRUE): TArray32;
     function                   AddArray64 (const sName: String; is_float: Boolean = TRUE): TArray64;

     function                   AddObject (const sName: String; obj: TObject): Integer; override;


     function                   ArrayByIndex (nIndex: Integer): TMatrixArray;
     function                   A32 (nIndex: Integer): TArray32;
     function                   A64 (nIndex: Integer): TArray64;

     function                   Check: Boolean;

     procedure                  Clear; override;
     procedure                  ClearItems;
     function                   Dump: String;
     procedure                  Extent (addCount: Integer);     // расширение входящих расширяемых массивов

     procedure                  LimitArrays(fset: DWORD; cnt: Integer = -1);
     procedure                  RemoveLast(cnt: Integer; fset: DWORD);
     procedure                  OptimizeData (min_bytes: Integer);

    end; // TDataMap

  TDataCreator = class
  protected

  public
   is_float: Boolean;
   row_size: Integer;

   { C & D }
   constructor         Create;

   { methods }
   function            CreateData: TMatrixArray; virtual;
  end; // TDataCreator

    TDataPool = class (TObjectList)
    // THREAD UNSAFE DATA CACHE
    private
     FDataCreator: TDataCreator;
     procedure          SetDataCreator(const Value: TDataCreator);

    protected
     FMemoryLimit: Integer;
     procedure          SetMemoryLimit(const Value: Integer); virtual;

    public

     property           DataCreator: TDataCreator read FDataCreator write SetDataCreator;
     property           MemoryLimit: Integer read FMemoryLimit write SetMemoryLimit;

     { C & D }
     constructor        Create;
     destructor         Destroy; override;

     { methods }

     function           Allocate: TMatrixArray;

     function           CheckDC (row_size: Integer; is_float: Boolean): Boolean; inline;
     procedure          CheckMemoryLimit;

     procedure          OnDataRelease (ma: TMatrixArray);
    end; // TDataPool


    TObjectStorage = class ( TObjectList )
    private
     FDefaultSearch: TBSCompareFunc;
     FDefaultCompare: TListSortCompare;
     FLastSorted: Boolean;
     FMutex: THandle;
    protected
     procedure          Notify(Ptr: Pointer; Action: TListNotification); override;
    public
     { props }
     property           DefaultSearch: TBSCompareFunc read FDefaultSearch write FDefaultSearch;
     property           DefaultCompare: TListSortCompare read FDefaultCompare write FDefaultCompare;
     property           LastSorted: Boolean read FLastSorted;

     { C & D }

     constructor        Create ( AOwnsObjects: Boolean ); overload;
     constructor        Create; overload;
     destructor         Destroy; override;

     { methods }

     procedure          Clear; override;
     procedure          DefaultSort; inline;
     procedure          DoSort (SortCompare: TListSortCompare); virtual;

     function           Find (ex: Pointer; cmpFunc: TBSCompareFunc = nil; pa: PInteger = nil; pb: PInteger = nil): Integer;

     procedure          SortedInsert (obj: TObject);


     function           TryLock (dwTimeout: DWORD = MAXINT): Boolean;
     procedure          Unlock;
    end;


var
   gAllocSize: Integer = 0;


implementation
uses Misc, Math, ModuleMgr;


var
   gArrHeap: TWindowsHeap = nil;

{ TMatrixArray }

function TMatrixArray.AddRef: Integer;
begin
 result := InterlockedIncrement (FRefCount);
 FUnused := FALSE;
end;

function TMatrixArray.AddRow: Pointer;
var
   bcnt, newSize: Integer;
begin
 bcnt := (Count + 1) * FRowSize;
 newSize := Size;
 while (bcnt >= newSize) do Inc (newSize, grow_step);
 if newSize > Size then Size := newSize;
 Inc(FRowsCount);
 result := LastRow; // check alert prevention
end; // AddRow

function TMatrixArray.AddRows(cntRows: Integer): Pointer;
var i: Integer;
    bcnt: Integer;
    newSize: Integer;
begin
 result := nil;
 if (cntRows <= 0) then exit;
 bcnt := ( Count + cntRows ) * FRowSize;
 newSize := Size;
 while (bcnt > newSize) do Inc (newSize, grow_step);
 if newSize > Size then
    Size := newSize;
 i := FRowsCount;
 Inc(FRowsCount, cntRows);
 result := Rows [i]; // check alert prevention
end; // AddRows

function TMatrixArray.CheckIndex(nIndex: Integer; bAssert: Boolean): Boolean;
begin
 result := (nIndex >= 0) and (nIndex <= Count) and ( (nIndex + 1) * FRowSize <= Capacity);
 if bAssert then
   Assert (result, Format ('Outbound index for [%s] = %d, Count = %d ', [Name, nIndex, Count] ) );
end; // CheckIndex

procedure TMatrixArray.Clear;
begin
 FRowsCount := 0;
 Position := 0;
 iFirstValid := -1;
 iLastValid := -1;
 // if Size > 0 then FillChar (Memory^, Size, 0);
end;

procedure TMatrixArray.CopyItems(src: TMatrixArray; iFirst, iLast: Integer);
var
   pdst: Pointer;
   cnt_copy: Integer;
begin
 ASSERT (src.RowSize = RowSize, 'TMatrixArray.CopyItems: RowSize массивов не совпадают.');
 cnt_copy := iLast - iFirst + 1;
 pdst := AddRows (cnt_copy);
 SafeMove (src.GetRow(iFirst)^, pdst^, cnt_copy * RowSize);
end;

procedure TMatrixArray.CorrectCount(new_count: Integer);
var
  old_count: Integer;
begin
 old_count := Count;
 iLastValid := new_count - 1;
 iFirstValid := Min(iFirstValid, iLastValid);
 if new_count >= 0 then
  begin
   FRowsCount := new_count;
   if old_count * 2 < new_count then OptimizeMemUsage;
  end;
end; // CorrectCount

procedure TMatrixArray.ImportIntItems(src: TMatrixArray; iFirst, iLast: Integer);
var n, cnt: Integer;
    dst32: PSingleArray;
    dst64: PDoubleArray absolute dst32;
    src32: PIntArray;
    src64: PInt64Array absolute src32;
begin
 cnt := iLast - iFirst + 1;
 dst32 := AddRows (iLast - iFirst + 1);
 src32 := src.GetRow(iFirst);

 if RowSize = 4 then
  begin
   if (src.rowSize = 4) then
      for n := 0 to cnt - 1 do
          dst32[n] :=  src32 [n];
   if (src.rowSize = 8) then
      for n := 0 to cnt - 1 do
          dst32[n] :=  src64 [n];
  end
  else
  if RowSize = 8 then
  begin
   if (src.rowSize = 4) then
      for n := 0 to cnt - 1 do
          dst64 [n] :=  src32 [n];
   if (src.rowSize = 8) then
      for n := 0 to cnt - 1 do
          dst64 [n] :=  src64 [n];
  end;
end;


constructor TMatrixArray.Create(nRowSize: Integer; AOwner: TDataMap; bMakeHeap: Boolean);
begin
 if bMakeHeap then
   begin
    FHeap := TWindowsHeap.Create(ClassName + '.FHeap');
    bOwnedHeap := TRUE;
   end
 else
   begin
    FHeap := gArrHeap;
    bOwnedHeap := FALSE;
   end;  
 FOwner := AOwner;

 mem_stat_id := ClassName + '.data';
 pwContext := 'TMatrixArray.Create';
 Assert (nRowSize >= 1, ClassName + '.Create: wrong row-size = ' + IntToStr (nRowSize));
 FRowsCount := 0;
 FRowSize := nRowSize;
 grow_step := 0;   // in bytes
 while (grow_step < FRowSize * 16) do
          Inc (grow_step, 8192);
 // m_share := TStateMutex.Create;
 sc_share := TCritSection.Create(ClassName + '.sc_share');
 // Size := grow_step;
 FMakeInfo := ClassName + '.ct' + IntToStr(GetCurrentThreadId);
 FName := Format('[$%p]', [Pointer(self)]);

end;

constructor TMatrixArray.Create;
begin
 Create(1);
end;


destructor TMatrixArray.Destroy;
begin
 FTags.Free;
 pwContext := 'TMatrixArray.Destroy';
 ASSERT (Assigned(self), ClassName + '.Destroy = self is unassigned');
 ASSERT (Assigned(sc_share), ClassName + '.sc_share is unassigned!');
 try
   if Count > 0 then DeleteItems (0, Count);
   Lock;
   if Size > 8 * 1024 * 1024 then
      ODS('[~T].~C0F #MEMORY: ~C0A ' + ClassName + '@' + Name +
                  '~C0F instance destroyed, size_in_bytes =~C0D ' + IntToStr (Size) + '~C07');


   if Assigned (FOwner) then
      FOwner.OnArrayFree(self);

   if RefCount > 0 then
      ODS('[~T].~C0C #WARN: ' + ClassName +'.RefCount = ' + IntToStr(RefCount) + ' on destroy~C07');

   if Size > 0 then  SetSize(0);


   if bOwnedHeap then
     FHeap.Free;
   // m_share.Free;
   sc_share.Free;
   sc_share := nil;
   OwnerList := nil;
   OwnerMap := nil;
   pwContext := '';
   Name := '';
 except
  on E: Exception do
    PrintError('Exception catched in ' + ClassName + '.Destroy: ' + E.Message );
 end;
 inherited;
end; // Destroy

procedure TMatrixArray.FillData(v: BYTE);
var
   cb_fill: Integer;
begin
 if ( Count = 0 ) or ( Memory = nil ) then exit;
 cb_fill := Count * RowSize;  // only data fill!
 FillChar (Memory^, cb_fill, v);
end;




procedure TMatrixArray.DeleteItems(iFirst, nCount: Integer);
var
   iSource, cb_mov: Integer;
begin
 if (iFirst + nCount < Count) then
    begin
     // перемещение остатка в начало
     // 0 1 2 3 4 5 6 7 8 9 A B C D E F
     //      -4
     // 0 1 2 7 8 9 A B C D E F
     iSource := iFirst + nCount;
     cb_mov := (Count - iSource + 1) * FRowSize; // количество строк в остатке
     CopyMemory ( Rows [iFirst], Rows[iSource], cb_mov);
     {$IFOPT D+}
     ZeroMemory ( Rows[Count - nCount], Size - FRowSize * (Count - nCount) );     // зачистко
     {$ENDIF}
    end;
 Dec (FRowsCount, nCount);
end;



function TMatrixArray.GetBottom: Integer;
begin
 result := FRectRange.Bottom;
end;

function TMatrixArray.GetFirstRow: Pointer;
begin
 result := Memory;
end;

function TMatrixArray.GetLastRow: Pointer;
begin
 result := nil;
 if Count > 0 then
    result := GetRow (Count - 1);
end;

function TMatrixArray.GetLeft: Integer;
begin
 result := FRectRange.Left;
end;

function TMatrixArray.GetRight: Integer;
begin
 result := FRectRange.Right;
end;

function TMatrixArray.GetRow(nRow: Integer): Pointer;

begin
 pwContext := 'TMatrixArray.GetRow';
 CheckIndex (nRow);
 result := Ptr ( DWORD ( Memory ) + DWORD(nRow * FRowSize) );
 pwContext := 'free';
end; // GetRow

function TMatrixArray.GetTags: TStringTags;
begin
 if FTags = nil then FTags := TStringTags.Create();
 result := FTags;
end;




function TMatrixArray.GetTop: Integer;
begin
 result := FRectRange.Top;
end;

function TMatrixArray.ObjectId: String;
begin
 result := Format ('$%p', [Pointer(self)]) + '@' + FMakeInfo;
end;

procedure TMatrixArray.OnSizeChanging(newSize: Integer);
var
    idx: Integer;
begin
 // may check items for unreleased pointers
 idx := newSize div RowSize;
 if idx < Count then
   // 0 1 2 3 = 4
   FRowsCount := idx;
end;

procedure TMatrixArray.OptimizeMemUsage;
begin
 // if NewSize > 0 then  NewSize := (NewSize shr 10 + 1) shl 10; // 1 KiB round
 Size := Count * RowSize;
end;

function TMatrixArray.Pop: Pointer;
begin
 result := LastRow;
 if (Count > 0) then Dec (FRowsCount);
end;


function TMatrixArray.Realloc(var NewCapacity: Longint): Pointer;
const MemoryDelta = 1024;
begin
 result := Memory;
 try
    if (NewCapacity > 0) and (NewCapacity <> Size) then
       NewCapacity := (NewCapacity + (MemoryDelta - 1)) and not (MemoryDelta - 1);


    if NewCapacity <> Capacity then
    begin
      if NewCapacity = 0 then
      begin
        if bUseHeap then
           FHeap.ReleaseMem(Memory)
        else
           FreeMem(Memory);
        Result := nil;
      end else
      begin
       // allocating or reallocating
        if Capacity = 0 then
         begin
          if bUseHeap then
               Result := FHeap.AllocMem(NewCapacity)
           else
               Result := AllocMem (NewCapacity)
         end
        else
         begin
          if bUseHeap then
               Result := FHeap.ReAllocMem(Result, NewCapacity)
          else
               ReallocMem(Result, NewCapacity);
         end;
      end;
    end;
 except
  on E: Exception do
   PrintError('Exception catched in TMatrixArray.Realloc(' + IntToStr(NewCapacity) + '): ' + E.Message);
 end;
end;

function TMatrixArray.Release: Integer;
begin
 result := 0;
 if FRefCount > 0 then
    result := InterlockedDecrement (FRefCount);
 if result <= 0 then
  begin
   if FDataPool = nil then Free
   else
    begin
     FOwner := nil;
     FDataPool.OnDataRelease (self);
    end;
  end;
end; // Release

// TMatrixArray.ReAlloc

procedure TMatrixArray.SafeLoad(src: TMatrixArray);
begin
 if src.RowSize <> src.RowSize then exit;

 Lock;
 Position := 0;
 src.Position := 0;
 LoadFromStream (src);
 FRowsCount := src.Count;
 Unlock;
end;

procedure TMatrixArray.SetBottom(const Value: Integer);
begin
 FRectRange.Bottom := Value;
end;

procedure TMatrixArray.SetBounds(nFirst, nLast, nset: Integer);
begin
 if nset and 2 > 0 then
   begin
    iFirstValid := nFirst;
    iLastValid := nLast;
   end;
end;



procedure TMatrixArray.SetHeap(const Value: TWindowsHeap);
begin
 if bOwnedHeap then
    Heap.Free;
 bOwnedHeap := FALSE;
 FHeap := Value;
end;

procedure TMatrixArray.SetLeft(const Value: Integer);
begin
 FRectRange.Left := Value;
end;

procedure TMatrixArray.SetReservCnt(const Value: Integer);
begin
 Clear;
 FReservCnt := Value;
 SetSize (0);
end;

procedure TMatrixArray.SetRight(const Value: Integer);
begin
 FRectRange.Right := Value;
end;

function TMatrixArray.ValidCount: Integer;
begin
 if Memory = nil then
    result := 0
 else
    result := iLastValid - iFirstValid + 1;
end;

procedure TMatrixArray.SetSize(NewSize: Integer);
var
  n_grow, old, used: Integer;
  {$IFDEF SHOW_MEM_STAT}
  diff: Integer;
  {$ENDIF}
  //s: String;
begin
 Lock;
 try
   n_grow := NewSize - LastSizePt;
   if n_grow > 128 * 1024 then
      LastSizePt := NewSize;
   if n_grow < 0 then LastSizePt := NewSize;
   old := Capacity;

   try
    OnSizeChanging (newSize);
    FRowsCount := Min (FRowsCount, Size div RowSize); // коррекция значения

    inherited SetSize(newSize);
    InterlockedExchangeAdd (gAllocSize, -old);
    InterlockedExchangeAdd (gAllocSize, Capacity);
    used := RowSize * Count;

    if ZeroInit and (Capacity > used) then
       FillChar (Rows[Count]^, Capacity - used, 0);

     ASSERT (gAllocSize < DATA_MEM_QUOTA, 'Выработана квота для использования памяти. Сейчас использованно ' + IntToStr(gAllocSize shr 20) + ' МиБ памяти.');

   except
    on E: Exception do
     PrintError('Перехвачено исключение при попытке изменить размер массива до ' +
                  IntToStr(NewSize div 1024) +' КиБ: ' + E.Message);
   end;

   {$IFDEF SHOW_MEM_STAT}
   Assert ( GetCollector() <> nil, 'function GetCollector returned nil');
   diff := Capacity - old;
   if diff <> 0 then
      GetCollector.LogAlloc(mem_stat_id, diff);
   {$ENDIF}

 finally
  Unlock;
 end;
end;

procedure TMatrixArray.SetTop(const Value: Integer);
begin
 FRectRange.Top := Value;
end;

procedure TMatrixArray.Sort(Compare: TListSortCompare);
var
   lsrt: TList;
   pdata, pmem: PLargeByteArray;
   cnt, n, ofs: Integer;

begin
 if ( Count <= 1 ) or ( RowSize <= 0 ) then exit;
 Lock;
 pdata := nil;
 lsrt := nil;
 try
  pmem := Memory;
  cnt := Count;

  GetMem (pdata, cnt * RowSize);
  lsrt := TList.Create;
  // создание полной копии данных
  CopyMemory (pdata, pmem, cnt * RowSize);
  ofs := 0;
  // копировать адреса элементов в буфере
  for n := 0 to cnt - 1 do
   begin
    lsrt.Add ( @pdata[ofs] );
    Inc (ofs, RowSize);
   end;

  // сортировать адреса
  lsrt.Sort (Compare);

  ofs := 0;
  // копировать элементы по адресам из буфера
  for n := 0 to lsrt.Count - 1 do
   begin
    SafeMove ( lsrt[n]^, pmem[ofs], RowSize);
    Inc (ofs, RowSize);
   end;

  Assert ( Count = cnt, ' Count changed while sorting operation ' );
 finally
  Unlock;
  FreeMem (pdata);
  lsrt.Free;
 end;
end;

procedure TMatrixArray.LimitData(cnt: Integer);
begin
 if cnt < 0 then cnt := iLastValid + 1;
 FRowsCount := Max(0, Min (FRowsCount, cnt));
end;

procedure TMatrixArray.Lock;
begin
 FProtected := TRUE;
 sc_share.Lock('MA-Lock', 50);
end;

procedure TMatrixArray.MoveItems(iFrom, nCount, nOffset: Integer);
begin
 // WARN: Very unsafe code
 MoveMemory ( Rows[iFrom + nOffset], Rows[iFrom], nCount * RowSize );
end;

procedure TMatrixArray.Unlock;
begin
 sc_share.Unlock;
end;


{ TArray32 }


function TArray32.AddFloat(f: Single): Integer;
begin
 FIsFloat := TRUE;
 FIsInteger := FALSE;
 PSingle (AddRow)^ := f;
 result := Count - 1;
end; // AddFloat

function TArray32.AddInt(i: Integer): Integer;
begin
 FIsFloat := FALSE;
 FIsInteger := TRUE;
 PInteger (AddRow)^ := i;
 result := Count - 1;
end; // AddInt

function TArray32.AddPtr(p: Pointer): Integer;
begin
 FIsFloat := FALSE;
 FIsInteger := TRUE;
 PPointer (AddRow)^ := p;
 result := Count - 1;
end;

constructor TArray32.Create;
begin
 inherited Create (4);
end;

function TArray32.GetSingleData: PSingleArray;
begin
 result := Memory;
end;

function TArray32.GetIntData: PIntArray;
begin
 result := Memory;
end;

function TArray32.GetPtrData: PPtrArray;
begin
 result := Memory;
end;

function TArray32.LastFloatVal: Single;
begin
 result := PSingle(LastRow)^;
end;

function TArray32.LastIntVal: Integer;
begin
 result := PInteger (LastRow)^;
end;

{ TDataMap }

function TDataMap.A32(nIndex: Integer): TArray32;
begin
 result := TArray32 (Objects [nIndex]);
end;

function TDataMap.A64(nIndex: Integer): TArray64;
begin
 result := TArray64 (Objects [nIndex]);
end;

function TDataMap.AddArray(const sName: String; nRowSize: Integer): TMatrixArray;
begin
 result := TMatrixArray.Create(nRowSize, self); // special array

 result.Name := sName;
 result.FOwner := self;
 result.Extensible := TRUE;
 if preCreateItems > 0 then
    result.AddRows (preCreateItems - result.Count);

 AddObject (sName, result);
end; // AddArray

function TDataMap.AddArray32;
begin
 if (DataPool <> nil) and ( DataPool.CheckDC (4, is_float) ) then
     result := TArray32 ( DataPool.Allocate )
 else
    result := TArray32.Create;
 result.FOwner := self;
 result.Name := Name + '.' + sName;
 result.FIsFloat := is_float;
 result.FIsInteger := not is_float;
 result.Extensible := TRUE;
 if preCreateItems > 0 then
    result.AddRows (preCreateItems - result.Count);

 AddObject (sName, result);
end; // AddArray32

function TDataMap.AddArray64;
begin

 if (DataPool <> nil) and ( DataPool.CheckDC (8, is_float) ) then
     result := TArray64 ( DataPool.Allocate )
 else
     result := TArray64.Create;
 result.Name := sName;
 result.FOwner := self;
 result.FIsFloat := is_float;
 result.FIsInteger := not is_float;
 result.Extensible := TRUE;
 if preCreateItems > 0 then
    result.AddRows (preCreateItems - result.Count);

 AddObject (sName, result);
end; // AddArray64

function TDataMap.AddObject(const sName: String; obj: TObject): Integer;
begin
 if obj is TMatrixArray then
    FArrayObjects.Add(obj); 
 result := inherited AddObject (sName, obj);
end;

function TDataMap.ArrayByIndex(nIndex: Integer): TMatrixArray;
begin
 result := TMatrixArray ( Objects [nIndex] );
end;

function TDataMap.Check: Boolean;
var n: Integer;

begin
 result := TRUE;
 for n := 0 to Count - 1 do
     result := result and ( Objects[n] <> nil );
end;

procedure TDataMap.ClearItems;
var n: Integer;
begin
 for n := 0 to Count - 1 do
  if Objects [n] is TMatrixArray then
     TMatrixArray ( Objects [n] ).Clear;
end;

constructor TDataMap.Create;
begin
 inherited Create (AOwner);
 OwnsObjects := TRUE;
 MaxItems := MAXINT;
 FArrayObjects := TObjectList.Create (FALSE);
 Sorted := TRUE;                // fast access
end;

destructor TDataMap.Destroy;
begin
 Clear;

 FArrayObjects.Clear;
 inherited;
 FArrayObjects.Free;
  // hmmm..
end; // destroy

procedure TDataMap.Clear;
var
   n, cb_size: Integer;
   obj: TObject;
begin
 // оценка занятой памяти
 {$IFOPT D+}
 cb_size := 0;
 for n := 0 to FArrayObjects.Count - 1 do
     Inc (cb_size, TMatrixArray(FArrayObjects[n]).Size);
 if cb_size > 16 * 1024 * 1024 then
    ODS('[~T]. #MEMORY: Releasing DataMap.' + Name + ', with allocated data size = ' + IntToStr(cb_size div 1024) + ' KiB');
 {$ENDIF}


 for n := Count - 1 downto 0 do
  begin
   obj := Objects [n];
   if not (obj is TMatrixArray) then continue;
   with TMatrixArray (obj) do
        while Release > 0 do;
   Objects[n] := nil;
  end;

 inherited;
end; // Clear

function TDataMap.Dump: String;
var n: Integer;
begin
 for  n := 0 to Count - 1 do
   if Objects [n] <> nil then
   with ArrayByIndex (n) do
     ODS(Format('  %d. %s = %s[$%p]'#13#10, [n, Strings[n], ClassName, Pointer (Objects[n]) ]));
end;

procedure TDataMap.Extent(addCount: Integer);
var n: Integer;

begin
 // DANGEROUS method - some object may be removed from self, but leaved in FArrayObjects
 for n := 0 to FArrayObjects.Count - 1 do
 with TMatrixArray (FArrayObjects [n] ) do
  begin
   if Extensible then SetSize (Size + RowSize * addCount);
  end;
end; // Extent

function TDataMap.GetArray(const sName: String): TMatrixArray;
var obj: TObject;
begin
 result := nil;
 obj := NamedObjects [sName];
 if obj = nil then exit;
 ASSERT (obj is TMatrixArray, 'object class is not  TMatrixArray');
 result := TMatrixArray (obj);
end; // GetArray

function TDataMap.GetArray32(const sName: String): TArray32;
var obj: TObject;
begin
 result := nil;
 obj := NamedObjects [sName];
 if obj = nil then exit;
 ASSERT (obj is TArray32, 'object class is not TArray32');
 result := TArray32 (obj);
end; // GetArray32

function TDataMap.GetArray64(const sName: String): TArray64;
var obj: TObject;
begin
 result := nil;
 obj := NamedObjects [sName];
 if obj = nil then exit;
 ASSERT (obj is TArray64, 'object class is not TArray64');
 result := TArray64 (obj);
end; // GetArray64

function TDataMap.GetObjectByName(const sName: String): TObject;
var i: Integer;
begin
 result := nil;
 i := IndexOf (sName);
 if i >= 0 then result := Objects [i];
end;

procedure TDataMap.LimitArrays(fset: DWORD; cnt: Integer);
var n: Integer;
    a: TMatrixArray;
begin
 for n := 0 to Count - 1 do
 if Assigned (Objects [n]) then
  begin
   a := self.ArrayByIndex(n);
   if (a.Flags and fset = 0) then continue;
   a.LimitData (cnt);
  end;
end;

procedure TDataMap.OnArrayFree(aobj: TMatrixArray);
begin
 FArrayObjects.Remove(aobj); 
end;

procedure TDataMap.OptimizeData(min_bytes: Integer);
var
   n: Integer;
   obj: TObject;
   ma: TMatrixArray;
begin
 for n := 0 to Count - 1 do
  begin
   obj := Objects [n];
   if (obj <> nil) and (obj is TMatrixArray) then else continue;
   ma := ArrayByIndex (n);
   if ma.Capacity > min_bytes then
      ma.OptimizeMemUsage;
  end;

end;

procedure TDataMap.RemoveLast(cnt: Integer; fset: DWORD);
var
   na, n: Integer;
   ma: TMatrixArray;
begin
 for na := 0 to Count - 1 do
 if Objects [na] <> nil then
  begin
   ma := ArrayByIndex (na);
   if (ma.flags and fset = 0) then continue;
   for n := 0 to cnt - 1 do ma.Pop;
  end;
end;

{ TArray64 }

function TArray64.AddFloat(f: Double): Integer;
begin
 FIsFloat := TRUE;
 FIsInteger := FALSE;

 result := Count;
 PDouble (AddRow)^ := f;
end;

function TArray64.AddInt(i: Int64): Integer;
begin
 FIsFloat := FALSE;
 FIsInteger := TRUE;
 result := Count;
 PInt64 (AddRow)^ := i;
end;

constructor TArray64.Create;
begin
 inherited Create (8);
end;

function TArray64.GetDoubleData: PDoubleArray;
begin
 result := Memory;
end;

function TArray64.GetIntData: PInt64Array;
begin
 result := Memory;
end;


{ TCustomPacket }

constructor TCustomPacket.Create;
begin
 inherited Create(1);
end;

procedure TCustomPacket.PutAnsi(at: DWORD; const s: String; cntb: Integer);
var
   pdst, psrc: PAnsiChar;

begin
 pdst := ResvPlace(at, cntb);
 psrc := PAnsiChar (AnsiString (s));
 SafeMove (psrc^, pdst, Min(Length(s), cntb) );

end; // PutAnsi

procedure TCustomPacket.PutBYTE(at: DWORD; value: BYTE);
var pb: PByte;
begin
 pb := ResvPlace (at, 1);
 pb^ := value;
end;

procedure TCustomPacket.PutDouble(at: DWORD; value: Double);
var pd: PDouble;
begin
 pd := ResvPlace (at, sizeof (value));
 pd^ := value;
end;

procedure TCustomPacket.PutDWORD(at, value: DWORD);
var pd: PDWORD;
begin
 pd := ResvPlace (at, sizeof (value));
 pd^ := value;
end;

procedure TCustomPacket.PutSingle(at: DWORD; value: Single);
var ps: PSingle;
begin
 ps := ResvPlace (at, sizeof (value));
 ps^ := value;
end;

procedure TCustomPacket.PutWORD(at: DWORD; value: WORD);
var pw: PWORD;
begin
 pw := ResvPlace (at, sizeof (value));
 pw^ := value;
end;

function TCustomPacket.ResvPlace(at, cntb: DWORD): Pointer;
var new_size: Integer;
begin
 new_size := at + cntb;
 if new_size > Count then
   begin
    result := AddRows (new_size - Count);
    FillChar (result^, new_size - Count, 0);
   end;
 result := @PByteArray (Memory) [at];
end; // ResvPlace

{ TDataPool }

constructor TDataPool.Create;
begin
 inherited Create (TRUE);
 FDataCreator := TDataCreator.Create;
 FMemoryLimit := 64 * 1024 * 1024; // 64 MiB default
end;

destructor TDataPool.Destroy;
var
   n: Integer;
   cb_size: Integer;
begin
 cb_size := 0;
 for n := Count - 1 downto 0 do
  if self[n] is TMatrixArray then
   with TMatrixArray (self[n]) do Inc (cb_size, Capacity);

 if cb_size > 1024 * 1024 then
   ODS('[~T/~I]. #PERF: DataPool consist arrays with summary capacity = ~C0D' +
        IntToStr(cb_size div 1024) + '~C07 KiB' );


 FDataCreator.Free;
 inherited;
end;

procedure TDataPool.OnDataRelease(ma: TMatrixArray);
begin
 ma.FUnused := TRUE;
 CheckMemoryLimit;
end;

function TDataPool.Allocate: TMatrixArray;
var n: Integer;
begin
 // выборка незанятого массива, или создание нового
 for n := Count - 1 downto 0 do
  begin
   result := TMatrixArray (self[n]);
   if not result.Unused then continue;
   result.AddRef;
   exit;
  end;

 result := DataCreator.CreateData;
 result.FDataPool := self;
 result.AddRef;                    // initial reference
 Add (result);
end; // Allocate

function TDataPool.CheckDC(row_size: Integer; is_float: Boolean): Boolean;
begin
 result := (DataCreator.is_float = is_float) and (DataCreator.row_size = row_size);
end;

procedure TDataPool.CheckMemoryLimit;
var
   n, cum_alloc: Integer;
   ma: TMatrixArray;

begin
  cum_alloc := 0;
  for n := Count - 1 downto 0 do
   begin
    ma := TMatrixArray ( self[n] );
    Inc (cum_alloc, ma.Capacity);
    if (cum_alloc > MemoryLimit) and (ma.Unused) then Delete (n);
   end;
end; // CheckMemoryLimit

procedure TDataPool.SetDataCreator(const Value: TDataCreator);
begin
 if Assigned (FDataCreator) then
    FDataCreator.Free;
 FDataCreator := Value;
end;

procedure TDataPool.SetMemoryLimit(const Value: Integer);
begin
  FMemoryLimit := Value;
  CheckMemoryLimit;
end; // SetMemoryLimit

{ TDataCreator }

constructor TDataCreator.Create;
begin
 inherited Create;
 is_float := TRUE;
 row_size := 4;
end;

function TDataCreator.CreateData: TMatrixArray;
begin
 result := nil;
 case row_size of
  4: result := TArray32.Create;
  8: result := TArray64.Create;
 end;
 if result <> nil then
  begin
   result.FIsInteger := not is_float;
   result.FIsFloat := is_float;
  end;
end; // CreateData



function OnModuleRqs (md: TModuleDescriptor; rqs, flags: DWORD): Boolean;
begin
 result := FALSE;
 case rqs of
  MRQ_INITIALIZE:
    begin
     result := (gArrHeap <> nil);
     if (not result) then exit;
     gArrHeap := TWindowsHeap.Create('_globalArrayHeap');
     md.Globals.Add(gArrHeap);
    end;
  MRQ_FINALIZE:
    begin
     result := (nil = gArrHeap);
     if (not result) then exit;
     gArrHeap := nil;
    end;
 end; // case
end; // OnModuleRqs

{ TObjectStorage }

constructor TObjectStorage.Create(AOwnsObjects: Boolean);
begin
 inherited Create (AOwnsObjects);
 FMutex := CreateMutex (nil, FALSE, nil);
end;

procedure TObjectStorage.Clear;
begin
 inherited Clear;
 FLastSorted := TRUE;
end;

constructor TObjectStorage.Create;
begin
 inherited Create;
 FMutex := CreateMutex (nil, FALSE, nil);
end;

procedure TObjectStorage.DefaultSort;
begin
 Assert (Assigned (DefaultCompare), 'DefaultCompare = nil');
 DoSort (DefaultCompare);
end;

destructor TObjectStorage.Destroy;
begin
 TryLock(100);
 CloseHandle (FMutex);
 inherited;
end;

procedure TObjectStorage.DoSort(SortCompare: TListSortCompare);
begin
 Sort (SortCompare);
 FLastSorted := TRUE;
end;

function TObjectStorage.Find(ex: Pointer; cmpFunc: TBSCompareFunc; pa, pb: PInteger): Integer;
var
   n: Integer;
begin
 result := -1;

 if not Assigned (cmpFunc) then
    cmpFunc := DefaultSearch;

 if LastSorted then
    result := Algs.BinarySearch (self.List, ex, Count, cmpFunc, pa, pb)
 else
  // значения pa, pb не устанавливаются при полном поиске
  begin
   if pa <> nil then pa^ := -1;
   if pb <> nil then pb^ := -1;

   for n := 0 to Count - 1 do
    if cmpFunc (n, List, ex) = 0 then
       begin
        result := n;
        exit;
       end;
  end;
end;

procedure TObjectStorage.Notify(Ptr: Pointer; Action: TListNotification);
begin
 inherited Notify (Ptr, Action);
 if Action = lnAdded then
    FLastSorted := ( Count <= 1 );
end;

procedure TObjectStorage.SortedInsert(obj: TObject);
var
   n, a, b: Integer;
begin
 Assert (Assigned (DefaultSearch), 'DefaultSearch = nil');
 a := -1;
 b := -1;

 if Count = 0 then
  begin
   Add (obj);
   exit;
  end;

 if not LastSorted then
    DefaultSort;

 { ведется поиск оптимальной позиции, для выполнения условия [a] <= [n] <= [b]
   функция поиска возвращает:
    if [n] = ex then 0
    if [n] > ex then +1
    if [n] < ex then -1


   test:  1  3  4  8  10
   ex = 5
   res:  -1 -1 -1 +1  +1

   Вставлять новый объект нужно, после того как пойдут нули или плюсы
 }

 Find (obj, DefaultSearch, @a, @b);

 a := Max (0, a - 1);
 b := Min (Count - 1, b + 1);

 for n := a to b do
  if DefaultSearch (n, List, obj) >= 0 then
   begin
    Insert (n, obj);
    FLastSorted := TRUE;
    exit;
   end;

 Add (obj); // добавить в конец, раз не удалось воткнуть ранее
 FLastSorted := TRUE;
end;

function TObjectStorage.TryLock(dwTimeout: DWORD): Boolean;
begin
 result := (WAIT_OBJECT_0 = WaitForSingleObject (FMutex, dwTimeout));
end;

procedure TObjectStorage.Unlock;
begin
 ReleaseMutex (FMutex);
end;

{ TMatrixArrayT<TItemType, PListType> }

constructor TMatrixArrayT<TItemType, PItemType, PListType>.Create;
begin
 inherited Create ( sizeof (TItemType) );
end;

function TMatrixArrayT<TItemType, PItemType, PListType>.FirstItem: PItemType;
begin
 Move ( Memory, result, sizeof(Pointer) );
end;

function TMatrixArrayT<TItemType, PItemType, PListType>.GetItem(index: Integer): PItemType;
var
   p: Pointer;
begin
 p := GetRow(index);
 Move ( p, result, sizeof(Pointer) );
end;

function TMatrixArrayT<TItemType, PItemType, PListType>.GetItems: PListType;
begin
 Move ( Memory, result, sizeof(Pointer) );
end;

function TMatrixArrayT<TItemType, PItemType, PListType>.LastItem: PItemType;
var
   p: Pointer;
begin
 p := LastRow;
 Move ( p, result, sizeof(Pointer) );
end;

{ TObjectListT<TClassName> }

function TObjectListT<TClassName>.GetObject(index: Integer): TClassName;
var
   item: TObject;
begin
 item := Items[index];
 System.Move(item, result, sizeof(Pointer)); // usafe typecase
end;

initialization
 RegModule ( 'UniArray', 'misc,MemStat', OnModuleRqs );
 InitializeModule ('UniArray');
finalization
 FinalizeModule ('UniArray');
end.
