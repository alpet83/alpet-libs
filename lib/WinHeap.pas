unit WinHeap;
{ Класс виндовой кучи памяти для аллокации древоподобных структур данных }
interface
uses Algs, Windows, SysUtils, Classes, Misc, ContNrs, DateTimeTools, FastSync;

const

    HEAP_CREATE_ENABLE_EXECUTE        = $00040000;
    HEAP_GENERATE_EXCEPTIONS          = $00000004;
    HEAP_NO_SERIALIZE                 = $00000001;
    HEAP_REALLOC_IN_PLACE_ONLY        = $00000010;


type
    QWORD = Int64;

    TFastMemAllocator = class;

    TBaseMM = record
              ud: Pointer;
       alloc_mem: function  ( ud: Pointer; cb: DWORD ): Pointer;
        free_mem: procedure ( ud, p: Pointer; cb: DWORD );
    end; // TBaseMM

    PBaseMM = ^TBaseMM;

    TMemBlock = packed record
     pp: Pointer;
     sz: DWORD;
    end;

    PMemBlock = ^TMemBlock;


    TMemBlockList = class (TList)
    private
     function GetItem(index: Integer): PMemBlock; inline;
    protected
     FBlocks: array of TMemBlock;
     FBCount: Integer;
     FBSize: Integer;

     procedure          Resize ( newSize: Integer );

    public

     property           Items [index: Integer]: PMemBlock read GetItem;

     { C & D }
     constructor        Create;
     destructor         Destroy; override;
     { Methods }

     function           AddBlock (p: Pointer; sz: DWORD): PMemBlock;
     procedure          Reindex;

    end; // TMemBlockList

    TWindowsHeap = class
    private
    protected
     FOptions: DWORD;
     FHandle: THandle;
     FName: String;
     FSize, FMaxSize: Integer;

    public

     property   Handle: THandle read FHandle;
     property   MaxSize: Integer read FMaxSize;
     property   Name: String read FName;
     property   Size: Integer read FSize;

     { C & D }
     constructor        Create(const sName: String;
                               flOptions: DWORD = HEAP_GENERATE_EXCEPTIONS or HEAP_CREATE_ALIGN_16 or HEAP_ZERO_MEMORY;
                               dwInitialSize: DWORD = 64 * 1024;
                               dwMaximumSize: DWORD = 0);
     destructor         Destroy; override;
     { Methods }
     function           AllocMem(dwBytes: DWORD; dwFlags: DWORD = HEAP_ZERO_MEMORY): Pointer;
     function           BlockSize(p: Pointer): Integer;
     function           ReAllocMem(p: Pointer; dwNewSize: DWORD): Pointer;
     procedure          ReleaseMem(p: Pointer; old_size: Integer = -1);

    end;


    TMemPage = record
        tag: array [0..3] of AnsiChar;
        act: array [0..3] of AnsiChar;
        fma: TFastMemAllocator;
       used: Integer;  // использовано из заблокированного
      freed: Integer;  // освобождено
      index: Integer;  // заблокировано
  alloc_cnt: Integer;  // количество выделенных блоков
   free_cnt: Integer;  // кол-во освобожденных блоков

       size: Integer;
      owner: DWORD; // ThreadID
   ptr_last: DWORD;
  free_last: NativeUInt;
      fblks: packed array [0..255] of WORD;
        crc: DWORD;

       data: packed array [$000..$FFFF] of BYTE; // initial array 64K

     function  blocks: Integer; inline;
     function  free: Integer; inline; // свободно по смещению index
     function  BlockAlloc ( cb_alloc: Integer ): Pointer;
     function  CalcCRC ( upd: Boolean = TRUE ): DWORD;
     function  FreeSaldo: DWORD;
     function  FormatInfo: String;
     function  LastAction: String;
     procedure OnFree ( cb: DWORD );
     function  TraceBlocks ( max_trace: Integer = 100; incl_free: Boolean = TRUE ): String;
     procedure Reset;
    end;

    PMemPage = ^TMemPage;

    TPtrDecoder = function (p: Pointer): String;

    // thread-unsafe memory allocation class
    TFastMemAllocator = class
    private
        FName: String;
      FShared: Boolean;
     function  AllocPage(cbData: DWORD): PMemPage;
     function  AllocLarge (cbSize: DWORD): Pointer;


     procedure CheckPage ( mp: PMempage ); inline;
     procedure FreePage ( i: Integer; lst: TList = nil );
     function  GetMemMgr: PBaseMM;
     procedure OptimizePages;
     procedure DistributeEmptyPages (lst: TList; min_free: Integer);
     procedure SavePage ( mp: PMemPage );
     function  GetPage(index: Integer): PMemPage; inline;


    protected
     FSmallPages: TList;

       FMemPages: TList;
       FMemCache: TList;
           FHeap: TWindowsHeap;
           FEasy: Boolean;

      FFreedSize: DWORD;
         FWorkMM: TBaseMM;
       FOwnerTID: DWORD;

              pt: TProfileTimer;

       last_page: PMemPage;
     lfreed_page: PMemPage;
      last_block: PMemBlock;

            sc_share: TCritSection;
     FSmallBlockSize: DWORD;

     MinPageAddr, MaxPageAddr: NativeUInt;

    public
     alloc_rqs, free_rqs, realloc_rqs: Integer;
       mfree_leak: Integer;
       alloc_peak: Int64;
     alloc_blocks: Integer;
     freed_blocks: Integer;
      total_alloc: Int64;
      total_freed: Int64;
         dbg_flag: Integer;
    dbg_free_byte: BYTE;
          decoder: TPtrDecoder;
             tags: array [0..7] of Pointer;

     { props }

     property           Easy: Boolean read FEasy write FEasy;

     property           FreedSize: DWORD read FFreedSize;
     property           Heap: TWindowsHeap read FHeap;
     property           MemPages: TList read FMemPages;
     property           Pages [index: Integer]: PMemPage read GetPage;
     property           Name: String read FName;
     property           Shared: Boolean read FShared write FShared; // disable asserts with multithread access.
     property           WorkMM: PBaseMM read GetMemMgr;


     property           SmallBlockSize: DWORD read FSmallBlockSize write FSmallBlockSize;
     { C & D }
     constructor        Create (const AName: String; bMakeHeap: Boolean = FALSE; bEasy: Boolean = FALSE);
     destructor         Destroy; override;

     { methods }
     function           AllocMem (cbSize: DWORD): Pointer;
     function           Allocated: Integer;
     function           Capacity: Integer;
     procedure          Clear (bFree: Boolean = FALSE); virtual;
     function           FreePagesCount: Integer;


     function           FreeMem (p: Pointer; cbSize: DWORD): Boolean;
     function           HeapAvail: Boolean;

     procedure          OptimizeFreed;
     function           ReAllocMem (p: Pointer; oldSize, cbSize: DWORD): Pointer;
     function           SetOwnerThread (tid: DWORD): DWORD;


    end; // TFastMemAlloc

implementation
uses Math, MemStat;

const
   BLOCK_HEADER_SIZE = sizeof (TMemBlock);
            GEBIBYTE = 1 shl 30;


var
   DefPage: TMemPage;

const
      PAGE_DATA_SIZE = sizeof(DefPage.data);
     MIN_LARGE_BLOCK = PAGE_DATA_SIZE div 2;
    PAGE_HEADER_SIZE = sizeof(DefPage) - PAGE_DATA_SIZE;


procedure _nop;
asm
 nop
end;

{ THeap }

constructor TWindowsHeap.Create(const sName: String; flOptions, dwInitialSize,  dwMaximumSize: DWORD);
begin
 FOptions := flOptions;
 FName := sName;
 FHandle := HeapCreate (flOptions, dwInitialSize, dwMaximumSize);
 if Handle = 0 then
   PrintError ('HeapCreate returned 0, LastError: ' + err2str (GetLastError));
end;

destructor TWindowsHeap.Destroy;
begin
 if Handle <> 0 then
  begin
   if (MaxSize > 0)  then
       ODS(Format('[~T]. #DBG: Destroying heap~C0A %40s~C07, size = %10d KiB, max-size = %10d KiB,  handle = $%x',
                  [Name, Size div 1024, MaxSize div 1024, Handle]));
   HeapDestroy (Handle);
   FHandle := 0;
  end;
 inherited;
end;


function TWindowsHeap.BlockSize(p: Pointer): Integer;
begin
 result := 0;
 try
  result := HeapSize(FHandle, 0, p);
 except
  on E: Exception do
     OnExceptLog (ClassName + '.BlockSize', E);
 end;
end;

// -------- methods ------------- //
function TWindowsHeap.AllocMem(dwBytes, dwFlags: DWORD): Pointer;
begin
 Assert ( Handle <> 0, ClassName + '.AllocMem Handle = 0');
 result := HeapAlloc(FHandle, dwFlags, dwBytes);
 if result <> nil then
    Inc(FSize, BlockSize(result) );
 FMaxSize := Max(FMaxSize, FSize);
end;


function TWindowsHeap.ReAllocMem(p: Pointer; dwNewSize: DWORD): Pointer;
var old: Integer;
begin
 Assert ( Handle <> 0, ClassName + '.ReAllocMem Handle = 0');
 old := BlockSize(p);
 result := HeapReAlloc(FHandle, FOptions and 1, p, dwNewSize);
 Inc (FSize, BlockSize(result) - old);
 FMaxSize := Max(FMaxSize, FSize);
end;

procedure TWindowsHeap.ReleaseMem(p: Pointer; old_size: Integer = -1);
begin
 Assert ( Handle <> 0, ClassName + '.FreeMem Handle = 0');
 // TODO: WARN - memory leak possibly!
 if ( DWORD(p) < FHandle ) or ( DWORD (p) > FHandle + DWORD(Size) ) then exit;

 try
  Dec (FSize, BlockSize(p) );
  HeapFree (FHandle, FOptions and 1, p);
 except
  on E: Exception do
     OnExceptLog (ClassName + '.ReleaseMem', E);
 end;
end;

{ TFastMemAllocator }

function TFastMemAllocator.Allocated: Integer;
var n: Integer;
    mp: PMemPage;
begin
 result := 0;
 for n := FMemPages.Count - 1 downto 0 do
  begin
   mp := FMemPages [n];
   Inc (result, mp.size - mp.index);
  end;

end;

function cmpBlockPtr (a, b: Pointer): Integer;
var
   ab: PMemBlock absolute a;
   bb: PMemBlock absolute b;
begin
 result := 0;
 if DWORD (ab.pp) > DWORD (bb.pp) then result := +1;
 if DWORD (ab.pp) < DWORD (bb.pp) then result := -1;
end;

function cmpPointer(a, b: Pointer): Integer;
begin
 result := 0;
 if DWORD (a) > DWORD (b) then result := +1;
 if DWORD (a) < DWORD (b) then result := -1;
end;

function cmpPageFree(a, b: Pointer): Integer;
var
   ap: PMemPage absolute a;
   bp: PMemPage absolute b;

begin
 result := 0;
 if ap.free > bp.free then result := +1;
 if ap.free < bp.free then result := -1;
 if result <> 0 then exit;
 if ap.blocks > bp.blocks then result := +1;
 if ap.blocks < bp.blocks then result := -1;
end;


function def_alloc_mem (ud: Pointer; cb: DWORD): Pointer;
begin
 result := AllocMem (cb);
end;

procedure def_free_mem (ud, p: Pointer; cb: DWORD);
begin
 FreeMem (p, cb);
end;

function TFastMemAllocator.AllocPage(cbData: DWORD): PMemPage;
var
   cb_alloc, extra: DWORD;
begin
  extra := Max(0, cbData - PAGE_DATA_SIZE );

  cb_alloc := sizeof (TMemPage) + extra;

  // GetMem (fmp, cb_alloc);
  result := FWorkMM.alloc_mem (FWorkMM.ud, cb_alloc);
  FillChar (result^, cb_alloc, 0);

  MemStat.GetCollector.LogAlloc ( Name, cb_alloc );

  result.tag := 'FMPG';
  result.fma := self;
  result.size := PAGE_DATA_SIZE + extra;

  result.ptr_last := DWORD ( @result.data ) + DWORD ( result.size - 1 );
  result.owner := GetCurrentThreadID;
  result.crc := CalcCRC32 (result, PAGE_HEADER_SIZE - 4);

  SavePage (result);
end;

function TFastMemAllocator.AllocLarge(cbSize: DWORD): Pointer;
var
   mb: PMemBlock;

begin
 Inc ( cbSize, BLOCK_HEADER_SIZE );

 if HeapAvail then
    mb := Heap.AllocMem (cbSize)
 else
    mb := System.AllocMem (cbSize);

 mb.pp := self;
 mb.sz := cbSize;

 InterlockedIncrement ( self.alloc_blocks );

 Inc ( total_alloc, cbSize );

 result := RelativePtr ( mb, BLOCK_HEADER_SIZE );
end;

function TFastMemAllocator.AllocMem(cbSize: DWORD): Pointer;


var n, scnt: Integer;
    bl_alloc: DWORD;
    fmp, mp: PMemPage;
       elps: Double;
      nplst: TList;
          s: String;

    // blfree: TBlockStack;
begin
 Inc (alloc_rqs);
 fmp := nil;
 result := nil;

 try

   if cbSize >= MIN_LARGE_BLOCK then
      begin
       result := AllocLarge (cbSize);
       exit;
      end;


   pt.StartOne (1);

   bl_alloc := cbSize + BLOCK_HEADER_SIZE; // +указатель на страницу, размер блока

   if bl_alloc and 3 > 0 then // дополнительное выравнивание 4 byte
      bl_alloc := bl_alloc or 3 + 1;


   nplst := FSmallPages;
   scnt := 0;

   // Последняя выделенная страница имеет шанс быть полезной
   for n := nplst.Count - 1 downto 0 do
    begin
     Inc (scnt);
     mp := nplst [n];
     if (mp.free < Integer(bl_alloc) ) then continue;
     fmp := mp;
     break;
    end;


   if fmp = nil then
     begin
      // запасные страницы
      for n := 0 to 3 do
        begin
         fmp := AllocPage (PAGE_DATA_SIZE);
         FSmallPages.Add (fmp);
        end;
     end;

   // while (fmp.index and 3 <> 0) do Inc (fmp.index);

   {$IFOPT D+}
   CheckPage (fmp);
   {$ENDIF}

   if ( bl_alloc = 52 ) and ( dbg_flag > 0 ) then
       __nop;
   if ( bl_alloc = 96 ) and ( dbg_flag > 0 ) and ( tags [0] <> nil ) then
      begin
       s := decoder ( tags [0] );
       if s <> '' then
          ODS (' allocated 96 from ' + s );
      end;


   result := fmp.BlockAlloc (bl_alloc);
   FillChar ( result^, cbSize, $00 );

   Inc ( total_alloc, bl_alloc );

   alloc_peak := Max (alloc_peak, total_alloc - total_freed);


   // delyabra
   Assert ( fmp.used <= fmp.index, Format('allocation logic mistake, used = %d, index = %d, freed = %d, a_blocks = %d, f_blocks = %d, alloc = %d ',
                                                                        [fmp.used, fmp.index, fmp.freed, fmp.alloc_cnt, fmp.free_cnt, bl_alloc] ) );


   if ( scnt > 200 )  then    // если поиск проверил 200 страниц(!)
        DistributeEmptyPages (nplst, 256); // оставить страницы наиболее пустые

   fmp.CalcCRC;

   last_page := fmp;
   last_block := RelativePtr (result, -BLOCK_HEADER_SIZE);

   Inc (alloc_blocks);

   elps := pt.Elapsed (1);

   if elps > 10 then
      ODS( CFormat('[~T]. #OPT: .AllocMem time = %.1f ms, CPUTime = %.1f ms', '~C07', [elps, pt.CPUElapsed ]));
 except
  on E: Exception do
    OnExceptLog ( ClassName + '.AllocMem', E);
 end;

end; // AllocMem

function TFastMemAllocator.Capacity: Integer;
const
    MiB = 1024.0 * 1024.0;
var  n: Integer;
    mp: PMemPage;
begin
 result := 0;

 sc_share.Lock('Capacity'); // exch-lock
 try
  for n := FMemPages.Count - 1 downto 0 do
   begin
    mp := FMemPages [n];
    CheckPage (mp);
    Inc (result, mp.size);
   end;


  if total_alloc - total_freed > MiB then
     ODS( CFormat('[~T].~C0F #PERF: %s capacity = %.3f MiB, total_alloc = %.3f MiB, total_freed = %.3f MiB, usage = %.3f MiB ~C07', '~C0F',
                  [ self.Name, result / MiB, total_alloc / MiB, total_freed / MiB, ( total_alloc - total_freed ) / MiB ] ) );
 finally
  sc_share.Unlock;
 end;

end;

procedure TFastMemAllocator.CheckPage(mp: PMempage);
begin
 Assert ( mp.tag = 'FMPG', Format( 'CheckPage: Wrong page $%p tag = %s. last_page = $%p, last_free_page = $%p, last_block = $%p  ',
                                [mp, String (mp.tag), last_page, lfreed_page, last_block] ) );
 Assert ( mp.crc = mp.CalcCRC ( FALSE ), 'CheckPage: Wrong CRC' );
 if ( mp.used = 0 ) and ( mp.free = 0 ) then
     Assert ( FALSE, 'CheckPage: mp.free = mp.used = 0');

 Assert ( (mp.used <= mp.index) and (mp.alloc_cnt >= mp.free_cnt),
                        Format('CheckPage: used = %d, index = %d, freed = %d, blocks = %d, last_action = %s ',
                                           [mp.used, mp.index, mp.freed, mp.alloc_cnt - mp.free_cnt, mp.LastAction] ) );
end;

procedure TFastMemAllocator.Clear;
var  n: Integer;
    mp: PMemPage;

begin
 sc_share.Lock('Clear'); // exch-lock
 try
   for n := 0 to FMemPages.Count - 1 do
    begin
     mp :=  FMemPages[n];
     if not Assigned (mp) then continue;

     if bFree then
        FreePage ( n )
     else
      begin
       mp.used := 0;
       mp.Reset;
       mp.act := 'PCLR';
       mp.CalcCRC;
       FillChar (mp.data, mp.size, $73);
      end;
    end;

   if bFree then
     begin
      FMemPages.Clear;
      FSmallPages.Clear;
      MinPageAddr := MAXINT;
      MaxPageAddr := 0;
     end;
 finally
  sc_share.Unlock;
 end;

 total_alloc := 0;
 total_freed := 0;
end;


constructor TFastMemAllocator.Create;
// var n: Integer;
begin
 FSmallPages := TList.Create;

 sc_share := TCritSection.Create(ClassName + '.sc_share');

 FOwnerTID := GetCurrentThreadID;

 FMemPages := TList.Create;
 FMemCache := TList.Create;
 FName := AName;
 FEasy := bEasy;

 MaxPageAddr := $0;
 MinPageAddr := $FFFFFFFF;

 FWorkMM.alloc_mem := def_alloc_mem;
 FWorkMM.free_mem := def_free_mem;
 // FWorkMM.realloc_mem := def_realloc_mem;

 pt := TProfileTimer.Create;

 SmallBlockSize := 512;

 // FFreeList := TMemBlockList.Create;
 // FFreeList.Capacity := 400 * 1024;

 dbg_free_byte := $88;

 if bMakeHeap then
   begin
    FHeap := TWindowsHeap.Create (ClassName + '.Heap', 4, sizeof (TMemPage) );
    if HeapAvail then
       SmallBlockSize := 8192
    else
      begin
       Heap.Free;
       FHeap := nil;
      end;
   end;
end;  // Create

destructor TFastMemAllocator.Destroy;
begin
 // OptimizeFreed;
 Clear (TRUE);
 FMemCache.Free;
 FSmallPages.Free;
 
 FMemPages.Free;
 FHeap.Free;
 FHeap := nil;
 pt.Free;
 sc_share.Free;
 inherited;
end;

function TFastMemAllocator.FreeMem(p: Pointer; cbSize: DWORD): Boolean;

var
   mp: PMemPage;
   mb: PMemBlock;
   dt: TDateTime;

begin
 Inc (free_rqs);
 Dec (alloc_blocks);
 result := Easy;

 if dbg_flag > 0 then
    __nop;
 if ( p = nil ) or (Easy) then exit;


 Assert ( cbSize < GEBIBYTE, 'trying FreeMem data ' + IntToStr(cbSize));


 try
   // прямое уничтожение блоков
   mb := RelativePtr (p, - BLOCK_HEADER_SIZE );

   if mb.pp = self then
     begin
      InterlockedIncrement (freed_blocks);
      Inc (total_freed, mb.sz);

      if HeapAvail then
         Heap.ReleaseMem (mb, mb.sz)
      else
         FreeMem (mb, mb.sz);
      exit;
     end;

   if not InRange ( NativeUInt (mb.pp), MinPageAddr, MaxPageAddr ) then
     begin
      PrintError ( ClassName + Format( '(%s).FreeMem: Unexpected page address = $%p, total pages = %d', [Name, mb.pp, MemPages.Count] ) );
      exit;
     end;

   mp := mb.pp;

   if cbSize = 0 then
      cbSize := mb.sz - 8
   else
    if cbSize >= mb.sz then
      __nop;



   if ( mp <> nil ) then
    begin
     CheckPage ( mp );
     Inc (freed_blocks);

     Inc (total_freed, mb.sz);
     Inc (FFreedSize, cbSize);

     if not shared then
        Assert ( mp.owner = GetCurrentThreadID, InfoFmt('Thread ~I trying free ptr from page owned by ' + IntToStr (mp.owner)) );

     Assert ( mb.sz < GEBIBYTE, 'Block header probably corrupted. Size = ' + IntToStr (mb.sz) );



     {$IFOPT D+}
     FillChar ( p^, cbSize, dbg_free_byte); // отладочный заполнитель
     if cbSize >= 8 then
      begin
       dt := g_timer.GetTime;
       Move ( dt, p^, 8 );
      end
     else
      _nop;

     {$ENDIF}
     CheckPage (mp);
     mp.free_last := NativeUInt (p);

     mp.OnFree ( mb.sz );

     mb.pp := nil; // mark wasted.

     if FFreedSize >= 16 * PAGE_DATA_SIZE then
        OptimizeFreed;

     result := TRUE;
     exit;
    end;



   exit;

   {

   FFreeList.AddBlock (p, cbSize);
   if (FFreedSize > 4096 * 1024) or ( FFreeList.Count >= 16384 ) or ( Allocated > 50 * 1024 * 1024 ) then
       OptimizeFreed;
   result := TRUE;

   }

 except
  on E: Exception do
    OnExceptLog ( ClassName + '.FreeMem', E);
 end;

end;

procedure TFastMemAllocator.FreePage( i: Integer; lst: TList );
var
   mp: PMemPage;
   sz: Integer;
begin
 if lst = nil then
    lst := FMemPages;

 mp := lst [i];
 lst [i] := nil;
 try
  CheckPage ( mp );

  lfreed_page := mp;
  DbgLogFree ( Name, mp.size + PAGE_HEADER_SIZE );

  sz := mp.size + PAGE_HEADER_SIZE;
  {$IFOPT D+}
  ODS('[~T]. #DBG: freed mem page #' + IntToStr(i) + ' at ' + FormatPtr(mp) + ' index = ' + IntToStr (mp.index) );
  FillChar (mp^, sz, $77);
  {$ENDIF}
  FWorkMM.free_mem  ( FWorkMM.ud, mp, sz );

 except
  on E: Exception do
     OnExceptLog ( ClassName + '.FreePage FWorkMM.free_mem = ' + FormatPtr ( Addr(FWorkMM.free_mem) ) );
 end;
end;


function TFastMemAllocator.FreePagesCount: Integer;
var
   n: Integer;
begin
 result := 0;
 for n := 0 to MemPages.Count - 1 do
   if Pages[n].used = 0 then
      Inc (result);
end;

procedure TFastMemAllocator.OptimizeFreed;
begin
 // TODO: incomplete & untested
 if (GetCurrentThread <> FOwnerTID) then exit;

 if dbg_flag = 1 then
    ODS('[~T].~C0F #PERF: TFastMemAllocator(' + Name + ').OptimizeFreed executing... ~C07');

 sc_share.Lock('OptimizeFreed');
 try
  MemPages.Sort (cmpPageFree);
  OptimizePages;
 finally
  sc_share.Unlock;
 end;
 FFreedSize := 0;
end; // OptimizeFree


procedure TFastMemAllocator.DistributeEmptyPages;
var
   n: Integer;
   mp: PMemPage;
begin
 FMemCache.Clear;
 for n := 0 to FMemPages.Count - 1 do
  begin
   mp := FMemPages [n];
   if mp.free >= min_free then
      FMemCache.Add ( mp );
  end;
 lst.Assign ( FMemCache );
 lst.Sort ( cmpPageFree );
end;

procedure TFastMemAllocator.OptimizePages;
var
   n_page: Integer;
   rs_cnt: Integer;
       mp: PMemPage;
begin
 FMemCache.Clear;
 // lst.Clear;
 if not shared then
     Assert ( FOwnerTID = GetCurrentThreadID, InfoFmt( 'Thread ~I trying OptimizePages for ' + Name ) );


 rs_cnt := FFreedSize div PAGE_DATA_SIZE + 4;

 MinPageAddr := MAXINT;
 MaxPageAddr := 0;

 for n_page := 0 to MemPages.Count - 1 do
  begin
   mp := MemPages [n_page];
   CheckPage (mp);

   if ( mp.index > 0 ) and
     ( ( mp.freed = mp.size ) or
       ( mp.freed = mp.index ) or
       ( mp.alloc_cnt = mp.free_cnt ) ) then
     begin
      mp.Reset;
     end;

   if mp.free = mp.size  then // страница полностью пуста
     begin
      Dec (rs_cnt);
      if rs_cnt <= 0 then
        begin
         FreePage ( n_page, MemPages );
         continue;
        end;
     end;

   // страница сохраняется в резерв, пустая или отчасти заполненная
   FMemCache.Add (mp)
 end; // for

 MemPages.Clear; // rebuild prepare
 FMemCache.Sort (cmpPageFree); // наиболее свободные в конец

 for n_page := 0 to FMemCache.Count - 1 do
     SavePage ( FMemCache [n_page] );

 // поместить свободные страницы в конце, с доступным пространством от килобайта
 DistributeEmptyPages ( FSmallPages, 1024 );
end;

function TFastMemAllocator.ReAllocMem(p: Pointer; oldSize, cbSize: DWORD): Pointer;
var
        mp: PMemPage;
        mb: PMemBlock;
   test_my: Boolean;

begin
 result := p;

 Inc (realloc_rqs);

 if ( oldSize > cbSize ) then exit;

 mb := RelativePtr ( p, -BLOCK_HEADER_SIZE );


 test_my := InRange ( NativeUInt (mb.pp), MinPageAddr, MaxPageAddr );


 if test_my then
   begin
    mp := mb.pp;
    test_my := ( mp <> nil ) and ( mp.tag = 'FMPG' )
   end;


 if ( oldSize < cbSize ) then
   begin
    result := AllocMem (cbSize);
    SafeMove(p^, result^, Min (oldSize, cbSize) );
   end;

 if test_my then
    self.FreeMem ( p, oldSize )
 else
    System.FreeMem ( p, oldSize );

 // mp.freed := mp.freed + cbSize - oldSize;
end;

procedure TFastMemAllocator.SavePage(mp: PMemPage);
var
   mpva: NativeUInt;
begin
 sc_share.Lock('SavePage');
 FMemPages.Add (mp);
 mpva := NativeUInt (mp);
 if mpva < MinPageAddr then MinPageAddr := mpva;
 if mpva > MaxPageAddr then MaxPageAddr := mpva;

 sc_share.Unlock;
end;

function TFastMemAllocator.SetOwnerThread(tid: DWORD): DWORD;
begin
 result := InterlockedExchange ( Integer(FOwnerTID), tid );
end;

function TFastMemAllocator.GetMemMgr: PBaseMM;
begin
 result := @FWorkMM;
end;

function TFastMemAllocator.GetPage(index: Integer): PMemPage;
begin
 result := FMemPages [index];
end;

function TFastMemAllocator.HeapAvail: Boolean;
begin
 result := (Heap <> nil) and (Heap.Handle <> 0);
end;



{ TMemBlockList }

function TMemBlockList.AddBlock (p: Pointer; sz: DWORD): PMemBlock;
begin
 result := @FBlocks [FBCount];
 result.pp := p;
 result.sz := sz;
 Inc ( FBCount );
 if FBCount >= FBSize then
    Resize ( FBCount + 4096 );

end;

constructor TMemBlockList.Create;
begin
 inherited Create;
 Capacity := 16384;
 Resize ( 4096 );
end;

destructor TMemBlockList.Destroy;
begin
 Resize ( 0 );
 inherited;
end;

function TMemBlockList.GetItem(index: Integer): PMemBlock;
begin
 result := PMemBlock ( Get (index) );
end;

procedure TMemBlockList.Reindex;
var n: Integer;
begin
 Clear;
 if Capacity < FBCount then
    Capacity := FBCount;

 for n := 0 to FBCount - 1 do Add ( @FBlocks [n] );

 Sort (cmpBlockPtr);
end;

procedure TMemBlockList.Resize(newSize: Integer);
begin
 SetLength ( FBlocks, newSize );
 FBSize := newSize;
 FBCount := Min ( FBCount, FBSize );
end;

{ TMemPage }

function TMemPage.BlockAlloc(cb_alloc: Integer): Pointer;
var
   mb: PMemBlock;
begin
 if used > index then
   __nop;

 act := 'AMBL';
 mb := @data [index];
 result := @data [index + BLOCK_HEADER_SIZE];

 mb.pp := @self;
 mb.sz := cb_alloc; // полный размер блока

 InterlockedAdd (index, cb_alloc); // уменьшение нефрагментированной части
 InterlockedAdd (used,  cb_alloc);
 InterlockedIncrement (alloc_cnt);
end;

function TMemPage.blocks: Integer;
begin
 result := alloc_cnt - free_cnt;
end;

function TMemPage.CalcCRC;
begin
 result := CalcCRC32 ( @tag, PAGE_HEADER_SIZE - 4);
 if upd then crc := result;
end;

function TMemPage.FormatInfo: String;
begin
 result := Format (' addr = $%8p used = %5d,  index = %5d, free = %5d, freed = %5d, blocks = %5d ', [@self.data, used, index, free, freed, blocks] );
 if blocks > 0 then
    result := result + ' block_trace: ' + TraceBlocks (8, FALSE);
end;

function TMemPage.free: Integer;
begin
 result := size - index;
end;

function TMemPage.FreeSaldo: DWORD;
var
   n: Integer;
begin
 result := 0;
 for n := 0 to free_cnt - 1 do
  if n < High (fblks) then
     Inc ( result, fblks [n] );
end;

function TMemPage.LastAction: String;
begin
 SetString (result, act, 4);
end;

procedure TMemPage.OnFree(cb: DWORD);
begin
 act := 'FMBL';
 if free_cnt <= High (fblks) then
    fblks [free_cnt] := cb;

 InterlockedAdd (freed,  Integer(cb) );
 InterlockedAdd (used, - Integer(cb) );
 InterlockedIncrement ( free_cnt );
 try
  if ( used <= index ) and ( freed <= index ) then
    else
        Assert (FALSE, Format('delocation logic mistake, used = %d, index = %d, freed_total = %d, free_saldo = %d, to_free = %d, blocks = %d ',
                                                                                [used, index, freed, FreeSaldo, cb, blocks] )  );
 except
  on E: Exception do
    OnExceptLog ('TMemPage.OnFree, block_trace: '#13#10 + TraceBlocks + #13#10, E);
 end;

 if ( ( freed = index ) or ( used = 0 ) ) and ( owner = GetCurrentThreadID ) then
      Reset
 else
      CalcCRC;
end;

procedure TMemPage.Reset;
begin
 Assert ( ( used = 0 ) or ( freed = size ), Format ( 'Trying reset page with { free = $%x, size = $%x, used = $%x }', [freed, size, used] ) );
 act := 'PRST';

 if fma.dbg_flag > 0 then
    __nop;

 InterlockedExchange ( alloc_cnt, 0 );
 InterlockedExchange ( free_cnt, 0 );
 InterlockedExchange ( freed, 0 );
 InterlockedExchange ( index, 0 );
 InterlockedExchange ( used, 0 );

 FillChar (data, size, $55);
 CalcCRC;
end;

function TMemPage.TraceBlocks ( max_trace: Integer; incl_free: Boolean ): String;
var
   mb: PMemBlock;
   fb: PMemBlock;
   tc: NativeUInt;
    o: NativeUInt;
begin
 result := '';

 fb := @data [0];
 mb := fb;
 tc := 0;

 while ( NativeUInt (mb) - NativeUInt (fb) < NativeUInt(index) ) do
  begin
   if incl_free or ( mb.pp <> nil ) then
     begin
      o := NativeUInt (mb) - NativeUInt (fb);
      result := result + IfV (mb.pp <> nil, '~C0F', '~C0C');
      result := result + IntToStr(o) + ':' + IntToStr (mb.sz) + ' ';
      Inc (tc, mb.sz);
      Dec ( max_trace );
     end;

   mb := RelativePtr (mb, mb.sz);
   if max_trace <= 0 then break;
  end;

 result := Trim (result) + '~C07=' + IntToStr(tc);
end;

initialization
 FillChar (DefPage, sizeof(DefPage), 0);
end.
