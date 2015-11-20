unit XrayMM;

interface
uses Windows, Types;


const
   GiB = 1024 * 1024 * 1024;

type
   SIZE_T = LongWord;

   TXrMemory = record
       x_m1: Pointer;   // + $00
       x_m2: Pointer;   // + $04   int m_counter
       x_m3: Pointer;   // + $08   ref xrMemCopy_x86  or xrMemCopy_MMX
       x_m4: Pointer;   // + $0C   ref xrMemFill_x86
       x_m5: Pointer;   // + $10   ref xrMemFill32_x86 or xrMemFill32_MMX
   end;

   PXrMemory = ^TXrMemory;


   PDebugMemoryManager = ^TDebugMemoryManager;


   TDebugMemoryManager = packed record
    cb_alloc: Int64;
    cb_freed: Int64;
    op_count: Int64;
    enabled: Boolean;


    _GetMem: function (Size: NativeInt): Pointer;
    _FreeMem: function(P: Pointer): Integer;
    _ReallocMem: function(P: Pointer; Size: NativeInt): Pointer;


    // debug wrappers
    function    GetMem(size: NativeInt): Pointer; inline;
    function    FreeMem(P: Pointer; Size: NativeInt): Integer; inline;
    function    Realloc(P: Pointer; OldSize, NewSize: NativeInt): Pointer; inline;

   end;


   // ----------------------------------------------------

   TMemBlockHdr = packed record
    dt: TDateTime;
    sz: DWORD;
    mm: PDebugMemoryManager;         // also uses as signature
   end;

   PMemBlockHdr = ^TMemBlockHdr;



{



        375  176 00017680 public: void * __thiscall xrMemory::mem_alloc(unsigned int)
        376  177 00016FA0 public: void __thiscall xrMemory::mem_compact(void)
        377  178 00001530 public: unsigned int __thiscall xrMemory::mem_counter_get(void)
        378  179 00001540 public: void __thiscall xrMemory::mem_counter_set(unsigned int)
        379  17A 00017770 public: void __thiscall xrMemory::mem_free(void *)
        380  17B 000177E0 public: void * __thiscall xrMemory::mem_realloc(void *,unsigned int)
        381  17C 00016DF0 public: unsigned int __thiscall xrMemory::mem_usage(unsigned int *,unsigned int *)

\}

procedure PreloadLog(const msg: WideString; print_flags: DWORD = 255); stdcall;

function XrTryAlloc(size: NativeInt): Pointer;

procedure DumpStats;

var
   fake_block: Pointer = nil;
   log_proc: procedure (const msg: WideString; print_flags: DWORD = 255); stdcall = PreloadLog;
   mm_list: array [0..2] of TDebugMemoryManager;



function InitMM(L: Pointer): Integer; cdecl;

implementation
uses Math, SysUtils, StrUtils;


const
           HDR_SIZE = sizeof (TMemBlockHdr);
       mem_obj_name = '?Memory@@3VxrMemory@@A';
      mem_free_name = '?mem_free@xrMemory@@QAEXPAX@Z';
     mem_alloc_name = '?mem_alloc@xrMemory@@QAEPAXI@Z';
   mem_realloc_name = '?mem_realloc@xrMemory@@QAEPAXPAXI@Z';
    XR_3DA_EXE: PChar = 'xr_3da.exe';
    NO_HEAP_MM: PChar = '-no_heap_mm';
     MMLogFile: PChar = '_xraymm.log';




var
   xrmm, oldmm: TMemoryManagerEx;
    Memory: PXrMemory = nil;
   ptr_first, ptr_last: DWORD;

     xr_mem_alloc: Pointer;
      xr_mem_free: Pointer;
   xr_mem_realloc: Pointer;


   alloc_ofs: Integer = 0;
   gHeap: THandle;
   szExe: array [0..MAX_PATH + 1] of CHAR;
szParams: array [0..1023] of CHAR;


function InterlockedAdd(var Addend: Integer; Increment: Integer): Integer;
asm
      MOV   ECX,EAX
      MOV   EAX,EDX
 LOCK XADD  [ECX],EAX
      ADD   EAX,EDX
end;

procedure FindMemoryObject;
var
   hLib: THandle;
     pp: Pointer;
begin
 hLib := GetModuleHandle ('xrCore.dll');

 if hLib = 0 then
  begin
   log_proc('[~T]. #ERROR: not found xrCore.dll in memory');
   exit;
  end;

 pp := GetProcAddress( hLib, mem_obj_name );

 xr_mem_alloc   := GetProcAddress( hLib, mem_alloc_name );
 xr_mem_free    := GetProcAddress( hLib, mem_free_name );
 xr_mem_realloc := GetProcAddress( hLib, mem_realloc_name );

 if pp = nil then
   log_proc('#ERROR: cannot get exported symbol ' + mem_obj_name)
 else
   begin
    if pp <> Memory then
       log_proc('#DBG: global xrMemory Memory at $' + IntToHex ( DWORD (pp), 8) );
    Memory := pp;
   end;
end;


procedure PreloadLog;
var
   s: String;
   t: Text;
begin
 s := ParamStr(0);
 s := LowerCase (s);
 if Pos('.exe', s) = 0 then exit;


 s := AnsiReplaceStr (s, 'bin\xr_3da.exe', 'logs\');
 s := AnsiReplaceStr (s, '\host.exe', '\logs\');
 s := AnsiReplaceStr (s, '\test.exe', '\logs\');



 if not DirectoryExists(s) then
        CreateDirectory(PChar (s), nil);

 s := s + MMLogFile;

 try
  AssignFile(t, s);
  {$I-}
  if FileExists (s) then
     Append (t)
  else
     ReWrite (t);
  s := FormatDateTime ('dd.mm.yy hh:nn:ss.zzz', Now);

  WriteLn (t, '[' + s + ']. ' + msg);

  CloseFile (t);

 except
  on E: Exception do
     OutputDebugString('Exception catched in PreloadLog');
 end; // try-exc


end; //


function XrTryAlloc(size: NativeInt): Pointer;
begin
 result := nil;

 try

   if (xr_mem_alloc = nil) or (Memory = nil) then exit;

   asm
    pushad
    mov  ecx, memory
    push size
    mov  eax, xr_mem_alloc
    call eax
    mov  result, eax
    popad
   end;
 except
  on E: Exception do
     log_proc('EXCEPTION catched in XrTryAlloc ' + E.Message + ', Memory = $' + IntToHex(DWORD(Memory), 8) );
 end;
end;

function XrRealloc(p: Pointer; newSize: NativeInt): Pointer;
begin
 result := nil;

 if (xr_mem_realloc = nil) or (Memory = nil)  then exit;


 try
  asm
   pushad
   mov  ecx, Memory
   mov  eax, xr_mem_realloc
   push newSize
   push p
   call eax
   mov  result, eax
   popad
  end;


 except
  on E: Exception do
     log_proc('EXCEPTION catched in XrRealloc ' + E.Message + ', Memory = $' + IntToHex(DWORD(Memory), 8) );
 end;

end;

function XrFree(p: Pointer): Integer;
begin
 result := 1;

 if (xr_mem_free = nil) or (Memory = nil) then exit;


 result := 0;
 try
  asm
   pushad
   mov  ecx, Memory
   mov  eax, xr_mem_free
   push p
   call eax
   popad
  end;


 except
  on E: Exception do
     log_proc('EXCEPTION catched in XrFree ' + E.Message + ', Memory = $' + IntToHex(DWORD(Memory), 8) );
 end;

end;


function HeapAllocMem(Size: NativeInt): Pointer;
begin
 Assert (gHeap <> 0, 'HeapAllocMem: gHeap == NULL');
 result := HeapAlloc ( gHeap, HEAP_GENERATE_EXCEPTIONS or HEAP_ZERO_MEMORY, size );
end;

function HeapReallocMem(P: Pointer; Size: NativeInt): Pointer;
begin
 result := HeapRealloc ( gHeap, HEAP_GENERATE_EXCEPTIONS, p, Size );
end;

function HeapFreeMem (p: Pointer): Integer;
begin
 result := 1;

 if (gHeap <> 0) and HeapFree ( gHeap, HEAP_GENERATE_EXCEPTIONS, p ) then result := 0;

end;





function RegisterExpectedMemoryLeak(p: Pointer): Boolean;
begin
 result := FALSE;
end;

function UnregisterExpectedMemoryLeak (p: Pointer): Boolean;
begin
 result := FALSE;
end;

function CheckMM (mm: Pointer): Boolean; inline;
begin
 result := ( mm = @mm_list [0] ) or ( mm = @mm_list [1] ) or ( mm = @mm_list [2] );
end;


{ Universal allocators }

function UniAllocMem(size: NativeInt): Pointer;
var
   phdr: PMemBlockHdr;
    fsz: NativeInt;
    mgr: PDebugMemoryManager;
      i: Integer;
begin

 result := nil;

 if size = 0 then exit;

 // TODO: very small block can allocated via oldmm

 fsz := size + HDR_SIZE;

 for i := 0 to High (mm_list) do
  begin
   mgr := @mm_list [i];
   if not mgr.enabled then continue;

   result := mgr.GetMem (fsz);
   if result <> nil then break;
  end;

 Assert (result <> nil, 'UniAllocMem failed to allocate block with size ' + IntToStr(fsz));

 phdr := result;

 phdr.dt := Now;
 phdr.sz := size;
 phdr.mm := mgr;


 Inc ( NativeUInt(result), HDR_SIZE);
end;

function UniFreeMem(p: Pointer): Integer;
var
   phdr: PMemBlockHdr;
begin
 phdr := p;
 Dec ( NativeUInt(phdr), HDR_SIZE );

 if CheckMM (phdr.mm) then
    result := phdr.mm.FreeMem (phdr, phdr.sz) // WARNING: Is important for block releasing/reallocating
 else
    result := oldmm.FreeMem (p);
end; // UniFreeMem


function UniReallocMem(p: Pointer; newSize: NativeInt): Pointer;
var
   phdr: PMemBlockHdr;
begin
 phdr := p;
 Dec ( NativeUINt(phdr), HDR_SIZE );

 if CheckMM (phdr.mm) then
   begin
    result := phdr.mm.Realloc (phdr, phdr.sz, newSize + HDR_SIZE);
    phdr := result;
    phdr.dt := Now;
    phdr.sz := newSize;
    Inc ( NativeUInt(result), HDR_SIZE );
   end
 else
    result := oldmm.ReallocMem (p, newSize); // UniAllocMem(newSize);
end; // UniReallocMem


function InitMM;
var
  pHeap: THandle;
begin
 result := 0;
 pHeap := GetProcessHeap;

 if Assigned (log_proc) then
   begin
    log_proc ('[~T]. #DBG: Process Heap  = $' + IntToHex(pHeap, 4) );
   end
 else
   exit;
end;

procedure DumpStats;
var
   i: Integer;
   s: String;
begin
 for i := 0 to High(mm_list) do
 with mm_list[i] do
  begin
   s := Format('mm_list[%d] cb_alloc = %.5f GiB, cb_freed = %.5f GiB, op_count = %d', [i, ( cb_alloc / GiB ), ( cb_freed / GiB ), op_count ] );
   log_proc (s);
  end;
end; // DumpStats


function IsDebuggerPresent: Boolean; stdcall; external 'kernel32.dll';


procedure LoadExeParams;
var
   n: Integer;
   s: String;

begin
 FillChar (szExe, sizeof(szExe), 0);
 GetModuleFileName ( $400000, szExe, MAX_PATH );
 StrLower(szExe);

 for n := 1 to ParamCount do
    begin
     if n > 1 then s := s + '|';
     s := s + ParamStr(n);
    end;

 log_proc( PChar('ExeParams: ' + s));

 StrLCopy ( szParams, PChar (s), 1000 );

 s := '';
end;

{ TDebugMemoryManager }

function TDebugMemoryManager.FreeMem(P: Pointer; Size: NativeInt): Integer;
begin
 result := _FreeMem (p);
 if result = 0 then
    Inc (cb_freed, Size); // TODO: thread unsafe
 Inc(op_count);
end;

function TDebugMemoryManager.GetMem(size: NativeInt): Pointer;
begin
 result := _GetMem(Size);
 if result <> nil then
  begin
   Inc(cb_alloc, Size);
   Inc(op_count);
  end;
end;

function TDebugMemoryManager.Realloc(P: Pointer; OldSize, NewSize: NativeInt): Pointer;
begin
 result := _ReallocMem (P, NewSize);
 Inc (cb_freed, OldSize);
 Inc (cb_alloc, NewSize);
 Inc(op_count);
end;

initialization
 IsMultiThread := TRUE;

 gHeap := GetProcessHeap;

 GetMemoryManager(oldmm);

 FillChar (mm_list, sizeof(mm_list), 0);

 with mm_list [0] do
  begin
   _FreeMem    := XrFree;
   _GetMem     := XrTryAlloc;
   _ReallocMem := XrRealloc;
  end;

 with mm_list [1] do
  begin
   Enabled := TRUE;
   _FreeMem    := HeapFreeMem;
   _GetMem     := HeapAllocMem;
   _ReallocMem := HeapReallocMem;
  end;


 with mm_list [2] do
  begin
   Enabled := TRUE;
   _FreeMem    := oldmm.FreeMem;
   _GetMem     := oldmm.GetMem;
   _ReallocMem := oldmm.ReallocMem;
  end;

 LoadExeParams;

 // gHeap := HeapCreate ( HEAP_GENERATE_EXCEPTIONS, 1024 * 1024, 0 );

 ptr_first := MAXINT;
 ptr_last  := 0;


 //if Assigned (malloc) and (not IsDebuggerPresent) then;
 xrmm.GetMem := UniAllocMem;
 xrmm.FreeMem := UniFreeMem;
 xrmm.AllocMem := UniAllocMem;
 xrmm.ReallocMem := UniReallocMem;


 xrmm.RegisterExpectedMemoryLeak := RegisterExpectedMemoryLeak;
 xrmm.UnregisterExpectedMemoryLeak := UnRegisterExpectedMemoryLeak;

 FindMemoryObject;

 // if Pos('xr_3da.exe', LowerCase(sExe) ) > 0 then
 if ( ( StrPos(szExe, XR_3DA_EXE) <> nil ) or IsDebuggerPresent ) and
      ( StrPos(szParams, NO_HEAP_MM) = nil )  then
  begin
   SetMemoryManager(xrmm);
  end;


 InitMM (nil);

 // ODS('[~T]. #DBG: Memory manager uses process heap now.');

 // {$ENDIF}

 log_proc('XrayMM: Initialization completed');
finalization

 DumpStats;
 // SetMemoryManager(oldmm);
end.
