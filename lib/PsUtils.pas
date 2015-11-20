unit PsUtils;

interface
uses Windows, SysUtils, Classes, StrClasses, StrUtils, TlHelp32, Misc, UniArray, madMapFile;
(*  ¬ данном модуле механизмы TlHelp реализуетс€ в виде классов.
-----------------------------*)

const
    PAGE_READ_ACCESS = PAGE_READONLY or PAGE_READWRITE or PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE; // mask cover all readable
    PAGE_RW_ACCESS   = PAGE_READWRITE or PAGE_EXECUTE_READWRITE;
    PAGE_EXEC_ACCESS = PAGE_EXECUTE or PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE or PAGE_EXECUTE_WRITECOPY;


    MEM_NO_ACCESS    = MEM_FREE or MEM_RESERVE;

    SYMOPT_UNDNAME              = $0002;
    SYMOPT_DEFERRED_LOADS       = $0004;
    SYMOPT_LOAD_LINES           = $0010;
    SYMOPT_LOAD_ANYTHING        = $0040;

type

      // јттрибуты страниц дл€ простого сравнени€
      TPageAttrs = (paReadable, paWriteable, paExecutable,
                     paGuarded, paNoaccess);
      TProtSet = set of TPageAttrs;
      TRegionType = DWORD;

      TVMOffset = NativeUInt;


      PVMRegion = ^TVMRegion;

      TVMRegion = packed record // partially same as TMemoryBasicInformation
          state: DWORD;
          rtype: TRegionType;
        protect: DWORD;
        protset: TProtSet;
          rsize: NativeUInt;
           size: NativeUInt;
         offset: TVMOffset;
          limit: TVMOffset;
           updt: TDateTime;
           busy: Integer;         // reserved field

        procedure      Import (mbi: TMemoryBasicInformation);
        procedure      Init (bp: Pointer; reg_size: NativeUInt; prot, st: DWORD; typ: DWORD = 0);
        function       Includes (addr: NativeUint): Boolean;
        function       RemoteLimit: TVMOffset; inline; // inbound addrs must be below this


        case byte of
         0 : (  base_addr,  alloc_addr: TVMOffset );
         1 : (   base_ptr,   alloc_ptr: Pointer );
         2 : ( local_addr, remote_addr: TVMOffset );
         3 : (  local_ptr,  remote_ptr: Pointer );
     end; // region

     TVMRegionArray = array [0..1023] of TVMRegion;
     PVMRegionArray = ^TVMRegionArray;

      TModuleInfo = packed record // partially as TModuleEntry32
           hModule: NativeUInt;
       modBaseSize: NativeUInt;
          szModule: WFILE_PATH;
         szExePath: WFILE_PATH;
      end;

      TThreadInfo = packed record
       threadId: DWORD;
       ownerPID: DWORD;
      end;


      PThreadInfo = ^TThreadInfo;
      TProcessInfo = packed record
          title: array [0..63] of Char;
          fname: WFILE_PATH;
       tid, pid: DWORD;   // primary thread and process ID
           icon: Integer; // index of stored icon
       hProcess: THandle;
           hWnd: THandle;
         weight: Integer;
      end;

      PProcessInfo = ^TProcessInfo;
      TSmallPSArray = array [0..7] of TProcessInfo;
      PSmallPSArray = ^TSmallPSArray;


     TProcessEntryList = packed array [0..4095] of TProcessEntry32;
     PProcessEntryList = ^TProcessEntryList;


     TModuleEntryList = packed array [0..255] of TModuleEntry32;
     PModuleEntryList = ^TModuleEntryList;


     IMAGEHLP_LINE64 = record
      SizeOfStruct: DWORD;
               Key: Pointer;
        LineNumber: DWORD;
          FileName: PWideChar;
           Address: UInt64;
     end;

     TImageHlpLine64 = IMAGEHLP_LINE64;
     PImageHlpLine64 = ^TImageHlpLine64;

     IMAGEHLP_SYMBOL64 = record
        SizeOfStruct: DWORD;                    // set to sizeof(IMAGEHLP_SYMBOL64)
             Address: UInt64;                   // virtual address including dll base address
                Size: DWORD;                    // estimated size of symbol, can be zero
               Flags: DWORD;                    // info about the symbols, see the SYMF defines
       MaxNameLength: DWORD;                    // maximum size of symbol name in 'Name'
                Name: array[0..0] of AnsiChar; // symbol name (null terminated string)
     end;

     PIMAGEHLP_SYMBOL64 = ^IMAGEHLP_SYMBOL64;



     API_VERSION = packed record
       MajorVersion: WORD;
       MinorVersion: WORD;
           Revision: WORD;
           Reserved: WORD;
     end;

     LPAPI_VERSION = ^API_VERSION;


     TTlh32 = class (TObject)
     private
      flags: dword;
      _srcpid: dword;
      FHandle: THandle;

     public
      { props }
      property Handle : THandle read FHandle;
      { C & D }
      constructor            Create (dwFlags : dword; SrcPid : dword = 0);
      destructor             Destroy; override;
      { methods }
      function               ModuleFirst (var me: TModuleEntry32): boolean;
      function               ModuleNext (var me: TModuleEntry32): boolean;
      function               ProcessFirst (var pe : TProcessEntry32) : boolean;
      function               ProcessNext (var pe : TProcessEntry32) : boolean;
      function               ThreadFirst (var te : TThreadEntry32) : boolean;
      function               ThreadNext (var te : TThreadEntry32) : boolean;
      procedure              Update; // UpdateSnapshot
      procedure              Close;
     end;
     PTlh32 = ^TTlh32;




     TProcessList = class (TMatrixArray)
     private

       need_first: Boolean;
        FComplete: Boolean;
           pe_upd: TProcessEntry32;
               th: TTlh32;

       function GetEntryList: PProcessEntryList; inline;
     public

      property          Complete: Boolean read FComplete;
      property          Items: PProcessEntryList read GetEntryList;
      { C & D }
      constructor       Create (); override;
      destructor        Destroy; override;
      { methods }

      procedure         Clear; override;
      function          FindByFile (const fname: string;
                                    updateBefore: boolean = false): Integer;
      function          ListHash: DWORD;
      function          UpdateOne (ppe: PProcessEntry32): Boolean;
      procedure         Update; virtual;
     end;

     TModuleList = class (TObject)
     private
      FCount: Integer;
      function GetItems: PModuleEntryList; inline;
    procedure SortByAddr;
     protected
      th: TTlh32;
      FItems: TModuleEntryList;

     public

      { props }

      property          Count: Integer read FCount;
      property          Items: PModuleEntryList read GetItems;


      { C & D }
      constructor       Create (srcpid: DWORD);
      destructor        Destroy; override;
      { methods }
      procedure         Dump;
      function          Find (const s: string): Integer;
      procedure         Update;
     end;


     TWndProcess = record
      hwnd: THandle;
      pid, tid: DWORD;
      title: String;
     end; // TWndProcess

     TWndProcessList = class
     private
      wpslist: array of TWndProcess;
      wpscount: Integer;
      wpslsize: Integer;
      function      GetItem (i: Integer): TWndProcess;
     public
      bAddInvisible: Boolean;
      bMainOnly: Boolean;
      pid_hash: DWORD;

      { props }
      property count: Integer read wpscount;
      property Items [Index: Integer]: TWndProcess read GetItem; default;


      { C & D }
      constructor   Create (size: Integer = 512);
      destructor    Destroy; override;
      { methods }
      function      AddWindow (hWnd: THandle): Boolean;
      procedure     Update;
     end;

     TModuleMap = class (TStrMap)
     private
      FFirstAddr: NativeUInt;
       FLastAddr: NativeUInt;
        FMapFile: TMapFile;

     protected
       FSegments: array [0..15] of NativeUInt;
       FSegClass: array [0..15] of String;

       FSections: array [0..63] of NativeUInt;
       FSectName: array [0..63] of String;

       seg_count: Integer;

      procedure        ImportMap;
     public

      hDLL: HMODULE;
      { props }

      property  FirstAddr: NativeUInt read FFirstAddr;
      property   LastAddr: NativeUInt read FLastAddr;

      { C & D }
      { methods }


      procedure        DumpMap (hp: THandle);

      procedure        LoadMap (const sFileName: String);
      function         PtrInfo (p: Pointer; bwd_range, fwd_range: NativeUInt ): String;

     end;

     TModuleMapList = class (TStrMap)
     public


     end;

     TProcessAccessor = class
     private
    FAttached: Boolean;

     protected
      FLastCode: Integer;
      FLastError: String;

     public
      { props }

      property        Attached: Boolean read FAttached;
      property        LastCode: Integer read FLastCode;
      property        LastError: String read FLastError;

      { C & D }
      constructor     Create;
      { methods }



      function        CheckAccess (lpAddr: Pointer; cbSize: NativeUInt; fProt: DWORD): Boolean; virtual;
      function        OpenAccess: Boolean; virtual;
      function        QueryBasicInfo ( lpAddr: Pointer ): TMemoryBasicInformation; virtual;
      procedure       ReleaseBlock (lpAddr: Pointer); virtual;

      function        ReadMemory (lpSource, lpDest: Pointer; cbSize: NativeUInt): NativeUInt; virtual;
      function        WriteMemory (lpDest, lpSource: Pointer; cbSize: NativeUInt): NativeUInt; virtual;


     end;

     TExtProcessAccessor = class (TProcessAccessor)
     private
      FPID: DWORD;
      FHandle: THandle;

     public

      { props }
      property               Handle: THandle read FHandle;
      property               PID: DWORD read FPID write FPID;


      { C & D }
      constructor     Create ( APID: DWORD );
      destructor      Destroy; override;
      { methods }

      function        OpenAccess: Boolean; override;
      function        QueryBasicInfo ( lpAddr: Pointer ): TMemoryBasicInformation;  override;
      function        ReadMemory (lpSource, lpDest: Pointer; cbSize: NativeUInt): NativeUInt; override;
      function        WriteMemory (lpDest, lpSource: Pointer; cbSize: NativeUInt): NativeUInt; override;

     end;


var
     ImagehlpApiVersion: function (): LPAPI_VERSION; stdcall = nil;
         SymInitializeW: function ( hProcess: THandle; szPath: PWideChar; fInvadeProcess: Boolean ): Boolean; stdcall = nil;
    SymGetSymFromAddr64: function ( hProcess: THandle; Address: UInt64; Displacement: PUInt64; Symbol: PIMAGEHLP_SYMBOL64): Boolean; stdcall = nil;
          SymGetOptions: function (): DWORD; stdcall = nil;
          SymSetOptions: function ( SymOptions: DWORD ): DWORD; stdcall = nil;
   SymGetLineFromAddr64: function ( hProcess: THandle; Address: UInt64; Displacement: PUInt64; Line: PImageHlpLine64): Boolean; stdcall = nil;
  UnDecorateSymbolNameW: function ( const DecoratedName: PWideChar; UnDecoratedName: PWideChar; UndecoratedLength, Flags: DWORD): DWORD; stdcall = nil;


function   GetProtSet (const prot: DWORD): TProtSet;
function   UnlockRegion (p: Pointer): String;
function   QueryAccessRights: boolean;
function   TestRegionProtect(p: Pointer; sz: DWORD; const pset: String = 'r'): Boolean;


implementation
uses Math;



function  GetProtSet (const prot: DWORD): TProtSet;
var bRead, bWrite, bExecute: Boolean;
begin
 bRead := prot in [PAGE_READONLY, PAGE_EXECUTE_READ, PAGE_READWRITE,
                   PAGE_EXECUTE_READWRITE];
 bWrite := prot in [PAGE_READWRITE, PAGE_EXECUTE_READWRITE, PAGE_WRITECOPY,
                                    PAGE_EXECUTE_WRITECOPY];
 bExecute := prot in [PAGE_EXECUTE, PAGE_EXECUTE_READWRITE, PAGE_EXECUTE_WRITECOPY];
 result := [];
 if (bRead) then result := result + [paReadable];
 if (bWrite) then result := result + [paWriteable];
 if (bExecute) then result := result + [paExecutable];
 if (prot = PAGE_GUARD) then result := result + [paGuarded];
 if (prot = PAGE_NOACCESS) then result := result + [paNoaccess];
end;


function   TestRegionProtect(p: Pointer; sz: DWORD; const pset: String = 'r'): Boolean;
var
   mbi: TMemoryBasicInformation;
    ps: TProtSet;
     i: Integer;
begin
 result := FALSE;
 FillChar(mbi, sizeof(mbi), 0);
 // TODO: need loop for check several pages
 if ( VirtualQueryEx (GetCurrentProcess, p, mbi, sizeof(mbi)) > 0 ) and (mbi.State <> MEM_FREE) then
  begin
   ps := GetProtSet (mbi.Protect);
   result := TRUE;
   for i := 0 to Length(pset) do
     case pset[i] of
      'r': result := result and (paReadable in ps);
      'w': result := result and (paWriteable in ps);
      'e': result := result and (paExecutable in ps);
     end;
  end;
end;



function    QueryAccessRights: boolean;

var
   _OpenProcessToken: function (ProcessHandle: THandle; DesiredAccess: DWORD; var TokenHandle: THandle): BOOL; stdcall;
   _LookupPrivilegeValue: function (lpSystemName, lpName: PChar; var lpLuid: TLargeInteger): BOOL; stdcall;
   _AdjustTokenPrivileges: function  (TokenHandle: THandle; DisableAllPrivileges: BOOL; const NewState: TTokenPrivileges; BufferLength: DWORD;
                                      PreviousState: PTokenPrivileges; ReturnLength: PDWORD): BOOL; stdcall;
var
   ts: TOKEN_PRIVILEGES;
   hToken: THandle;
   pvalue: Int64;
   hlib: THandle;
begin
 result := false;
 hToken := 0;
 hlib := LoadLibrary (advapi32);
 if hlib = 0 then exit;
 _OpenProcessToken := GetProcAddress (hlib, 'OpenProcessToken');
 _LookupPrivilegeValue := GetProcAddress (hlib, 'LookupPrivilegeValueA');
 _AdjustTokenPrivileges := GetProcAddress (hlib, 'AdjustTokenPrivileges');
 if (@_OpenProcessToken <> nil) and
    (@_LookupPrivilegeValue <> nil) and
    (@_AdjustTokenPrivileges <> nil) then
 repeat
  if not _OpenProcessToken (GetCurrentProcess,
                   TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY,
                   hToken) then break;

  if not _LookupPrivilegeValue(nil, 'SeDebugPrivilege',  pvalue) then break;
  ts.PrivilegeCount := 1;
  ts.Privileges [0].Luid := pvalue;
  ts.Privileges [0].Attributes := SE_PRIVILEGE_ENABLED;
  result := _AdjustTokenPrivileges (hToken, FALSE, ts, sizeof (ts), nil, nil);
 until true;
 if hToken > 0 then CloseHandle (hToken);
 FreeLibrary (hlib);
 // if result then ODS ('OK. ѕолучены дополнительные права (SeDebugPrivilege)');
end; // QAR



function UnlockRegion (p: Pointer): String;
var
   mbi: TMemoryBasicInformation;
   old: DWORD;
    sz: DWORD;
begin
 FillChar (mbi, sizeof(mbi), 0);

 if VirtualQuery (p, mbi, sizeof (mbi)) = 0 then
   result := Format ('VirtualQuery failed for address $%p: %s', [p, Err2Str(GetLastError)])
 else
  begin
   sz := Max ( DWORD(P) - DWORD (mbi.BaseAddress),  mbi.RegionSize);
   if VirtualProtect (mbi.BaseAddress, sz, PAGE_EXECUTE_READWRITE, old) then
      result := Format ('#SUCCESS: VirtualProtect unprotects region at $%p, size = %f MiB: ', [mbi.BaseAddress, sz / 1048576])
   else
      result := Format ('VirtualProtect failed for address $%p, size = %f MiB: ', [mbi.BaseAddress, sz / 1048576]) + Err2Str (GetLastError);
  end;
end;

{ TTlh32 }

procedure TTlh32.Close;
begin
 if (self = nil) then exit;
 if Handle <> 0 then CloseHandle (Handle);
 FHandle := 0;
end;

constructor TTlh32.Create(dwFlags, srcPid: dword);
begin
 FHandle := 0;
 flags := dwFlags;
 _srcpid := srcpid;
end;

destructor TTlh32.Destroy;
begin
 close;
 inherited;
end;


function TTlh32.ModuleFirst(var me: TModuleEntry32): boolean;
begin
 me.dwSize := sizeof (me);
 result := Module32First (handle, me);
end;

function TTlh32.ModuleNext(var me: TModuleEntry32): boolean;
begin
 me.dwSize := sizeof (me);
 result := Module32Next (handle, me);
end;

function TTlh32.ProcessFirst(var pe: TProcessEntry32): boolean;
begin
 pe.dwSize := SizeOf (pe);
 result := Process32First (Handle, pe);
end;

// Ќахождение следующего процесса
function TTlh32.ProcessNext(var pe: TProcessEntry32): boolean;
begin
 pe.dwSize := SizeOf (pe);
 result := Process32Next (Handle, pe);
end;

function TTlh32.ThreadFirst(var te: TThreadEntry32): boolean;
begin
 te.dwSize := SizeOf (te);
 result := Thread32First (Handle, te);
end;

function TTlh32.ThreadNext(var te: TThreadEntry32): boolean;
begin
 te.dwSize := SizeOf (te);
 result := Thread32Next (Handle, te);
end;

procedure TTlh32.Update;
begin
 Close;
 FHandle := CreateToolHelp32Snapshot (flags, _srcPid);
end; // Update handle

{ TProcessList }

procedure TProcessList.Clear;
begin
 inherited;
 FComplete := FALSE;
 need_first := TRUE;
end;

constructor TProcessList.Create;
begin
 th := TTlh32.Create (TH32CS_SNAPPROCESS);
 inherited Create ( sizeof (TProcessEntry32) );
 SetSize (256);
 FillChar (pe_upd, sizeof(pe_upd), 0);
 pe_upd.dwSize := sizeof (pe_upd);
 need_first := TRUE;
end;

destructor TProcessList.Destroy;
begin
 th.Free;
 inherited;
end;

function TProcessList.FindByFile;
var
   n: dword;
   s: string;
   pel: PProcessEntryList;
begin
 if (updateBefore) then Update;

 result := -1; // not found :-(

 pel := Items;
 s := LowerCase (fName);

 for n := 0 to Count - 1 do
  if ( pos (s, LowerCase (pel [n].szExeFile)) > 0 ) then
   begin
    result := n;
    exit;
   end;

end; // FindByFile

function TProcessList.GetEntryList: PProcessEntryList;
begin
 result := Memory;
end;


function TProcessList.ListHash: DWORD;
begin
 result := CalcCRC32 (Items, RowSize * Count );
end;


procedure TProcessList.Update;
begin
 Clear;
 while UpdateOne ( nil ) do;
end; // Update

function TProcessList.UpdateOne(ppe: PProcessEntry32): Boolean;
begin
 if ppe = nil then
    ppe := AddRow;

 if need_first and Complete then Clear;

 // need_first := need_first or (Count = 0);

 if need_first then
   begin
    th.Update;
    result :=  th.ProcessFirst(pe_upd);
    need_first := not result;
    FComplete  := FALSE;
   end
 else
   begin
    result := th.ProcessNext(pe_upd);
    FComplete := not result;
    need_first := Complete;
   end;
 ppe^ := pe_upd;
end; // UpdateOne



{ TModuleList }

constructor TModuleList.Create(srcpid: DWORD);
begin
 th := tTlh32.Create(TH32CS_SNAPMODULE, srcpid);
end;

destructor TModuleList.Destroy;
begin
 th.Free;
 inherited;
end; // ml destroy


function B2MiB(v: Double): Double;
begin
 result := v / 1048576.0;
end;

function GetFileSizeEx (const sFileName: String): Int64;
var
   L: LARGE_INTEGER;
   hFile: THandle;
begin
 result := -1;
 if not FileExists (sFileName) then exit;

 hFile := CreateFile ( PChar (sFileName), GENERIC_READ, 7, nil, OPEN_EXISTING, 0, 0);
 if hFile = INVALID_HANDLE_VALUE then exit;

 L.LowPart := GetFileSize (hFile, @L.HighPart);

 CloseHandle (hFile);

 result := L.QuadPart;
end;

procedure TModuleList.Dump;

var
   n: Integer;
   d, r, s: String;
   file_size: Int64;
    mem_size: Int64;

   pme: PModuleEntry32;
begin
 d := '[~T].~C0F #DBG: Module(DLLs) dump:~C07'#13#10;
 mem_size := 0;

 for n := 0 to Count - 1 do
  begin
   r := '';
   pme := @Items [n];
   if pme.dwSize <> sizeof (TModuleEntry32) then continue;

   s := Trim ( pme.szModule );
   while Length (s) < 15 do s := s + ' ';

   file_size := GetFileSizeEx ( Trim (pme.szExePath) );

   Inc (mem_size, pme.modBaseSize);

   r := CFormat ('  BA=$%p     szMod = %s     modBaseSize = %7.3f MiB,   fileSize = %7.3f MiB', '~C07',
                 [pme.modBaseAddr, s, B2MiB (pme.modBaseSize), B2MiB(file_size) ]);

   s := Trim ( pme.szExePath );
   if FileExists (s) then
     begin
      s := GetFileVersionStr (s);
      // while Length (s) < 20 do s := ' ' + s;
      r := r + '        version = ~C0B' + s + '~C07';
     end;

   d := d + r + #13#10;
  end;

 d := d + CFormat('memory allocated for modules = %.3f MiB', '~C07', [B2MiB(mem_size)]);
 ODS (d, PMF_OUTFILE);

end;



function TModuleList.Find(const s: string): Integer;
var n: Integer;
begin
 result := -1;
 for n := 0 to Count - 1 do
  if (pos (LowerCase (s), LowerCase (FItems [n].szModule)) > 0) then
   begin
    result := n;
    exit;
   end;
end; // ModuleList.Find

function TModuleList.GetItems: PModuleEntryList;
begin
 result := @FItems[0];
end;

function cmpModuleAddr (a, b: Pointer): Integer;
var
   am, bm: PModuleEntry32;

begin
 am := a;
 bm := b;
 result := 0;
 if DWORD (am.modBaseAddr) > DWORD (bm.modBaseAddr) then result := +1;
 if DWORD (am.modBaseAddr) < DWORD (bm.modBaseAddr) then result := -1;
end;

procedure TModuleList.SortByAddr;
var
   pme: PModuleEntry32;
   n: Integer;
   lsrt: TList;
begin
 lsrt := TList.Create;
 try

   for n := 0 to Count - 1 do
    begin
     pme := AllocMem ( sizeof(TModuleEntry32) );
     SafeMove ( Items [n], pme^, sizeof(TModuleEntry32) );
     lsrt.Add (pme);
    end;

   lsrt.Sort (cmpModuleAddr);

   for n := 0 to lsrt.Count - 1 do
    begin
     pme := lsrt [n];
     SafeMove ( pme^, Items [n], sizeof (TModuleEntry32));
     FreeMem (pme);
    end;
 finally
  lsrt.Free;
 end;
end;

procedure TModuleList.Update;
var n: dword;
begin
 th.Update;
 FCount := 0;
 n := 0;
 if th.ModuleFirst (FItems [n]) then
  repeat
   Inc (n);
  until (n > 255) or ( not th.ModuleNext (FItems [n]) )
 else exit;
 FCount := n;
 SortByAddr;
end; // ml update

{ TWndProcessList }

function TWndProcessList.AddWindow(hWnd: THandle): Boolean;
var
    bAdd: Boolean;
    stitle: array [0..256] of char;
begin
 //
 result := count < wpslsize;
 if not result then exit;
 bAdd := IsWindow (hWnd);
 bAdd := bAdd and ( IsWindowVisible (hWnd) or bAddInvisible );
 bAdd := bAdd and ( (0 = GetParent (hWnd)) and
                    (0 = GetWindow (hWnd, GW_OWNER)) or
                    (not bMainOnly) );
 if bAdd then
  begin
   wpslist [count].hWnd := hWnd;
   with wpslist [count] do
    begin
     tid  := GetWindowThreadProcessId (hWnd, pid);
     pid_hash := 1 + pid_hash xor pid;
     GetWindowText (hWnd, stitle, 256);
     title := format ('(0x%x) %s', [pid, stitle]);
    end;
   inc (wpscount);
  end;
end;

constructor TWndProcessList.Create(size: Integer);
begin
 //
 bMainOnly := true;
 wpslsize := size;
 wpscount := 0;
 SetLength (wpslist, size);
end;


function EnWinProc (h: THandle; p: LPARAM): boolean; stdcall;
var
   wlist: TWndProcessList;
begin
 result := false;
 wlist := TWndProcessList (p);
 if not Assigned (wlist) then exit;
 // ƒобавление окна
 result := wlist.AddWindow (h);
end; // EnWinProc

destructor TWndProcessList.Destroy;
begin
 wpslsize := 0;
 wpscount := 0;
 SetLength (wpslist, 0);
end; // Destroy

function TWndProcessList.GetItem(i: Integer): TWndProcess;
begin
 FillChar (result, sizeof (result), 0);
 if (i < 0) or (i >= count) then exit;
 result := wpslist [i];
end; // GetItem

procedure TWndProcessList.Update;
begin
 wpscount := 0;
 pid_hash := 0;
 EnumWindows (@EnWinProc, Integer (self));
end;


function FormatDump ( src: PByteArray; cb, need: Integer ): String;
var
   i: Integer;
   s: String;
begin
 s := '';
 for i := 0 to cb - 1 do
     s := s + ' ' + IntToHex ( src [i], 2 );
 for i := cb to need - 1 do
     s := s + '   ';
 result := s;
end;

{ TModuleMap }

procedure TModuleMap.DumpMap;
var
  fconst: TStrMap;
   fname: String;
    dump: array [0..31] of BYTE;
     tmp: TStrMap;
     ofs: DWORD;
     pfv: PSingle;
      va: Pointer;
      vn: Pointer;
      vr: Pointer;
      rb: NativeUInt;
      sz: NativeUInt;
      l2: Integer;
      nm: String;
      sv: String;
       s: String;
       n: Integer;
       i: Integer;
begin
 fname := AnsiReplaceStr (Name, '.map', '.dump');
 fconst := TStrMap.Create;
 // fconst.Duplicates := dupIgnore;
 // fconst.Sorted := TRUE;
 tmp := TStrMap.Create (nil);

 try
  tmp.Add ( 'DLL_LOAD_ADDR = ' + IntToHex( hDLL, 8 ) );

  l2 := Count - 2;

  for n := 0 to l2 do
   begin
    va := Pointer ( Objects [n + 0] );
    vn := Pointer ( Objects [n + 1] );

    ofs := DWORD (va) - hDLL;
    sz := DWORD(vn) - DWORD (va);
    rb := 0;
    nm := Strings [n];
    if Pos ( 'locret_', Strings[n + 1] ) > 0 then
      begin
       if n < l2 then
        begin
         vr := Pointer ( Objects [n + 1] );
         sz := DWORD (vr) - DWORD (va); // true function[rest] length (!)
        end
       else
        Inc (sz);
      end;


    s := IntToHex ( ofs, 8 ) + ' ' + IntToHex ( sz, 4 ) + #9;

    ReadProcessMemory (hp, RelativePtr ( va, - 8), @dump, 8, rb);                   // pre function bytes
    s := s + FormatDump ( @dump, rb, 8 ) + '|';
    ReadProcessMemory (hp, va, @dump, Min (sz, sizeof(dump)), rb);  // function bytes
    s := s + FormatDump ( @dump, rb, sizeof (dump) );



    if ( Pos('$float_const', nm ) > 0 ) and ( rb >= 4 ) then
     try
      pfv := @dump;
      sv := '?_' + ftow ( pfv^ );
      sv := AnsiReplaceStr ( sv, '-', 'minus_');
      sv := AnsiReplaceStr ( sv, '.', '_');
      i := fconst.Add(sv);
      sv := sv + '@f' + IntToHex(i, 3);
      nm := AnsiReplaceStr ( nm, '$float_const', sv + '@@3MB');

     except
      on E: Exception do
         nm := AnsiReplaceStr ( nm, '$float_const', '?float_val_NAN@@3MB' );
     end;

    s := s + ' =>> ' + nm;
    tmp.Add (s);
   end;

  tmp.SaveToFile (fname);
 finally
  fconst.Free;
  tmp.Free;
 end;

end;


procedure TModuleMap.ImportMap;
var
   pub: TMfPublic;
    va: NativeUInt;
     i: Integer;
begin
 if not Assigned (FMapFile) then exit;

 for i := 0 to 15 do
  begin
   // seg := FMapFile.fin
  end;

 for i := 0 to 65535 do
   begin
    pub := FMapFile.FindPublic (i);
    if not pub.IsValid then
       break;

    va := NativeUInt (pub.Address) + hDLL;
    AddObject ( pub.Name, Ptr(va) );
   end;
end;

procedure TModuleMap.LoadMap(const sFileName: String);
const
    BASE_MASK = $0FFFFFFF; // едва-ли модуль будет больше 256 мб размером




var
 flds: TStrMap;
 base: NativeUInt;
 buff: array [0..255] of WideChar;
  tmp: TStrMap;

  ssz: NativeUInt;
  seg: NativeUInt;
  ofs: NativeUInt;
  prv: NativeUInt;
   bz: NativeUInt;
   sz: NativeUInt;

   sp: Boolean; // sections parsing
   ap: Boolean; // addrs parsing
   sm: Boolean; // have column sum rva+base
   ns: Integer;
   sc: Integer; // sections counter
    n: Integer;
    p: Integer;

    s: String;
    t: String;
begin
 Clear;
 Name := sFileName;

 Assert (hDLL > 0, 'LoadMap: hDLL = 0');

 FMapFile := LoadMapFile (sFileName);
 if Pos('xrgame', LowerCase(sFileName)) > 0 then
    __nop;

 if not FMapFile.IsValid then
    FreeAndNil (FMapFile);

 tmp := TStrMap.Create (self);
 tmp.LoadFromFile (sFileName);
 flds := TStrMap.Create (self);


 {
 if tmp.FindSub ('Detailed map of segments') +
    tmp.FindSub ('Publics by Name') > 0 then
      begin
       tmp.Free;
       ODS('[~T]. #DBG: Detected Delphi MAP file format, using indirect');
       ImportMap;
       exit;
      end; // }




 try
  ap := FALSE;
  sp := FALSE;

  FFirstAddr := NativeUInt (-1);
  FLastAddr := 0;


  for n := 0 to High (FSections) do FSections [n] := 0;

  FSegments  [1] := $1000;
  FSegClass [0] := 'CODE';

  seg_count := 0;

  seg := 0;
  ssz := $1000; // почти всегда страничка уходит под заголовок PE
  sc := 0;
  sm := FALSE;
  base := $10000000;

  try

    prv := 0;


    for n := 0 to tmp.Count - 1 do
     begin
      s := Trim ( tmp [n] );
      if ( s = 'Exports' ) or ( Pos('Line numbers', s ) > 0 ) then break; // disabled

      if ( n < 10 ) and ( Pos('Start ', s) > 0 )   and ( Pos ('Length ', s) > 0 )  then
          begin
           sp := TRUE;
           continue;
          end;
      if ( n > 01 ) and ( Pos('Address ', s) > 0 ) and ( Pos ('Publics ', s) > 0 ) then
          begin
           prv := 0;
           ap := TRUE;
           sm := ( Pos('Rva+Base', s) > 0 );
           continue;
          end;

      if ( n > 01 ) and ( Pos('Detailed map', s) > 0 ) then
          begin
           sp := FALSE; // large segments end
           continue;
          end;

      if ( Pos (':', s) = 0 ) or ( Pos ('Timestamp', s) > 0 ) or ( Length (s) < 16 ) then continue; // non parseable



      t := '$' + Copy (s, 1, 4);      // segment index
      ns := atoi (t);

      System.Delete (s, 1, 5);

      ofs := atoi ( '$' + Copy (s, 1, 8) );

      System.Delete (s, 1, 8); // remove segment offset
      s := Trim (s);

      Assert (ns < High (FSegments), 'Index of segment to large = ' + IntToStr(ns) +
                  Format(#13#10'line: %d = "%s"', [n, tmp[n]]) );

      // Dec (ns);

      repeat
        p := Pos('  ', s);
        if p > 0 then System.Delete (s, p, 1);
      until p = 0;

      flds.Split (space_replace, HideSP(s));

      if flds.Count < 1 then continue;

      if ap then
        begin
         // приведение индекса сегмента
         if Count = 0 then
            sc := ns;

         if ns < 0 then continue;

         if sm and (flds.Count >= 2) then
            begin
             bz := atoi ('$' + flds[1]);  // Rva+base addr

             if Pos('__ImageBase', s) > 0  then
                base := bz;

             if bz >= base then
                Dec (bz, base)
             else
                bz := bz and BASE_MASK;

             // сегменты иногда очень непредсказуемо располагаютс€, и рассчитать их точное положение из map файла не выйдет
             if ns > Integer(prv) then
                FSegments[ns] := bz - ofs;

            end;

         prv := ns;
         bz := FSegments [ns];
         Inc (ofs, bz);
         Inc (ofs, hDLL);   // generale  base
         FFirstAddr := Min (FFirstAddr, ofs);
         FLastAddr  := Max (FLastAddr, ofs);

         if ( Pos('byte_', s) = 1 ) then continue;
         // if ( Pos('flt_', s) = 1 ) then s := '$float_const'
         s := flds[0];
         if Pos ( '??_7', s ) = 1 then
                 __nop;  // vftable?

         if ( Pos('?', s) > 0 ) and Assigned (UnDecorateSymbolNameW) then
           begin
            FillChar(buff, sizeof(buff), 0);
            try
              if UnDecorateSymbolNameW (PWideChar(s), buff, 255, 0) > 0 then
                 s := buff;

            except
             on e: Exception do
               PrintError('exception ' + E.Message + '  catched in UnDecorateSymbolNameW for ' + s);
            end;
           end;

         s := FSegClass [ns] + ':.' + s;

         AddObject ( s, Ptr (ofs) );
        end
      else
      if sp then
        begin
         p := Pos ('H', s);
         t := '$' + Copy (s, 1, p - 1);

         sz := atoi ( t );

         if sz and $3 > 0 then sz := sz or $3 + 1;

         if flds.Count >= 3 then
            FSectName  [sc] := IntToHex(ns, 4) + '.' + flds [flds.Count - 2];

         if ns > Integer(prv) then
          // new segment detected
          begin
           if ssz and $FFF > 0 then ssz := ssz or $FFF + 1;
           Inc (seg, ssz);
           FSegments [ns] := seg;
           FSegClass [ns] := flds [flds.Count - 1];
           seg_count := ns + 1;
           ssz := 0;
           prv := ns;
          end;

         // if sc = 0 then
         FSections [sc]     := ofs;
         FSections [sc + 1] := FSections [sc] + sz; // next section RVA
         Inc (ssz, sz);
         Inc (sc);
         if sc = High (FSections) then
           begin
            PrintError('To many sections. Incompatible file format? Breaked at line ' + IntToStr (n + 1) );
            break;
           end;
        end;


     end; // for
  except
   on E: Exception do
       OnExceptLog ('TModuleMap.LoadMap', E);
  end;

 finally
  flds.Free;
  tmp.Free;
 end;
end;

function TModuleMap.PtrInfo ( p: Pointer; bwd_range, fwd_range: NativeUInt ): String;
var
      n,  i: Integer;
     lbound: NativeUInt;
     rbound: NativeUInt;
        pub: TMfPublic;
    ptrp_va: NativeUInt;
    test_va: NativeUInt;
begin
 result := '';

 ptrp_va := NativeUInt(p);

 if self.Count = 0 then
  begin
   if FMapFile = nil then exit;
   pub := FMapFile.FindPublic ( p );
   exit;
  end;

 if ( ptrp_va < FirstAddr ) or ( ptrp_va > LastAddr ) then exit;

 i := -1;

 if ( bwd_range + fwd_range = 0 ) then
    i := IndexOfObject (p)
 else
   for n := 0 to Count - 1 do
    begin
     // опорный указатель из хранилища
     test_va := NativeUInt( Objects[n] );
     lbound := test_va - bwd_range;
     rbound := test_va + fwd_range;
     // все значени€ в диапазоне base .. base + range
     if ( lbound <=  ptrp_va  ) and ( ptrp_va <= rbound ) then
         begin
          i := n;
          if ptrp_va > test_va then
             fwd_range := Min ( fwd_range, ptrp_va - test_va ); // зажатие диапазона forward
          if ptrp_va < test_va then
             bwd_range := Min ( bwd_range, test_va - ptrp_va ); // зажатие диапазона backward

         end
    end;



 if i >= 0 then
   begin
    test_va := NativeUInt( Objects[i] );
    result := self [i]; // base_name
    if test_va < ptrp_va then
       result := result + ' + $' + IntToHex(ptrp_va - test_va, 4);
    if test_va > ptrp_va then
       result := result + ' - $' + IntToHex(test_va - ptrp_va, 4);

   end;


end;

{ TProcessAccessor }

function TProcessAccessor.CheckAccess(lpAddr: Pointer; cbSize: NativeUInt; fProt: DWORD): Boolean;
var
   mbi: TMemoryBasicInformation;
   lim: NativeUInt;
   nxb: NativeUInt;
begin
 result := FALSE;
 mbi := QueryBasicInfo (lpAddr);
 if mbi.RegionSize = 0 then exit;

 lim := NativeUInt (lpAddr) + cbSize;
 nxb := NativeUInt (mbi.BaseAddress) + mbi.RegionSize;

 // fProt := PAGE_READONLY;
 // какой-то доступ разрешен и блок умещаетс€ в регион
 result := ( mbi.Protect and fProt > 0 ) and ( lim <= nxb ) and ( mbi.State and MEM_NO_ACCESS = 0 );
end;

constructor TProcessAccessor.Create;
begin
 FAttached := TRUE; // insider mode
end;

function TProcessAccessor.OpenAccess: Boolean;
begin
 result := TRUE;
end;

function TProcessAccessor.QueryBasicInfo(lpAddr: Pointer): TMemoryBasicInformation;
begin
 FillChar (result, sizeof(result), 0);
 VirtualQuery (lpAddr, result, sizeof(result));
end;

function TProcessAccessor.ReadMemory(lpSource, lpDest: Pointer; cbSize: NativeUInt): NativeUInt;
begin
 result := 0;

 if CheckAccess (lpSource, cbSize, PAGE_READ_ACCESS ) then
    try
     if lpDest <> lpSource then
        CopyMemory (lpDest, lpSource, cbSize);
     result := cbSize;
    except
     on E: EAccessViolation do
        result := 0;

    end;

end;

procedure TProcessAccessor.ReleaseBlock(lpAddr: Pointer);
begin
 // no actions
 FreeMem (lpAddr);
end;

function TProcessAccessor.WriteMemory(lpDest, lpSource: Pointer; cbSize: NativeUInt): NativeUInt;
begin
 if CheckAccess (lpDest, cbSize, PAGE_READWRITE or PAGE_EXECUTE_READWRITE ) then
   begin
    result := cbSize;
    if lpDest <> lpSource then
       CopyMemory (lpDest, lpSource, cbSize);
   end
 else
    result := 0;

end;

{ TExtProcessAccessor }

constructor TExtProcessAccessor.Create(APID: DWORD);
begin
 inherited Create();
 PID := APID;
 FAttached := FALSE;
end;

destructor TExtProcessAccessor.Destroy;
begin
 CloseHandle (Handle);
 inherited;
end;

function TExtProcessAccessor.OpenAccess: Boolean;
begin
 if Handle > 0 then
    CloseHandle (Handle);

 FHandle := OpenProcess (PROCESS_ALL_ACCESS, FALSE, PID);
 FAttached := ( Handle <> 0 );
 result := Attached;
end;

function TExtProcessAccessor.QueryBasicInfo(lpAddr: Pointer): TMemoryBasicInformation;
begin
 FillChar (result, sizeof(result), 0);
 if VirtualQueryEx (Handle, lpAddr, result, sizeof(result)) = 0 then
   begin
    result.RegionSize := 0;
    FLastCode := GetLastError;
    FLastError := err2str (LastCode);
   end;
end;

function TExtProcessAccessor.ReadMemory(lpSource, lpDest: Pointer; cbSize: NativeUInt): NativeUInt;
begin
 result := 0;
 if not CheckAccess (lpSource, cbSize, PAGE_READ_ACCESS) then
    begin
     FLastError := 'Not accessible ' + FormatPtr(lpSource);
     FLastCode := -38005;
     exit;
    end;
 if  ReadProcessMemory (Handle, lpSource, lpDest, cbSize, result) then
     ASSERT (  result = cbSize, 'ReadProcessMemory returned ' + IntToStr(result) )
 else
    begin
     FLastCode := GetLastError;
     FLastError := err2str (FLastCode);
    end;
end;

function TExtProcessAccessor.WriteMemory(lpDest, lpSource: Pointer; cbSize: NativeUInt): NativeUInt;
begin
 result := 0;
 if not CheckAccess (lpDest, cbSize, PAGE_RW_ACCESS) then exit;
 if  WriteProcessMemory (Handle, lpDest, lpSource, cbSize, result) then
     ASSERT (  result = cbSize, 'WriteProcessMemory returned ' + IntToStr(result) );

end;

{ TVMRegion }

procedure TVMRegion.Import(mbi: TMemoryBasicInformation);
begin
 Init ( mbi.BaseAddress, mbi.RegionSize, mbi.Protect, mbi.State, mbi.Type_9 );
 alloc_ptr := mbi.AllocationBase;
end;

function TVMRegion.Includes(addr: NativeUint): Boolean;
begin
 result := (addr >= remote_addr) and (addr < RemoteLimit);
end;

procedure TVMRegion.Init(bp: Pointer; reg_size: NativeUInt; prot, st, typ: DWORD);
begin
 alloc_ptr := bp;
 base_ptr := bp;
 size := reg_size;
 rsize := reg_size;
 protect := prot;
 protset := GetProtset (prot);
 rtype := typ;
 state := st;
 limit := base_addr + rsize;
 updt := Now;
end;

function TVMRegion.RemoteLimit: TVMOffset;
begin
 result := remote_addr + rsize;
end;


function  CheckVersion ( src: String ): Boolean;
var
   ver: DWORD;
begin
 ver := GetFileVersion ( src );
 result := ( ver >= $51000 );
end;


procedure LoadDebugHelp;
var
   hLib: HMODULE;
   from: String;
    src: String;
    ver: LPAPI_VERSION;

begin
 src := 'DbgHelp.dll';
 hLib := GetModuleHandle (PChar(src));

 if hLib = 0 then
     begin
       if  not FileExists (src) or
           not CheckVersion (src)  then
            begin
             from := GetEnvironmentVariable ('SystemRoot');
             if from = '' then
                from := GetEnvironmentVariable ('WINDIR');

             if DirectoryExists ( from + '\SysWOW64' ) then
                from := from + '\SysWOW64'
             else
                from := from + '\System32';


             src := CorrectFilePath (from + '\DbgHelp.dll');

            end;

       Assert ( FileExists (src), ' file not exists ' + src );

       hLib := LoadLibrary ( PChar(src) );
     end;
 if hLib = 0 then exit;
 try
   SymInitializeW         := GetProcAddress (hLib, 'SymInitializeW');
   SymGetSymFromAddr64    := GetProcAddress (hLib, 'SymGetSymFromAddr64');
   SymGetOptions          := GetProcAddress (hLib, 'SymGetOptions');
   SymSetOptions          := GetProcAddress (hLib, 'SymSetOptions');
   SymGetLineFromAddr64   := GetProcAddress (hLib, 'SymGetLineFromAddrW64');
   ImagehlpApiVersion     := GetProcAddress (hLib, 'ImagehlpApiVersion');
   UnDecorateSymbolNameW  := GetProcAddress (hLib, 'UnDecorateSymbolNameW');

   // Assert ( Assigned(ImagehlpApiVersion), 'DbgHelp.dll is very old!' );
   {$IFNDEF CPUX64}

   ver := ImagehlpApiVersion;
   Assert ( CheckVersion (src),
              Format ('DbgHelp.dll API version is old = %d.%d revision %d ',
                          [ver.MajorVersion, ver.MinorVersion, ver.Revision] ) )
   {$ENDIF}

  except
    on E: Exception do
      begin
       PrintError (E.Message);
       // ExitProcess ( $10000 );
      end;


  end;


end;

initialization
 LoadDebugHelp;

end.

