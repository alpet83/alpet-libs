unit misc;

interface
uses Windows, PsAPI, TlHelp32, SysUtils, Classes, ContNrs, StrUtils, ModuleMgr, AnsiStrings;

{$WARN SYMBOL_PLATFORM OFF}


const
 DIR_CRC_POLY = $04C11DB7;
 REV_CRC_POLY = $EDB88320;

 LOG_STREAMS = 8; // сколько потоков журналирования (соотв. файлов тоже)

 EVENT_CONSOLE_END_APPLICATION = $4007;

 INVALID_SET_FILE_POINTER = DWORD(-1);


    EC_ASSERTION  = $0EEDFADE;
 MS_VC_EXCEPTION =  $406d1388;

 PMF_NO_ODS    = $001;
 PMF_OUTFILE   = $002;
 PMF_OUTCON    = $004;
// PMF_DIRECT    = $008;
 PMF_UNFMT     = $100;
 PMF_DIRECT    = $200;
 PMF_SYSTIME   = $400;
 PMF_NOCACHE   = $800;
 PMF_IMPORTANT = $0FE;

 FILE_READ_EA         = $0008;
 FILE_READ_ATTRIBUTES = $0080;

 MIB_BYTES = 1048576;
 MEBIBYTE = MIB_BYTES;
 MIB_COEF = 1.0 / MIB_BYTES;

 MSEC_IN_SEC         = 1000;
 SEC_IN_HOUR         = 3600;

 SEC_PER_DAY         = 24.0 * SEC_IN_HOUR;
 MS_PER_DAY          = SEC_PER_DAY * MSEC_IN_SEC;
 ONE_SEC_COEF        = 1.0 / SEC_PER_DAY;
 ONE_MSEC_COEF       = 1.0 / MS_PER_DAY;

 // privileges
 SE_CRGLOBAL_NAME = 'SeCreateGlobalPrivilege';
 SE_MEMLOCK_NAME =  'SeLockMemoryPrivilege';
 SE_INCREASE_QUOTA_NAME = 'SeIncreaseQuotaPrivilege';
 SE_DEBUG_PRIVILEGE   = 'SeDebugPrivilege';


 NT_INFORMATION      = $40000000;
 NT_WARNING          = $80000000;
 NT_ERROR            = $C0000000;




 WAIT_ALL  = 1;
 ConfigDir = 'conf';

 FNAME_WRONG_CHARS = [#0..#31, ':', '\', '/', '|', '<', '>'];

 EXCEPTION_CONTINUE_SEARCH    = $00000000;
 EXCEPTION_EXECUTE_HANDLER    = $00000001;
 EXCEPTION_CONTINUE_EXECUTION = $ffffffff;


  VT_EMPTY	= 0;
  VT_NULL	= 1;
  VT_I2	        = 2;
  VT_I4	        = 3;
  VT_R4	        = 4;
  VT_R8	        = 5;
  VT_CY	        = 6;
  VT_DATE	= 7;
  VT_BSTR	= 8;
  VT_DISPATCH	= 9;
  VT_ERROR	= 10;
  VT_BOOL	= 11;
  VT_VARIANT	= 12;
  VT_UNKNOWN	= 13;
  VT_DECIMAL	= 14;
  VT_I1	        = 16;
  VT_UI1	= 17;
  VT_UI2	= 18;
  VT_UI4	= 19;
  VT_I8	        = 20;
  VT_UI8	= 21;
  VT_INT	= 22;
  VT_UINT	= 23;
  VT_VOID	= 24;
  VT_HRESULT	= 25;
  VT_PTR	= 26;
  VT_SAFEARRAY	= 27;
  VT_CARRAY	= 28;
  VT_USERDEFINED    = 29;
  VT_LPSTR	        = 30;
  VT_LPWSTR         = 31;
  VT_RECORD	        = 36;
  VT_INT_PTR	        = 37;
  VT_UINT_PTR	        = 38;
  VT_FILETIME	        = 64;
  VT_BLOB	        = 65;
  VT_STREAM	        = 66;
  VT_STORAGE	        = 67;
  VT_STREAMED_OBJECT	= 68;
  VT_STORED_OBJECT	= 69;
  VT_BLOB_OBJECT	= 70;
  VT_CF	                = 71;
  VT_CLSID	        = 72;
  VT_VERSIONED_STREAM	= 73;
  VT_BSTR_BLOB	        = $fff;
  VT_VECTOR	        = $1000;
  VT_ARRAY	        = $2000;
  VT_BYREF	        = $4000;
  VT_RESERVED	        = $8000;
  VT_ILLEGAL	        = $ffff;
  VT_ILLEGALMASKED	= $fff;
  VT_TYPEMASK	        = $fff;


type
    TDynByteArray = array of BYTE;
    TByteVector = TByteArray;

    TTimeFunc = function: TDateTime;
    PTextFile = ^Text;

    PUIntPtr = ^UIntPtr;
    UIntPtr = NativeUInt;
    IntPtr = NativeInt;
    NTSTATUS = DWORD;
    CLIENT_ID = DWORD;
    KAFFINITY = DWORD;
    KPRIORITY = DWORD;

    AFILE_PATH = array [0..MAX_PATH] of AnsiChar;
    WFILE_PATH = array [0..MAX_PATH] of WideChar;


    THREAD_BASIC_INFORMATION  = record
         ExitStatus: NTSTATUS;
     TebBaseAddress: Pointer;
           ClientId: CLIENT_ID;
       AffinityMask: KAFFINITY;
           Priority: KPRIORITY;
       BasePriority: KPRIORITY;
    end;

    PNT_TIB = ^NT_TIB;

    NT_TIB = packed record
      StackBase: NativeUInt;
     StackLimit: NativeUInt;
      SubSysTib: Pointer;
      FiberData: Pointer; // also Version
      ArbitraryUserPointer: Pointer;
           self: PNT_TIB;
    end;



    PEXCEPTION_POINTERS = ^EXCEPTION_POINTERS;

    TVectoredHandler = function (ExceptionInfo: PEXCEPTION_POINTERS): DWORD; stdcall;
    LPTOP_LEVEL_EXCEPTION_FILTER = TVectoredHandler;

    BigInt = Int64;
    TCharSet = TSysCharSet;
    // TCharSet = set of CHAR;
    TStrMsgEvent = procedure (const AMsg: String; Sender: TObject = nil) of object;  // TNotifyEventEx
    TByteArray2GB = packed array [0..$7FFFFFF0] of BYTE;
    PByteArray2GB = ^TByteArray2GB;

    TODSProc = procedure (msg: PWideChar; print_flags: DWORD = 255); stdcall;

    TLibInitInfo = packed record
     dwVersion: DWORD;
      ODS_proc: TODSProc;
     szMessage: array [0..63] of AnsiChar;
    end;

    PLibInitInfo = ^TLibInitInfo;


    TStringBuff = class
    private
     FLength: Integer;
       FSize: Integer;
     FMemMgr: TMemoryManagerEx;

     procedure SetSize(const Value: Integer);
     function  GetStr: String;
     procedure SetStr(const Value: String);

    protected
     FData: PCHAR;
    public


     property _data: PChar read FData;
     property _size: Integer read FSize write SetSize;
     property _length: Integer read FLength;

     property MemMgr: TMemoryManagerEx read FMemMgr write FMemMgr;
     property StrVal: String read GetStr write SetStr;
     { C & D }

     constructor        Create;
     destructor         Destroy; override;
    end; // TStringBuff


    TDataBlock = class
    private
     FData: Pointer;
     FSize: DWORD;

    public

     { props }
     property           Data: Pointer read FData;
     property           Size: DWORD read FSize;
     { C & D }
     constructor        Create (dwSize: DWORD);
     destructor         Destroy; override;
     { methods }
     procedure          FillZero; inline;
     function           Relative ( ofs: DWORD ): Pointer;
     procedure          Resize (newSize: DWORD);

    end; // TDataBlock


    // комплекс переменных общих для одного процесса
    TMiscSharedVars = packed record
          views_opened: Integer;
          first_module: array [0..271] of CHAR;
          open_history: array [0..16383] of CHAR;

     log_verbose_level: Integer;
       perf_warn_ratio: Integer;
           con_buffers: array [0..255] of DWORD; // TODO: allways check size
              local_dt: TDateTime;
                log_th: TThread;
               reserev: array [0..15] of DWORD;
    end;

    PMiscSharedVars = ^TMiscSharedVars;



var
    ExeFileName: String = '?????';
    ExePath, DllPath, PluginsPath, ModuleName: String;
    ps_creation_time: TDateTime = 0;
             hInput: THandle = 0;
            hOutput: THandle = 0;
    // fcon: Text;
          dtStartup: TDateTime;
        pctrStartup: Int64;
    pctrCoefficient: Double;
         alert_cntr: Integer = 0;
        g_time_func: TTimeFunc = Now;
        g_last_time: TDateTime;


     ve_exception: PEXCEPTION_POINTERS = nil;

    space_replace: Char = '^';
      dbg_present: Boolean = FALSE;

      error_times: array [0..15] of TDateTime;
      main_thread: DWORD = 0;
        lt_writer: DWORD = 0;
      gGlobalStop: Boolean = FALSE;
    gSoundsEnable: Boolean = TRUE;
   ShutdownReason: String = 'DEFAULT';
    g_pfc_coef: Double = 0;


function  AnsiTrim (const s: AnsiString): AnsiString;
function  AnsiTrim2W (const s: AnsiString): String; inline;

function Quote (const s: String): String;
function Unquote (const s: string): String;

function Strtok (var s: string; const tchars: TCharset): String;
procedure ODS (const msg: WideString; print_flags: DWORD = 255); stdcall;
procedure LocalODS (msg: PWideChar; print_flags: DWORD = 255); stdcall;
function  ActiveConsole: Integer;
procedure SelectConsole ( ls: Integer );

procedure PrintError (const msg: String);
procedure ProcessMsg (const cmsg: String; const smsg: String = ''; flags: DWORD = 255);
procedure DeleteColorTags (var msg: String);
function  RemoveColorTags (const msg: String): String; inline;
function  GetModuleHandle (sModName: String): DWORD;


function atof (const s: String): Double;
function ftoa (v: Double; const fmt: String = '%f'): AnsiString;
function ftow (v: Double; const fmt: String = '%f'): WideString;
function ftos (v: Double; max_dig: Integer = 5): String;
function ftosf (v: Double; digits: Integer = 5): String;

function atoi (const s: String): Int64; inline;
function AdSP (const s: String; const lcs: TCharSet = [' ', #9]): String; // добавить пробел, если строка не пустая, и последний символ не входит в множество

function ListIntVal (sl: TStrings; const field: String): Integer;

function TryEnter (var scs: RTL_CRITICAL_SECTION; timeOut: Integer): Boolean;

function HideSP(const s: String): String; inline;
function UnhideSP (const s: String): String; inline;
procedure ColorPrint ( msg: String; colored: Boolean = TRUE; hConOutput: THandle = 0 );
function InfoFmt (s: String; ct: TDateTime = 0): String;
function RealTime (bOnlyTime: Boolean): Extended;
function LimStr (const s: String; maxLen: Integer): String;
function Err2str (nError: Integer = -1): String;
function Ansi2Oem (const s: AnsiString): AnsiString;
function Wide2Oem (const s: WideString): AnsiString;


function ConEventCount: DWORD;
function ReadKey: Char;
function IsKeyDown ( vk: DWORD ): Boolean;
function IsKeyPressed ( vk: DWORD ): Boolean;
function TryReadLn(dwMsec: DWORD = 50) : String;

procedure CBeep( freq, ms: DWORD );


function Z2P(const s: String): String;

function CalcCRC32  ( psrc: Pointer; cntBytes: Integer; CRC_start: DWORD = INFINITE ): DWORD;      // IEEE 802.3 inverted
function CalcCRC32D ( psrc: Pointer; cntBytes: Integer; CRC_start: DWORD = INFINITE ): DWORD;      // IEEE 802.3 direct

function MakeMemStream (psrc: Pointer; cntBytes: Integer): TStream;


function StrSuffix (const src, prefix: String): String;
function StrICmp(const a, b: String): Integer;
procedure SafeExec (Method: TNotifyEvent; Param: TObject = nil);

function  IfV (b: Boolean; i1, i2: Int64): Int64; overload;   inline;
function  IfV (b: Boolean; f1, f2: Double): Double; overload; inline;
function  IfV (b: Boolean; s1, s2: String): String; overload; inline;
function  IfV (b: Boolean; p1, p2: Pointer): Pointer; overload;   inline;

function  CheckMakeDir (sDir: String): Boolean;

procedure InitPaths;
procedure InitConsoleBuffers;
procedure StartLogging (const sPath: String);
procedure SetConSize (x, y, cx, cy: Integer);
function  InBound (v, vmin, vmax: Integer): Boolean;
function  Comma2Pt (const s: String): String;
function  RelativePtr(p: Pointer; ofst: Int64): Pointer; inline;
procedure FreeListItems(l: TList); // освобождает память для элементов
procedure AddMsgToList(sl: TStrings; const msg: String; max_msgs: Integer = 10);
function  GetAddressSpaceUsed: Cardinal;
function  LastChar(const s: String): CHAR;
procedure DeleteLast(var s: String; nCharCount: Integer);
function  GetCPUCount: Integer;
function  XSleep(msec: DWORD; bAlertable: Boolean = FALSE): DWORD;
procedure PlaySoundFile (const sFileName: String; bWarnsNoExist: Boolean = TRUE; bAsync: Boolean = TRUE);
function  TimeToStrMS(dt: TDateTime; decimals: Integer = 3): String;
function  FmtFloat (f: Double; decimals: Integer = 3): String;

procedure HideConsole;
function  ShowConsole(sw_code: Integer = SW_SHOWNORMAL): THandle; stdcall;
procedure CloseConsole;

function  GetConsoleWindow: HWND; stdcall;
function  GetFileVersionStr(fileName: String): String;


function AddSlash (const s: String): String;
function DecRound(f: Extended; dec: Integer): Extended;

// цветное форматирование
function CFormat(const sFormat, fgCol: string; const Args: array of const): string;
procedure wprintf (const sFormat: string; const Args: array of const; flags: DWORD = 255 );

procedure IncFloat (var f: Single; i: Single = 1.0); inline;
procedure IncDouble (var f: Double; i: Double = 1.0); inline;

function CheckInstanceUnique: Boolean;

function CorrectFilePath (sFileName: String): String;

function FindConfigFile (const sFileName: String): String; // позволяет определить полный путь

function IsNumber( s: String): Boolean;
function IsQuotedStr(const s: String): Boolean;

// postfix increment
function PostInc (var i: Integer): Integer; inline; overload;
function PostInc (var i: Int64): Int64;   inline; overload;
function PostInc (var i: DWORD): DWORD; inline; overload;
function PostInc (var i: UInt64): UInt64;   inline; overload;


procedure OnExceptLog (sLocation: String; E: Exception = nil; bContinue: Boolean = FALSE; ctx: PContext = nil); stdcall;
function  VEHandler(ExceptionInfo: PEXCEPTION_POINTERS): DWORD; stdcall;

function FileWriteTime (const sFileName: String): TDateTime;

function InterlockedAdd(var Addend: Integer; Increment: Integer): Integer; register;
function InterlockedAnd(var value: Integer; mask: Integer): Integer; register;

function ReplaceChar (const sText: String; chfrom, chto: CHAR): String; inline;
function ReplaceChars (const sText: String; chars: TCharSet; const rep_to: String ): String;

function LocalTime (nMethod: Integer): TDateTime;

function LZero (const s: String; min_width: Integer): String;

procedure LibInit ( pinf: PLibInitInfo ); stdcall;

function  LimitInt (a, b: Integer): Integer;

function InStr (const a, b: String): Boolean; inline;

function EnvStr (const name: String): String; inline;

function IsDebuggerPresent: Boolean; stdcall;
procedure FixRefTimestamp(maxLoops: Int64 = 100 * 1000 * 1000);

function DumpRegisters(ctx: TContext; cltag: String = '~C07'; tab: String = ''): String;
function FormatException(code, ei0, ei1: DWORD): String;
function FormatPtr(p: Pointer): String;
function StackUndName (const s: String): String;

function KillHandle (h: THandle): Boolean; inline;
function FileSizeEx (const sFileName: String): Int64;
function GetObjInfo (o: TObject): String;

procedure RaiseLastError(const msg: String); inline;
procedure DefLogDbg(const s: String);


function  AdjustPrivilege (const sName: String; bEnable: Boolean = TRUE): Boolean;
function  PosLowerCase(const sub, src: String): Integer; inline;


procedure SafeMove (const src; var dst; cbSize: DWORD);


function  SafeCmpStr (const a, b: String): Boolean;
function  CmpFloat (a, b: Double; prec: Double = 1e-7): Integer;

procedure SetStrZ (dst: PAnsiChar; const src: String; cbSize: Integer);
procedure SetStrWZ (dst: PWideChar; const src: String; cbSize: Integer);

function  SetBit (v: Int64; n_bit: Integer; b_set: Boolean): Int64; inline;

procedure InterlockedMove ( const Source; var Dest; Count: Integer );

procedure FreeObjectList (  list: Pointer; Count: Integer );

function  GetMemoryStatusEx: TMemoryStatusEx;

function  RawLoadFile ( const sFileName: String; var buff: TDynByteArray ): String;
function  FormatRight (const s, addl: String): String;
function  AutoComma (const s: String): String;


function  GetStackTop: DWORD;
function  GetStackBottom: DWORD;
function  SimpleStackTrace ( Context: PContext; eip: NativeUInt = 0; ebp: NativeUInt = 0; stack_btm: NativeUInt = 0; stack_top: NativeUInt = 0 ): String;


function  local_time: TDateTime; inline;
function  log_verbose: Integer; inline;
function  PerfWarnRatio: Integer; inline;
function  ReadSysReg ( path, param: String ): String;
function  UpTrunc ( v: Double ): Int64;

function  PopValue (list: TObjectList): TObject; overload;


// asm micro-functions
procedure __finit;
procedure __int3;
procedure __nop;
procedure __pause;



// WARNING: fake procedure used only as variables list



type
   TExceptionInfo = packed record
     ExceptionRecord: TExceptionRecord;
     ContextRecord: TContext;
    end;

   PExceptionInfo = ^TExceptionInfo;
var
         con_enabled: Boolean = FALSE;
         con_visible: Boolean = FALSE;
          con_origin: COORD;
           con_mutex: THandle = 0;
         con_sw_last: Integer = -1;
         gConfigPath: String;
         dbgt_status: Integer = 0;

      last_msg_time: TDateTime;
     log_flush_time: TDateTime;
           ODS_Proc: TODSProc = LocalODS;




     gLogPath,
     gLogFileName: String;

   gLogFileRename: array [0..LOG_STREAMS - 1] of String;
        gLogFiles: array [0..LOG_STREAMS - 1] of THandle;
       gLogStream: Integer; // must be masked by LOG_STREAMS


     hWndCon: HWND;
    hConFont: THandle = 0;
   on_err_callback: procedure (const msg: String) = nil;
   g_except_count: Integer = 0;
   _GetPtrInfo: function (p: Pointer): String = FormatPtr;
   lMDesc: TModuleDescriptor = nil;

   _exception_info: TExceptionInfo;

    _exception_flag: Integer = 0;
   _exception_quiet: Integer = 0;

   _SafeLog: procedure(const msg: String) = DefLogDbg;

   CaptureStackBackTrace: function ( FramesToSkip, FramesToCapture: DWORD; BackTrace: PPointerArray; BackTraceHash: PDWORD = nil ): WORD; stdcall = nil;

             last_dbg_msg: array [0..32767] of CHAR;
                dbg_dumps: Boolean = FALSE;
                  need_dt: Boolean = TRUE;

            p_shared_vars: PMiscSharedVars = nil;
             h_shared_map: THandle = 0;
             shared_owner: Boolean = FALSE;
threadvar
          gThreadLogFlags: DWORD; // this binary module flags for ODS


implementation
uses SyncObjs, MMSystem, Math, ShlWAPI, PsUtils, Registry
     {$IFDEF FullDebugMode}, FastMM4 {$ENDIF}
      ,madStackTrace, madExcept {$IFDEF CPUX86} {$ENDIF};
        // StrClasses;


const
    S_ALLOCATED = $1000;
       S_FILLED = $2000;
         S_FREE = $0000;


type
    TSyncSection = class
    protected
     FSection: RTL_CRITICAL_SECTION;
    public

     { C & D }
     constructor       Create;
     destructor        Destroy; override;
     { methods }
     function   TryLock (timeout: DWORD): Boolean;
     procedure   Unlock;
    end; // TSyncSection


    TMsgSlot = record
     _index: Integer;
     szBuff: Integer;
      ccLen: Integer;
      flags: DWORD;
       buff: PCHAR;


     function   GetStr: String;
     procedure  Load(const s: String);
     procedure  SetSize (newSize: Integer);
    end;

    PMsgSlot = ^TMsgSlot;



    TConBuffer = object
    private
      sc_share: TSyncSection;
       FActive: Boolean;
       FHandle: THandle;
         users: Integer;
         index: Integer;

     function  AllocHandle: THandle;
     procedure SetActive(const Value: Boolean);
     function  GetMakeHandle: THandle;


    public
     { props }
     property                  Active: Boolean read FActive write SetActive;
     property                  Handle: THandle read GetMakeHandle;

     { C & D }
     procedure                 Init ( AIndex: Integer );
     procedure                 Release;
     { methods }

     procedure                 SyncWrite(const s: String; flags: DWORD);
    end;

    TConsoleBuffers = array [0..LOG_STREAMS - 1] of TConBuffer;
    PConsoleBuffers = ^TConsoleBuffers;


    TAppLoggingThread = class (TThread)
    private
     ss_list: TSyncSection;
     evt_msg: TEvent;
     load_msg: TEvent;
       cntlps: Integer;
       max_sc: Integer;

     in_buff_count: Integer;

     time_last: TDateTime;
     last_owner: DWORD;
     slots: array [0..255] of TMsgSlot;  // message list
     m_idx: array [0..255] of Integer;   // active messages index list
     ri, wi: Integer;

    protected

     function           AllocSlot: Integer;

     procedure          Execute; override;
    public
     con_pipe: THandle;

     { C & D }
     constructor        Create (CreateSuspended: Boolean);
     destructor         Destroy; override;

     { methods }
     procedure          AddMsg (const sMsg: String; flags: DWORD);
    end;

    TThreadInfo = record
     ID, Status: DWORD;
     time_set: DWORD;
     name: array [0..123] of Char;
    end; // TThreadInfo


    TMoveLogRec = record
     src: Pointer;
     dst: Pointer;
      cb: DWORD;
     tid: DWORD;
     tms: TDateTime;
    end;

    PMoveLogRec = ^TMoveLogRec;

var
    in_psm: Integer = 0;
    con_owner: Boolean = FALSE;
          hConBuff: THandle;
     dtRefTime_int: Double  = 0;
    dtRefTime_frac: Double = 0;
       pfcRefStamp: Int64;
        fix_counts: Integer;
        res_equals: Integer;
      fix_timeouts: Integer;


       gConBuffers: PConsoleBuffers = nil;


    _UnDecorateSymbolName: function ( src, dst: PAnsiChar; len, flags: DWORD ): DWORD; stdcall = nil;
    RtlNtStatusToDosError: function ( status: NTSTATUS ): DWORD; stdcall = nil;

    th_list: array [0..255] of TThreadInfo;
    th_count: Integer = 0;

    mlog_data: array [0..512 * 1024 - 1] of TMoveLogRec;
    mlog_indx: Integer = 0;




threadvar
     sm_entered: Boolean;
     dwThreadID: DWORD;


const
   SM_CTX: PChar = 'misc.SafeMove';
   LOG_MASK = High (mlog_data);


procedure __finit;
asm
  finit
end;

procedure __int3;
asm
  int 3
end;

procedure __nop;
asm
  nop
end;

procedure __pause;
asm
  pause
end;


function  PopValue (list: TObjectList): TObject; overload;
begin
 result := nil;
 if list.Count > 0 then
    begin
     result := list.Last;
     Assert (not list.OwnsObjects, 'PopValue from list that owned object');
     list.Delete (list.Count - 1);
    end;
end;

function UpTrunc ( v: Double ): Int64;
var
   s: Integer;
begin
 s := Sign (v);
 v := Abs (v);
 result := Trunc(v);
 if Frac(v) > 0 then Inc (result);
 result := result * s;
end;


function  local_time: TDateTime; inline;
begin
 result := p_shared_vars.local_dt;
end;

function  log_verbose: Integer; inline;
begin
 result := p_shared_vars.log_verbose_level;
end;

function  PerfWarnRatio: Integer;
begin
 result := p_shared_vars.perf_warn_ratio;
end;

function logging_thread: TAppLoggingThread; inline;
begin
 result := TAppLoggingThread ( p_shared_vars.log_th );
end;

function ReadSysReg ( path, param: String ): String;
var
   r: TRegistry;
   s: String;
   t: CHAR;
   i: Integer;
   b: array of byte;

begin
 r := TRegistry.Create;
 s := Copy ( path, 1, 4 );
 Delete ( path, 1, 5 ); // HK**\

 if s = 'HKCR' then r.RootKey := HKEY_CLASSES_ROOT  else
 if s = 'HKLM' then r.RootKey := HKEY_LOCAL_MACHINE else
 if s = 'HKCC' then r.RootKey := HKEY_CURRENT_CONFIG;

 t := 's';
 // param type modifier
 i := Pos (':', param);
 if ( i > 0 ) and ( i < Length(param) ) then
    begin
     t := param [i + 1];
     Delete ( param, i, 2 );
    end;

 result := '???';
 try
  SetLastError (0);
  if r.OpenKeyReadOnly ( path ) then
     case t of
      'b': begin
            result := '#ERROR: data type is not binary';
            if r.GetDataType (param) <> rdBinary then exit;
            i := r.GetDataSize (param);
            result := '#ERROR: data size wrong = ' + IntToStr(i);
            if ( i <= 0 ) or ( i > 1048576 ) then exit;
            SetLength ( b, i );
            r.ReadBinaryData ( param, b[0], i );
            result := '';
            for i := 0 to Length (b) do
                result := result + IntToHex ( b[i], 2 ) + ' ';

           end;

      'd': result := FormatDateTime ( 'dd.mm.yy', r.ReadDate (param) );
      't': result := FormatDateTime ( 'dd.mm.yy hh:nn:ss.zzz', r.ReadDateTime (param) );
      'f': result := ftow ( r.ReadFloat (param) );
      'i': result := IntToStr ( r.ReadInteger (param) );
      's': result := r.ReadString (param);
      else
         result := '#ERROR: unknown type tag ' + t;
     end
   else
     result := '#ERROR: cannot open key <' + path + '>: ' + r.LastErrorMsg;

 finally
  SetLength (b, 0);
  r.Free;
 end;

end;



{$IFDEF CPUX86}
function GetTIB: PNT_TIB;
asm
 mov eax, fs:[18h]
end;

function GetStackTop: DWORD;
begin
 result := GetTIB.StackLimit;
end;

function GetStackBottom: DWORD;
begin
 result := GetTIB.StackBase;
end;
{$ELSE}

function GetStackTop: DWORD;
begin
 result := 0;
end;

function GetStackBottom: DWORD;
begin
 result := 0;
end;

{$ENDIF}


function SimpleStackTrace ( Context: PContext; eip: NativeUInt = 0; ebp: NativeUInt = 0; stack_btm: NativeUInt = 0; stack_top: NativeUInt = 0 ): String;


begin
 {$IFDEF CPUX86}
 if Assigned (Context) then
  begin
   if eip = 0 then
      eip := Context.Eip;

   if ( ebp = 0 ) then ebp := Context.Ebp;
   if ( stack_btm = 0 ) then
        stack_btm := GetStackBottom;
   if ( stack_top = 0 ) then
        stack_top := GetStackTop;

   if ( stack_btm = 0 ) then
        stack_btm := Context.Esp;
  end;

 result := StackTrace ( FALSE, TRUE, TRUE, nil, Context, Ptr(eip), FALSE, FALSE, stack_btm, stack_top, nil, nil, nil, nil, nil, ebp );

 {$ELSE}
 result := 'not supported';
 {$ENDIF}
end;


function  GetModuleHandle (sModName: String): DWORD;
begin
 if ( sModName = '' ) or ( Pos ('?', sModName) > 0 ) then
    begin
     PrintError( Format('GetModuleHandle wrong module name "%s" ', [sModName] ) );
     sModName := GetModuleName (0);
    end;
 result := Windows.GetModuleHandleW ( PWideChar (sModName) );
end;


function  AutoComma (const s: String): String;
var
   i: Integer;
begin
 result := '';
 if s = '' then exit;
 i := Length (s);
 while ( i > 1 ) and CharInSet ( s[i], [#9, ' '] ) do Dec (i);

 if i > 1 then
    result := s + ','
 else
    result := s;
end; // AutoComma


function FormatRight (const s, addl: String): String;
begin
 result := addl + AnsiReplaceStr (s, #10, #10 + addl);
end;

function GetMemoryStatusEx: TMemoryStatusEx;
begin
 result.dwLength := sizeof (result);
 GlobalMemoryStatusEx (result);
end;


function  RawLoadFile ( const sFileName: String; var buff: TDynByteArray ): String;
var
  fsz: LARGE_INTEGER;
   rb: DWORD;
    h: THandle;
begin
 ///
 result := '#OK';
 SetLength (buff, 0);

 if not FileExists (sFileName) then
   begin
    result := '#ERROR: not found ' + sFileName;
    exit;
   end;

 h := CreateFile (PChar(sFileName), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil, OPEN_ALWAYS, 0, 0);
 if h = INVALID_HANDLE_VALUE then
   begin
    result := '#ERROR: cannot open file ' + sFileName + ': ' + err2str;
    exit;
   end;
 try
  fsz.LowPart := GetFileSize (h, @fsz.HighPart);
  if ( fsz.HighPart > 0 ) or ( fsz.QuadPart > GetMemoryStatusEx.ullAvailVirtual ) then
     begin
      result := '#ERROR: To large file for load, size = ' + IntToStr (fsz.QuadPart);
      exit;
     end;
  if fsz.QuadPart = 0 then
     begin
      result := '#WARN: file is void';
      exit;
     end;


  rb := 0;
  SetLength ( buff, fsz.QuadPart );
  ReadFile ( h, buff [0], fsz.LowPart, rb, nil );
  if rb < fsz.LowPart then
   begin
    result := Format ( '#PART: readed %d bytes from %d ', [rb, fsz.LowPart] );
    SetLength ( buff, rb );
   end;

 finally
  CloseHandle (h);
 end;
end;



procedure FreeObjectList (  list: Pointer; Count: Integer );
var
   pl: PPointerArray;
    n: Integer;
begin
 pl := list;

 for n := 0 to Count - 1 do
     FreeAndNil ( pl [n] );
end;


procedure InterlockedMove ( const Source; var Dest; Count: Integer );
{$IFDEF CPUX64}
asm
 push rcx
 push rsi
 push rdi
 mov  rsi, rax
 mov  rdi, rdx

 rep  movsb

 pop  rdi
 pop  rsi
 pop  rcx
end;
{$ELSE}
asm
 push ecx
 push esi
 push edi

 mov  esi, eax
 mov  edi, edx

 rep movsb  // not fast...

 pop edi
 pop esi
 pop ecx
end;
{$ENDIF}

function CmpFloat (a, b, prec: Double): Integer;
begin
 // compare with hysteresis
 prec := prec * ( Abs(a) + Abs (b) );

 result := 0;
 if a > b + prec then result := +1;
 if a < b - prec then result := -1;
end;


function  SetBit (v: Int64; n_bit: Integer; b_set: Boolean): Int64;
var
   mask: Int64;
begin
 mask := 1 shl n_bit;
 if b_set then
    result := v or mask
 else
    result := v and (not mask);
end;

procedure SetStrZ (dst: PAnsiChar; const src: String; cbSize: Integer); overload;
begin
 {$IFOPT D+}
 if cbSize < Length (src) then
    __int3;
 {$ENDIF}

 AnsiStrings.StrLCopy (dst, PAnsiChar ( AnsiString(src) ), cbSize - 1);
end;


procedure SetStrWZ (dst: PWideChar; const src: String; cbSize: Integer);
begin
 {$IFOPT D+}
 if cbSize < Length (src) then
    __int3;
 {$ENDIF}

 StrLCopy (dst, PWideChar ( WideString(src) ), cbSize - 1);
end;


function SafeCmpStr (const a, b: String): Boolean;
begin

 Assert ( Assigned ( @a[1] ), 'Unassigned left opperand' );
 Assert ( Assigned ( @b[1] ), 'Unassigned right opperand' );
 result := (a = b);
end;


function GetThreadName (tid: WORD): String;
begin
 {$IFDEF CPUX86}
 result := String ( madExcept.GetThreadName(tid) );
 {$ELSE}
 result := IntToStr(tid);
 {$ENDIF}
end;

procedure LogMove (const src; var dst; cbSize: DWORD);

var
   mlr: PMoveLogRec;
     i: Integer;
begin
 //  if sm_entered then exit;
 Assert ( not sm_entered, 'SafeMove: Already entered');
 sm_entered := TRUE;
 if TRUE then
   begin
    i := InterlockedAdd (mlog_indx, 1) and LOG_MASK;
    mlog_indx := mlog_indx and LOG_MASK;
    mlr := @mlog_data[i];
    mlr.src := @src;
    mlr.dst := @dst;
    mlr.cb  := cbSize;
    mlr.tid := GetCurrentThreadID;
    mlr.tms := Now;
   end;

  //Assert (stmp <> '', 'Somthing wrongs');


 sm_entered := FALSE;
end;

procedure SafeMove (const src; var dst; cbSize: DWORD);

begin
 try
  if IsDebuggerPresent then
     LogMove (src, dst, cbSize);
  System.Move(src, dst, cbSize);
 except
  on E: Exception do
     OnExceptLog (SM_CTX, E);
 end;
end;


procedure DumpMovePtrs(np: Pointer); // dump nearest
var
   n, i, ix: Integer;
   mlr: PMoveLogRec;
   dmp: TStrings;
     s: String;
begin

 dmp := TStringList.Create;

 if np = nil then
  begin
   ix := mlog_indx;
   for n := 0 to 1023 do
    begin
     if ix < 0 then ix := LOG_MASK;

     i := ix and LOG_MASK;
     Dec (ix);

     mlr := @mlog_data[i];

     s := FormatDateTime('hh:nn:ss.zzz', mlr.tms) + ' ' + FormatPtr(mlr.dst) + '..';
     s := s + FormatPtr( Ptr( NativeUInt(mlr.dst) + mlr.cb - 1) );

     dmp.Add (s);

    end;
  end;

 dmp.SaveToFile(gLogPath + '\moves_dump.log');
 dmp.Free;
end;

{ procedures }

procedure DefLogDbg(const s: String);
var
   fname: String;
    fmsg: String;
    ftxt: Text;

begin
 fmsg := FormatDateTime ('[dd.mm.yy hh:nn:ss.zzz].', Now) + #9;
 fmsg := fmsg + Format('PID%0.4d'#9, [GetCurrentProcessID]) + s;

 if gLogPath = '' then InitPaths;

 if IsDebuggerPresent then
    OutputDebugString (PChar(fmsg));

 fname := gLogPath +'\app_init.log';
 try
   CheckMakeDir (gLogPath);
   {$I-}
   AssignFile (ftxt, fname);


   if FileExists(fname) then
      Append (ftxt)
   else
      ReWrite (ftxt);
   if IOresult = 0 then
      WriteLn (ftxt, fmsg);
   CloseFile (ftxt);
   {$I+}
 except
  on E: EInOutError do
     OutputDebugString ( PChar ( E.ClassName + ':' + E.Message ) );
 end;
end;

procedure DefLogConsole(const s: String);
begin
 DefLogDbg(s);
 WriteLn(s);
end;


function  PosLowerCase(const sub, src: String): Integer;
begin
 result := Pos (LowerCase(sub), LowerCase(src));
end;

function AdjustPrivilege (const sName: String; bEnable: Boolean = TRUE): Boolean;

var
   priv, last_priv: _TOKEN_PRIVILEGES;
   rlen: DWORD;
   hTok: THandle;

begin
 result := FALSE;

 if (not OpenProcessToken (GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hTok)) then
   begin
    PrintError('OpenProcessToken returned ~C0F' + Err2Str (GetLastError));
    exit;
   end;

 if LookupPrivilegeValue (nil, PChar(sName), priv.Privileges[0].Luid) then
  begin
   priv.PrivilegeCount := 1;
   priv.Privileges[0].Attributes := IfV (bEnable, SE_PRIVILEGE_ENABLED, 0);
   result := AdjustTokenPrivileges (hTok, False, priv, sizeof (priv), last_priv, rlen);
   if result then
      _SafeLog('#DBG: success ' + IfV(bEnable, 'enabled', 'disabled') + ' {' + sName + '} ')
   else
      RaiseLastError ('AdjustTokenPrivileges(' + sName + ') failed');
  end
 else
  _SafeLog('LookupPrivilegeValue returned ~C0F' + Err2Str (GetLastError));

 CloseHandle (hTok);
 hTok := 0;
end;

function GetObjInfo (o: TObject): String;
begin
 result := o.ClassName + '[' + FormatPtr(o) + ']';
end;

procedure RaiseLastError(const msg: String); inline;
var
   c: DWORD;
   e: EOSError;
begin
 c := GetLastError;
 e := EOSError.Create (msg + ' Description: ' + err2Str(c));
 e.ErrorCode := c;
 raise E;
end;

function AnsiTrim;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then Result := '' else
  begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function   AnsiTrim2W (const s: AnsiString): String;
begin
 result := String (AnsiTrim (s));
end;

function FileSizeEx (const sFileName: String): Int64;
var
   hFile: THandle;
   fsz: LARGE_INTEGER;
begin
 hFile := CreateFile (PChar(sFileName), GENERIC_READ,
                                        FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil, OPEN_ALWAYS, 0, 0);
 result := 0;
 if (hFile = 0) or (hFile = INVALID_HANDLE_VALUE) then exit;

 fsz.LowPart := GetFileSize (hFile, @fsz.HighPart);
 result := fsz.QuadPart;

 CloseHandle (hFile);
end;

function KillHandle (h: THandle): Boolean;
begin
 SetLastError (0);
 result := TRUE;
 if h <> 0 then
    result := CloseHandle (h);
end; // KillHandle


function undname(const func: String): String;
var
   buff: array [0..1023] of AnsiCHAR;
   sa: AnsiString;
begin
 result := func;
 if not Assigned (_UnDecorateSymbolName) then exit;

 FillChar (buff, sizeof(buff), 0);
 sa := AnsiString (func);
 if _UnDecorateSymbolName ( PAnsiChar (sa), buff, 1023, 0) > 0 then
    result := Trim ( String(buff) );
end;


function StackUndName (const s: String): String;
var
  tmp: TStringList;
  row, nm: String;
  n, i: Integer;
begin
 result := s;
 if not Assigned (_UnDecorateSymbolName) then exit;

 tmp := TStringList.Create;
 try
  tmp.Text := s;
  for n := 0 to tmp.Count - 1 do
   begin
    row := tmp [n];

    if ( Pos ('@@', row) = 0 ) and
       ( Pos ('__', row) = 0 ) and
       ( Pos ('??', row) = 0 ) then continue; // not decorated;

    for i := Length (row) downto 1 do
     if row [i] = ' ' then
      begin
       nm := Copy (row, i + 1, Length (row) - i ); // '1_3_5'
       Delete (row, i + 1, Length (nm));
       nm := undname (nm);
       tmp [n] := row + '[C/C++]: ' +  nm;
       break; // loop i breaks
      end;

   end;

  result := tmp.Text;
 finally
  tmp.Free;
 end;
end;

    // = (nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil);

function DumpRegisters(ctx: TContext; cltag, tab: String): String;
begin
 with ctx do
  try
   {$IFDEF CPUX86}
   result := tab + CFormat('#INT_REGS: EAX = $%.8X EBX = $%.8X ECX = $%.8X EDX = $%.8X EDI = $%.8X ESI = $%.8X EBP = $%.8X, ESP = $%.8X'#13#10, cltag,
                      [eax, ebx, ecx, edx, edi, esi, ebp, esp]);
   if ContextFlags and CONTEXT_CONTROL <> 0 then
      result := result + tab +
      CFormat('#CTL_REGS: CS = $%.4X DS = $%.4X ES = $%.4X GS = $%.4X SS = $%.4X EIP = $%.8X, EFL = $%.8X'#13#10, cltag,
              [SegCS, SegDS, SegES, SegGS, SegSS, eip, EFlags]);

   {$ELSE}
   result := tab + CFormat('#INT_REGS: RAX = $%.16X RBX = $%.16X RCX = $%.16X RDX = $%.16X RDI = $%.16X RSI = $%.16X RBP = $%.16X RSP = $%.16X'#13#10, cltag,
                      [rax, rbx, rcx, rdx, rdi, rsi, rbp, rsp]);

   if ContextFlags and CONTEXT_CONTROL <> 0 then
      result := result + tab +
      CFormat('#CTL_REGS: CS = $%.4X DS = $%.4X ES = $%.4X GS = $%.4X SS = $%.4X RIP = $%.8X, EFL = $%.8X'#13#10, cltag,
              [SegCS, SegDS, SegES, SegGS, SegSS, rip, EFlags]);


   {$ENDIF}



   if ContextFlags and CONTEXT_DEBUG_REGISTERS <> 0 then
      result := result + tab +
      CFormat('#DBG_REGS: DR0 = $%.8X DR1 = $%.8X DR2 = $%.8X DR3 = $%.8X DR6 = $%.8X DR7 = $%.8X'#13#10, cltag,
                      [dr0, dr1, dr2, dr3, dr6, dr7]);

   { if ContextFlags and CONTEXT_FLOATING_POINT <> 0 then
      result := result +
      CFormat('#FPT_REGS: ST0 = %9-f', cltag, [ctx.FloatSave]); }
  except
    on E: Exception do
       PrintError('Exception in DumpRegisters ' + E.Message);

  end;
end;

function  FormatException(code, ei0, ei1: DWORD): String;

begin
 case code of
        STATUS_TIMEOUT: result := 'Timeout';
     STATUS_BREAKPOINT: result := 'Breakpoint';
    STATUS_SINGLE_STEP: result := 'Single step or data break';


  NT_ERROR + $005: result := 'Access violation, ' + IfV(ei0 = 0, 'read', 'write') + ' of address $' + IntToHex(ei1, 8);
  NT_ERROR + $006: result := 'In Page Error';
  NT_ERROR + $008: result := 'Invalid Handle';
  NT_ERROR + $017: result := 'No Memory';
  NT_ERROR + $01D: result := 'Illegal Instruction';
  NT_ERROR + $025: result := ' Nonconttinuable Exception';
  NT_ERROR + $026: result := 'Invalid Disposition';
  NT_ERROR + $08C: result := 'Array Bounds Exceeded';
  NT_ERROR + $094: result := 'Integer Divide By Zero';
  NT_ERROR + $095: result := 'Integer Overflow';
  NT_ERROR + $096: result := 'Privileged Instruction';
  NT_ERROR + $0FD: result := 'Stack Overflow';
  NT_ERROR + $13A: result := 'Control C Exit';
   EC_ASSERTION: result := 'Delphi Assertion';
      $40000015: result := 'C/C++ Abort Exception';
 else
    begin
     if Assigned ( RtlNtStatusToDosError ) then
        result := Err2Str ( RtlNtStatusToDosError (code) )
     else
        result := '$' + IntToHex (code, 8);
    end;
 end;

end;

function FormatPtr(p: Pointer): String;
begin
 result := Format('$%p', [p]);
end;


function EnvStr (const name: String): String;
begin
 result := GetEnvironmentVariable (Name);
end;


function FastThreadName (ID: DWORD): String;
var
   n, i: Integer;
   name: String;
   upd: Boolean;
   st: TSystemTime;
begin
 i := -1;
 GetLocalTime (st);

 upd := FALSE;

 for n := th_count - 1 downto 0 do
  if th_list[n].ID = ID then
   begin
    i := n;
    break;
   end;


 if i < 0 then
  begin
   name := GetThreadName (ID);
   upd := (name <> '');
   if upd then i := InterlockedIncrement (th_count) - 1;
  end
 else
  begin
   if st.wMinute <> th_list [i].time_set then
     begin
      name := GetThreadName (ID);
      upd := (name <> '');
     end
   else
     name := th_list [i].name;
  end;

 if upd then
    begin
     th_list [i].ID := ID;
     th_list [i].time_set := st.wMinute;
     SetStrWZ (th_list [i].name, name, High(th_list[i].name) );
    end;
 result := String (name);
end;


function IsDebuggerPresent: Boolean; stdcall; external 'kernel32.dll';

function InStr (const a, b: String): Boolean; inline;
begin
 result := ( Pos (a, b) > 0 );
end;

function LimitInt;
begin
 while (a > b) do a := a - b;
 result := a;
end;

function LZero (const s: String; min_width: Integer): String;
begin
 result := s;
 while Length (result) < min_width do
    result := '0' + result;
end;

function ReplaceChar (const sText: String; chfrom, chto: CHAR): String;
var
   n: Integer;
begin
 result := sText;
 for n := 1 to Length (result) do
   if result [n] = chfrom then
      result [n] := chto;
end;

function ReplaceChars (const sText: String; chars: TCharSet; const rep_to: String ): String;
var n, d, l, lr: Integer;
    ch, chfirst: Char;
    rest: String;
begin
 result := sText;
 if sText = '' then exit;
 chfirst := sText [1];
 l := Length (sText);
 lr := Length (rep_to);

 rest := Copy (rep_to, 2, lr - 1);

 if IsDebuggerPresent then
    __nop;

 d := 1;
 for n := 1 to l do
  begin
   ch := sText [n];
   if CharInSet (ch, chars) then
     begin
      result [d] := chfirst;
      if lr > 1 then
         Insert (rest, result, d + 1);
      Inc (d, lr);
     end
   else
     Inc (d);

  end;

end;


function InterlockedAdd(var Addend: Integer; Increment: Integer): Integer;
asm
      MOV   ECX,EAX   // addend
      MOV   EAX,EDX   // increment
 LOCK XADD  [ECX],EAX
      ADD   EAX,EDX
end;

function InterlockedAnd(var value: Integer; mask: Integer): Integer;
asm
      MOV   ECX, EAX   // value
 LOCK AND  [ECX], EDX
      MOV   EAX, [ECX] // TODO: not true interlocked
end;


function FileWriteTime;
var
   fsrc: THandle;
   info: TByHandleFileInformation;
   st: TSystemTime;
begin
 result := 0;
 fsrc := CreateFile (PChar(sFileName), FILE_READ_ATTRIBUTES or FILE_READ_EA, FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil, OPEN_ALWAYS, 0, 0);
 if (fsrc = 0) or (fsrc = INVALID_HANDLE_VALUE) then exit;
 try
  // получение даты файла из информационного блока
  if GetFileInformationByHandle (fsrc, info) and
     FileTimeToSystemTime (info.ftLastWriteTime, st) then result := SystemTimeToDateTime (st);
 finally
  CloseHandle (fsrc);
 end;

end; // FileWriteTime



var
   in_ve: Boolean = FALSE;
   cur_ebp, cur_esp: DWORD;

function SelfStackTrace ( ctx: PContext; eip, ebp, sb, st: NativeUInt ): String;
var
   s: String;
begin
  try
   s := SimpleStackTrace ( ctx, eip, ebp, sb, st );
   // StackTrace ( False, True, True, nil, nil, Ptr(eip), False, False, 0, 0, nil, nil, nil, nil, nil, ebp  );
   s := StackUndName ( PChar(s) );
   result := '~C0B  #STACK(madStackTrace.StackTrace): ~C0E'#13#10 + s + '~C07';
  except
   on E: Exception do
         result := result + 'madStackTrace generated inner exception: ' + E.Message;
  end;
end; // SelfStackTrace

procedure _OnExceptionLog (szLocation: Pointer; E: Exception; bContinue: Boolean; ctx: PContext); stdcall;

var
   fctx: TContext;
   ftxt: Text;
   fdat: File;
   exr: PExceptionRecord;
   path, fname, s, ltxt: String;
   {$IFDEF CPUX86}
   stp, eip: DWORD;
   {$ENDIF}


begin
 cur_ebp := 0;
 cur_esp := 0;
 {$IFDEF CPUX86}
 asm
  mov cur_ebp, ebp
  mov cur_esp, esp
 end;
 {$ENDIF}


 {$R-}

 exr := nil;

 if Assigned(E) and (E is EExternalException) then
    exr := EExternalException(E).ExceptionRecord;

 if exr = nil then
    exr := @_exception_info.ExceptionRecord;



 if ctx <> nil then
   SafeMove (ctx^, fctx, sizeof (TContext))
 else
   FillChar (fctx, sizeof (fctx), 0);

 if szLocation <> nil then
     begin
       if E <> nil then
        begin
          PrintError( 'Исключение класса ~C0E' + E.ClassName + '~C0C перехвачено в ~C0A' + PChar (szLocation) + '~C0C:~C0F ' + E.Message +
                      #13#10'~C0C Описание:~C0F '#13#10 + E.StackTrace + '~C07' );

        end
       else
          PrintError( 'Неизвестное исключение перехвачено в ~C0F' + PChar(szLocation) + '~C07');
      end;

 if ctx <> nil then E := nil;

 Inc (g_except_count);

 ODS('~C0F[~T]. #DBG: Repeat last_dbg_msg: ~C07'#13#10 + last_dbg_msg + '~C07' );

 path := ExePath;
 if gLogPath <> '' then
     path := AddSlash (gLogPath);

 // ShowConsole (SW_SHOWNA);

 {$IFDEF CPUX86}
 if fctx.Eax or fctx.Ebx or fctx.Ecx or fctx.Edx or fctx.Esi or fctx.Edi <> 0 then DumpRegisters(fctx); //
 {$ENDIF}

 Sleep(500);

 DumpMovePtrs(nil);

 CopyFile ( PChar(gLogFileName), PChar(ExePath + 'crash_copy.log'), FALSE );



 {$IFDEF CPUX86}

 if ctx <> nil then
    ODS( SelfStackTrace ( ctx, 0, 0, 0, 0 ) )
 else
  with exr^ do
   begin
    stp := Max (ExceptionInformation [5], ExceptionInformation [6]);
    eip := DWORD (ExceptAddr);
    ODS( SelfStackTrace ( nil, eip, 0, 0, stp ) )
   end;
 {$ENDIF}

 if dbg_dumps then
  begin
   fname := path + 'moves_' + FormatDateTime('yyyy.mm.dd_hh.nn.ss', Now) + '_i@' + IntToStr(mlog_indx) + '.dat';
   AssignFile (fdat, fname);
   try
    {$I-}
    ReWrite (fdat, 1);
    BlockWrite ( fdat, mlog_data, sizeof(mlog_data) );
   finally
    CloseFile (fdat);
   end;
  end;


 {$IFDEF CPUX86}

 fname := Format(path + '%s-%s-%x_crash.log', [ExtractFileName(ExeFileName), FormatDateTime('yyyy.mm.dd_hh.nn.ss', Now), GetCurrentProcessID]);
 try
  AssignFile ( Output, fname );
  ReWrite ( Output );
  if IOResult <> 0 then
     PrintError ('Cannot create file ' + fname + ' for Output')
  else
     madExcept.HandleException ( etNormal, E, Ptr (fctx.Eip), bContinue and (g_except_count < 1000), cur_esp, cur_ebp, ctx, esRuntimeError, nil, 0, nil );

  Sleep (2000);
  CloseFile ( Output );
 finally
  AssignFile ( Output, 'con');
  ReWrite ( Output );
 end;

 {$ENDIF}

 if not FileExists ( fname ) then exit;
 AssignFile (ftxt, fname);
 {$I-}
 Reset (ftxt);
 if IOresult = 0 then
 try
  ltxt := '';
  while not Eof (ftxt) do
   begin
    ReadLn (ftxt, s);
    ltxt := ltxt + s + #13#10;
   end;

  ODS('~C0F[~T]. #DBG_DUMP(madExcept.HandleException): ~C0B'#13#10 + StackUndName(ltxt) + '~C07', PMF_IMPORTANT);
  Sleep(2000);
 finally
  CloseFile (ftxt);
 end;
end;


type
   PEXCEPTION_REGISTRATION = ^TExceptionRegistration;
   TSEHFilter = function (pExcptRec: Windows.PExceptionRecord; pExcptReg: PEXCEPTION_REGISTRATION; pctx: PCONTEXT; pDispatcherContext: Pointer; handler: FARPROC): DWORD; stdcall;

   TScopeTableEntry = packed record
      dwReserved: DWORD;
      lpfnFilter: FARPROC;
     lpfnHandler: FARPROC;
   end;

   TScopeTable = array [0..65535] of TScopeTableEntry;

   PScopeTable = ^TScopeTable;

   _EXCEPTION_REGISTRATION = packed record
       prev: PEXCEPTION_REGISTRATION;
    handler: TSEHFilter;
      extra: array [0..15] of Pointer;
   end;
   TExceptionRegistration = _EXCEPTION_REGISTRATION;




function VEHandler(ExceptionInfo: PEXCEPTION_POINTERS): DWORD; stdcall;


var
      s, desc: String;
       ps, px: String;
         i, n: Integer;
   psfl, psfc: PEXCEPTION_REGISTRATION;
     break_ps: Boolean;
            p: Pointer;
            E: Exception;
begin
 break_ps := FALSE;
 _exception_flag := 100;



 if ExceptionInfo <> nil then
 with ExceptionInfo^ do
   begin
      try
       if ExceptionInfo.ExceptionRecord <> nil then
          SafeMove ( ExceptionInfo.ExceptionRecord^, _exception_info.ExceptionRecord, sizeof (_exception_info.ExceptionRecord) );
       if ExceptionInfo.ContextRecord <> nil then
          SafeMove ( ExceptionInfo.ContextRecord^,   _exception_info.ContextRecord,   sizeof (_exception_info.ContextRecord) );
       except
        // Nothings
       end;

     if NativeUInt (ExceptionInfo.ExceptionRecord) > $1000 then
     with ExceptionInfo.ExceptionRecord^ do
     if NativeUInt (@ExceptionInformation) > $1000  then
      begin
       break_ps :=  (ExceptionFlags and EXCEPTION_NONCONTINUABLE <> 0);
       s := #13#10#9#9;
       for n := 0 to NumberParameters - 1 do
           s := s + Format('$%x ', [ExceptionInformation[n]]);

       desc :=  FormatException ( ExceptionCode, ExceptionInformation[0], ExceptionInformation[1] );

       ODS( CFormat ('[~T/~U/~i]. #EXCEPT(VEHandler):\n ExceptionAddress = %s, ExceptionCode = %s, ExceptionFlags = $%x, ExceptInfo dump: %s', '~C07',
                        [ _GetPtrInfo (ExceptionAddress), desc, ExceptionFlags, s]) );


        if ExceptionInfo <> ve_exception then
           wprintf('[~T]. #DBG: ExceptionInfo = $%p, ve_exception = $%p ', [ExceptionInfo, ve_exception]);

       // TODO: incomplete classes
        case ExceptionCode of
         STATUS_ACCESS_VIOLATION:
           E := EAccessViolation.CreateFmt( desc, [0] );
        else
           E := EExternalException.CreateFmt ( desc, [0] );
        end;

        if E is EExternalException then
           EExternalException (E).ExceptionRecord := @_exception_info.ExceptionRecord;
      end;

    {$IFDEF CPUX86}

    if ContextRecord <> nil then
      with ContextRecord^ do
      if ContextFlags and CONTEXT_INTEGER <> 0 then
       begin

        ODS( CFormat('[~T]. #EXCEPT(VEHandler):\n EAX = $%x EBX = $%x ECX = $%x EDX = $%x EDI = $%x ESI = $%x EBP = $%x ESP = $%x EIP = %s ', '~C07',
                        [eax, ebx, ecx, edx, edi, esi, ebp, esp, _GetPtrInfo ( Ptr(eip) )]) );
       end;

    {$ENDIF}
   end;

 break_ps := break_ps or (g_except_count >= 5);

 // psr := nil;

 // psfl := nil;

 if not in_ve then
  begin
   in_ve := TRUE;
   _OnExceptionLog ( nil, nil, not break_ps, ExceptionInfo.ContextRecord );
   in_ve := FALSE;
  end;

 // SEH: tracing / redirecting

 psfl := nil;
 {$IFDEF CPUX86}

 asm
  mov eax, fs:[0]
  mov psfl, eax
 end;

 {$ENDIF}
 psfc := psfl;

 s := ModuleFileName (HInstance);
 s := ExtractFileName (s);
 s := UpperCase (s);

 n := 0;
 while (psfc <> nil) do
  begin
   Inc (n);
   p := Addr ( psfc.handler );
   desc := _GetPtrInfo (p);

   // передать первому чужому обработчику
   // if (psfl = nil) and ( Pos (s, UpperCase(desc)) = 0 ) then  psfl := psfc;
   s := '';

   for i := 0 to 7 do
    begin
     if psfc.extra [i] = nil then
        ps := 'nil'
     else
        ps := _GetPtrInfo ( psfc.extra [i] );
     px := FormatPtr( psfc.extra [i] );

     if ps <> px then
        ps := px + ' = ' + ps;

     s := s + #9#9 + ps + #13#10;
    end;

   ODS( Format('[~T]. #DBG_SEH(%d): addr = $%p, disp_filter = %-50s, extra: '#13#10, [n, psfc, desc] ) + s );




   {
   if Assigned ( psfc.stable ) and ( psfc._trylvl < 50 ) and not IsBadReadPtr ( psfc.stable, 4 ) then
      for i := 0 to psfc._trylvl do
        begin
         ent := @psfc.stable [i];
         if ( ent.lpfnHandler = nil ) or ( ent.lpfnFilter = nil ) then break;

         ODS ( CFormat( ' scopetable[%2d], PrevTryLevel or prvEntry: $%03.x, filter = %s, handler = %s ', '~C07',
                [ i, ent.dwReserved, _GetPtrInfo (ent.lpfnFilter), _GetPtrInfo (ent.lpfnHandler) ] ) );
        end; // }

   if Integer (psfc.prev) = -1 then break;
   psfc := psfc.prev;
  end;

 if n > 0 then
    ODS ('[~T]. #DBG_SEH: dump handlers complete.');

 if n > 135 then
  begin
   PrintError('To many nested SEH frames. Process will be Terminated.');
   CBeep (400, 1000);
   TerminateProcess ( GetCurrentProcess, $0200305 );
  end;

 psfc := psfl;
 n := 0;

 while (psfc <> nil) do
    begin
     Inc (n);
     ps := _GetPtrInfo ( @psfc.handler );
     psfl := psfc.prev;
     // TODO: add test in-handler exceptions
     if Pos('VEHandler', ps) = 0 then
      begin
       wprintf ('[~T]. #DBG: Executing SEH handler~C0D #%d~C07, @handler = %s ', [n, ps] );
       result := EXCEPTION_CONTINUE_SEARCH;
       if Assigned (psfc.handler) then
          result := psfc.handler ( ExceptionInfo.ExceptionRecord, psfc, ExceptionInfo.ContextRecord, nil, nil );

       case result of
        EXCEPTION_CONTINUE_EXECUTION:
            desc := 'CONTINUE_EXECUTION';
        EXCEPTION_EXECUTE_HANDLER:
            desc := 'EXECUTE_HANDLER';
        EXCEPTION_CONTINUE_SEARCH:
            desc := 'CONTINUE_SEARCH';

        else
            desc := '?=$' + IntToHex(result, 4);

       end; // case

       ODS ( Format('[~T]. #DBG: SEH handler returned~C0A %s~C07', [ desc ]) );
       if ( result = EXCEPTION_CONTINUE_EXECUTION ) then break;
     end;

     if Integer (psfl) = -1 then break;
     psfc := psfl;
    end;

 if g_except_count < 2 then
    result := EXCEPTION_CONTINUE_SEARCH
 else
    result := EXCEPTION_EXECUTE_HANDLER;
end;

function GetStackInfoStringProc (Info: Pointer): String;
begin
 result := String ( PChar (Info) );
end;


var
   x_save_esp: DWORD;
   safe_stack: array [0..16383] of DWORD;
    in_except: Integer = 0;


function GetExceptionStackInfoProc (P: PExceptionRecord): Pointer;
var
   s: String;
   r: PChar;
   n: Integer;

begin
 result := PAnsiChar('');
 InterlockedIncrement(in_except);
 if in_except > 1 then
  begin
   InterlockedDecrement(in_except);
   PrintError('Exception in Exception was raised');
   exit;
  end;

 {$IFNDEF CPUX64}

 asm
   mov x_save_esp, esp
   lea esp, safe_stack[16383]
 end;
 {$ENDIF}

 try


 {$IFDEF CPUX64}
 s := 'not adopted for x64';
 r := StrAlloc ( Length(s) );
 StrPCopy (r, s);
 result := r;
 {$ELSE}

 if not Assigned (p) then
    s := 'p = nil'
 else
  begin
   LogMove (p^, _exception_info, sizeof(_exception_info));

   System.Move (p^, _exception_info, sizeof(_exception_info));

   s := Format ( 'Exception at $%p, with code = $%x, flags = $%x, Object = $%p'#13#10, [p.ExceptionAddress, p.ExceptionCode, p.ExceptionFlags, p.ExceptObject]);
   s := s + 'ExceptionInformation dump: '#13#10#9;
   for n := 0 to p.NumberParameters - 1 do
          s := s + Format('0%x:$%x ', [n, p.ExceptionInformation[n]]);

   if p.ExceptionRecord <> nil then
    begin
     s := s + 'INNER ExceptionInformation  dump: '#13#10#9;
     for n := 0 to p.ExceptionRecord.NumberParameters - 1 do
          s := s + Format('0%x:$%x ', [n, p.ExceptionRecord.ExceptionInformation[n]]);
    end;

   {$IFDEF CPUX86}
   if _exception_info.ContextRecord.Eip > 0 then

       s := s + DumpRegisters ( _exception_info.ContextRecord );
   {$ENDIF}
  end;

 n := Length (s) or $0F + 17;
 r := StrAlloc ( n );
 StrPCopy (r, s);

 result := r;
 {$ENDIF}
 finally
  {$IFNDEF CPUX64}
  asm
    mov esp, x_save_esp
  end;
  {$ENDIF}
 end;


 InterlockedDecrement(in_except);
end;

procedure CleanUpStackInfoProc (info: Pointer);
begin
 StrDispose ( PChar (info) );
end;




procedure OnExceptLog(sLocation: String; E: Exception; bContinue: Boolean; ctx: PContext); stdcall;
begin
 try
  _exception_flag := 100;
  _OnExceptionLog ( PChar (sLocation), E, bContinue, ctx);
 except
  on E: Exception do PrintError('Inner exception in OnExceptLog: ' + E.Message);
 end;
end;


function AddSlash (const s: String): String;
begin
 result := s;
 if (s <> '') and ( s [ Length(s) ] <> '\' ) then
    result := result + '\';
end;

function PostInc (var i: Integer): Integer;
begin
 result := i; Inc (i);
end;

function PostInc (var i: Int64): Int64;   inline; overload;
begin
 result := i; Inc (i);
end;

function PostInc (var i: DWORD): DWORD; inline; overload;
begin
 result := i; Inc (i);
end;

function PostInc (var i: UInt64): UInt64;   inline; overload;
begin
 result := i; Inc (i);
end;


function IsNumber(s: String): Boolean;
const
    numchset: set of AnsiCHAR = ['$', '+', '-', '.', '0'..'9', 'E'];

var pp: Integer;
    v: Int64;
    t: String;
    n, e: Integer;
begin
 result := FALSE;
 for n := 1 to Length(s) do
     if not CharInSet(s[n], numchset) then exit;

 s := AnsiReplaceStr ( UpperCase(s), ',', '.' ); // to eng-format
 pp := Pos('.', s);
 if pp > 0 then
  begin
    // удаление нулей и точки при необходимости
    while ( pp < Length(s) ) and ( LastChar(s) = '0' ) do
           Delete (s, Length(s), 1);
   if pp = Length(s) then Delete (s, Length(s), 1);
  end;

 result := TRUE;
 if ( Pos('$', s) = 1 ) then
  begin
   // удаление нулей для компарации
   while (Length(s) > 2) and (s [2] = '0') do  Delete (s, 2, 1);
   Val(s, v, e);
   t := Format('$%x',[v]);
   result := (e = 0);
  end
 else
  t := FormatFloat('0.###########', atof(s));

 result := result and (t = s );
end;

function IsQuotedStr(const s: String): Boolean;
var ch: AnsiChar;
    l: Integer;
begin
 result := FALSE;
 l := Length (s);
 if l < 2 then exit;
 ch := AnsiChar ( s[1] );
 result := (ch = AnsiChar(s[l])) and CharInSet(ch, ['"', #$27] );
end; // IsQuotedStr


function CorrectFilePath (sFileName: String): String;
var buff: array [0..MAX_PATH] of CHAR;
begin
 try
  while ( Pos ('\\', sFileName) > 0 ) do
          sFileName := AnsiReplaceStr(sFileName, '\\', '\');
  PathCanonicalize ( buff, PChar( sFileName ) );
 except
  on E: Exception do
     PrintError ('Exception catched in CorrectFilePath: ' + E.Message);
 end;
 result := Trim (buff);
end;

function FindConfigFile (const sFileName: String): String;

begin
 result := ExePath + sFileName;
 if FileExists (result) then exit;
 result := DllPath + sFileName;
 if FileExists (result) then exit;
 result := CorrectFilePath ( gConfigPath + '\' + sFileName );
 if FileExists (result) then exit;
 result := CorrectFilePath ( DllPath + '..\' + ConfigDir + '\' + sFileName );
 if FileExists (result) then exit;
 result := sFileName;
end; // FindConfigFile

function CheckInstanceUnique: Boolean;
begin
 result := TRUE;
end;

procedure IncFloat(var f: Single; i: Single);
begin
 f := f + i;
end;

procedure IncDouble (var f: Double; i: Double = 1.0);
begin
 f := f + i;
end;

function CFormat(const sFormat, fgCol: string; const Args: array of const): string;
const
   fmtchrs = ['a'..'z', 'A'..'Z'];
   ATTR_PURP  = '~C0D';
   ATTR_GREEN = '~C0A';

var
   pch: array [0..1] of CHAR;
   fgc: array [0..4] of CHAR;
   chf: CHAR;
   sym: Boolean;
   dst: PChar;

   n, i, l, cps, maxl: Integer;

   sfmt: String;
   sRes: array of CHAR;

begin
 maxl := Length(sFormat) + 512;


 SetLength(sRes, maxl);

 dst := PChar(@sRes[0]);

 // FillChar (chf, sizeof(chf), 0);
 FillChar (pch, sizeof(pch), 0);
 FillChar (fgc, sizeof(fgc), 0);

 SetStrWZ (@fgc, fgCol, 5);

 FillChar (dst^, maxl * sizeof(CHAR), 0);

 sFmt := '';
 pch := '%';
 cps := -1;

 n := 1;
 l := Length(sFormat);


 while (n <= l) do
  begin
   chf := #0;


   if (sFormat [n] = '%') and (pch[0] <> '%') then
    begin
     for i := n + 1 to Length(sFormat) do
      begin
       // scan format chars
       if CharInSet(sFormat[i], fmtchrs) then
        begin
         chf := sFormat[i];
         cps := i;
         break;
        end;
       if sFormat[i] = '%' then break;
      end;
     if cps > n then
     case UpCase(chf) of
      'F', 'D', 'X', 'P': StrLCat(dst, ATTR_PURP, maxl);  //  sFmt := sFmt + '~C0D';
                     'S': StrLCat(dst, ATTR_GREEN, maxl);                 // sFmt := sFmt + '~C0A';
     end;
    end; // if '%' found

   pch[0] := sFormat[n];

   if (pch[0] = '\') and (n < l) then
     begin
      sym := TRUE;
      case sFormat [n + 1] of
       'n': pch [0] := #10;
       'r': pch [0] := #13;
       't': pch [0] := #9;
       else sym := FALSE;
      end; // case

      if sym then Inc(n);
     end;

   StrLCat( dst, PChar(@pch), maxl);

   //sFmt := sFmt + pch;
   if n = cps then StrLCat ( dst, PChar(@fgc), maxl); // sFmt := sFmt + fgCol;

   Inc (n);

  end; // format


 {
 if Pos ('\', sFmt) > 0 then
  begin
   sFmt := AnsiReplaceStr(sFmt, '\n',  #10);
   sFmt := AnsiReplaceStr(sFmt, '\r',  #13);
   sFmt := AnsiReplaceStr(sFmt, '\t',  #9);
  end;
  }

 SetString (sFmt, dst, StrLen(dst));
 result := SysUtils.Format( sFmt, Args );

 SetLength(sRes, 0);

end;


procedure wprintf (const sFormat: string; const Args: array of const; flags: DWORD = 255 );
var
   dfc: String;
     i: Integer;
begin
 dfc := '~C07';
 i := Length(sFormat) - 4;
 if ( i > 0 ) then
  begin
   dfc := Copy(sFormat, i, 4); // closer tag is default color tag
   if dfc [1] <> '~' then dfc := '~C07';
  end;

 ODS ( CFormat (sFormat, dfc, args), flags );
end;


function StrICmp(const a, b: String): Integer;
begin
 result := CompareStr (UpperCase(a), UpperCase(b));
end;

function DecRound(f: Extended; dec: Integer): Extended;
var n: Integer;
begin
 for n := 1 to dec do f := f * 10;
 f := Round(f);
 for n := 1 to dec do f := f * 0.1;
 result := f;
end;

function GetConsoleWindow: HWND; stdcall; external 'kernel32.dll';


function OpenLogFile: THandle;
begin
 result :=  CreateFile (PChar(gLogFileName), GENERIC_WRITE,
                              FILE_SHARE_READ, nil, OPEN_ALWAYS, 0, 0);

 if result = INVALID_HANDLE_VALUE then
  begin
   result := 0;
   exit;
  end;

 // SetFilePointer(result, 1, nil, FILE_END);

end;


procedure CloseLogFile (var fh: THandle);
{var
   ch: AnsiCHAR;
   wb: DWORD;}
begin
 if fh = 0 then exit;
 // ch := #26;
 // WriteFile (gLogFile, ch, 1, wb, nil);
 CloseHandle (fh);
 fh := 0;
end;


procedure LogMsgToFile ( const msg: String; ns: Integer = 0 );
var
   fn: String;
    f: Text;

begin
   if ( gLogFileRename [ns] <> '' ) and ( gLogFileRename [ns] <> gLogFileName ) then
      begin
       // CloseLogFile (gLogFile);
       MoveFile( PChar ( gLogFileName ), PChar ( gLogFileRename [ns] ) );
       gLogFileName := gLogFileRename [ns];
      end;

   fn := gLogFileName;
   if ns > 0 then
      fn := AnsiReplaceStr (fn, '.log', '_' + IntToStr(ns) + '.log' );

   AssignFile (f, fn);
   {$I-}
   if FileExists (fn) then
    Append (f)
   else
    ReWrite (f);

   if IOresult = 0 then
     begin
      WriteLn (f, msg);
      CloseFile (f);
     end;
  {$I+}
end;


procedure CloseConsole;
begin
 if not con_enabled then exit;
 con_enabled := FALSE;
 con_owner := FALSE;
 hOutput := 0;
 Sleep(100);
 if con_owner then
  begin
   SetStdHandle (STD_OUTPUT_HANDLE, 0);
  end;
 FreeConsole;
end;


function MakeConsole (sw_init: Integer = 5): THandle;
var
    h: THandle;
begin
 SetLastError(0);
 result := GetStdHandle (STD_OUTPUT_HANDLE);
 if con_enabled then exit;

 h := GetConsoleWindow;
 if ( h = 0 ) or ( not IsWindow (h) ) then
   begin
    AllocConsole;
    DefLogConsole( 'Console allocated by this app, sw_init = ' + IntToStr (sw_init) );
    con_owner := TRUE;
    con_visible := ( sw_init = SW_HIDE );
    if not con_visible  then
       ShowWindow ( GetConsoleWindow, SW_HIDE );
   end;

 hConBuff := GetStdHandle (STD_OUTPUT_HANDLE);
 h := hConBuff;
 if (h = 0) or (h = DWORD(INVALID_HANDLE_VALUE)) then
   begin
    h := gConBuffers^ [0].FHandle;
    con_owner := FALSE;
    con_enabled := TRUE;
   end;
 hOutput := h;

 result := GetConsoleWindow;

 con_enabled := IsWindow (result) and (hOutput <> 0);

 {
 DefLogConsole('MakeConsole complete');
 _SafeLog := DefLogConsole; }

 SelectConsole (0);

end;

procedure HideConsole;
begin
 ShowWindow (GetConsoleWindow, SW_HIDE);
 con_visible := FALSE;
 con_sw_last := SW_HIDE;
end;

function ShowConsole;
var
     r: TRect;
    cx, cy, y, last: Integer;

    msg: tagMsg;

begin
 // asm int 3 end;
 result := hWndCon;
 if (sw_code = con_sw_last) and (con_visible) then exit;
 con_sw_last := sw_code;

 MakeConsole (sw_code); // попытка создания консоли, если её нет

 hWndCon := GetConsoleWindow;

 if ( sw_code = SW_HIDE ) and (con_visible) then
   begin
    HideConsole ();
    exit;
   end;

 if IsWindow (hWndCon) then
  begin
   con_visible := TRUE;
   ShowWindow (hWndCon, sw_code);
  end
 else
  begin
   hWndCon := 0;
   con_visible := FALSE;
   con_enabled := FALSE;
   exit;
  end;

 // SetConsoleOutputCP (CP_UTF8);

 // wait window size, and align down
 GetWindowRect (hWndCon, r);
 last := (r.Bottom - r.Top);

 for y := 0 to 150 do
   if PeekMessage (msg, 0, 0, 0, PM_REMOVE) then
    begin
     TranslateMessage (msg);
     DispatchMessage (msg);
    end
   else
    begin
     Sleep (10);
     GetWindowRect (hWndCon, r);
     cy := (r.Bottom - r.Top);
     if cy <> last then break;
    end;


 if (r.Bottom - r.Top < 800) then r.Bottom := r.Top + 800;


 cx := r.Right - r.Left;
 cy := GetSystemMetrics (SM_CYSCREEN) - 100;
 SetWindowPos (hWndCon, HWND_NOTOPMOST, con_origin.X, con_origin.Y, cx, cy, 0);
 MoveWindow (hWndCon, con_origin.X, con_origin.Y, cx, cy, TRUE);

 SetConsoleOutputCP ($4B0); // unicode
 result := hWndCon;
end; //

function  XSleep(msec: DWORD; bAlertable: Boolean = FALSE): DWORD;
begin
 result := GetTickCount;
 SleepEx(msec, bAlertable);
 result := GetTickCount - result;
end; // XSleep

function GetCPUCount: Integer;
var ps_mask, sys_mask: NativeUInt;
    n: Integer;
begin
 result := 0;
 if GetProcessAffinityMask(GetCurrentProcess, ps_mask, sys_mask) then
    for n := 0 to 31 do
     if (ps_mask and (1 shl n) <> 0) then Inc (result);
 result := Max(result, 1);
end; // GetCPUCount


function GetFileVersionStr(fileName: String): String;

var
   pc: array [0..260] of CHAR;
    p: pointer;
   sz: dword;
   xx: dword;
 data: ^VS_FIXEDFILEINFO;

begin
 // StrPCopy (pc, ParamStr (0));   // Имя модуля: wgc.exe default
 if fileName = '' then
    fileName := ModuleFileName (hInstance);

 SetStrWZ (pc, fileName, 260 );

 sz := GetFileVersionInfoSize (pc, xx); // Размер инфы
 result := '0.0';
 p := AllocMem (sz);
 if GetFileVersionInfo (pc, 0, sz, p) then
  begin
   VerQueryValue (p, '\', pointer (data), sz);
   result := IntToStr (data.dwFileVersionMS shr 16) + '.' +
             IntToStr (data.dwFileVersionMS and $FFFF) + '.' +
             IntToStr (data.dwFileVersionLS shr 16) + '.' +
             IntToStr (data.dwFileVersionLS and $FFFF);
  end;
 FreeMem (p);
end; // GetVersionStr


procedure DeleteLast(var s: String; nCharCount: Integer);
var i: Integer;
begin
 i := Length(s) - nCharCount + 1;
 if i <= 0 then
   s := ''
 else
   Delete(s, i, nCharCount);
end;

function  LastChar(const s: String): CHAR;
begin
 result := #0;
 if Length(s) > 0 then
    result := s[Length(s)];
end; // LastChar


function GetAddressSpaceUsed: Cardinal;
var
  PMC: TProcessMemoryCounters;
begin
  GetProcessMemoryInfo(GetCurrentProcess, @PMC, SizeOf(PMC));
  Result := PMC.PageFileUsage; // obtain virtual memory usage
end;

procedure AddMsgToList (sl: TStrings; const msg: String; max_msgs: Integer);
begin
 sl.Add(InfoFmt(msg));
 while (sl.Count > max_msgs) do sl.Delete(0);
end;

procedure FreeListItems(l: TList);
var n: Integer;
begin
 for n := 0 to l.Count - 1 do  FreeMem (l[n]);
end; // ClearList

function  RelativePtr(p: Pointer; ofst: Int64): Pointer;
begin
 result := Pointer( Int64(p) + ofst );
end;


function  Comma2Pt (const s: String): String;
begin
 result := AnsiReplaceStr (s, ',', '.');
end;

function  InBound (v, vmin, vmax: Integer): Boolean;
begin
 result := (vmin <= v) and (v <= vmax);
end;

procedure SetConSize (x, y, cx, cy: Integer);
var c: COORD;
    h: THandle;
    r: SMALL_RECT;
begin
 h := GetStdHandle (STD_OUTPUT_HANDLE);
 if h = INVALID_HANDLE_VALUE then exit;
 c.X := cx;
 c.Y := cy;
 SetConsoleScreenBufferSize (h, c);
 r.Left := x;
 r.Top := y;
 r.Right := r.Left + cx;
 r.Bottom := r.Top + cy;
end; // SetConSize


function  CheckMakeDir;
begin
 if sDir = '' then
  begin
   result := FALSE;
   PrintError('CheckMakeDir param sDir length = 0');
   exit;
  end;

 sDir := AnsiReplaceStr ( sDir, '%DateDir%', FormatDateTime( 'yy.mm.dd - dddd', Now ) );
 result := TRUE;
 if not DirectoryExists (sDir) then
  try
   result := ForceDirectories (sDir);
  except
   on E: Exception do
     PrintError('Exception catched while creating directory "' + sDir + '", message: ' + E.ClassName + '@' + E.Message);
  end;
end; // CheckMakeDir


function  IfV (b: Boolean; i1, i2: Int64): Int64; overload;
begin
 if b then result := i1 else result := i2;
end;

function  IfV (b: Boolean; f1, f2: Double): Double; overload;
begin
 if b then result := f1 else result := f2;
end;

function  IfV (b: Boolean; s1, s2: String): String; overload;
begin
 if b then result := s1 else result := s2;
end;

function  IfV (b: Boolean; p1, p2: Pointer): Pointer; overload;   inline;
begin
 if b then result := p1 else result := p2;
end;



procedure SafeExec (Method: TNotifyEvent; Param: TObject);
begin
 try
  Method (Param);
 except
  on E: Exception do
    PrintError ('#SAFE-EXEC: Exception catched = ' + E.Message);

 end;
end;

function StrSuffix (const src, prefix: String): String;
var p, l: Integer;
begin
 result := '';
 p := Pos(prefix, src);
 if p <= 0 then exit;
 l := Length (prefix);
 result := Copy (src, p + l, Length (src) - l);
 if result <> '' then exit;
end; // StrSuffix

function MakeMemStream (psrc: Pointer; cntBytes: Integer): TStream;
begin
 result := TMemoryStream.Create;
 result.Write(psrc^, cntBytes); 
end; // MakeMemStream

var
    sc_print: TSyncSection;


procedure CBeep ( freq, ms: DWORD );
begin
 if gSoundsEnable then
    Windows.Beep (freq, ms)
 else
    Sleep (ms);
end;


procedure PlaySoundFile;
var sFile: String;
    flags: DWORD;
begin
 if gSoundsEnable then
 try
  if Pos ('\', sFileName) = 0 then
     sFile := CorrectFilePath ( EXEPath + '..\sounds\' + sFileName )
  else
     sFile := sFileName;

  flags := SND_FILENAME or SND_NOWAIT;
  if bAsync then
     flags := flags or SND_ASYNC;
  if FileExists (sFile) then
     PlaySound( PChar( sFile ), 0, flags )
  else
   if bWarnsNoExist then
       PrintError('PlaySoundFile - not found file ' + sFile);
 except
  on e: Exception do
    ProcessMsg ('~C0C PlaySound caused exception ' + e.Message + #13#10 +
                'Possilbe file ' + sFileName + ' not exist~C07');
 end;
end;


procedure PrintError (const msg: String);
begin
 Move ( error_times [0], error_times [1], sizeof(TDateTime) * High (error_times) );

 error_times [0] := g_time_func();

 ODS ('[~T/~U/~i]. ~C0C #ERROR: '#13#10#9 + msg + '~C07', PMF_IMPORTANT);
 alert_cntr := 300;
 if gSoundsEnable then
    PlaySoundFile ( 'chord.wav', FALSE );
 if Assigned (on_err_callback) then
    on_err_callback (msg);
end;

function Z2P;
var zp: Integer;
begin
 result := s;
 repeat
  zp := Pos(',', result);
  if zp > 0 then result[zp] := '.';
 until zp <= 0;
end;

function ConEventCount: DWORD;
begin
 result := 0;
 if hInput = 0 then hInput := GetStdHandle (STD_INPUT_HANDLE);
 GetNumberOfConsoleInputEvents(hInput, result);
end;

function ReadKey: Char;
var rb, evts: DWORD;
    irec: TInputRecord;
begin
 if hInput = 0 then hInput := GetStdHandle (STD_INPUT_HANDLE);
 rb := 0;
 result := #0;
 repeat
  evts := ConEventCount;
  if evts = 0 then break;
  rb := 1;
  ReadConsoleInput(hInput, irec, 1, rb);
  if (irec.EventType = KEY_EVENT) and (irec.Event.KeyEvent.bKeyDown) then
     {$IFDEF UNICODE}
       result := irec.Event.KeyEvent.UnicodeChar
     {$ELSE}
       result := irec.Event.KeyEvent.AsciiChar;
     {$ENDIF}
 until result <> #0;
end; // ReadKey


var
   lks: array [0..255] of SHORT;

function IsKeyDown ( vk: DWORD ): Boolean;
var
   st: SHORT;
begin
 result := FALSE;
 if vk >= 256 then exit;

 st := GetAsyncKeyState ( vk );
 result := ( st and $8000 <> 0 ) and ( lks [vk] = 0 );

 lks [vk] := st;
end;


function IsKeyPressed ( vk: DWORD ): Boolean;
begin
 result := ( GetAsyncKeyState ( vk ) and $C000 <> 0 );
end;



var
   gReadBuff: String;

function TryReadLn(dwMsec: DWORD): String;
var
  ch: CHAR;
  l: Integer;
begin
 result := '';

 repeat
  ch := ReadKey;
  l := Length(gReadBuff);
  case ch of
   #0: begin
        Sleep(dwMsec);
        break;
       end;
   #8: if l > 0 then Delete(gReadBuff, l, 1);
   #13: begin
         result := gReadBuff;
         gReadBuff := '';
         break;
        end;
   #14..#30:;
   else
      begin
       gReadBuff := gReadBuff + ch;
      end;
  end;
  if hOutput <> 0 then Write(#13 + gReadBuff + '  ');
 until ch = #0;
end; // TryReadLn

function AdSP (const s: String; const lcs: TCharSet): String; // добавить пробел, если строка не пустая, и последний символ не входит в множество
var i: Integer;
    ch: CHAR;
begin
 result := s;
 if s <> '' then
  begin
   i := Length (s);
   ch := s [i];
   if CharInSet (ch, lcs) then exit;
   result := result + ' ';
  end;
end; // AdSP

function        Ansi2Oem (const s: AnsiString): AnsiString;
begin
 SetLength (result, Length (s) + 1);
 result [Length(s) + 1] := #0;
 CharToOemA (PAnsiChar (s), PAnsiChar(result));
end;

function Wide2Oem (const s: WideString): AnsiString;
begin
 result := AnsiString(s);
 SetLength(result, Length(s) + 2);
 ZeroMemory (@result[1], Length(s) + 2);
 CharToOemW ( PWideChar(s), PAnsiChar(result) );

end; // Wide2Oem

procedure SetAnsiStr (pstr: PAnsiChar; const src: String; nMaxChars: Integer);
begin
 {$IFDEF UNICODE}
   SetStrZ (pstr,  src, nMaxChars);
 {$ELSE}
   SetStrZ(pstr,  PChar( src ), nMaxChars);
 {$ENDIF}
end; // SetAnsiString

function Err2str;
var
    p : array [0..255] of WideChar;
begin
 if nError = -1 then
    nError := GetLastError;

 ZeroMemory (@p, sizeof(p));

 FormatMessage (FORMAT_MESSAGE_FROM_SYSTEM, nil, nError,
                LANG_NEUTRAL or (SUBLANG_SYS_DEFAULT shl 10), P, 256, nil);

 result := ' ' + IntTOStr(nError) + '.- ' +  p;
end;


function LimStr (const s: String; maxLen: Integer): String;
begin
 if Length (s) > maxLen then
   result := Copy (s, 1, maxLen)
 else
   result := s;
end;

function AlignedPtr(pbuff: Pointer; mask: LongWord): PInt64;
begin
 result := pbuff;
 while ( LongWord (result) and mask <> 0 ) do
         Inc ( NativeUInt (result) );
end;

var
   pfc_coef_dt: Double = 0;
   g_tick: Integer;


function GetPFC: Int64;
var
   PV: PInt64;
   buff: array [0..31] of BYTE;

begin
 PV := AlignedPtr (@buff, $0F);
 QueryPerformanceCounter (PV^);
 result := PV^;
end;

function GetTimeStamp: Double;
var
   PV: PInt64;
   buff: array [0..31] of BYTE;
   cnt: Integer;
begin
 cnt := 0;
 Inc (g_tick);

 PV := AlignedPtr (@buff, $0F);
 if (0 = g_pfc_coef) or (g_tick and $FF = 0) then
   repeat
    QueryPerformanceFrequency(PV^);
    // 1 = TimeStamp / PV
    // C * TimeStamp = S
    if PV^ > 0 then
     begin
      g_pfc_coef := PV^; // multiply adapted
      g_pfc_coef := 1000 / g_pfc_coef;
     end;

    Inc (cnt);
   until (g_pfc_coef > 0) and (g_pfc_coef < 1) or (cnt > 10);

 pfc_coef_dt := g_pfc_coef * ONE_MSEC_COEF;


 if cnt > 10 then
    Inc (fix_timeouts, 10000);

 result := (GetPFC * g_pfc_coef);
end;

procedure FixRefTimestamp;

var
   loops: Int64;
   fix: Int64;
   timeout: Boolean;
   tss, tmx: Double;
   t_int, t_frac: Double;

   st, prv: TSystemTime;
begin
 Sleep(20);
 // High CPU usage up-to 15.6 msec, for ajusting time at timer tick
 loops := 0;
 tss := GetTimeStamp;
 Inc (fix_counts);
 GetLocalTime (prv);


 Repeat
  GetLocalTime (st);
  Inc (loops);
  tmx := GetTimeStamp - tss;
  timeout := (loops > MaxLoops) or (tmx > 10000);

 Until (st.wMilliseconds <> prv.wMilliseconds) or (st.wSecond <> prv.wSecond) or timeout;


 t_int := EncodeDate (st.wYear, st.wMonth, st.wDay);
 t_frac := EncodeTime (st.wHour, st.wMinute, st.wSecond, st.wMilliseconds);

 fix := GetPFC;
 tmx := 0;

 if pfcRefStamp > 0 then
   begin
    tmx := (fix - pfcRefStamp);
    tmx := Max(1, g_pfc_coef * tmx) * ONE_MSEC_COEF; // предполагается, что менее 1-мс не пройдет
   end;

 pfcRefStamp := fix;


 if t_frac <> dtRefTime_frac then
   begin
    dtRefTime_int := t_int;
    dtRefTime_frac := t_frac;
   end
 else
   begin
    dtRefTime_frac := dtRefTime_frac + tmx;
    while dtRefTime_frac >= 1 do
      begin
       dtRefTime_frac := dtRefTime_frac - 1.0;
       dtRefTime_int := Trunc (dtRefTime_int + 1);
      end;

    Inc (res_equals);
   end;




 if timeout then
    Inc (fix_timeouts);


 if (loops > 70 * 1000 * 1000) then
    PrintError('FixRefTimestamp detected clocks hang!');
end;

function RealTime(bOnlyTime: Boolean): Extended;
var
   reft: TDateTime;
   cdta: Int64;
begin
 {$IFDEF CPUX86}
 asm
  finit
 end;
 {$ENDIF}

 reft := + dtRefTime_frac;

 result := reft;

 if pfc_coef_dt = 0 then
  begin
   Inc (fix_timeouts);
   exit;
  end;


 cdta := (GetPFC - pfcRefStamp);

 if (cdta = 0) then
    begin
     Inc (res_equals, 100000);
    end
 else
    begin
     result := cdta * pfc_coef_dt; // convert to msec
     if result <= 0 then Inc (res_equals, 10);
     result := result + reft;
    end;

 if not bOnlyTime then
  begin
   result := result + dtRefTime_int;
   p_shared_vars.local_dt := result;
  end;

end; // RealTime



function LocalTime (nMethod: Integer): TDateTime;
begin
 case nMethod of
  1: result := RealTime(FALSE);       // + тики в сутки
  2: result := local_time;
  else
      result := Now;
 end;
end; // LocalTime

function SysTimeToStr (const st: TSystemTime; dwInclude: DWORD = $FFFF): String;

  procedure AddBV(var s: String; b: BYTE); inline;
  begin
   if s <> '' then s := s + ':';
   if b < 10 then s := s + '0';
   s := s + IntToStr(b);
  end; // AddBY

begin
 result := '';
 if dwInclude and $8 <> 0 then AddBV (result, st.wHour);
 if dwInclude and $4 <> 0 then AddBV (result, st.wMinute);
 if dwInclude and $2 <> 0 then AddBV (result, st.wSecond);
 if dwInclude and $1 <> 0 then
    result := result + '.' + LZero ( IntToStr(st.wMilliseconds), 3 );
end;

procedure ReplaceInsert (var t: String; i, cnt: Integer; const sub: String);
begin
 Delete (t, i, cnt);
 Insert (sub, t, i);
end;

function TimeToStrMS(dt: TDateTime; decimals: Integer = 3): String;
var
   fs: TFormatSettings;
   ms: Double;
    s: String;

begin
 // Assert ( MSecsPerDay > 0, 'MSecsPerDay = ' + IntToStr(MSecsPerDay) );

 fs.LongTimeFormat := 'hh:nn:ss';
 fs.ShortTimeFormat := fs.LongTimeFormat;

 {for n := 1 to decimals do
     fs.LongTimeFormat := fs.LongTimeFormat + 'z';}

 ms := Frac ( dt ); // отсечка времени от даты
 ms := Frac ( ms * SEC_PER_DAY ); // остаток от секунд

 s := ftow( ms, '%.' + IntToStr(decimals) + 'f');
 Delete (s, 1, 1); // удаление нуля перед
 fs.TimeSeparator := ':';
 fs.DecimalSeparator := '.';

 try
  result := TimeToStr (dt, fs) + s;
 except
  on E: Exception do
    OnExceptLog ( 'TimeToStrMS ' + ftow(dt), E );
 end;
end;


function  FmtFloat (f: Double; decimals: Integer = 3): String;
var
   fmt: String;
begin
 fmt := '%.' + IntToStr (decimals) + 'f';
 result := ftow (f, fmt);
end;

function InfoFmt;
var
   dt: TDateTime;
   df: Double;
   //fr: Double;
   dsc, ex: CHAR;
   i, dcnt: Integer;
       add: String;

begin
 result := s;
 if Pos('~', s) = 0 then exit;
 if ct <= 0 then
    ct := g_time_func();


 result := '';
 Repeat
  i := Pos('~', s);
  if ( i = 0 ) then break;

  result := result + Copy (s, 1, i - 1); // копирование остаточка

  ex := #0;
  dsc := s [i + 1];
  if Length (s) >= i + 2 then
               ex := s [i + 2];


  dcnt := 1; // по умолчанию тильду и дескриптор

  case dsc of
     'B': begin
           if g_last_time = 0 then g_last_time := ct;
           df := (ct - g_last_time) * MS_PER_DAY;
           if df < 9.9 then
              result := result + ftow(df, '%5.3f' ) // 0.777
           else

           if df < 1000 then
              result := result + ftow(df, '%5.1f' )
           else
              result := result + ftow(df, '%5.0f' );
           g_last_time := ct;
          end;
     't': result := result + FormatDateTime ('hh:nn:ss', ct);
     'd': begin
           if Trunc (ct) < 1 then
             begin
              PrintError ('InfoFmt date-tag ct < 1');
              ct := Now;
             end;
           result := result + FormatDateTime ('d mmm', ct);
          end;
     'D': begin
           if Trunc (ct) < 1 then
             begin
              PrintError ('InfoFmt date-tag ct < 1');
              ct := Now;
             end;
           result := result + FormatDateTime ('d.mm.yyyy', ct);
          end;
     'I': result := result + Format('%4d', [GetCurrentThreadId()]);
     'i': result := result + Format('%s(%4d)', [FastThreadName(GetCurrentThreadId), GetCurrentThreadId]);
     'P': result := result + Format('%4d', [GetCurrentProcessId()]);
     'V': result := result + ftow(local_time, '%.7f');
     'M': result := result + ModuleName;
     'U', 'u':   // UPTIME
          if ps_creation_time > 0 then
          begin
           if ct = 0 then ct := LocalTime (1);

           dt := ct - ps_creation_time;
           if ( dsc = 'U' ) and ( dt >= 1 ) then
              add := '+' + IntToStr( Trunc(dt) ) +  FormatDateTime (' hh:nn:ss.zzz', dt)
           else
              add := '+' + FormatDateTime (' hh:nn:ss.zzz', dt);

           add := AnsiReplaceStr (add, ' 00:00:', '');
           add := AnsiReplaceStr (add, ' 00:', '');

           result := result + add;
          end
         else
          result := result + '+????';
     'n': result := result + #13#10;
     'T': begin
            dcnt := 2; // толстый дескриптор
            if ct = 0 then ct := LocalTime (1);

            case ex of
             'P': result := result + TimeToStrMS (ct, 6);
             'R': result := result + TimeToStrMS (Now); // ftow(ct, '%.7f') + '=' +
             'L': begin
                   result := result + TimeToStrMS (local_time);
                  end;
             'X': begin
                   result := result + TimeToStrMS (RealTime(TRUE)) + ' @' +
                      ftow( RealTime(FALSE), '%.7f')  +  ' vs ' +
                      ftow( dtRefTime_frac, '%.7f') + ' vs ' +
                      ftow( Frac (Now), '%.7f') +
                      '/' + IntToStr (fix_timeouts) + '/' + IntToStr(res_equals);
                   // result := result + ftow (fr, ' /%.7f');
                  end;

             else
                  begin
                   result := result + TimeToStrMS (ct); // вывод с заданным количеством миллисекунд

                   dcnt := 1; // все-таки обычный дескриптор
                  end;
            end; // case-small for ~T

           end; // ~T
      else
          result := result + '~' + dsc; // копировать чужой тег полностью
    end; // case desc char

  Delete (s, 1, i + dcnt); // удалить вместе с тильдой и тегом
 Until (s = '');
 result := result + s;
 // system.string
end;

function HideSP(const s: String): String;
begin
 result := ReplaceChar (s, ' ', space_replace);
end;

function UnhideSP (const s: String): String;
begin
 result := ReplaceChar (s, space_replace, ' ');
end;


function TryEnter (var scs: RTL_CRITICAL_SECTION; timeOut: Integer): Boolean;
var t_start, elapsed: Int64;
begin
 t_start := GetTickCount();
 repeat
  result := TryEnterCriticalSection (scs);
  if not result then sleep (10);
  elapsed := GetTickCount() - t_start;
 until (result) or (elapsed > timeOut);
end; // TryEnter

function atof (const s: String): Double;
var err: Integer;
    v: Int64;
begin
 if s = 'INF' then
    result := Infinity
 else
 if s = '-INF' then
    result := -Infinity
 else
   begin
    if Pos('$', s) = 1 then
       begin
        val (s, v, err);
        result := v;
       end
    else


    if Pos(',', s) > 0 then
       val ( ReplaceChar (s, ',', '.'), result, err )
    else
       Val ( s, result, err );
   end;
end;

function ftoa;
begin
 result := AnsiString ( ReplaceChar (Format (fmt, [v]), ',', FormatSettings.DecimalSeparator) );
end; // ftoa

function ftow;
begin
 result := ReplaceChar (Format (fmt, [v]), ',', FormatSettings.DecimalSeparator);
end; // ftoa

function ftos (v: Double; max_dig: Integer = 5): String;
var
   sfmt: String;
   n: Integer;
begin
 sfmt := '0.';
 for n := 1 to max_dig do sfmt := sfmt + '#';
 result := AnsiReplaceStr ( FormatFloat(sfmt, v), ',', FormatSettings.DecimalSeparator);
end; // ftos

function ftosf (v: Double; digits: Integer = 5): String;
begin
 result := ftow (v, '%.' + IntToStr(digits) + 'f');
end;


function atoi (const s: String): Int64;
var err: Integer;
begin
 val (s, result, err);
end;

function ListIntVal (sl: TStrings; const field: String): Integer;
begin
 result := atoi (sl.values [field]);
end;

procedure ColorPrint (msg: String; colored: Boolean; hConOutput: THandle );
var
    attrs, p: Integer;
         wch: DWORD;
      locked,
     put_eol: Boolean;
           s: String;

begin
 if hConOutput = 0 then
    hConOutput := GetStdHandle (STD_OUTPUT_HANDLE);



 if (lMDesc <> nil) and (lMDesc.Status = MST_FINALIZED) then exit;

 if ( hConOutput = 0 ) or (not con_enabled) then exit;

  if (nil = sc_print) then
    begin
     InitializeModule ('Misc');
     if sc_print = nil then exit;
    end;

 locked := sc_print.TryLock(500);

 try
  try
   put_eol := ( Pos('~\', msg) <= 0 );
   if not put_eol then
      msg := AnsiReplaceStr(msg, '~\', '');

   while (msg <> '') do
    begin
     p := Pos('~C', msg);
     if p <= 0 then
       begin
        WriteConsoleW ( hConOutput, PWideChar (msg), Length (msg), wch, nil );
        msg := '';
       end
     else
       begin
        if p > 1 then
          begin
           s := Copy(msg, 1, p - 1);    // before-tag msg
           Delete (msg, 1, p - 1);
           WriteConsoleW ( hConOutput, PWideChar (s), Length (s), wch, nil );
          end
        else
          begin
           s := Copy (msg, p + 2, 2); // attrs in hex
           attrs := atoi ('$' + s);
           if colored then
              SetConsoleTextAttribute ( hConOutput, attrs );
           Delete (msg, 1, p + 3); // delete attribute tag
          end;
       end;
    end; // while
    if put_eol then
       WriteConsoleA ( hConOutput, PAnsiChar(#13#10), 2, wch, nil );

  except
   on E: Exception do
     begin
      ProcessMsg('Exception catched while in ColorPrint: ' + E.Message, '', PMF_OUTFILE);
      con_enabled := FALSE;
     end;
  end;
 finally
  if locked and (nil <> sc_print) then sc_print.Unlock;
 end;
end; // ColorPrint


procedure DeleteColorTags (var msg: String);
var n: Integer;
begin
 repeat
  n := Pos ( '~C', msg);
  if n <= 0 then break;
  Delete (msg, n, 4);
 until FALSE;
end;

function  RemoveColorTags (const msg: String): String;
begin
 result := msg;
 DeleteColorTags (result);
end; // RemoveColorTags


procedure ProcessMsg;
var
    s: String;
   ns: Integer;
   pm: PString;

begin
 InterlockedIncrement (in_psm);

 if ( _exception_flag >= 0 ) and ( Length(cmsg) < High(last_dbg_msg) ) then
  begin
   FillChar (last_dbg_msg, sizeof(last_dbg_msg), 0);
   SetStrWZ (last_dbg_msg, PChar (cmsg), High (last_dbg_msg) + 1);
  end
 else
  Dec (_exception_flag);

 ns := ( flags shr 24 ) and High (TConsoleBuffers); // high byte


 if (flags and PMF_OUTCON > 0) and (hWndcon <> 0) and (con_enabled) and Assigned ( gConBuffers ) then
     gConBuffers^ [ns].SyncWrite ( cmsg, flags );

    // ColorPrint (cmsg, flags and PMF_UNFMT = 0);

 // msg := RemoveColorTags ( String( msg ) );
 if smsg = '' then
   begin
    s := cmsg;
    DeleteColorTags (s);
    pm := @s;
   end
 else
    pm := @smsg;


 if ( flags and PMF_NO_ODS = 0 ) and ( ns = gLogStream ) and dbg_present then
      OutputDebugStringW ( PWideChar (pm^) );



 if ( flags and PMF_OUTFILE <> 0 ) and (gLogFileName <> '') then
     LogMsgToFile( pm^, ns );
 InterlockedDecrement (in_psm);
end; // ProcessMsg



procedure ODS (const msg: WideString; print_flags: DWORD); stdcall;
begin
 print_flags := print_flags or gThreadLogFlags;

 if Assigned (ODS_proc) then
    ODS_proc ( @msg[1], print_flags )
 else
    LocalODS ( @msg[1], print_flags );
end; // ODS

procedure LocalODS (msg: PWideChar; print_flags: DWORD = 255); stdcall;
var
   imsg: WideString;
   dt: TDateTime;
   nc: Integer;
begin
 if ( lMDesc = nil ) or ( lMDesc <> nil ) and (lMDesc.Status <> MST_INITIALIZED) or ( p_shared_vars = nil ) then
  begin
   ProcessMsg ( imsg, '', print_flags );
   exit;
  end;

 if (logging_thread = nil) and (need_dt) then
      begin
       p_shared_vars.log_th := TAppLoggingThread.Create(FALSE);
       lMDesc.Globals.Add (logging_thread);
       nc := 0;
       while ( dbgt_status <> 1 ) and (nc < 100) do
         begin
          Inc (nc);
          Sleep(5);
         end;
      end;

 if print_flags and PMF_UNFMT = 0 then
   begin
    if print_flags and PMF_SYSTIME = 0 then
       dt := g_time_func
    else
       dt := Now;

    imsg := InfoFmt (msg, dt);
   end
 else
    imsg := msg;



 // internal handling
 if ( logging_thread <> nil ) and ( not logging_thread.Terminated ) and (dbgt_status = 1) and (print_flags and PMF_DIRECT = 0) then
    logging_thread.AddMsg(imsg, print_flags)
 else
   begin
    ProcessMsg ( imsg, '', print_flags );
   end;
end; // ODS


function  ActiveConsole: Integer;
begin
 result := gLogStream;
end;

procedure SelectConsole ( ls: Integer );
begin
 ls := ls and High (TConsoleBuffers);
 if ( ls = gLogStream ) and gConBuffers^ [gLogStream].Active then exit;

 gConBuffers^ [gLogStream].Active := FALSE;
 gConBuffers^ [ls].Active := TRUE;

 gLogStream := ls;
end;


procedure InitPaths;
var
   modPath, modName: String;
begin
 modName := ModuleMgr.ModuleFileName (HInstance);
 modPath := ExtractFilePath ( Trim(modName) );

 if gLogPath = '' then
    gLogPath := modPath + '..\logs\%DateDir%\';


 gLogPath := AnsiReplaceStr ( gLogPath, '%ExePath%', ExePath );
 gLogPath := AnsiReplaceStr ( gLogPath, '%DateDir%', FormatDateTime( 'yy.mm.dd - dddd', Now ) );
 gLogPath := CorrectFilePath (gLogPath);

 SetConsoleTitle ( PChar( ExtractFileName (ExeFileName) + ' - logs path: ' + gLogPath));
 if not CheckMakeDir ( gLogPath ) then exit;

end;

procedure StartLogging;
var
   fn, modPath, modName: String;

   h: THandle;
   n: Integer;
begin
 if gLogFileName <> '' then exit;

 fn := sPath;
 gLogPath := sPath;
 InitPaths;


 modName := ModuleMgr.ModuleFileName (HInstance);
 modPath := ExtractFilePath ( Trim(modName) );



 _SafeLog( ' #INIT: app staring ' + modName );
 modName := ExtractFileName ( Trim(modName) );



 h := GetModuleHandle ( ExeFileName );
 if h <> HInstance then
    modName := ExtractFileName (ExeFileName) + '+' + modName;


 for n := Length(modName) downto 2 do
  if modName [n] = '.' then
    begin
     modName := Copy (modName, 1, n - 1);
     break;
    end;


 gLogFileName := gLogPath + '\' + modName + FormatDateTime(' hh_nn', Now) +
                 ' PID@' + IntToStr(GetCurrentProcessId) + '.log';

 {$IFDEF FullDebugMode}
 fn := AnsiReplaceStr(gLogFileName, '.log', '-mm.log');
 SetMMLogFileName( PAnsiChar( AnsiString(fn) ) );
 {$ENDIF}
 if fn = '' then exit;
end;

function StrTok (var s: String; const tchars: TCharset): String;
var n, c: Integer;

begin
 result := '';
 c := 0;
 n := 1;
 while n <= Length (s) do
  begin
   // если символ не разделитель, его добавить в результат
   if not ( AnsiChar( s [n] ) in tchars) then
     begin
      result := result + s [n];
      Inc (c);    // количество вырезаемых символов с начала строки
      Inc (n);
     end
    else
     begin
      Delete (s, 1, n); // удалить всё до текущего разделителя
      c := 0;
      // завершить поиск разделителей, при наличии токена, или окончании строки
      if (result <> '') or ( n >= Length(s) ) then exit;
     end;
  end; // while
  if c > 0 then Delete (s, 1, c);
end; // StrTok

function Quote (const s: String): String;
begin
 result := '"' + s + '"';
end; // Quote

function Unquote (const s: string): String;
var p: Integer;
begin
 result := s;
 repeat
  p := pos ('"', result);
  if p > 0 then Delete (result, p, 1);
 until p = 0;
end; // Unquote


var
  CRCTableR: array[BYTE] Of DWORD;
  CRCTableD: array[BYTE] Of DWORD;


function Reflect(ref, ch: DWORD): DWORD;
var
   i: DWORD;
begin
 result := 0;

 for i := 1 to ch do
  begin
   if ref and 1 <> 0 then
      result := result or ( 1 shl DWORD(ch - i) );
   ref := ref shr 1;
  end;

end;

procedure BuildCRCTables;
const
   HI_BIT = $80000000;

var
  i, j: DWORD;
     r: DWORD;
begin
  FillChar(CRCTableD, SizeOf(CRCTableD), 0);
  FillChar(CRCTableR, SizeOf(CRCTableR), 0);

  for i := 0 to 255 do
    begin
      r := i shl 1;
      for j := 8 downto 0 do
        if (r and 1) <> 0 then
          r := (r shr 1) xor REV_CRC_POLY
        else
          r := r shr 1;

      CRCTableR [i] := r;
     end;

  for i := 0 to 255 do
    begin
      r := i shl 24;
      for j := 0 to 7 do
          r := IfV ( r and HI_BIT <> 0, ( r shl 1 ) xor DIR_CRC_POLY, r shl 1 );
      CRCTableD [i] := r; // }
    end

end;

function CalcCRC32( psrc: Pointer; cntBytes: Integer; CRC_start: DWORD ): DWORD;
var
     i: Integer;
   pba: PByteArray2GB absolute psrc;
begin
  result := CRC_start;
  try
    for i := 0 to cntBytes - 1 do
          result := CRCTableR [ $FF and (result xor DWORD(pba^[i]) ) ] xor ( (result shr 8) and $00FFFFFF )

  except
   on E: EAccessViolation do
      PrintError(
        Format('CalcCRC32 - possibly page error. psrc=$%p, cntBytes=$%x.'#13#10#9 +
                ' Exception: %s', [psrc, cntBytes, E.Message])
                );
  end;
  result := not result;
end;

function CalcCRC32D ( psrc: Pointer; cntBytes: Integer; CRC_start: DWORD ): DWORD;
var
     i: Integer;
     a: DWORD;
     b: BYTE;
   pba: PByteArray2GB absolute psrc;
   pbs: PAnsiChar absolute psrc;
begin
  result := CRC_start;
  if pbs <> nil then
  try
    {
      for(i=0;i<og->header_len;i++)
         crc_reg=(crc_reg<<8)^crc_lookup[((crc_reg >> 24)&0xff)^ogh->header[i]];
    }

    for i := 0 to cntBytes - 1 do
       begin
        a := result shl 8;
        b := result shr 24;
        result := a xor CRCTableD [ b xor pba^[i] ];
       end
  except
   on E: EAccessViolation do
      PrintError(
        Format('CalcCRC32D - possibly page error. psrc=$%p, cntBytes=$%x.'#13#10#9 +
                ' Exception: %s', [psrc, cntBytes, E.Message])
                );
  end;
end;




threadvar
    spam_counter: Integer;

{ TDebugThread }



procedure TAppLoggingThread.AddMsg;
var
   i, loops: Integer;
   pms: PMsgSlot;
    sc: Integer;
begin
 if ( Terminated ) or (dbgt_status <> 1) then exit;


 loops := 0;
 sc := spam_counter + 1;
 Inc (spam_counter);

 if ( sc > 2028 ) or ( in_buff_count = 0 ) then
   begin
    spam_counter := 0;
    sc := 0;
   end;

 if sc >= max_sc then
   begin
    InterlockedExchange (max_sc, sc);
    if in_buff_count > High (slots) - 16 then
            WaitForSingleObject (load_msg.Handle, 250); // надо избежать обгона очереди...
   end;



 repeat
  i := AllocSlot;
  if i < 0 then
     begin
      // load_msg.ResetEvent;
      load_msg.WaitFor (150); // затор сообщений, кольцевой буфер не успевает!
     end;

  Inc (loops);
 until (i >= 0) or (loops > 250);


 if ( i >= 0 )  then // InitializeCriticalSection (FSection);
  begin
   {   asm
       nop
      end; }

   pms := @slots[i and $FF];
   pms._index := i;
   pms.flags := flags;
   pms.Load (sMsg);
   m_idx [i and $FF] := S_FILLED;


   i := InterlockedIncrement (in_buff_count);

   if (i >= 8) and Assigned (evt_msg) then
       evt_msg.SetEvent;


  end
 else
   ProcessMsg ( '!!!' + sMsg, '', flags );
end; // AddMsg



function TAppLoggingThread.AllocSlot: Integer;
var
   widx, i, t, n: Integer;
begin
 result := -1;

 // if in_buff_count >= High (slots) - 4 then exit;

 t := wi;

 for n := 0 to High(slots) do
  begin
   Inc (t);
   i := t and $00FF;

   widx := InterlockedCompareExchange ( m_idx[i], S_ALLOCATED, 0);

   if widx <> 0 then continue;

   wi := t;
   result := t;
   break;
  end;

end;

constructor TAppLoggingThread.Create(CreateSuspended: Boolean);
var
   n: Integer;
begin
 for n := 0 to High (slots) do
     slots[n].SetSize(1024);

 inherited Create (CreateSuspended);
 self.Priority := tpHigher;
end;

destructor TAppLoggingThread.Destroy;
var
   n: Integer;
begin
 for n := 0 to High (slots) do
     slots[n].SetSize(0);

 try
  inherited;
 except
   on E: EMonitorLockException do;
 end;

end;

procedure TAppLoggingThread.Execute;
const
   DT_ONE_SEC = 1.0 / (24 * 3600);
   cb_Read = 1023;
var
   ltmp: TStrings;
   prvt: TDateTime;
   msgc: String;
   //lock: Boolean;
   oldf, msgf: DWORD;
    pms: PMsgSlot;
    li, i, n, best: Integer;
      s: String;
begin
 ltmp := TStringList.Create ();
 {for n := 0 to High(list_list) do
     list_list[n] := TStrMap.Create;}

 evt_msg := TEvent.Create(nil, TRUE, FALSE, '');
 load_msg := TEvent.Create(nil, FALSE, FALSE, '');

 ss_list := TSyncSection.Create;
 last_owner := ThreadID;
 ProcessMsg ( InfoFmt( '[~T] #DBG: Created Debug Thread, ID = ~I') );

 {$IFDEF CPUX86}
  asm
   finit
  end;
  s := ClassName + '@=$' + IntToHex( DWORD(self), 8) +'_M' + IntToHex(hInstance, 8);
  madExcept.NameThread ( ThreadId, s );
 {$ENDIF}

 try
   dbgt_status := 1;
   prvt := Now;
   Repeat
    Inc (cntlps);

    if ( g_time_func = Now ) then
     begin
      if ( cntlps and 15 = 0 ) then FixRefTimestamp ();
      time_last := LocalTime (1);
     end
    else
      time_last := g_time_func ();

    // flushing log file (forced)
    if time_last > prvt + 2 * DT_ONE_SEC then
     begin
      prvt := time_last;
      for i := 0 to High ( gLogFiles ) do
       if gLogFiles [i] <> 0 then
        begin
         FlushFileBuffers ( gLogFiles [i] );
         log_flush_time := prvt;
        end;
     end;

    if alert_cntr > 0 then Dec (alert_cntr);

    // поток завершен
    if (Terminated) then break;

    // waiting for message....
    if (in_buff_count = 0) and ( WaitForSingleObject (evt_msg.Handle, 75) = WAIT_OBJECT_0 ) then
        evt_msg.ResetEvent;



    ltmp.Clear;
    li := ri + 1; // load index

    // lock := ss_list.TryLock (100);

    // oldest := li;
    best := -1;

    for n := 0 to High (slots) + 1 do
     begin
      i := li and $00FF;
      if m_idx[i] <> S_FILLED then continue;

      pms := @slots[i];

      if best < 0 then
         best := pms._index
      else
         best := Min (best, pms._index);

      // oldest := Min (oldest, pms._index);

      if (pms._index <> li) then continue; // ordering check


      ri := pms._index;                                 // read index update
      li := ri + 1;

      if pms.ccLen > 0 then
         ltmp.AddObject ( pms.GetStr, Ptr ( pms.flags ) )
      else
         ltmp.AddObject ( '', Ptr ( pms.flags ) );

      m_idx[i] := S_FREE; // mark unused


      if InterlockedDecrement (in_buff_count) = 0 then
         begin
          break;
         end;
     end;

    if max_sc > 0 then InterlockedDecrement (max_sc);

    if (in_buff_count > 0) and (ltmp.Count = 0) and ( best >= 0 ) and ( Abs (best - ri) >= 255 ) then
        ri := best - 1;

    // ltmp.Clear;

    load_msg.SetEvent;
    if ltmp.Count = 0 then continue;

    last_msg_time := time_last;

    dbg_present := IsDebuggerPresent;
    msgc := ltmp [0];
    oldf := DWORD (ltmp.Objects [0]);
    // сборка строк в унифицированные по флажкам
    try
       // дампинг сообщений в медленные получатели
       for n := 1 to ltmp.Count - 1 do
         begin
          if ltmp[n] = 'STOP' then Terminate;
          msgf := DWORD (ltmp.Objects [n]);

          if (msgf = oldf) and (oldf and PMF_NOCACHE = 0) then
            begin
              s := ltmp [n];
              if Pos('~/', s) > 0 then
                 msgc := msgc + s
              else
                 msgc := msgc + #13#10 + s;
            end
          else
           begin
            if msgc <> '' then
               ProcessMsg (msgc, '', oldf); // unified output

            if (msgf and PMF_NOCACHE <> 0) then
              begin
               ProcessMsg (msgc, '', msgf);
               msgc := '';
              end
            else
               msgc := ltmp [n];

            oldf := msgf;

           end;

         end;

        if msgc <> '' then
           ProcessMsg (msgc, '', oldf); // lastest output

        ltmp.Clear;
     finally
     end;

   Until Terminated or ( nil = logging_thread );


 finally
  dbgt_status := -1;
  // блокировка доступа
  if ss_list.TryLock (1000) then ss_list.Unlock;

  ss_list.Free;
  evt_msg.Free;
  load_msg.Free;
  ltmp.Free;
 end;

 try
  ProcessMsg( InfoFmt('[~T]. #DBG: DebugThread - complete execution'), '', 15 );
 except
  on E: Exception do
     OutputDebugString ( PChar ('Shutdown exception ' + E.Message) );
 end;

end;  // Execute

procedure LibInit ( pinf: PLibInitInfo ); stdcall;
begin
 if pinf.dwVersion > 0 then
  begin
   ODS_proc := pinf.ODS_proc;
  end;

end;


procedure InitSharedVars;
const
     BUFFERS_SZ = sizeof (TMiscSharedVars);


var
    map_name: String;
     hist_ln: String;

begin
 p_shared_vars := nil;
 ModuleName := GetModuleName ( HInstance );
 map_name := 'SHARED_VARS_' + IntToHex (GetCurrentProcessId, 4);
 shared_owner := FALSE;
 h_shared_map := OpenFileMapping ( FILE_MAP_READ or FILE_MAP_WRITE, TRUE, PChar (map_name));
 if 0 = h_shared_map then
    begin
     h_shared_map := CreateFileMapping ( 0, nil, PAGE_READWRITE or SEC_COMMIT, 0, BUFFERS_SZ, PChar (map_name) );
     shared_owner := TRUE;
    end;

 Assert ( 0 <> h_shared_map, 'Failed create file-mapping with name <' + map_name + '>:' + err2str );

 p_shared_vars := MapViewOfFile ( h_shared_map, FILE_MAP_READ or FILE_MAP_WRITE, 0, 0, BUFFERS_SZ );

 hist_ln := #13#10#9 + InfoFmt('[~d ~T] ') + ModuleName;

 if shared_owner then
   begin
    _SafeLog('#DBG: 1st create shared_vars for module ' + ModuleName + ', location = ' + FormatPtr (p_shared_vars) );
    p_shared_vars.views_opened := 1;
    p_shared_vars.perf_warn_ratio := 1;
    p_shared_vars.log_verbose_level := 3;
    p_shared_vars.local_dt := Now;
    p_shared_vars.log_th := nil;
    StrLCopy ( p_shared_vars.first_module, PChar (ModuleName), 260 );
    StrLCopy ( p_shared_vars.open_history, PChar ( hist_ln ), 260 );
   end
  else
   begin
    InterlockedIncrement ( p_shared_vars.views_opened );
    _SafeLog ('#DBG: opened shared_vars for module ' + ModuleName + ', location = ' + FormatPtr (p_shared_vars) );
    StrLCat ( p_shared_vars.open_history, PChar ( hist_ln ), High (p_shared_vars.open_history) );
    _SafeLog ( p_shared_vars.open_history );
   end;

end;

procedure init_Misc(md: TModuleDescriptor; rqs, flags: DWORD);
var
            s: String;
         hLib: THandle;
           pf: Pointer;
         _crt,
         _ext,
         _krt,
         _ust: FILETIME;
           st: TSystemTime;

begin

   // --------------------------------------------------------
   con_mutex := CreateMutex (nil, FALSE, PChar ('misc.con_mutex'));
   ExeFileName := ModuleFileName (0);
   ExePath := ExtractFilePath (ExeFileName);


   ODS_proc := LocalODS;
   FormatSettings.DecimalSeparator := '.';
   hLib := GetModuleHandle ('dbghelp.dll');
   if hLib = 0 then
      hLib := LoadLibrary ('dbghelp.dll');
   if hLib <> 0 then
      _UnDecorateSymbolName := GetProcAddress (hLib, 'UnDecorateSymbolName');


   if GetProcessTimes (GetCurrentProcess, _crt, _ext, _krt, _ust) then
     begin
      FileTimeToLocalFileTime (_crt, _krt);
      FileTimeToSystemTime (_krt, st);
      ps_creation_time := SystemTimeToDateTime (st);
     end;

   QueryPerformanceCounter (pctrStartup);

   dtStartup := Now;

   need_dt := TRUE;


   if (nil = sc_print) then
      begin
       sc_print := TSyncSection.Create();
       md.Globals.Add(sc_print);
      end;

   hLib := LoadLibrary ('ntdll.dll');
   if hLib > 0 then
      RtlNtStatusToDosError := GetProcAddress ( hLib, 'RtlNtStatusToDosError' );

   hLib := LoadLibrary ('kernel32.dll');
   if hLib > 0 then
      CaptureStackBackTrace := GetProcAddress ( hLib, 'RtlCaptureStackBackTrace' );

   gConfigPath := CorrectFilePath ( ExePath + '..\conf\' );

   PluginsPath := ExePath + 'plugins\';
   s := CorrectFilePath (ExePath + '..\plugins');
   if DirectoryExists (s) then
      PluginsPath := AddSlash (s);

   if ModuleIsLib then
     begin
      pf := @init_Misc;
      hLib := System.FindHInstance (pf);
      ModuleName := GetModuleName ( hLib );
      if hLib <> HInstance then
         OutputDebugString ( PChar ( Format('#WARN: HLib = $%x, HInstance = $%x', [hLib, HInstance] ) ) );

     end
   else
     begin
      ModuleName := GetModuleName ( HInstance );
     end;

   DLLPath := ExtractFilePath (ModuleName);
   ModuleName := ExtractFileName (ModuleName);

   hLib := GetModuleHandle ( ExeFileName );

   if hLib <> HInstance then
        begin
         ODS_proc := GetProcAddress (hLib, 'LocalODS');
         if Assigned (ODS_proc) then
           begin
            ODS ('[~T]. #DBG:~C0F Assigned LocalODS from module ~C0A' + ExeFileName + '~C07');
            need_dt := FALSE;
           end
         else
            ODS_proc := LocalODS;

        end;

   DLLPath := ExtractFilePath (DLLPath);
   dbg_present := IsDebuggerPresent;

   if need_dt then
      InitConsoleBuffers;

   ODS (' ');

   SetEnvironmentVariableA('FastMMLogFilePath', PAnsiChar( AnsiString(gLogPath)  ));
   // -------------------------------------------------------

end;


procedure finalize_Misc(md: TModuleDescriptor; rqs, flags: DWORD);
var
    dt: TAppLoggingThread;
     n: integer;

begin
   if con_enabled and con_visible then
      ODS('[~T]. #DBG(CheckPoint): finalize_Misc');
   ODS_proc := nil;
   if ( nil <> logging_thread ) then
    begin
     dt := logging_thread;
     p_shared_vars.log_th := nil;

     dt.AddMsg ('STOP', 0);
     WaitForSingleObject (dt.Handle, 500);
     dt.Terminate;
     if ( WaitForSingleObject ( dt.Handle, 1000)  = WAIT_TIMEOUT ) and
        ( dbgt_status >= 0 )  then
        begin
         ProcessMsg('#WARN: Debug thread - soft terminate waiting timeout.', '', 15);
         TerminateThread(dt.Handle, $C9);
        end;
    end;

   if con_owner then
                 CloseConsole;

   con_enabled := FALSE;
   CloseHandle (con_mutex);
   con_mutex := 0;

   for n := 0 to High ( TConsoleBuffers ) do
       gConBuffers^ [n].Release;
   gConBuffers := nil;


   InterlockedIncrement ( p_shared_vars.views_opened );
   UnmapViewOfFile ( p_shared_vars );
   CloseHandle ( h_shared_map );
   sc_print := nil;
end;

{ TMiscModuleDesc }
function OnModuleRqs (md: TModuleDescriptor; rqs, flags: DWORD): Boolean;
var
   i: Integer;
begin
 result := FALSE;
 case rqs of
      MRQ_INITIALIZE:  // ==================================================================================================== //
          begin
           result := (MST_INITIALIZED <> md.Status);
           if not result then exit;
           init_Misc (md, rqs, flags);

           exit;
          end;
      MRQ_FINALIZE: // ==================================================================================================== //
          begin
           for i := 0 to High ( gLogFiles ) do
                 CloseLogFile ( gLogFiles [i] );

           result := (logging_thread <> nil) or (sc_print <> nil);
           if not result then exit;
           finalize_Misc (md, rqs, flags);
          end;
     end; // case
 end; // OnModuleRqs


{ TSyncSection }

constructor TSyncSection.Create;
begin
 if CPUCount >= 2 then
    InitializeCriticalSectionAndSpinCount (FSection, 1000000)
 else
    InitializeCriticalSection (FSection);
end;

destructor TSyncSection.Destroy;
begin
 DeleteCriticalSection (FSection);
 inherited;
end;

function TSyncSection.TryLock(timeout: DWORD): Boolean;
var
   ts: Int64;
begin
 result := FALSE;

 ts := GetTickCount;
 try
   while not TryEnterCriticalSection (FSection) do
    begin
     if Abs ( GetTickCount - ts ) > timeout then exit;
     Sleep (1);
    end;
 except
  on E: Exception do
     OnExceptLog('TSyncSection.TryLock at ' + FormatPtr(self), E);
 end;

 result := TRUE;
end;

procedure TSyncSection.Unlock;
begin
 LeaveCriticalSection (FSection);
end;



{ TDataBlock }

constructor TDataBlock.Create(dwSize: DWORD);
begin
 Assert (dwSize <> 0, ClassName + '.Create (dwSize = 0)');
 FData := nil;
 FSize := dwSize;
 FData := AllocMem (FSize);
end;

destructor TDataBlock.Destroy;
begin
 FreeMem (FData);
 inherited;
end;


procedure TDataBlock.FillZero;
begin
 ZeroMemory (Data, Size);
end;

function TDataBlock.Relative(ofs: DWORD): Pointer;
begin
 Assert ( ofs < self.Size, 'TDataBlock.Relative: offset > size ' );
 result := RelativePtr ( FData, ofs );
end;

procedure TDataBlock.Resize(newSize: DWORD);
begin
 ReallocMem ( FData, newSize );
 FSize := newSize;
end;

{ TStringBuff }

constructor TStringBuff.Create;
begin
 GetMemoryManager (FMemMgr);
end;

destructor TStringBuff.Destroy;
begin
 _size := 0;
 inherited;
end;


function TStringBuff.GetStr: String;
begin
 if FData = nil then
    result := ''
 else
    result := FData;
end;

procedure TStringBuff.SetSize(const Value: Integer);
begin
 if Value <= 0 then
  begin
   if FData <> nil then
      MemMgr.FreeMem (FData);
   FData := nil;
  end
 else
  if FSize <> Value then
    begin
      if FData = nil then
         FData := MemMgr.AllocMem (Value)
      else
         MemMgr.ReallocMem (FData, FSize);
    end;
 FSize := Value;
 FLength := Min (FLength, FSize);
 // SetLength (FData, FSize);
end;

procedure TStringBuff.SetStr(const Value: String);
begin
 if Length(Value) > _size then
    _size := ( Length (Value) div 64 + 1 ) * 64;
 FLength := Length (value);
 if _length > 0 then
    SafeMove ( value[1], FData[0], _length * sizeof (CHAR) );
end;

{ TMsgSlot }

function TMsgSlot.GetStr: String;
begin
 SetString (result, PChar (@buff[0]), ccLen);
end;

procedure TMsgSlot.Load(const s: String);
const
     MAX_SIZE = 8192;

begin
 ccLen := Length(s);
 /// ======= resizing
 if ccLen > szBuff then
     SetSize ( ( ccLen shr 4  + 3 ) shl 4 );

 if (ccLen < MAX_SIZE) and (szBuff > MAX_SIZE) then
     SetSize (MAX_SIZE);

 if ccLen > 0 then
   begin
    SetStrWZ ( @buff[0], PChar(s), szBuff );
    // buff [ccLen] := #0;
   end;

end;

procedure TMsgSlot.SetSize(newSize: Integer);
begin
 if newSize = szBuff then exit;
 try
  if newSize <= 0 then
     FreeMem (buff)
  else
    if (szBuff > 0) and (newSize > 0) then
        ReallocMem (buff, newSize * sizeof (CHAR))
    else
        buff := AllocMem (newSize * sizeof (CHAR));
 except
    on E: Exception do
       OnExceptLog ('TMsgSlot.SetSize (from ' + IntToStr(szBuff) + ' to ' + IntToStr(newSize) + ' )', E);
 end;

 szBuff := newSize;
end;


procedure misc_SharedVars;
asm
 dd     0, 0, 0, 0, 0, 0, 0, 0
 dd     0, 0, 0, 0, 0, 0, 0, 0
 dd     0, 0, 0, 0, 0, 0, 0, 0
 dd     0, 0, 0, 0, 0, 0, 0, 0
 dd     0, 0, 0, 0, 0, 0, 0, 0
 dd     0, 0, 0, 0, 0, 0, 0, 0
 dd     0, 0, 0, 0, 0, 0, 0, 0
 int    3
end;


{ TConBuffer }

procedure TConBuffer.Init;
begin
 Index := AIndex;
 InterlockedIncrement ( users );
 if sc_share = nil then
    sc_share := TSyncSection.Create;

 if Index = 0 then Active := TRUE;
end;


procedure TConBuffer.Release;
begin
 if InterlockedDecrement ( users ) <= 0 then
  begin
   CloseHandle ( FHandle );
   FHandle := 0;
   sc_share.Free;
  end;

 inherited;
end;

function TConBuffer.AllocHandle: THandle;
var
   se: String;
   sz: _COORD;
   sr: TSmallRect;
    i: Integer;

begin
 result := INVALID_HANDLE_VALUE; // не применимо при отсутствии консоли
 FHandle := result;

 if GetConsoleWindow = 0 then exit;

 // sometime buffer cannot be created
 for i := 1 to 3 do
  begin
   SetLastError (0);

   result := CreateConsoleScreenBuffer ( GENERIC_READ or GENERIC_WRITE, 0, nil, CONSOLE_TEXTMODE_BUFFER, nil );

   if result = INVALID_HANDLE_VALUE then
     begin
      se := Format ( '#ERROR: CreateConsoleScreenBuffer[%d] failed, err: ', [index] );
      _SafeLog ( se + err2str );
      AllocConsole;
      _SafeLog ( 'Allocated console after fail.' );
      continue;
     end;


   se := Format ( ' #SUCCESS: CreateConsoleScreenBuffer[%d] returned $%x in module %s', [index, result, ModuleName] );
   if IsDebuggerPresent then
      __nop;

   _SafeLog ( se );
   break;
  end; // for


 sz.X := 200;
 sz.y := 900;
 SetConsoleScreenBufferSize ( result, sz );
 sr.Top := 0;
 sr.Left := 0;
 sr.Right := sz.x - 1;
 sr.Bottom := 79;
 SetConsoleWindowInfo ( result, TRUE, sr );

 FHandle := result;
end;

function TConBuffer.GetMakeHandle: THandle;
begin
 result := FHandle;
 if ( result > 0 ) and ( result <> INVALID_HANDLE_VALUE ) then exit;

 result := AllocHandle; // FHandle updates automatically
end;

procedure TConBuffer.SetActive(const Value: Boolean);
begin
 if ( Handle = INVALID_HANDLE_VALUE ) or ( Handle = 0 ) then exit;

 if Value then
   begin
    Assert ( FHandle > 0, 'TConBuffer.FHandle = ' + IntToStr(Handle) );
    SetStdHandle ( STD_OUTPUT_HANDLE, FHandle );
    if ( GetConsoleWindow = 0 ) then exit;
    SetConsoleActiveScreenBuffer ( FHandle );
    _SafeLog ( Format ( 'Active console buffer selected [%d], handle = $%x ', [index, FHandle] ) );
    FActive := Value;
   end;


end;


procedure TConBuffer.SyncWrite (const s: String; flags: DWORD);
begin
 Assert ( (Handle > 0) and (Handle <> INVALID_HANDLE_VALUE), 'TConBuffer.SyncWrite, FOutput = $' + IntToHex(FHandle, 4) );

 while not sc_share.TryLock(100) do;
 try
  ColorPrint ( s, flags and PMF_UNFMT = 0, FHandle );
 finally
  sc_share.Unlock;
 end;
end;


procedure InitConsoleBuffers;

var
   a, b, n: Integer;
begin
 a := sizeof ( p_shared_vars.con_buffers );
 b := sizeof ( TConsoleBuffers );

 Assert ( a >= b, Format ( 'No space for console buffers  %d < %d ', [a, b] ) );

 gConBuffers := @p_shared_vars.con_buffers;

 for n := 0 to High (TConsoleBuffers) do
          gConBuffers^ [n].Init (n);
end;


initialization
 gLogStream := LOG_STREAMS - 1;
 gThreadLogFlags := 0;
 BuildCRCTables;
 InitSharedVars;

 _SafeLog := DefLogDbg;
 GetStackBottom;
 main_thread := GetCurrentThreadID;

 {$IFDEF MAD_EXCEPT}
 madExcept.NameThread(main_thread, 'MainThread');
 {$ENDIF}

 {$IFDEF CPUX86}
 Exception.GetStackInfoStringProc := GetStackInfoStringProc;
 Exception.GetExceptionStackInfoProc := GetExceptionStackInfoProc;
 Exception.CleanUpStackInfoProc := CleanUpStackInfoProc;
 {$ENDIF}

 con_origin.X := Random (10) * 5 + 5;
 con_origin.Y := 10;

 lMDesc := RegModule ('misc', 'ModuleMgr', OnModuleRqs);
 InitializeModule ('misc');
finalization
 FinalizeModule ('misc');
 lMDesc := nil;
end.



