unit DateTimeTools;

interface
uses Windows, SysUtils, StrUtils, StrClasses, DateUtils, Perf, IPCUtils, Misc;

const
    MAX_STAMPS = 64;
    DEFAULT_TIMER = 1;
    SEC_IN_DAY  = 24 * 3600;
    MSEC_IN_DAY = SEC_IN_DAY * 1000;


    DT_ONE_HOUR    = 1.0 / 24;
    DT_ONE_MINUTE  = DT_ONE_HOUR / 60.0;
    DT_ONE_SECOND  = DT_ONE_MINUTE / 60.0;
    DT_ONE_MSEC    = DT_ONE_SECOND / 1000.0;
    DT_ONE_MCS     = DT_ONE_MSEC / 1000.0;
    DT_100_NS      = DT_ONE_MCS * 0.1;


    VTD_VERSION    = $010002;
    VTD_SLOTS      = 4;
    VTD_SLOTS_MASK = VTD_SLOTS - 1;

type

    PFormatSettings = ^TFormatSettings;

    TIME_DATA = packed record
     current: Int64;
     high_nv: DWORD;  // for sync exchange
    end;


   PTIME_DATA = ^TIME_DATA;


    TIntTime = Integer;  // relative TTimeStamp
    TIntDate = Integer;  // relative TTimeStamp
    TSeconds = Integer;

    TIME_FIELDS = packed record

     Year, Month, Day, Hour, Minute, Second, Milliseconds, Weekday: WORD;

     function     AsDateTime: TDateTime;
     procedure    Init ( src: TDateTime );
    end;

    PTIME_FIELDS = ^TIME_FIELDS;

    PTimeStamp = ^TTimeStamp;

    TSecondsRange = record
      stime, etime: TSeconds;
    end; // TSecondsRange

    TTimeRange = record
    public
     tmBegin, tmEnd: TDateTime;


     procedure GetTimeStamps ( var tsBegin, tsEnd: TTimeStamp );
     function  InRange ( dt: TDateTime = 0 ): Boolean;
     function  DayStart: TDateTime;
    end;

    PTimeRange = ^TTimeRange;

    TSmallBuff = array [0..31] of BYTE;

    TProfileTimer = class
    private
           FPFCRatio: Double;
        FIsReference: Boolean;
        FLastStarted: Integer;
     FEnableWarnings: Boolean;
       FAdjustedCoef: Double;
          FCorrector: Double;
          FFrequency: Int64;


     function GetStarted(index: Integer): DWORD; inline;
     procedure SetCoef(const Value: Double);
    protected
            FTriggers: array [1..MAX_STAMPS] of Double;
        FTimerStarted: array [1..MAX_STAMPS] of Int64; // in PFC-stamps
     FCPUTimerStarted: array [1..MAX_STAMPS] of  Double;
       FClocksStarted: array [1..MAX_STAMPS] of Double; // RDTSC
             FStarted: array [1..MAX_STAMPS] of DWORD;



     FCoef: Double;
     // tmp: array [0..63] of BYTE;



     function           AlignedPtr (pbuff: Pointer; mask: NativeUInt = $000F): PLargeInteger; inline;


    public


     Complex: array [1..MAX_STAMPS] of Double;
     Names: array [1..MAX_STAMPS] of String;



     { props }

     property           AdjustedCoef: Double read FAdjustedCoef;
     property           Coef: Double read FCoef write SetCoef;
     property           EnableWarnings: Boolean read FEnableWarnings write FEnableWarnings;
     property           IsReference: Boolean read FIsReference;
     property           PFCRatio: Double read FPFCRatio write FPFCRatio;
     property           Started [index: Integer]: DWORD read GetStarted;

     { C & D }
     constructor        Create;
     destructor         Destroy; override;

     { Methods }
     function           CheckTrigger (nStamp: Integer): Boolean;
     function           CPUElapsed (nStamp: Integer = DEFAULT_TIMER): Double;
     function           ClocksElapsed (nStamp: Integer = DEFAULT_TIMER): Double;
     function           Diff (nStamp1, nStamp2: Integer): Double;
     function           Elapsed (nStamp: Integer = DEFAULT_TIMER; bRestart: Boolean = FALSE): Double;
     function           TimerStarted (nStamp: Integer = DEFAULT_TIMER): Int64; inline;

     function           GetTimeStamp: Int64; inline;
     function           GetClocksStamp: Double;
     function           GetCPUTimeStamp: Double; inline;
     function           IsStarted (nStamp: Integer): Boolean;

     procedure          Next (const sID: String = '');

     procedure          SetTriggerTime(nStamp, nDelay: Integer);
     procedure          Start (nStampSet: Int64 = 1);
     procedure          StartOne (nStamp: Integer = DEFAULT_TIMER; flags: DWORD = $F);
     procedure          StartPFC (nStamp: Integer = DEFAULT_TIMER);
     procedure          Stop (nStamp: Integer);
     function           UpdateCalibration ( bForce: Boolean ): Int64;
    end; // TProfileTimer


     TProfStat = class
     public
      chk_count: Integer;
      mask: DWORD;
      ftime, ctime: Double;

      procedure         Add(pst: TProfStat);
      procedure         Reset;
      procedure         Update (pt: TProfileTimer);
     end; // TProfStat


     TProfStatList = class(TStrMap)
     protected
      function          GetStat(const sName: String): TProfStat;
      function          GetStatByIndex(nIndex: Integer): TProfStat;
     public
      property          Items[const sIndex: String]: TProfStat read GetStat;
      property          Stats[index: Integer]: TProfStat read GetStatByIndex;
      { C & D }
      constructor       Create(AOwner: TObject);
      { Methods }
      function          AddStat(const sName: String): TProfStat;
      procedure         ResetAll;
     end; // TProfStatList


     TTimeSource = class
     public
      function       GetTime ( bSilent: Boolean = FALSE ): TDateTime; virtual;
     end;

     PVirtualTimerRecord = ^TVirtualTimerRecord;

     TVirtualTimerRecord = packed record

     // --- ver. $10001 fields --- //
          base: TDateTime;  // 00
      pfc_coef: Double;     // 08: PFC-diff to TDateTime diff (original)
      pfc_base: Int64;      // 10: PFC value for base time moment
      pfc_last: Int64;      // 18
      pfc_freq: Int64;      // 20
      pfc_corr: Double;     // 28: correction for system freq
       dt_last: TDateTime;  // 30
       upd_cnt: Integer;    // 38
       syn_cnt: Integer;    // 3C
       rw_lock: Integer;    // 40: for multithread access
       clients: Integer;    // 44
     clock_res: Integer;    // 48: system timer resolution
     clock_adj: Integer;    // 4C: system timer adjust value
       version: Integer;    // 50
       pfc_dvg: Single;     // 54: last sync divergens in ms, valid for PFC
     clock_dvg: Single;     // 58
       pfc_syn: Integer;    // 5C: sync pfc in milliseconds from midnight
     clock_syn: Integer;    // 60: sunc clock in milliseconds from midnight
     aprox_pfc: Single;     // 64: approximation factor for TMService
      slot_idx: Integer;    // 68: slot position
         crc32: DWORD;      // 6C
     function   Hash: DWORD;
    end;


    TVTDSlots = packed array [0..VTD_SLOTS - 1] of TVirtualTimerRecord;
    PVTDSlots = ^TVTDSlots;




    TVirtualTimerData = packed record
         slots: TVTDSlots;
        reserv: array [0..4095 - sizeof (TVirtualTimerRecord) * 4 - 12] of BYTE;
        active: Integer;
       sw_lock: Integer;
       data_sz: DWORD;


       function   CloneActive ( next: Integer ): PVirtualTimerRecord;

       function   Fork: TVirtualTimerRecord;

       function   GetSlot (index: Integer): PVirtualTimerRecord; inline;


       procedure  Lock (value: Integer); // block active exchange
       procedure  Unlock; inline;

     end;

     PVirtualTimerData = ^TVirtualTimerData;


     TVirtualTimer = class (TTimeSource)
     private
      FDiffTimer: TProfileTimer;
      FSyncPort: PVirtualTimerData;

      FOwnRights: Boolean;

      max_no_sync: Double;

      FMapping: TFileMapping;

     protected

      property SyncPort: PVirtualTimerData read FSyncPort;

      function  SwitchSlot ( upd, curr: Integer ): Boolean;

     public
      { props }

      property DiffTimer: TProfileTimer read FDiffTimer;

      property OwnRights: Boolean read FOwnRights;

      { C & D }
      constructor       Create;
      destructor        Destroy; override;

      { methods }

      function          ActiveSlot: PVirtualTimerRecord; inline;
      function          NextSlot ( i: Integer ): Integer; inline;

      function          GetTime ( bSilent: Boolean = TRUE ): TDateTime; override;
      function          Ready: Boolean;
      function          SyncNow ( bCalibrate: Boolean ): Boolean;
      function          SyncWith ( src: TProfileTimer; idx: Integer; base: TDateTime ): Boolean;
      procedure         TestSync;
      function          Update ( const src: TVirtualTimerRecord ): Boolean;
     end;


     TTimeDeviator = class
      private
       FOriginalTime: PTIME_DATA;
       FTimeBase1600: PTIME_DATA;  // around 1600 year
       FGetLocalTime: PPointer;
      FGetSystemTime: PPointer;
     FGetSysTimeAsFT: PPointer;
           FFakeTime: TIME_DATA;
                buff: array [0..31] of BYTE;

        function    GetDateTime: TDateTime;
        procedure   SetDateTime (const Value: TDateTime);
        function    Convert2DT (src: TIME_DATA): TDateTime;

        procedure   PutPtr ( ofs: DWORD; p: NativeUInt );
       function     DoSinglePatch(pwp, ptm: NativeUInt): Boolean;
      protected


      public

       property        DateTime: TDateTime read GetDateTime write SetDateTime;



       { C & D }
       constructor     Create;


       { methods }
       function        Patch ( bPath: Boolean = TRUE ): Boolean;

       function        SysDateTime: TDateTime;

     end;


procedure PatchQPF;
function  CalcVT ( base: TDateTime; elapsed: Double ): TDateTime; inline;
function  CalcTimeDiff (a: Integer; b: Integer = -1): Integer;
function  DiffTS (a, b: TTimeStamp): Int64;

function  CurrentTime: TTimeStamp; inline;
function  CurrentDateTime (psrc: PFileTime = nil): TDateTime;
function  EncodeIntDate (yy, mm, dd: WORD): TIntDate;

function  FileModifiedAt (const sFileName: String; UTC: Boolean = TRUE): TDateTime;


function  FormatDate(const sFormat: String; nDate: TIntDate): String;
function  FormatTime(const sFormat: String; nTime: TIntTime): String;
function  FormatTS (const sFormat: String; ts: TTimeStamp): String;


function  StrDateDecode (sDate: String; fs: PFormatSettings = nil): TTimeStamp;
function  StrTimeDecode (sTime: String; fs: PFormatSettings = nil): TTimeStamp;
function  StrDateTimeDecode (const sDateTime: String; fs: PFormatSettings = nil): TTimeStamp; overload;
function  StrDateTimeDecode (const sDate, sTime: String; fs: PFormatSettings = nil): TTimeStamp; overload;
function  DT2Seconds (dt: TDateTime): TSeconds;
function  MSeconds2DT (msec: DWORD): TDateTime;

function  TimeToSec(hh, mm: Integer; ss: Integer = 0): TSeconds;
function  TimeStamp (nDate: TIntDate; nTime: TIntTime): TTimeStamp;
function  TimeStamp2DT (nDate: TIntDate; nTime: TIntTime): TDateTime; inline;
function  TimeStampDiff (const a, b: TTimeStamp): Int64;
function  MonthOfTS (nDate: TIntDate): Integer;

function  TimeStampToSysTime (nDate, nTime: Integer): TSystemTime;

{ EasyLanguage conversion routines }
function  ELDateToDateTime (ELDate: Integer): TDateTime;
function  ELDateToTSDate   (ELDate: Integer): TIntDate;
function  ELTimeToSeconds  (ELTime: Integer): TSeconds;


function TimeLine (dwDelayMsec: DWORD): TDateTime;

function TickAdjust (times: Integer = 1): Int64;
function PreciseTime: TDateTime;

function DailyLimitMS (msec: Integer): Integer;

// для работы с системным временем в 100-нс юнитах
function RtlTimeToTimeFields ( const tm: LARGE_INTEGER; var ptf: TIME_FIELDS ): Boolean; stdcall;
function RtlTimeFieldsToTime ( const tf: TIME_FIELDS; var tm: LARGE_INTEGER ): Boolean; stdcall;

function FT64 ( const ft: TFileTime ): Int64; inline;
function FileTimeToDateTime ( const ft: TFileTime ): TDateTime;


procedure SuperLocalTime ( var lpSystemTime: TSystemTime ); stdcall;



function SetTimerResolution( DesRes: DWORD; bSetRes: Boolean; var ActRes: DWORD ): NTSTATUS;
function DateTimeToUnixTime32 (DelphiDate: TDate): LongInt;  // seconds
function DateTimeToUnixTime64 (DelphiDate: TDate): UInt64;   // microseconds
function UnixTimeToDateTime(AUnixTime: DWord; ABias: Integer): TDateTime;
function GetLocalTimeBias: Integer;

function UTCDateTime: TDateTime;

var

   ps_start_time: TDateTime = 0;
   fake_pfc_corr: Double = 0;
       zero_freq: Boolean = FALSE;
      dt_context: Integer;
         lt_func: Pointer = @SuperLocalTime;

        RtlQueryPerformanceCounter: function ( var ct: TLargeInteger ): LongBool; stdcall = nil;
        RtlQueryPerformanceFrequency: function  (  var freq: TLargeInteger ): LongBool; stdcall = nil;

   g_pfc_value: Int64 = 0;
   g_timer: TVirtualTimer = nil;

implementation /// <<<<<<<<<<<<<<<<<<<<<<============================================================================================================================================
uses WinApi.AclAPI, WinAPI.AccCtrl, Math;

var
   slmap: TStrMap;
   _NtSetTimerResolution: function ( DesRes: DWORD; bSetRes: Boolean; var ActRes: DWORD ): NTSTATUS; stdcall;

//q   dd: Int64;



function RtlTimeToTimeFields ( const tm: LARGE_INTEGER; var ptf: TIME_FIELDS ): Boolean; stdcall; external 'ntdll.dll';
function RtlTimeFieldsToTime ( const tf: TIME_FIELDS; var tm: LARGE_INTEGER ): Boolean; stdcall; external 'ntdll.dll';
// function  RtlQueryPerformanceCounter ( freq: PLargeInteger ): Boolean; stdcall; external 'ntdll.dll';
// function  RtlQueryPerformanceFrequency  ( freq: PLargeInteger ): Boolean; stdcall; external 'ntdll.dll';

function GetLocalTimeBias: Integer;
var
   timeBias: _TIME_ZONE_INFORMATION;
begin
   FillChar ( timeBias, sizeof(timeBias), 0 );
   GetTimeZoneInformation (timeBias);
   Result := timeBias.Bias;
   if timeBias.DaylightDate.wMonth > 0 then
      Inc ( Result, timeBias.DaylightBias );
end;


procedure InitRoutines;
var
   p: Pointer;
   m: HMODULE;
begin
 m := GetModuleHandle ('ntdll.dll');

 _NtSetTimerResolution := GetProcAddress ( m, 'NtSetTimerResolution' );

 p := GetProcAddress ( m, 'RtlQueryPerformanceCounter');
 if Assigned (p) then
    RtlQueryPerformanceCounter := p
 else
    RtlQueryPerformanceCounter := QueryPerformanceCounter;

 p := GetProcAddress ( m, 'RtlQueryPerformanceFrequency');
 if Assigned (p) then
    RtlQueryPerformanceFrequency := p
 else
    RtlQueryPerformanceFrequency := QueryPerformanceFrequency;
end;

{Convert Unix time to TDatetime}
const
      SecPerMin  = 60;
      SecPerHour = SecPerMin * 60;
      SecPerDay  = SecPerHour * 24;
 MicrosecPerSec = 1e6;

function UTCDateTime: TDateTime;
begin
 result := PreciseTime - DT_ONE_MINUTE * GetLocalTimeBias;
end;

function UnixTimeToDateTime(AUnixTime: DWord; ABias: Integer): TDateTime;

const
  UnixDateDelta = 25569; { 1970-01-01T00:00:00,0 }
  MinDayFraction = 1 / (24 * 60);


begin
  Result := UnixDateDelta + (AUnixTime div SecPerDay) { Days }
  + ((AUnixTime mod SecPerDay) / SecPerDay) { Seconds }
  - ABias * MinDayFraction { Bias to UTC in minutes };
end;

function DateTimeToUnixTime32 (DelphiDate : TDate) : LongInt;
begin
    Result := Trunc( DateTimeToUnixTime64 (DelphiDate) / MicrosecPerSec );
end;

function DateTimeToUnixTime64 (DelphiDate : TDate): UInt64;
const
         Offset1970 = 25569;
begin
    Result := Trunc ( (DelphiDate - Offset1970) * SecPerDay * MicrosecPerSec );
end;


function CalcVT ( base: TDateTime; elapsed: Double ): TDateTime;
begin
 result := base + elapsed * DT_ONE_MSEC;
end;


procedure SuperLocalTime ( var lpSystemTime: TSystemTime ); stdcall;
var
   dt: TDateTime;
begin
 dt := PreciseTime ();
 DateTimeToSystemTime ( dt, lpSystemTime );
end;


function SetTimerResolution ( DesRes: DWORD; bSetRes: Boolean; var ActRes: DWORD ): NTSTATUS;
begin
 result := 0;
 if Assigned (_NtSetTimerResolution) then
    result := _NtSetTimerResolution ( DesRes, bSetRes, ActRes );
end;

function  DiffTS (a, b: TTimeStamp): Int64;
begin
 result := Int64 (a.Date - b.Date) * MSEC_IN_DAY + (a.Time - b.Time);
end;


function DailyLimitMS (msec: Integer): Integer;
const
    LIMIT_MS = MSEC_IN_DAY - 1;

begin
 if msec < 0 then msec := 0;
 if msec > LIMIT_MS then msec := LIMIT_MS;

 result := msec;
end;

function  FileModifiedAt (const sFileName: String; UTC: Boolean): TDateTime;
var
    h: THandle;
   lw: TFileTime;
   st: TSystemTime;
begin
 result := 0;

 if FileExists (sFileName) then
  begin
   h := CreateFile ( PChar(sFileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_ALWAYS, 0, 0);
   if h = INVALID_HANDLE_VALUE then exit;
   if GetFileTime (h, nil, nil, @lw) then
     begin
      if not UTC then
        FileTimeToLocalFileTime (lw, lw);
      FileTimeToSystemTime (@lw, st);
      result := SystemTimeToDateTime (st);
    end; // if get
   CloseHandle(h);
  end;
end;

function  TimeStampToSysTime (nDate, nTime: Integer): TSystemTime;
var
   dt: TDateTime;
begin
 dt := TimeStampToDateTime ( TimeStamp (nDate, nTime) );
 DateTimeToSystemTime (dt, result);
end;

function TickAdjust (times: Integer = 1): Int64;
var
   st, ref: TFileTime;
   ts: Int64;
begin
 // using 100% one CPU core usage, while time not ticks
 Sleep(1); // skip quant
 result := 0;
 ts := GetTickCount;
 repeat
  GetSystemTimeAsFileTime (ref);
  // GetLocalTime (ref);
  repeat
   {$IFDEF X64}
   for n := 1 to 100 do
     asm
      nop
      nop
      nop
      pause // Intel HT pause
      pause
      pause
      pause
     end;
   {$ENDIF}

   GetSystemTimeAsFileTime (st);
   // GetLocalTime (st);
   Inc (result);
   if ( result and $FFFF = 0 ) and ( GetTickCount - ts > times * 20 ) then break; // fail
  until ( st.dwLowDateTime <> ref.dwLowDateTime ) or ( result > 10000000 );
  Dec (times);
 until times <= 0;
end;

function  CalcTimeDiff (a: Integer; b: Integer = -1): Integer;
begin
 if b < 0 then b := CurrentTime.Time;
 result := 0;
 if a < b then result := b - a;
end;

function TimeLine (dwDelayMsec: DWORD): TDateTime;

begin
 result := dwDelayMsec * 1.0;
 result := result / MSEC_IN_DAY;
 result := result + Now;
end;

function  EncodeIntDate (yy, mm, dd: WORD): TIntDate;
begin
 result := DateTimeToTimeStamp ( EncodeDate (yy, mm, dd) ).Date;
end;

function  ELDateToDateTime(ELDate: Integer): TDateTime;
var dd, mm, yy: Integer;
begin
 dd :=    (ELDate mod 100);
 ELDate := ELDate div 100;
 mm :=     ELDate mod 100;
 ELDate := ELDate div 100;
 yy :=     ELDate + 1900;
 result := EncodeDateTime (yy, mm, dd, 0, 0, 0, 0);
end; // ELDateToDateTime

function  ELDateToTSDate(ELDate: Integer): TIntDate;
begin
 result := DateTimeToTimeStamp ( ELDateToDateTime(ELDate) ).Date;
end; // ELDateToTSDate

function  ELTimeToSeconds(ELTime: Integer): TSeconds;
begin
 result := TimeToSec(ELTime div 100, ELTime mod 100);
end; // ELTimeToSeconds

function  TimeToSec(hh, mm, ss: Integer): TSeconds;
begin
 result := hh * 3600 + mm * 60 + ss;
end;

function  DT2Seconds (dt: TDateTime): TSeconds;
begin
 result := DateTimeToTimeStamp (dt).Time div 1000;
end; // DT2Seconds

function  MSeconds2DT (msec: DWORD): TDateTime;
var
   hh, mm, ss, ms: DWORD;
begin
 ms := msec mod 1000;
 msec := msec div 1000;
 ss := msec mod 60;
 msec := msec div 60;
 mm := msec mod 60;
 hh := msec div 60;
 result := EncodeTime(hh, mm, ss, ms);
end;


function  MonthOfTS (nDate: TIntDate): Integer;
begin
 result := MonthOf (TimeStamp2DT(nDate, 0));
end;

function  CurrentTime: TTimeStamp;
var
   st: TSystemTime;
begin
 GetLocalTime (st);
 result := DateTimeToTimeStamp ( SystemTimeToDateTime (st) );
end; // CurrentTime


function FT64 ( const ft: TFileTime ): Int64; inline;
var
   li: LARGE_INTEGER;
begin
 li.LowPart := ft.dwLowDateTime;
 li.HighPart := ft.dwHighDateTime;
 result := li.QuadPart;
end;


function FileTimeToDateTime ( const ft: TFileTime ): TDateTime;
var
   rest: Double;
    dbt: TFileTime;
    tmv: Int64;
     st: TSystemTime;

begin
 FileTimeToSystemTime ( ft, st ); // потеря точности!

 result := SystemTimeToDateTime ( st );

 st.wHour := 0;
 st.wMinute := 0;
 st.wSecond := 0;
 st.wMilliseconds := 0;

 SystemTimeToFileTime ( st, dbt ); // время на начало дня

 // восстановление точного времени (точнее быть не может! теоретически...)

 tmv := FT64 (ft) - FT64 (dbt); // количество юнитов по 100-нс, или 1/10000 мсек или 1/10 микросекунды
 rest := DT_100_NS * tmv;
 result := Int (result) + rest;
end;


function  CurrentDateTime (psrc: PFileTime = nil): TDateTime;
var
   ft, lft: TFileTime;
begin

 if psrc <> nil then
    lft := psrc^
 else
   begin
    GetSystemTimeAsFileTime ( ft );
    FileTimeToLocalFileTime ( ft, lft );
   end;

 result := FileTimeToDateTime ( lft );
end;

function  FormatDate(const sFormat: String; nDate: TIntDate): String;
begin
 if nDate > 0 then
    result := FormatDateTime ( sFormat, TimeStamp2DT (nDate, 0))
 else
    PrintError ('FormatDate: Wrong nDate param = ' + IntToStr (nDate));
end;

function  FormatTime(const sFormat: String; nTime: TIntTime): String;

var
   days: Integer;
begin
 if nTime < 0 then
   result := 'Bad time stamp: ' + IntToStr(nTime)
 else
  try

   days := 0;

   while nTime > MSEC_IN_DAY do
     begin
      Dec (nTime, MSEC_IN_DAY);
      Inc (days);
     end;


   result := '';
   if days > 0 then
      result := '+' + IntToStr(days) + 'd ';


   result := result + FormatDateTime ( AnsiReplaceStr(sFormat, 'mm', 'nn'), TimeStamp2DT (CurrentTime.Date, nTime));
  except
   on E: EConvertError do
     result := 'Convert error: ' + E.Message;

  end;
end;


function  FormatTS (const sFormat: String; ts: TTimeStamp): String;
var
   dt: TDateTIme;
begin
 try
  Assert (ts.Date < 2000000, 'Wrong ts.date = ' + IntToStr(ts.Date));
  Assert (ts.Time < MSEC_IN_DAY, 'Wrong ts.time = ' + IntToStr(ts.Time));

  if ts.Date = 0 then
     ts.Date := CurrentTime.Date;

  dt := TimeStampToDateTime (ts);
  result := FormatDateTime (sFormat, dt);
 except
  on E: Exception do
      OnExceptLog ('ts = ' + IntToStr(ts.Date) + '.' + IntToStr(ts.Time), E);
 end;
end;

function  StrDateDecode (sDate: String; fs: PFormatSettings = nil): TTimeStamp;
{
        packed record
        case BYTE of
         0: (dd, mm, yy: DWORD);
         1: (dv:
}
var
   dt: TDateTime;
   s: AnsiString;
   dr, ds: array [0..3] of DWORD;
   df, dbg: String;


   i, nv, l: Integer;
   ch: Integer;
begin
 if fs = nil then fs := @FormatSettings;

 sDate := Trim (sDate);
 dt := 0;
 l := Length(sDate);



 if ( l <= 8 ) and ( Pos( fs.DateSeparator, sDate) = 0 ) then
  begin
   Insert( fs.DateSeparator, sDate, Length(sDate) - 4 ); // mm.dd
   Insert( fs.DateSeparator, sDate, 3 ); // dd.
  end;

 dbg := 'nope';

 if l < 12 then
 try
  // dv = dd.mm.yy[yy]
  nv := 0;
  s := AnsiString (sDate);

  dr[0] := 0;
  dr[1] := 0;
  dr[2] := 0;


  for i := 1 to l do
   begin
    ch := BYTE ( s[i] );
    if ch in [$30..$39] then
        dr [nv] := dr [nv] * 10 + DWORD (ch - $30)
    else
      begin
       Inc (nv);
       if nv > 3 then break;
      end;

   end;

  ds := dr;


  nv := 0;
  df := LowerCase (fs.ShortDateFormat);

  // mapping
  for i := 1 to Length (df) do
      case df [i] of
        'd': ds[nv] := dr[0];
        'm': ds[nv] := dr[1];
        'y': ds[nv] := dr[2];
        else
          begin
           Inc (nv);
           if nv = 3 then break;
          end;
      end; // case


  if (ds [2] > 1000 ) and ( ds[2] < 1970 ) then ds [2] := 1970; // minimal time stamp
  if ds[2] < 100 then Inc (ds [2], 2000);

  dbg := Format ('%d/%d/%d', [ds[2], ds[1], ds[0]]);

  dt := EncodeDate(ds[2], ds[1], ds[0]); // yy, mm, dd

 except
  on E: Exception do

    OnExceptLog ('StrDateDecode for string "' + sDate + '", ctx = ' + IntToStr (dt_context) + ', dbg = ' + dbg, E);
 end;

 result := DateTimeToTimeStamp ( dt );

 Assert ( dt < 120000, 'Wrong date result, from ' + sDate + ' = ' + ftow(dt, '%.0f') + ', dbg = ' + dbg);
end;


function StrTimeDecode;
const
   pts_tm6 = 'h.m.s~~~~~~~~~~';
   pts_tmL = 'hh.mm.ss.zzz~~~';


var
    i, v, c, ls: Integer;
    bs: array [0..15] of Byte;
    tm: array [0..3] of Integer;
    pt: PChar;
    //  hh, mm, ss, ms: String;

begin
  if fs = nil then
     fs := @FormatSettings;
  result.Date := 0;
  result.Time := 0;

  ls := Length (sTime);


  if ( ls <= 5 ) then
     begin
      // append seconds
      if ( Pos( fs.TimeSeparator, sTime) > 0 ) then
          begin
           sTime := sTime + fs.TimeSeparator + '00';
           Inc (ls, 3);
          end
      else
          begin
           sTime := sTime + '00';
           Inc (ls, 2);
          end;
     end;



  if ls > 12 then exit;

  if sTime[1] > '9' then exit;

  if Pos( fs.TimeSeparator, sTime) = 2 then
     sTime := '0' + sTime;



  FillChar (bs, sizeof(bs), 0);
  FillChar (tm, sizeof(tm), 0);

  SafeMove ( AnsiString(sTime)[1], bs, ls );


  if ls = 6 then
     pt := @pts_tm6[1]
  else
     pt := @pts_tmL[1];

  c := 0;

  for i := 0 to ls - 1 do
   begin
    v := bs [i] - $30;

    if v in [0..9] then
       tm[c] := tm[c] * 10 + v;

    if pt [i] = '.' then
      begin
       Inc (c);
       if c = 4 then break;
      end;
   end;


  result.Time := tm[0] * 3600000 + tm[1] * 60000 + tm[2] * 1000 + tm[3];

  Assert (result.Time < MSEC_IN_DAY, 'StrTimeDecode = ' + IntToStr(result.Time) + ' from ' + sTime );

  if sTime = '' then exit;

end; // StrTimeDecode

function  StrDateTimeDecode (const sDate, sTime: String; fs: PFormatSettings): TTimeStamp;
begin
 result.date := StrDateDecode (sDate, fs).date;
 result.Time := StrTimeDecode (sTime, fs).Time;
end;

function StrDateTimeDecode (const sDateTime: String; fs: PFormatSettings): TTimeStamp; overload;
var
   sd, st: String;
begin
 st := HideSP (sDateTime);
 sd := StrTok(st, ['^']);
 if st <> '' then
    result := StrDateTimeDecode (sd, st, fs)
 else
    begin
     result := StrTimeDecode(sd, fs);
     if result.Time > CurrentTime.Time then
        result.Date := CurrentTime.Date - 1
     else
        result.Date := CurrentTime.Date; // текущая дата, что ещё придумать?
    end;
end; // StrDateTimeDecode

function  TimeStamp(nDate: TIntDate; nTime: TIntTime): TTimeStamp;
begin
 result.Date := nDate;
 result.Time := nTime;
end; // TimeStamp

function  TimeStamp2DT;
begin
 try
  result := TimeStampToDateTime (TimeStamp (nDate, nTime));
 except
  on E: EConvertError do
    begin
     PrintError ( Format('#EXCEPTION EConvertError catched in TimeStamp2DT(%d, %d)', [nDate, nTime]) );
     result := Now;
    end;
 end;
end;

function  TimeStampDiff (const a, b: TTimeStamp): Int64;
begin
 result := a.Date - b.Date;
 result := result * MSEC_IN_DAY;
 result := result + a.Time - b.Time;
end;

function RDTSC() : int64;
asm rdtsc end;

{ TProfileTimer }

function TProfileTimer.AlignedPtr(pbuff: Pointer; mask: NativeUInt): PLargeInteger;
begin
 result := pbuff;
 while ( NativeUInt (result) and mask <> 0 ) do
          NativeUInt (result) := NativeUInt (result) + 1;
end; // AlignedPtr

function TProfileTimer.CheckTrigger(nStamp: Integer): Boolean;
begin
 result := Elapsed (nStamp) > FTriggers [nStamp];
end;

function TProfileTimer.ClocksElapsed (nStamp: Integer): Double;
begin
 result := GetClocksStamp - FClocksStarted [nStamp];
end;

function TProfileTimer.CPUElapsed (nStamp: Integer): Double;
begin
 result :=  GetCPUTimeStamp - FCPUTimerStarted [nStamp];
end;

constructor TProfileTimer.Create;
begin
 PFCRatio := 1;
 try
  Assert( Assigned(self), 'TProfileTimer.Create self = nil');
  UpdateCalibration ( FALSE );
  EnableWarnings := TRUE;
  FCorrector := 0;
  Start ($1); // only one started
 except
  on E: Exception do
     OnExceptLog ('TProfileTimer.Create, self = ' + FormatPtr (self), E);

 end;
end;


destructor TProfileTimer.Destroy;
var
   n: Integer;
begin
  for n := 1 to MAX_STAMPS do Names [n] := '';
  inherited;
end;

function TProfileTimer.Diff(nStamp1, nStamp2: Integer): Double;
begin
 result := ( FTimerStarted [nStamp2] - FTimerStarted [nStamp1] );
 result := result * AdjustedCoef;

end;

function   TProfileTimer.UpdateCalibration ( bForce: Boolean ): Int64;
var
   PVC: PLargeInteger;
   buff: TSmallBuff;
   cnt: Integer;
    pr: PVirtualTimerRecord;
begin
 cnt := 0;
 if ( not bForce ) and ( g_timer <> nil ) and ( g_timer.ActiveSlot <> nil ) and ( g_timer.ActiveSlot.pfc_coef > 0 ) then
    try
     pr := g_timer.ActiveSlot;
     result := pr.pfc_freq;
     FFrequency := result;
     FPFCRatio := pr.pfc_corr;
     Coef := pr.pfc_coef;
     g_pfc_coef := FCoef;
     exit;
    except
     on E: Exception do
        OnExceptLog ('TProfileTimer.UpdateCalibration', E);
    end;

 PVC := AlignedPtr (@buff);
 PVC^ := 0;


 if not Assigned(RtlQueryPerformanceFrequency) then
    InitRoutines;



   Repeat
    RtlQueryPerformanceFrequency(PVC^);        // сколько тактов в секунду
    // 1 = TimeStamp / PV
    // C * TimeStamp = S
    FFrequency := PVC^; // multiply adapted
    if FFrequency  > 0 then
       Coef := 1000.0 / FFrequency;
    Inc (cnt);

   Until (Coef > 0) and (Coef < 1) or (cnt > 10);

 result := PVC^;
end;


function TProfileTimer.Elapsed (nStamp: Integer; bRestart: Boolean) : Double;
var
   ts: Int64;

begin
 // Assert ( self <> nil, ' self = nil ' );

 if nStamp < 32 then
    ts := GetTimeStamp // cmp ~= 3 ns
 else
    ts := g_pfc_value;

 result := Max ( 0, ts - FTimerStarted [nStamp] - FCorrector );
 result := result * AdjustedCoef; //
 if bRestart then
    StartOne (nStamp);
end;

function TProfileTimer.GetClocksStamp: Double;
begin
 result := RDTSC() / 1e6;
end;

function TProfileTimer.GetCPUTimeStamp: Double;
var
   ct, et, kt, ut: FILETIME;
   kti: Int64 absolute kt;
   uti: Int64 absolute ut;
begin
 GetThreadTimes (GetCurrentThread, ct, et, kt, ut);
 result := 1.0 * (kti + uti) / 10000;
end;

function TProfileTimer.GetStarted(index: Integer): DWORD;
begin
 if ( index >= Low (FStarted) ) and ( index <= High (FStarted) ) then
      result := FStarted [index]
 else
      result := 0;
end;

function TProfileTimer.GetTimeStamp: Int64;
var
   PV: PLargeInteger;
   buff: TSmallBuff;
begin
 PV := AlignedPtr (@buff);
 result := -1;
 try
  RtlQueryPerformanceCounter (PV^);
  g_pfc_value := PV^;
  result := (PV^);
 except
  on E: Exception do
     OnExceptLog ( ClassName + '.GetTimeStamp, QPC = ' + FormatPtr (@RtlQueryPerformanceCounter), E );
 end;

end;

function TProfileTimer.IsStarted(nStamp: Integer): Boolean;
begin
 result := Started [nStamp] <> 0;
end;


procedure TProfileTimer.Next;
begin
 Stop (FLastStarted);
 if sID <> '' then
    Names [FLastStarted] := sID
 else
    Names [FLastStarted] := IntToStr(FLastStarted);

 if FLastStarted < MAX_STAMPS then
    StartOne (FLastStarted + 1);
end;

procedure TProfileTimer.SetCoef(const Value: Double);
begin
 FCoef := Value;
 FAdjustedCoef := FCoef * PFCRatio;
end;

procedure TProfileTimer.SetTriggerTime(nStamp, nDelay: Integer);
begin
 FTriggers [nStamp] := nDelay;
end;

procedure TProfileTimer.Start(nStampSet: Int64);
var
   msk: Int64;
     n: Integer;
begin
 msk := 1;

 for n := 1 to High (FTimerStarted) do
  begin
   if nStampSet and msk <> 0 then StartOne (n);
   msk := msk shl 1;
  end;
end;

procedure TProfileTimer.StartOne (nStamp: Integer; flags: DWORD);
begin
 Assert ( Assigned (self), 'StartOne: self = nil ');
 Assert ( nStamp <= High (FTimerStarted), 'Bad timer num ' + IntToStr(nStamp));


 try
   if flags and $01 > 0 then
      StartPFC (nStamp);

   if flags and $02 > 0 then
      FCPUTimerStarted [nStamp] := GetCPUTimeStamp;

   if flags and $04 > 0 then
      FClocksStarted   [nStamp] := GetClocksStamp;


   FStarted [nStamp] := flags;
   FLastStarted := nStamp;
 except
  on E: Exception do
    OnExceptLog ( Format(' TProfileTimer.StartOne (nStamp = %d, flags = $%x) ', [nStamp, flags]), E);
 end;
end; // StartOne


procedure TProfileTimer.StartPFC(nStamp: Integer);
var
   v: Int64;
begin
 v := g_pfc_value;
 if nStamp < 32 then v := GetTimeStamp;
 FTimerStarted [nStamp] := v;
end;




procedure TProfileTimer.Stop(nStamp: Integer);
begin
 FStarted [nStamp] := 0;
 Complex [nStamp] := Complex [nStamp] + Elapsed (nStamp);
end;



function TProfileTimer.TimerStarted(nStamp: Integer): Int64;
begin
 result := FTimerStarted [nStamp];
end;

{ TProfStatList }

function TProfStatList.AddStat(const sName: String): TProfStat;
var i: Integer;
begin
 i := IndexOf (sName);
 if i >= 0 then
  result := TProfStat (Objects [i])
 else
  begin
   result := TProfStat.Create;
   AddObject(sName, result);
  end;
end; // AddStat

constructor TProfStatList.Create(AOwner: TObject);
begin
 inherited Create (AOwner);
 OwnsObjects := TRUE;
 Sorted := TRUE;
end;

function TProfStatList.GetStat(const sName: String): TProfStat;
var
   i: Integer;
begin
 result := nil;
 if Trim(sName) = '' then exit;
 i := IndexOf ( Trim (sName) ) ;
 if i < 0 then
   result := AddStat( UpperCase(sName) )
 else
   result := TProfStat ( Objects[i] );
end;

function TProfStatList.GetStatByIndex(nIndex: Integer): TProfStat;
begin
 result := TProfStat(Objects[nIndex]);
end;

procedure TProfStatList.ResetAll;
var n: Integer;
begin
 for n := 0 to Count - 1 do
     TProfStat(Objects[n]).Reset;                            
end;

{ TProfStat }

procedure TProfStat.Add(pst: TProfStat);
begin
 Inc (chk_count, pst.chk_count);
 ftime := ftime + pst.ftime;
 ctime := ctime + pst.ctime;
end;

procedure TProfStat.Reset;
begin
 chk_count := 0;
 ftime := 0;
 ctime := 0;
end;

procedure TProfStat.Update(pt: TProfileTimer);
begin
 Inc (chk_count);
 ftime := ftime + pt.Elapsed;
 ctime := ctime + pt.CPUElapsed;
end; // Update

{ TSystemTimer }

function TVirtualTimer.ActiveSlot: PVirtualTimerRecord;
begin
 result := SyncPort.GetSlot (SyncPort.active);
end;

function TVirtualTimer.NextSlot ( i: Integer ): Integer;
begin
 result := (i + 1) and VTD_SLOTS_MASK;
end;



function TVirtualTimer.Ready: Boolean;
begin
 result := Assigned (self) and Assigned (FDiffTimer);
end;

constructor TVirtualTimer.Create;
const
     FMP_NAME = 'Global\VIRTUALTIMER'; //

var
   priv: Boolean;

begin
 SetLastError (0);

 FOwnRights := FALSE;
 FMapping.Init (4096);

 try
   priv := AdjustPrivilege(SE_CRGLOBAL_NAME);
   SetLastError (0);

   if not FMapping.Open ( FMP_NAME ) then
      begin
       _SafeLog('[~T]. #DBG: OK. Cannot open timer section, trying to create');
       // ReadLn;
       FOwnRights := FMapping.Create ( FMP_NAME, TRUE );
      end;

   if ( FMapping.hMapping = 0 ) and ( GetLastError <> 0 ) then
         RaiseLastError ('CreateFileMapping(' + FMP_NAME + ') failed. ');

   if priv then AdjustPrivilege(SE_CRGLOBAL_NAME, FALSE);

   if GetLastError = ERROR_ALREADY_EXISTS then
      OutputDebugString ( PChar ('Mapping reused ' + PChar (FMP_NAME) ) );

   SetLastError (0);

   FSyncPort := FMapping.MapView;

   if FSyncPort <> nil then
      _SafeLog ('MapViewOfFile successed, ownrights = ' + IfV(FOwnRights, 'yes', 'no') );

   Assert (FSyncPort <> nil, ClassName + '.Create: Cannot open sync-port, hMap = $' + IntToHex(FMapping.hMapping, 4) + '. Error: ' + Err2Str(GetLastError) );
 except
  on E: Exception do
     _SafeLog( ClassName + '.Create, catched exception ' + E.Message );
 end;

 if FSyncPort = nil then
   begin
    _SafeLog('#WARN: Using local SyncPort');
    FSyncPort := AllocMem ( 4096 );
    FOwnRights := TRUE;
   end;

 max_no_sync := (1800 + Random(555) ) * 1000 * DT_ONE_MSEC;

 if g_timer = nil then g_timer := self;

 FDiffTimer := TProfileTimer.Create;
 FDiffTimer.EnableWarnings := FALSE;
 FDiffTimer.FIsReference := TRUE;
 FDiffTimer.UpdateCalibration ( FOwnRights );


 if OwnRights then
   begin
    ZeroMemory ( SyncPort, sizeof ( TVirtualTimerData ) );
    SyncPort.data_sz := sizeof ( TVirtualTimerData );
    ActiveSlot.version := VTD_VERSION;
    ActiveSlot.aprox_pfc := 0.9;
    ActiveSlot.Hash;
   end
 else
   Assert ( ActiveSlot.version = VTD_VERSION, Format ( ' VirtualTimerData version mistmatch, required %x <> %x ', [VTD_VERSION, ActiveSlot.version] ) );
 Assert ( SyncPort.data_sz = sizeof ( TVirtualTimerData ), ' sizeof (TVirtualTimerData)  <> data_sz ' );
 if ActiveSlot.pfc_base = 0 then
   begin
    SyncNow ( TRUE );
    _SafeLog('VirtualTimer created and synchronized. Base time = ' + DateTimeToStr ( ActiveSlot.base ) );
   end
 else
    _SafeLog('VirtualTimer created, but not synchronized. Probably used global, base time = ' + DateTimeToStr ( ActiveSlot.base ) );

end;

destructor TVirtualTimer.Destroy;
begin


 if FMapping.hMapping <> 0 then
   FMapping.Close
 else
   FreeMem (FSyncPort);

 FDiffTimer.Free;
 inherited;
end;

function TVirtualTimer.GetTime ( bSilent: Boolean ): TDateTime;

var
   elps: Double;
   diff: Int64;
   pfcv: Int64;
   slot: PVirtualTimerRecord;
      i: Integer;
      r: Integer;

begin

 result := Now;

 if SyncPort = nil then exit;

 // если в данный момент поток грохнется, будет очень плохо!


 slot := ActiveSlot;

 pfcv := FDiffTimer.GetTimeStamp;
 {$IFDEF CPUX86}
 asm
  finit
 end;
 {$ENDIF CPUX86}

 for i := 1 to 100 do
  if slot.base = 0 then
   begin
    _SafeLog('Trying resync virtual timer');
    SyncNow(TRUE);
   end
  else
   break;

 Assert ( slot.base > 0, ClassName + '.GetTime: slot.base = ' + ftow(slot.base));

 diff := pfcv - slot.pfc_base;
 elps := diff * slot.pfc_coef * slot.pfc_corr * DT_ONE_MSEC;

 if elps < 0 then
    _SafeLog ( '#WARN: ' + ClassName + '.GetTime: elps = ' + ftow(elps));

 result := slot.base + elps;
 p_shared_vars.local_dt := result;

 if bSilent then exit;


 SyncPort.lock (1);
 try
  r := SyncPort.active;
  i := NextSlot (r);
  slot := SyncPort.CloneActive (i);

  slot.dt_last := result;
  slot.pfc_last := pfcv;
  Inc ( slot.upd_cnt );
  SwitchSlot ( i, r );
 finally
  SyncPort.unlock;
 end;

end;


function TVirtualTimer.SwitchSlot ( upd, curr: Integer): Boolean;
begin
 SyncPort.GetSlot (upd).Hash;
 result := InterlockedCompareExchange ( SyncPort.active, upd, curr ) = curr; // здесь возможна конкурентная ошибка
end;

function TVirtualTimer.SyncNow;
var
    dst: PVirtualTimerRecord;
    tmp: TVirtualTimerRecord;
      i: Integer;
      r: Integer;
begin
 result := FALSE;

 if SyncPort = nil then
     begin
      _SafeLog('#FATAL: SyncPort = nil');
      exit;
     end;

 if ( not FOwnRights ) and ( ActiveSlot.pfc_coef > 0 ) then
     begin
      _SafeLog('#FATAL: No have own rights for VirtualTimer');
      exit;
     end;

 Sleep(1);
 __finit;

 TickAdjust(2);
 tmp.pfc_last := FDiffTimer.GetTimeStamp;
 tmp.base := CurrentDateTime;
 if bCalibrate then
    tmp.pfc_freq := FDiffTimer.UpdateCalibration ( TRUE );
 tmp.pfc_coef := FDiffTimer.Coef;

 SyncPort.Lock (2);
 try
   r := SyncPort.active;
   i := NextSlot (r);
   dst := SyncPort.CloneActive (i);

   if dst.pfc_corr = 0 then dst.pfc_corr := 1;

   // saving newest values
   dst.base := tmp.base;
   dst.pfc_base := tmp.pfc_last;
   dst.pfc_last := tmp.pfc_last;
   dst.pfc_corr := FDiffTimer.PFCRatio;
   dst.pfc_coef := tmp.pfc_coef;

   if bCalibrate then
      dst.pfc_freq := tmp.pfc_freq;

   Inc ( dst.syn_cnt );

   SwitchSlot ( i, r );

   _SafeLog ('VirtualTimer sync complete');
   result := TRUE;
 finally
  SyncPort.Unlock;
 end;

 // CBeep (1100, 300);
end;


function TVirtualTimer.SyncWith;
var
   dst: PVirtualTimerRecord;
     r: Integer;
     i: Integer;
begin
 result := FALSE;
 if not OwnRights then exit;


 SyncPort.Lock (2);
 try
   r := SyncPort.active;
   i := NextSlot (r);

   dst := SyncPort.CloneActive (i);

   // saving newest values
   dst.base     := base;
   dst.pfc_base := src.FTimerStarted [idx];
   dst.pfc_last := dst.pfc_base;
   dst.pfc_corr := src.PFCRatio;
   dst.pfc_coef := src.Coef;
   dst.pfc_freq := src.FFrequency;

   FDiffTimer.FPFCRatio := src.PFCRatio;
   FDiffTimer.Coef := src.Coef;

   Inc ( dst.syn_cnt );

   result := SwitchSlot ( i, r );
 finally
  SyncPort.Unlock;
 end;
end;

procedure TVirtualTimer.TestSync;
begin
 if ( ActiveSlot = nil ) or ( FDiffTimer = nil ) then exit;
 try
  if Now - ActiveSlot.base > max_no_sync then
     SyncNow ( FDiffTimer.Coef = 0 );
 except
  on E: Exception do
    OnExceptLog ('TestSync', E);
 end;
end;

function TVirtualTimer.Update(const src: TVirtualTimerRecord): Boolean;
var
   i, r: Integer;
    dst: PVirtualTimerRecord;
begin
 SyncPort.Lock (3);
 try
  r := SyncPort.active;
  i := NextSlot (r);
  dst := SyncPort.GetSlot (i);
  dst^ := src;
  result := SwitchSlot ( i, r );
 finally
  SyncPort.Unlock;
 end;
end;

var
   zero_time: Integer = 0;

function PreciseTime: TDateTime;
begin
 if g_timer.Ready then
   result := g_timer.GetTime (TRUE)
 else
   result := Now;

 if Frac(result) <= DT_ONE_MSEC then
   begin
    Inc (zero_time);
    if zero_time < 20 then exit;
    if zero_time >= 20 then
      begin
       PrintError ( Format( 'Zero time (midnight) returned %d times, result = %s ', [zero_time, FormatDateTime('dd.mm.yy hh:nn:ss.zzz', result) ] ) );
       FreeAndNil ( g_timer );
       zero_time := 0;
      end;

    __finit;
   end
  else
    zero_time := 0;
end;

{ TTimeSource }

function TTimeSource.GetTime ( bSilent: Boolean = FALSE ): TDateTime;
begin
 result := Now;
end;

exports
     PreciseTime;


procedure InitModule;
var
   e: String;
   p: Pointer;
   m: HMODULE;

begin
 InitRoutines;

 g_timer := nil; // manual created always  TVirtualTimer.Create;


 ps_start_time := Now;
 slmap := TStrMap.Create;

 e := ExtractFileName ( GetModuleName (0) );
 m := GetModuleHandle ( PChar (e) );

 p := @Now;

 if m <> 0 then
  begin
   p := GetProcAddress ( m, 'PreciseTime' );
   if p = nil then
      p := @PreciseTime;
  end;

 if p <> nil then
    misc.g_time_func := p;
end;

{ TTimeRange }

function TTimeRange.DayStart: TDateTime;
begin
 result := Trunc ( Min (tmBegin, tmEnd) );
end;

procedure TTimeRange.GetTimeStamps(var tsBegin, tsEnd: TTimeStamp);
begin
 tsBegin := DateTimeToTimeStamp ( tmBegin );
 tsEnd := DateTimeToTimeStamp ( tmEnd );
end;

function TTimeRange.InRange(dt: TDateTime): Boolean;
begin
 if dt <= 0 then dt := Now;

 result := ( tmBegin <= dt ) and ( dt <= tmEnd );
end;


{ TTimeDeviator }

constructor TTimeDeviator.Create;
var
   hDLL: HMODULE;

begin
 hDLL := GetModuleHandle('kernelbase.dll');
 if hDLL = 0 then exit;
 FGetLocalTime := GetProcAddress (hDLL, 'GetLocalTime');
 FGetSystemTime := GetProcAddress (hDLL, 'GetSystemTime');
 FGetSysTimeAsFT := GetProcAddress (hDLL, 'GetSystemTimeAsFileTime');
 {$IFDEF CPUX64}
 Assert (FALSE, 'Not designed for x64');
 {$ENDIF}


 if ( FGetLocalTime = nil ) or ( FGetSystemTime = nil ) or ( FGetSysTimeAsFT = nil ) then exit;

 Inc ( NativeUInt (FGetLocalTime), $2E ); // смещение к команде mov ecx, [ ADDR_32 ]
 Inc ( NativeUInt (FGetSystemTime), $13 );  // mov ecx, [ DATA + 14h ] -- FILETIME dwHighDateTime
 Inc ( NativeUInt (FGetSysTimeAsFT), $10 ); // mov ecx, [ DATA + 14h ] -- FILETIME dwHighDateTime



 FOriginalTime := FGetLocalTime^;    // копировать оригинальный указатель

 ODS ( CFormat ( '[~T]. #DBG(TTimeDeviator): time_ref1 = $%p, time_ref2 = $%p', '~C07', [FOriginalTime, FGetSysTimeAsFT^] ) );


 if ( NativeUInt (FOriginalTime) <> NativeUInt (FGetSysTimeAsFT^) ) or
    ( NativeUInt (FOriginalTime) <> NativeUInt (FGetSystemTime^) )   then
    begin
     FGetSysTimeAsFT := nil;
     FGetSystemTime := nil;
     FGetLocalTime := nil;
     FOriginalTime := nil;
     exit; // проверка на совпадение
    end;

 FTimeBase1600 := FOriginalTime;

 Inc ( NativeUInt (FTimeBase1600), 12 ); // несколько выше в памяти
end;

function TTimeDeviator.GetDateTime: TDateTime;
begin
 result := Convert2DT ( FFakeTime );
end;

function TTimeDeviator.Convert2DT ( src: TIME_DATA ): TDateTime;
var
   rt: LARGE_INTEGER;
   tf: TIME_FIELDS;

begin
 result := Now;
 if FTimeBase1600 = nil then exit;

 rt.QuadPart := src.current - FTimeBase1600.current;
 RtlTimeToTimeFields ( rt, tf );

 result := tf.AsDateTime;
end;

procedure TTimeDeviator.SetDateTime(const Value: TDateTime);
var
   rt: LARGE_INTEGER;
   tf: TIME_FIELDS;
   rs: Extended;

begin
 if FTimeBase1600 = nil then exit;

 tf.Init (Value); // всего-то дата нужна

 tf.Hour := 0;
 tf.Minute := 0;
 tf.Second := 0;
 tf.Milliseconds := 0;

 RtlTimeFieldsToTime ( tf, rt );

 rs := Frac ( Value ) / DT_100_NS; // количество мгновений

 rt.QuadPart := rt.QuadPart + Trunc (rs);

 Inc (rt.QuadPart, FTimeBase1600.current);

 FFakeTime.current := rt.QuadPart;
 FFakeTime.high_nv := rt.HighPart;

end;


function TTimeDeviator.Patch ( bPath: Boolean ): Boolean;
var
    ptm: NativeUInt;

begin
 result := FALSE;

 if FGetLocalTime = nil then exit;

 if bPath then
    ptm := NativeUInt ( @FFakeTime )
 else
    ptm := NativeUInt ( FOriginalTime );

 // AMAZING MS REPEATELY CODE
 result := DoSinglePatch ( NativeUInt ( FGetLocalTime ) - 6, ptm ) and
           DoSinglePatch ( NativeUInt ( FGetSystemTime ) - 6, ptm ) and
           DoSinglePatch ( NativeUInt ( FGetSysTimeAsFT ) - 6, ptm );
end;


function TTimeDeviator.DoSinglePatch (pwp, ptm: NativeUInt): Boolean;
var
   aptr: Pointer;
    ofs: DWORD;
     cb: NativeUInt;
begin
 // Патч осуществляется групповой (атомарной) заменой всех адресов, для чего используется буфер
 result := FALSE;
 aptr := Ptr ( pwp and $FFFFFFF0 );
 ofs := pwp and $F;
 cb := 0;


 if not ReadProcessMemory ( GetCurrentProcess, aptr, @buff, sizeof (buff), cb ) then exit;


 PutPtr ( ofs + $0, ptm + 4 );
 PutPtr ( ofs + $6, ptm + 0 );
 PutPtr ( ofs + $C, ptm + 8 );


 FFakeTime := FOriginalTime^; // чтобы небыло неожиданностей

 result := WriteProcessMemory ( GetCurrentProcess, aptr, @buff, sizeof (buff), cb );
end;

procedure TTimeDeviator.PutPtr;
var
   pp: PNativeUint;
begin
 pp := @buff [ofs];
 pp^ := p; // unaligned write possible
end;

function TTimeDeviator.SysDateTime: TDateTime;
begin
 if Assigned (FOriginalTime) then
    result := Convert2DT ( FOriginalTime^ )
 else
    result := Now;
end;


function QPF_Fake (lpFrequency: PInt64): BOOL; stdcall;
var
   tmp: TVirtualTimerRecord;
begin
 if zero_freq or ( g_timer = nil ) then
    lpFrequency^ := 0
 else
   begin
    tmp := g_timer.SyncPort.Fork;
    lpFrequency^ := Round ( tmp.pfc_freq * tmp.pfc_corr * fake_pfc_corr );
   end;


 result := TRUE;
end;

{
asm
 xor  eax, eax
 mov  edx, lpFrequency
 mov  [edx], eax
 mov  [edx + 4], eax
 mov  eax, 1
end;// }


var
   func_ptr: Pointer;


procedure PatchQPF;
var
   hDLL: HMODULE;
     pp: PPointer;
     pf: Pointer;
     wb: NativeUInt;



begin
 hDLL := GetModuleHandle ('kernel32.dll');
 if hDLL = 0 then exit; // absurd
 pp := GetProcAddress ( hDLL, 'QueryPerformanceFrequency' );

 if pp = nil then exit;

 Inc ( NativeUInt (pp), $0F );

 func_ptr := @QPF_Fake;
 pf := @func_ptr;

 ODS ( CFormat ('[~T]. #DBG: target DWORD at $%p = $%p, repl = $%p ', '~C07', [pp, pp^, pf] ) );

 WriteProcessMemory ( GetCurrentProcess, pp, @pf, sizeof(pf), wb );

 ODS ( CFormat ('[~T]. #DBG: target DWORD at $%p = $%p, repl = $%p ', '~C07', [pp, pp^, pf] ) );

 // CBeep (1000, 500);
end;


{ TIME_FIELDS }

function TIME_FIELDS.AsDateTime: TDateTime;
begin
 result := EncodeDateTime ( Year, Month, Day, Hour, Minute, Second, Milliseconds );
end;

procedure TIME_FIELDS.Init(src: TDateTime);
begin
 DecodeDateTime ( src, Year, Month, Day, Hour, Minute, Second, Milliseconds );
end;


function TVirtualTimerRecord.Hash: DWORD;
begin
 result := CalcCRC32 ( @self, sizeof (self) - 4 )
end;


{ TVirtualTimerData }

function TVirtualTimerData.CloneActive (next: Integer): PVirtualTimerRecord;
begin
 result := GetSlot (next);
 result^ := Slots [active];
 result.slot_idx := next;
end;

function TVirtualTimerData.Fork: TVirtualTimerRecord;
var
   n: Integer;
begin

 for n := 1 to 10000 do
  begin
   result := Slots [active];

   if  ( result.rw_lock = 0 ) and ( result.crc32 = result.Hash ) then break; // information valid

   __pause;

   if n = 10000 then
      ODS ( '[~T] ~C0C #WARN: TVirtualTimerData.Fork maximum loops exceeded ~C07', PMF_UNFMT );
  end;
end;





function TVirtualTimerData.GetSlot(index: Integer): PVirtualTimerRecord;
begin
 result := @Slots [ index and VTD_SLOTS_MASK ];
end;


procedure TVirtualTimerData.Lock(value: Integer);
var
   tout: Integer;
begin
 tout := 100000000;
 while ( InterlockedCompareExchange ( sw_lock, 2, 0 ) <> 0 ) and ( tout > 0 ) do
     begin
      Dec ( tout );
      if tout and $FFFF = 0 then
         SwitchToThread;
     end;

 if tout < 10 then
    PrintError (' VirtualTimerData lock timeout ' );
end;

procedure TVirtualTimerData.Unlock;
begin
 sw_lock := 0;
end;


initialization
 InitModule;
finalization
 misc.g_time_func := Now;
 FreeAndNil (g_timer);
 FreeAndNil (slmap);
end.

