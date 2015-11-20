unit Perf;

interface
uses Windows, SysUtils, StrClasses, Classes, Registry, Misc, SyncObjs, Math;


const
    PDH_MORE_DATA        = $800007D2;
    PDH_FMT_RAW          = $00000010;
    PDH_FMT_ANSI         = $00000020;
    PDH_FMT_UNICODE      = $00000040;
    PDH_FMT_LONG         = $00000100;
    PDH_FMT_DOUBLE       = $00000200;
    PDH_FMT_LARGE        = $00000400;
    PDH_FMT_NOSCALE      = $00001000;
    PDH_FMT_1000         = $00002000;
    PDH_FMT_NODATA       = $00004000;
    PDH_FMT_NOCAP100     = $00008000;
    PERF_DETAIL_COSTLY   = $00010000;
    PERF_DETAIL_STANDARD = $0000FFFF;

    PDH_CSTATUS_VALID_DATA              = $00000000;
    PDH_CSTATUS_NEW_DATA                = $00000001;
    PDH_CSTATUS_NO_MACHINE              = $800007D0;
    PDH_CSTATUS_NO_INSTANCE             = $800007D1;
    PDH_CSTATUS_ITEM_NOT_VALIDATED      = $800007D3;
    PDH_RETRY                           = $800007D4;
    PDH_NO_DATA                         = $800007D5;
    PDH_CALC_NEGATIVE_DENOMINATOR       = $800007D6;
    PDH_CALC_NEGATIVE_TIMEBASE          = $800007D7;
    PDH_CALC_NEGATIVE_VALUE             = $800007D8;
    PDH_DIALOG_CANCELLED                = $800007D9;
    PDH_END_OF_LOG_FILE                 = $800007DA;
    PDH_ASYNC_QUERY_TIMEOUT             = $800007DB;
    PDH_CANNOT_SET_DEFAULT_REALTIME_DATASOURCE      = $800007DC;
    PDH_UNABLE_MAP_NAME_FILES           = $80000BD5;
    PDH_PLA_VALIDATION_WARNING          = $80000BF3;
    PDH_CSTATUS_NO_OBJECT               = $C0000BB8;
    PDH_CSTATUS_NO_COUNTER              = $C0000BB9;
    PDH_CSTATUS_INVALID_DATA            = $C0000BBA;
    PDH_MEMORY_ALLOCATION_FAILURE       = $C0000BBB;
    PDH_INVALID_HANDLE                  = $C0000BBC;
    PDH_INVALID_ARGUMENT                = $C0000BBD;
    PDH_FUNCTION_NOT_FOUND              = $C0000BBE;
    PDH_CSTATUS_NO_COUNTERNAME          = $C0000BBF;
    PDH_CSTATUS_BAD_COUNTERNAME         = $C0000BC0;
    PDH_INVALID_BUFFER                  = $C0000BC1;
    PDH_INSUFFICIENT_BUFFER             = $C0000BC2;
    PDH_CANNOT_CONNECT_MACHINE          = $C0000BC3;
    PDH_INVALID_PATH                    = $C0000BC4;
    PDH_INVALID_INSTANCE                = $C0000BC5;
    PDH_INVALID_DATA                    = $C0000BC6;
    PDH_NO_DIALOG_DATA                  = $C0000BC7;
    PDH_CANNOT_READ_NAME_STRINGS        = $C0000BC8;
    PDH_LOG_FILE_CREATE_ERROR           = $C0000BC9;
    PDH_LOG_FILE_OPEN_ERROR             = $C0000BCA;
    PDH_LOG_TYPE_NOT_FOUND              = $C0000BCB;
    PDH_NO_MORE_DATA                    = $C0000BCC;
    PDH_ENTRY_NOT_IN_LOG_FILE           = $C0000BCD;
    PDH_DATA_SOURCE_IS_LOG_FILE         = $C0000BCE;
    PDH_DATA_SOURCE_IS_REAL_TIME        = $C0000BCF;
    PDH_UNABLE_READ_LOG_HEADER          = $C0000BD0;
    PDH_FILE_NOT_FOUND                  = $C0000BD1;
    PDH_FILE_ALREADY_EXISTS             = $C0000BD2;
    PDH_NOT_IMPLEMENTED                 = $C0000BD3;
    PDH_STRING_NOT_FOUND                = $C0000BD4;
    PDH_UNKNOWN_LOG_FORMAT              = $C0000BD6;
    PDH_UNKNOWN_LOGSVC_COMMAND          = $C0000BD7;
    PDH_LOGSVC_QUERY_NOT_FOUND          = $C0000BD8;
    PDH_LOGSVC_NOT_OPENED               = $C0000BD9;
    PDH_WBEM_ERROR                      = $C0000BDA;
    PDH_ACCESS_DENIED                   = $C0000BDB;
    PDH_LOG_FILE_TOO_SMALL              = $C0000BDC;
    PDH_INVALID_DATASOURCE              = $C0000BDD;
    PDH_INVALID_SQLDB                   = $C0000BDE;
    PDH_NO_COUNTERS                     = $C0000BDF;
    PDH_SQL_ALLOC_FAILED                = $C0000BE0;
    PDH_SQL_ALLOCCON_FAILED             = $C0000BE1;
    PDH_SQL_EXEC_DIRECT_FAILED          = $C0000BE2;
    PDH_SQL_FETCH_FAILED                = $C0000BE3;
    PDH_SQL_ROWCOUNT_FAILED             = $C0000BE4;
    PDH_SQL_MORE_RESULTS_FAILED         = $C0000BE5;
    PDH_SQL_CONNECT_FAILED              = $C0000BE6;
    PDH_SQL_BIND_FAILED                 = $C0000BE7;
    PDH_CANNOT_CONNECT_WMI_SERVER       = $C0000BE8;
    PDH_PLA_COLLECTION_ALREADY_RUNNING  = $C0000BE9;
    PDH_PLA_ERROR_SCHEDULE_OVERLAP      = $C0000BEA;
    PDH_PLA_COLLECTION_NOT_FOUND        = $C0000BEB;
    PDH_PLA_ERROR_SCHEDULE_ELAPSED      = $C0000BEC;
    PDH_PLA_ERROR_NOSTART               = $C0000BED;
    PDH_PLA_ERROR_ALREADY_EXISTS        = $C0000BEE;
    PDH_PLA_ERROR_TYPE_MISMATCH         = $C0000BEF;
    PDH_PLA_ERROR_FILEPATH              = $C0000BF0;
    PDH_PLA_SERVICE_ERROR               = $C0000BF1;
    PDH_PLA_VALIDATION_ERROR            = $C0000BF2;
    PDH_PLA_ERROR_NAME_TOO_LONG         = $C0000BF4;
    PDH_INVALID_SQL_LOG_FORMAT          = $C0000BF5;
    PDH_COUNTER_ALREADY_IN_QUERY        = $C0000BF6;
    PDH_BINARY_LOG_CORRUPT              = $C0000BF7;
    PDH_LOG_SAMPLE_TOO_SMALL            = $C0000BF8;
    PDH_OS_LATER_VERSION                = $C0000BF9;
    PDH_OS_EARLIER_VERSION              = $C0000BFA;
    PDH_INCORRECT_APPEND_TIME           = $C0000BFB;
    PDH_UNMATCHED_APPEND_COUNTER        = $C0000BFC;
    PDH_SQL_ALTER_DETAIL_FAILED         = $C0000BFD;
    PDH_QUERY_PERF_DATA_TIMEOUT         = $C0000BFE;


    // from ntstatus.h:
    STATUS_INVALID_PARAMETER            = $C000000D;
    STATUS_BUFFER_TOO_SMALL             = $C0000023;
    STATUS_ACCESS_DENIED                = $C0000022;


type
   PDH_STATUS = DWORD;
   PDH_HQUERY = THandle;
   PDH_HCOUNTER = THandle;

   POWER_INFORMATION_LEVEL = ( SystemPowerPolicyAc = 0, SystemPowerPolicyDc = 1, SystemPowerCapabilities = 4, SystemBatteryState = 5, ProcessorInformation = 11 );


   _PROCESSOR_POWER_INFORMATION = record
                Number: DWORD;
                MaxMhz: DWORD;
            CurrentMhz: DWORD;
              MhzLimit: DWORD;
          MaxIdleState: DWORD;
      CurrentIdleState: DWORD;
    end;

    TProcessorPowerInformation = _PROCESSOR_POWER_INFORMATION;
    PProcessorPowerInformation = ^TProcessorPowerInformation;

    TProcessorPowerInfoSummary = record
     cpu_cnt: Integer;
     status: NTSTATUS;
     info: packed array [0..31] of TProcessorPowerInformation;
    end;

    PProcessorPowerInfoSummary = ^TProcessorPowerInfoSummary;




(*
typedef struct _PDH_FMT_COUNTERVALUE {
    DWORD    CStatus;
    union {
        LONG        longValue;
        double      doubleValue;
        LONGLONG    largeValue;
        LPCSTR      AnsiStringValue;
        LPCWSTR     WideStringValue;
    };
} PDH_FMT_COUNTERVALUE, * PPDH_FMT_COUNTERVALUE;
*)

   PDH_FMT_COUNTERVALUE = record
    cStatus: DWORD;
    case BYTE of
     0:   (longValue: LongInt);
     1: (doubleValue: Double);
     2:  (largeValue: Int64);
     3:   (ansiValue: PAnsiChar);
     4:   (wideValue: PWideChar);
     5:   ( ptrValue: Pointer);
   end; // PDH_FMT_COUNTERVALUE

   TExRegistry = class(TRegistry)
   public
    procedure ReadStrings(const ValueName: String; lout: TStrings); overload;
    function  ReadStrings(const ValueName: String): String; overload;
   end;

   TSystemCountersList = class;


   TSystemCounter = class
     private
      FOwner: TSystemCountersList;
      FHandle: PDH_HCOUNTER;
     protected
     public
      { props }
      property          Handle: PDH_HCOUNTER read FHandle;
      property          Owner: TSystemCountersList read FOwner write FOwner;

      { C & D }
      { methods }
      function          GetFmtValue (dwFormat: DWORD): PDH_FMT_COUNTERVALUE;

   end;  // TSystemCounter


   TSystemCountersList = class (TStrMap)
     private
      FQuery: PDH_HQUERY;
      FQueryEvent: TEvent;
      FCounterMap: TStrings;
      function GetItem(index: Integer): TSystemCounter; inline;

     protected
     public
      { props }
      property          CounterMap: TStrings read FCounterMap;
      property          Items[index: Integer]: TSystemCounter read GetItem;
      property          Query: PDH_HQUERY   read FQuery;
      property          QueryEvent: TEvent  read FQueryEvent;

      { C & D  }
      constructor       Create (AOwner: TObject = nil);
      destructor        Destroy; override;
      { methods }

      function          AddCounter (sFullCounterPath, key: String): TSystemCounter;
      function          AddCPUCounters(const sub: String; maxc: Integer = 48 ): Integer;

      function          CollectQueryData (dwIntervalTime: DWORD): DWORD;

      function          EnuToLocal (const sCtrName: String): String;
      function          LookupCounterName (idx: Integer; en: Boolean = FALSE): String;
      function          IndexOfCounter(const sCtrName: String): Integer;


   end; // TSystemCounterList



function PdhAddCounter(hQuery: PDH_HQUERY; szFullCounterPath: PChar; dwUserData: DWORD_PTR; var phCounter: PDH_HCOUNTER): PDH_STATUS; stdcall;
function PdhLookupPerfIndexByName (szMachineName, szNameBuffer: PChar; out dwIndex: DWORD): PDH_STATUS; stdcall;
function PdhLookupPerfNameByIndex (szMachineName: PChar; dwNameIndex: DWORD; szNameBuffer: PChar;
                                   var pcchNameBufferSize: DWORD): PDH_STATUS; stdcall;

function PdhOpenQuery( szDataSource: PChar; dwUserData: DWORD_PTR; var phQuery: PDH_HQUERY): PDH_STATUS; stdcall;
function PdhCloseQuery( hQuery: PDH_HQUERY ): PDH_STATUS; stdcall;

function PdhCollectQueryDataEx( hQuery: PDH_HQUERY; dwIntervalTime: DWORD; hNewDataEvent: THandle): PDH_STATUS; stdcall;
function PdhGetFormattedCounterValue( hCounter: PDH_HCOUNTER; dwFormat: DWORD;
                                      var lpdwType: DWORD; var value: PDH_FMT_COUNTERVALUE ): PDH_STATUS; stdcall;


var
   _CallNtPowerInformation: function ( InformationLevel: POWER_INFORMATION_LEVEL;
                                       lpInputBuffer: Pointer;
                                       nInputBufferSize: DWORD;
                                       pOutputBuffer: Pointer;
                                       nOutputBufferSize: DWORD ): NTSTATUS; stdcall;

function GetCPUPowerInfo: PProcessorPowerInfoSummary;

implementation

const
  PerfLib = 'pdh.dll';


function PdhCloseQuery; external PerfLib name 'PdhCloseQuery';
function PdhCollectQueryDataEx; external PerfLib name 'PdhCollectQueryDataEx';
function PdhGetFormattedCounterValue; external PerfLib name 'PdhGetFormattedCounterValue';

{$IFDEF UNICODE}
 function PdhAddCounter; external PerfLib name 'PdhAddCounterW';
 function PdhLookupPerfIndexByName; external PerfLib name 'PdhLookupPerfIndexByNameW';
 function PdhLookupPerfNameByIndex; external PerfLib name 'PdhLookupPerfNameByIndexW';
 function PdhOpenQuery; external PerfLib name 'PdhOpenQueryW';
{$ELSE}
 function PdhAddCounter; external PerfLib name 'PdhAddCounterA';
 function PdhLookupPerfIndexByName; external PerfLib name 'PdhLookupPerfIndexByNameA';
 function PdhLookupPerfNameByIndex; external PerfLib name 'PdhLookupPerfNameByIndexA';
 function PdhOpenQuery; external PerfLib name 'PdhOpenQueryA';
{$ENDIF}

{ TExRegistry }

procedure TExRegistry.ReadStrings(const ValueName: String; lout: TStrings);
var
  ValueType : DWORD;
  ValueLen  : DWORD;
  P, Buffer : PChar;
begin
  SetLastError( RegQueryValueEx(CurrentKey, PChar (ValueName), nil,
                @ValueType, nil, @ValueLen) );
  if GetLastError = ERROR_SUCCESS then
  begin
    if ValueType = REG_MULTI_SZ then
    begin
      GetMem(Buffer, ValueLen);
      try
        RegQueryValueEx(CurrentKey, PChar(ValueName), nil, nil, PBYTE(Buffer), @ValueLen);
        P := Buffer;
        while P^ <> #0 do
        begin
          lout.Add ( Trim(p) );
          Inc(P, lstrlen(P) + 1);
        end;
      finally
        FreeMem (Buffer);
      end;
    end
    else
      raise ERegistryException.Create ('String list expected');
  end
  else
    raise Exception.Create ('Unable read MULTI_SZ value');
end;


function TExRegistry.ReadStrings(const ValueName: String): String;
var
   tmp: TStringList;
begin
 result := '';
 tmp := TStringList.Create;
 try
  ReadStrings (ValueName, tmp);
  result := tmp.Text;
 finally
  tmp.Free;
 end;
end;

{ TSystemCountersList }

function TSystemCountersList.AddCounter(sFullCounterPath, key: String): TSystemCounter;
var
   h: PDH_HCOUNTER;
   st: PDH_STATUS;
begin
 result := nil;
 st := PdhAddCounter (FQuery, PChar (sFullCounterPath), DWORD(self), h);
 SetLastError (st);
 if key = '' then key := sFullCounterPath;

 if ERROR_SUCCESS = st then
  begin
   result := TSystemCounter.Create;
   result.FHandle := h;
   AddObject (key, result);
  end;
end;

function TSystemCountersList.AddCPUCounters(const sub: String; maxc: Integer): Integer;
var
   i: Integer;
   root, psn, pst: String;
begin
 psn := EnuToLocal ('Processor');
 pst := EnuToLocal (sub); // '% Processor Time'

 result := 0;

 for i := 0 to maxc - 1 do
    begin
     root := '\' + psn + '(' + IntToStr (i) + ')\';
     if AddCounter (root + pst, 'SC_' + IntToStr(count)) = nil then break;
     Inc (result);
    end;

end;

function TSystemCountersList.CollectQueryData(dwIntervalTime: DWORD): DWORD;
begin
 result := PdhCollectQueryDataEx (FQuery, dwIntervalTime, FQueryEvent.Handle);
end;

constructor TSystemCountersList.Create(AOwner: TObject);
var
   reg: TExRegistry;
begin
 inherited Create (AOwner);
 FQueryEvent := TEvent.Create;
 FCounterMap := TStringList.Create;
 PdhOpenQuery (nil, DWORD (self), FQuery);

 reg := TExRegistry.Create (KEY_READ);
 try
  reg.RootKey := HKEY_LOCAL_MACHINE;

  if reg.OpenKeyReadOnly ('\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Perflib\009') then
     reg.ReadStrings ('Counter', CounterMap);
 finally
  reg.Free;
 end;

end;

destructor TSystemCountersList.Destroy;
begin
 PdhCloseQuery (FQuery);
 FreeAndNil (FCounterMap);
 FreeAndNil (FQueryEvent);
 inherited;
end;

function TSystemCountersList.EnuToLocal(const sCtrName: String): String;
var
   idx: Integer;
begin
 result := '';
 idx := IndexOfCounter (sCtrName);
 if idx >= 0 then
    result := LookupCounterName (idx);
end;

function TSystemCountersList.GetItem(index: Integer): TSystemCounter;
begin
 result := TSystemCounter ( Objects [index] );
end;

function TSystemCountersList.IndexOfCounter(const sCtrName: String): Integer;
begin
 result := CounterMap.IndexOf (sCtrName);
 if result > 0 then
    result := atoi ( CounterMap [result - 1] );
end;

function TSystemCountersList.LookupCounterName(idx: Integer; en: Boolean): String;
var
   i: Integer;
   sz, res: DWORD;

begin
 result := '';
 if en then
  begin
   i := CounterMap.IndexOf ( IntToStr (idx) );
   if i >= 0 then
     begin
      result := CounterMap [i + 1];
      exit;
     end;
  end;

 sz := 32;
 SetLength ( result, sz + 1 );
 FillZero ( result, sz );


 res := PdhLookupPerfNameByIndex ( nil, idx, @result[1], sz );
 if PDH_MORE_DATA = res then
  begin
   SetLength ( result, sz + 1 );
   FillZero ( result, sz );
   res := PdhLookupPerfNameByIndex ( nil, idx, @result[1], sz );
  end;
 SetLastError (res);

 result := Copy (result, 1, StrLen ( PChar (result) ) );

 result := Trim (result);
end;

{ TSystemCounter }

function TSystemCounter.GetFmtValue(dwFormat: DWORD): PDH_FMT_COUNTERVALUE;
var
   res, dwType: DWORD;

begin
 FillChar (result, sizeof (result), 0);
 result.cStatus := PDH_CSTATUS_NO_INSTANCE;
 res := PdhGetFormattedCounterValue (Handle, dwFormat, dwType, result);
 if dwType = dwFormat then
    SetLastError (res);
end;


function GetCPUPowerInfo: PProcessorPowerInfoSummary;
var
   si: TSystemInfo;
   cb, defc, extra: Integer;

begin
 FillChar (si, sizeof(si), 0);
 GetSystemInfo (si);

 defc := 1 + High (result.info); // default count of CPUs
 extra := Max (0, Integer (si.dwNumberOfProcessors) - defc );

 cb := sizeof (TProcessorPowerInfoSummary) + extra * sizeof (TProcessorPowerInformation);

 result := nil;

 if not Assigned (_CallNtPowerInformation) then exit;

 GetMem ( result, cb );
 FillChar (result^, cb, 0);

 result.cpu_cnt := si.dwNumberOfProcessors;

 result.status := _CallNtPowerInformation (ProcessorInformation, nil, 0, @result.info, result.cpu_cnt * sizeof (TProcessorPowerInformation));

end;

procedure DoInitModule;
var
   hLib: THandle;
begin
 hLib := LoadLibrary ('PowrProf.dll');
 if hLib <> 0 then
    _CallNtPowerInformation := GetProcAddress (hLib, 'CallNtPowerInformation');
end;

initialization
 DoInitModule;
end.
