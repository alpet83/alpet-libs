unit P2ClientGateMTA_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// $Rev: 41960 $
// File generated on 24.08.2012 11:54:53 from Type Library described below.

// ************************************************************************  //
// Type Lib: L:\P2FORTSGate\P2ClientGateMTA.dll (1)
// LIBID: {B20A3AED-0B23-4064-B6ED-2CBBE865741C}
// LCID: 0
// Helpfile: 
// HelpString: P2ClientGateMTA 1.0 Type Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// Errors:
//   Hint: Parameter 'type' of IP2MessageReceiver.GetFilter changed to 'type_'
//   Hint: Symbol 'type' renamed to 'type_'
//   Error creating palette bitmap of (TCP2Connection) : Server L:\P2FORTSGate\P2ClientGateMTA.dll contains no icons
//   Error creating palette bitmap of (TCP2BLMessage) : Server L:\P2FORTSGate\P2ClientGateMTA.dll contains no icons
//   Error creating palette bitmap of (TCP2BLMessageFactory) : Server L:\P2FORTSGate\P2ClientGateMTA.dll contains no icons
//   Error creating palette bitmap of (TCP2TableSet) : Server L:\P2FORTSGate\P2ClientGateMTA.dll contains no icons
//   Error creating palette bitmap of (TCP2Record) : Server L:\P2FORTSGate\P2ClientGateMTA.dll contains no icons
//   Error creating palette bitmap of (TCP2DataStream) : Server L:\P2FORTSGate\P2ClientGateMTA.dll contains no icons
//   Error creating palette bitmap of (TCP2DataBuffer) : Server L:\P2FORTSGate\P2ClientGateMTA.dll contains no icons
//   Error creating palette bitmap of (TCP2Application) : Server L:\P2FORTSGate\P2ClientGateMTA.dll contains no icons
//   Error creating palette bitmap of (TCP2TableEnum) : Server L:\P2FORTSGate\P2ClientGateMTA.dll contains no icons
//   Error creating palette bitmap of (TCP2RecordEnum) : Server L:\P2FORTSGate\P2ClientGateMTA.dll contains no icons
//   Error creating palette bitmap of (T__Impl_IP2DataStreamEvents) : Server L:\P2FORTSGate\P2ClientGateMTA.dll contains no icons
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses Windows, ActiveX, Classes, Graphics, OleServer, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  P2ClientGateMTAMajorVersion = 1;
  P2ClientGateMTAMinorVersion = 0;

  LIBID_P2ClientGateMTA: TGUID = '{B20A3AED-0B23-4064-B6ED-2CBBE865741C}';

  IID_IP2Connection: TGUID = '{1E7F68E9-DCA1-44E6-91CB-8B8C17D5601B}';
  IID_IP2ConnectionEvent: TGUID = '{4EF00151-3499-46D3-BC33-9A1763827D89}';
  IID_IP2MessageReceiver: TGUID = '{795E97AD-7B4D-4276-AE78-A7443FAC2754}';
  CLASS_CP2Connection: TGUID = '{43FB494A-620B-4588-A9DD-DB0BE4B6694A}';
  DIID_IP2AsyncMessageEvents: TGUID = '{BEB8ED4A-7EB0-44A0-8F98-9DC9515AB3FF}';
  IID_IP2BLMessage: TGUID = '{A85BC7A3-9CE4-429E-A370-5875DEFEB3E9}';
  DIID_IP2AsyncSendEvent2: TGUID = '{98C080E2-7CB1-43C4-B036-DB144AF3BCDB}';
  CLASS_CP2BLMessage: TGUID = '{2281C925-902A-4612-B80D-414ED8F62C0D}';
  IID_IP2BLMessageFactory: TGUID = '{0F3918B5-0FA6-4483-9261-EF64A019FA39}';
  CLASS_CP2BLMessageFactory: TGUID = '{EAB94E8C-7106-42F7-BB04-C57ACD895DE3}';
  IID_IP2TableSet: TGUID = '{CBE24405-8376-4B25-8C31-F4F4268EC7A1}';
  CLASS_CP2TableSet: TGUID = '{7AA8F6D7-8375-4345-9010-6F0DB3E08A51}';
  IID_IP2Record: TGUID = '{76033FC9-8BB0-4415-A785-9BF86AAF4E99}';
  CLASS_CP2Record: TGUID = '{9A69614A-BC25-4A97-922B-3AFB993144B0}';
  IID_IP2DataStream: TGUID = '{C14F57CF-BEE6-4ADB-85D0-44A307AD9444}';
  IID_IP2DataStreamEvents: TGUID = '{7F23A734-2F90-4DC6-9007-E5DC27FDC613}';
  IID_IP2DataStreamEvents2: TGUID = '{C0027852-2CDC-41FB-A68F-97DD0F9EE368}';
  CLASS_CP2DataStream: TGUID = '{70F3774E-E66C-47DE-A47B-5AD3ECA67AF5}';
  IID_IP2DataBuffer: TGUID = '{48169C9B-8E7F-427C-836B-8BB168CA8042}';
  CLASS_CP2DataBuffer: TGUID = '{9791418D-3998-4CAC-BA23-87E9EDA14FEB}';
  IID_IP2TableRecords: TGUID = '{3A19BD02-482B-4A7A-AE9F-AD879BE573C8}';
  IID_IP2Application: TGUID = '{1F5B9148-F21F-4980-A5D5-23CD5F9CA0A5}';
  CLASS_CP2Application: TGUID = '{D42CDADE-DA8E-4133-844B-7602659D4F12}';
  CLASS_CP2TableEnum: TGUID = '{44B26EE1-964F-4643-BFEE-16A8E3C8CE71}';
  CLASS_CP2RecordEnum: TGUID = '{D444BF81-1360-4836-AFD7-5DF15A3199DE}';
  CLASS___Impl_IP2DataStreamEvents: TGUID = '{31CD73F0-719C-3992-AA71-96149583D1FC}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum TConnectionStatus
type
  TConnectionStatus = TOleEnum;
const
  CS_CONNECTION_DISCONNECTED = $00000001;
  CS_CONNECTION_CONNECTED = $00000002;
  CS_CONNECTION_INVALID = $00000004;
  CS_CONNECTION_BUSY = $00000008;
  CS_ROUTER_DISCONNECTED = $00010000;
  CS_ROUTER_RECONNECTING = $00020000;
  CS_ROUTER_CONNECTED = $00040000;
  CS_ROUTER_LOGINFAILED = $00080000;
  CS_ROUTER_NOCONNECT = $00100000;

// Constants for enum TRequestType
type
  TRequestType = TOleEnum;
const
  RT_LOCAL = $00000000;
  RT_COMBINED_SNAPSHOT = $00000001;
  RT_COMBINED_DYNAMIC = $00000002;
  RT_REMOTE_SNAPSHOT = $00000003;
  RT_REMOVE_DELETED = $00000004;
  RT_REMOTE_ONLINE = $00000008;

// Constants for enum TDataStreamState
type
  TDataStreamState = TOleEnum;
const
  DS_STATE_CLOSE = $00000000;
  DS_STATE_LOCAL_SNAPSHOT = $00000001;
  DS_STATE_REMOTE_SNAPSHOT = $00000002;
  DS_STATE_ONLINE = $00000003;
  DS_STATE_CLOSE_COMPLETE = $00000004;
  DS_STATE_REOPEN = $00000005;
  DS_STATE_ERROR = $00000006;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IP2Connection = interface;
  IP2ConnectionDisp = dispinterface;
  IP2ConnectionEvent = interface;
  IP2ConnectionEventDisp = dispinterface;
  IP2MessageReceiver = interface;
  IP2MessageReceiverDisp = dispinterface;
  IP2AsyncMessageEvents = dispinterface;
  IP2BLMessage = interface;
  IP2BLMessageDisp = dispinterface;
  IP2AsyncSendEvent2 = dispinterface;
  IP2BLMessageFactory = interface;
  IP2BLMessageFactoryDisp = dispinterface;
  IP2TableSet = interface;
  IP2TableSetDisp = dispinterface;
  IP2Record = interface;
  IP2RecordDisp = dispinterface;
  IP2DataStream = interface;
  IP2DataStreamDisp = dispinterface;
  IP2DataStreamEvents = interface;
  IP2DataStreamEventsDisp = dispinterface;
  IP2DataStreamEvents2 = interface;
  IP2DataBuffer = interface;
  IP2DataBufferDisp = dispinterface;
  IP2TableRecords = interface;
  IP2TableRecordsDisp = dispinterface;
  IP2Application = interface;
  IP2ApplicationDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  CP2Connection = IP2Connection;
  CP2BLMessage = IP2BLMessage;
  CP2BLMessageFactory = IP2BLMessageFactory;
  CP2TableSet = IP2TableSet;
  CP2Record = IP2Record;
  CP2DataStream = IP2DataStream;
  CP2DataBuffer = IP2DataBuffer;
  CP2Application = IP2Application;
  CP2TableEnum = IUnknown;
  CP2RecordEnum = IP2TableRecords;
  __Impl_IP2DataStreamEvents = IP2DataStreamEvents;


// *********************************************************************//
// Interface: IP2Connection
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1E7F68E9-DCA1-44E6-91CB-8B8C17D5601B}
// *********************************************************************//
  IP2Connection = interface(IDispatch)
    ['{1E7F68E9-DCA1-44E6-91CB-8B8C17D5601B}']
    function Get_Status: Integer; safecall;
    function Get_AppName: WideString; safecall;
    procedure Set_AppName(const pVal: WideString); safecall;
    function Get_NodeName: WideString; safecall;
    function Get_Host: WideString; safecall;
    procedure Set_Host(const pVal: WideString); safecall;
    function Get_Port: LongWord; safecall;
    procedure Set_Port(pVal: LongWord); safecall;
    procedure Set_Password(const Param1: WideString); safecall;
    function Get_Timeout: LongWord; safecall;
    procedure Set_Timeout(pVal: LongWord); safecall;
    function Get_LoginStr: WideString; safecall;
    procedure Set_LoginStr(const pVal: WideString); safecall;
    function Connect: LongWord; safecall;
    procedure Disconnect; safecall;
    procedure Login; safecall;
    procedure Logout; safecall;
    procedure ProcessMessage(out cookie: LongWord; pollTimeout: LongWord); safecall;
    function RegisterReceiver(const newReceiver: IP2MessageReceiver): LongWord; safecall;
    procedure UnRegisterReceiver(cookie: LongWord); safecall;
    function ResolveService(const service: WideString): WideString; safecall;
    function ProcessMessage2(pollTimeout: LongWord): LongWord; safecall;
    function Connect2(const connStr: WideString): LongWord; safecall;
    function ProcessMessage3(pollTimeout: LongWord): LongWord; safecall;
    function GetObjID: Largeuint; safecall;
    property Status: Integer read Get_Status;
    property AppName: WideString read Get_AppName write Set_AppName;
    property NodeName: WideString read Get_NodeName;
    property Host: WideString read Get_Host write Set_Host;
    property Port: LongWord read Get_Port write Set_Port;
    property Password: WideString write Set_Password;
    property Timeout: LongWord read Get_Timeout write Set_Timeout;
    property LoginStr: WideString read Get_LoginStr write Set_LoginStr;
  end;

// *********************************************************************//
// DispIntf:  IP2ConnectionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1E7F68E9-DCA1-44E6-91CB-8B8C17D5601B}
// *********************************************************************//
  IP2ConnectionDisp = dispinterface
    ['{1E7F68E9-DCA1-44E6-91CB-8B8C17D5601B}']
    property Status: Integer readonly dispid 1;
    property AppName: WideString dispid 2;
    property NodeName: WideString readonly dispid 3;
    property Host: WideString dispid 4;
    property Port: LongWord dispid 5;
    property Password: WideString writeonly dispid 6;
    property Timeout: LongWord dispid 7;
    property LoginStr: WideString dispid 8;
    function Connect: LongWord; dispid 9;
    procedure Disconnect; dispid 10;
    procedure Login; dispid 11;
    procedure Logout; dispid 12;
    procedure ProcessMessage(out cookie: LongWord; pollTimeout: LongWord); dispid 13;
    function RegisterReceiver(const newReceiver: IP2MessageReceiver): LongWord; dispid 14;
    procedure UnRegisterReceiver(cookie: LongWord); dispid 15;
    function ResolveService(const service: WideString): WideString; dispid 16;
    function ProcessMessage2(pollTimeout: LongWord): LongWord; dispid 17;
    function Connect2(const connStr: WideString): LongWord; dispid 18;
    function ProcessMessage3(pollTimeout: LongWord): LongWord; dispid 19;
    function GetObjID: Largeuint; dispid 20;
  end;

// *********************************************************************//
// Interface: IP2ConnectionEvent
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4EF00151-3499-46D3-BC33-9A1763827D89}
// *********************************************************************//
  IP2ConnectionEvent = interface(IDispatch)
    ['{4EF00151-3499-46D3-BC33-9A1763827D89}']
    procedure ConnectionStatusChanged(const conn: IP2Connection; newStatus: TConnectionStatus); safecall;
  end;

// *********************************************************************//
// DispIntf:  IP2ConnectionEventDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {4EF00151-3499-46D3-BC33-9A1763827D89}
// *********************************************************************//
  IP2ConnectionEventDisp = dispinterface
    ['{4EF00151-3499-46D3-BC33-9A1763827D89}']
    procedure ConnectionStatusChanged(const conn: IP2Connection; newStatus: TConnectionStatus); dispid 1;
  end;

// *********************************************************************//
// Interface: IP2MessageReceiver
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {795E97AD-7B4D-4276-AE78-A7443FAC2754}
// *********************************************************************//
  IP2MessageReceiver = interface(IDispatch)
    ['{795E97AD-7B4D-4276-AE78-A7443FAC2754}']
    procedure GetFilter(out from: OleVariant; out type_: OleVariant; out category: OleVariant); safecall;
    procedure PutMessage(const pMsg: IDispatch); safecall;
  end;

// *********************************************************************//
// DispIntf:  IP2MessageReceiverDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {795E97AD-7B4D-4276-AE78-A7443FAC2754}
// *********************************************************************//
  IP2MessageReceiverDisp = dispinterface
    ['{795E97AD-7B4D-4276-AE78-A7443FAC2754}']
    procedure GetFilter(out from: OleVariant; out type_: OleVariant; out category: OleVariant); dispid 1;
    procedure PutMessage(const pMsg: IDispatch); dispid 2;
  end;

// *********************************************************************//
// DispIntf:  IP2AsyncMessageEvents
// Flags:     (4096) Dispatchable
// GUID:      {BEB8ED4A-7EB0-44A0-8F98-9DC9515AB3FF}
// *********************************************************************//
  IP2AsyncMessageEvents = dispinterface
    ['{BEB8ED4A-7EB0-44A0-8F98-9DC9515AB3FF}']
    procedure DeliveryEvent(const reply: IP2BLMessage; errCode: LongWord); dispid 1;
  end;

// *********************************************************************//
// Interface: IP2BLMessage
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A85BC7A3-9CE4-429E-A370-5875DEFEB3E9}
// *********************************************************************//
  IP2BLMessage = interface(IDispatch)
    ['{A85BC7A3-9CE4-429E-A370-5875DEFEB3E9}']
    function Get_Name: WideString; safecall;
    function Get_Id: LongWord; safecall;
    function Get_Version: WideString; safecall;
    function Get_DestAddr: WideString; safecall;
    procedure Set_DestAddr(const pVal: WideString); safecall;
    function Get_Field(const Name: WideString): OleVariant; safecall;
    procedure Set_Field(const Name: WideString; pVal: OleVariant); safecall;
    function Send(const conn: IP2Connection; Timeout: LongWord): IP2BLMessage; safecall;
    procedure Post(const conn: IP2Connection); safecall;
    procedure SendAsync(const conn: IP2Connection; Timeout: LongWord; const event: IDispatch); safecall;
    procedure SendAsync2(const conn: IP2Connection; Timeout: LongWord; const event: IDispatch; 
                         eventParam: Int64); safecall;
    function Get_FieldAsLONGLONG(const Name: WideString): Int64; safecall;
    property Name: WideString read Get_Name;
    property Id: LongWord read Get_Id;
    property Version: WideString read Get_Version;
    property DestAddr: WideString read Get_DestAddr write Set_DestAddr;
    property Field[const Name: WideString]: OleVariant read Get_Field write Set_Field;
    property FieldAsLONGLONG[const Name: WideString]: Int64 read Get_FieldAsLONGLONG;
  end;

// *********************************************************************//
// DispIntf:  IP2BLMessageDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A85BC7A3-9CE4-429E-A370-5875DEFEB3E9}
// *********************************************************************//
  IP2BLMessageDisp = dispinterface
    ['{A85BC7A3-9CE4-429E-A370-5875DEFEB3E9}']
    property Name: WideString readonly dispid 1;
    property Id: LongWord readonly dispid 2;
    property Version: WideString readonly dispid 3;
    property DestAddr: WideString dispid 4;
    property Field[const Name: WideString]: OleVariant dispid 5;
    function Send(const conn: IP2Connection; Timeout: LongWord): IP2BLMessage; dispid 6;
    procedure Post(const conn: IP2Connection); dispid 7;
    procedure SendAsync(const conn: IP2Connection; Timeout: LongWord; const event: IDispatch); dispid 8;
    procedure SendAsync2(const conn: IP2Connection; Timeout: LongWord; const event: IDispatch; 
                         eventParam: Int64); dispid 9;
    property FieldAsLONGLONG[const Name: WideString]: Int64 readonly dispid 10;
  end;

// *********************************************************************//
// DispIntf:  IP2AsyncSendEvent2
// Flags:     (4096) Dispatchable
// GUID:      {98C080E2-7CB1-43C4-B036-DB144AF3BCDB}
// *********************************************************************//
  IP2AsyncSendEvent2 = dispinterface
    ['{98C080E2-7CB1-43C4-B036-DB144AF3BCDB}']
    procedure SendAsync2Reply(const reply: IP2BLMessage; errCode: LongWord; eventParam: Int64); dispid 1;
  end;

// *********************************************************************//
// Interface: IP2BLMessageFactory
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0F3918B5-0FA6-4483-9261-EF64A019FA39}
// *********************************************************************//
  IP2BLMessageFactory = interface(IDispatch)
    ['{0F3918B5-0FA6-4483-9261-EF64A019FA39}']
    procedure Init(const structFile: WideString; const signFile: WideString); safecall;
    function CreateMessageByName(const msgName: WideString): IP2BLMessage; safecall;
    function CreateMessageById(msgId: LongWord): IP2BLMessage; safecall;
  end;

// *********************************************************************//
// DispIntf:  IP2BLMessageFactoryDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {0F3918B5-0FA6-4483-9261-EF64A019FA39}
// *********************************************************************//
  IP2BLMessageFactoryDisp = dispinterface
    ['{0F3918B5-0FA6-4483-9261-EF64A019FA39}']
    procedure Init(const structFile: WideString; const signFile: WideString); dispid 1;
    function CreateMessageByName(const msgName: WideString): IP2BLMessage; dispid 2;
    function CreateMessageById(msgId: LongWord): IP2BLMessage; dispid 3;
  end;

// *********************************************************************//
// Interface: IP2TableSet
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CBE24405-8376-4B25-8C31-F4F4268EC7A1}
// *********************************************************************//
  IP2TableSet = interface(IDispatch)
    ['{CBE24405-8376-4B25-8C31-F4F4268EC7A1}']
    procedure InitFromIni(const structFile: WideString; const signFile: WideString); safecall;
    procedure InitFromDB(const connectString: WideString; const signFile: WideString); safecall;
    procedure AddTable(const tableName: WideString; const fieldlList: WideString; rev: Largeuint); safecall;
    function Get_FieldList(const tableName: WideString): WideString; safecall;
    procedure Set_FieldList(const tableName: WideString; const pVal: WideString); safecall;
    function Get_rev(const tableName: WideString): Int64; safecall;
    procedure Set_rev(const tableName: WideString; pVal: Int64); safecall;
    procedure DeleteTable(const tableName: WideString); safecall;
    function Get_Count: Integer; safecall;
    function _NewEnumBug: IUnknown; safecall;
    function Get_FieldTypes(const tableName: WideString): WideString; safecall;
    procedure InitFromIni2(const iniFileName: WideString; const schemeName: WideString); safecall;
    function Get_LifeNum: Integer; safecall;
    procedure Set_LifeNum(pVal: Integer); safecall;
    procedure SetLifeNumToIni(const iniFileName: WideString); safecall;
    function GetObjID: Largeuint; safecall;
    function _NewEnum: IUnknown; safecall;
    property FieldList[const tableName: WideString]: WideString read Get_FieldList write Set_FieldList;
    property rev[const tableName: WideString]: Int64 read Get_rev write Set_rev;
    property Count: Integer read Get_Count;
    property FieldTypes[const tableName: WideString]: WideString read Get_FieldTypes;
    property LifeNum: Integer read Get_LifeNum write Set_LifeNum;
  end;

// *********************************************************************//
// DispIntf:  IP2TableSetDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {CBE24405-8376-4B25-8C31-F4F4268EC7A1}
// *********************************************************************//
  IP2TableSetDisp = dispinterface
    ['{CBE24405-8376-4B25-8C31-F4F4268EC7A1}']
    procedure InitFromIni(const structFile: WideString; const signFile: WideString); dispid 1;
    procedure InitFromDB(const connectString: WideString; const signFile: WideString); dispid 2;
    procedure AddTable(const tableName: WideString; const fieldlList: WideString; rev: Largeuint); dispid 3;
    property FieldList[const tableName: WideString]: WideString dispid 4;
    property rev[const tableName: WideString]: Int64 dispid 5;
    procedure DeleteTable(const tableName: WideString); dispid 6;
    property Count: Integer readonly dispid 7;
    function _NewEnumBug: IUnknown; dispid 8;
    property FieldTypes[const tableName: WideString]: WideString readonly dispid 9;
    procedure InitFromIni2(const iniFileName: WideString; const schemeName: WideString); dispid 10;
    property LifeNum: Integer dispid 11;
    procedure SetLifeNumToIni(const iniFileName: WideString); dispid 12;
    function GetObjID: Largeuint; dispid 13;
    function _NewEnum: IUnknown; dispid -4;
  end;

// *********************************************************************//
// Interface: IP2Record
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {76033FC9-8BB0-4415-A785-9BF86AAF4E99}
// *********************************************************************//
  IP2Record = interface(IDispatch)
    ['{76033FC9-8BB0-4415-A785-9BF86AAF4E99}']
    function Get_Count: LongWord; safecall;
    function GetValAsString(const fieldName: WideString): WideString; safecall;
    function GetValAsStringByIndex(fieldIndex: LongWord): WideString; safecall;
    function GetValAsLong(const fieldName: WideString): Integer; safecall;
    function GetValAsLongByIndex(fieldIndex: LongWord): Integer; safecall;
    function GetValAsShort(const fieldName: WideString): Smallint; safecall;
    function GetValAsShortByIndex(fieldIndex: LongWord): Smallint; safecall;
    function GetValAsVariant(const fieldName: WideString): OleVariant; safecall;
    function GetValAsVariantByIndex(fieldIndex: LongWord): OleVariant; safecall;
    function GetObjID: Largeuint; safecall;
    property Count: LongWord read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  IP2RecordDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {76033FC9-8BB0-4415-A785-9BF86AAF4E99}
// *********************************************************************//
  IP2RecordDisp = dispinterface
    ['{76033FC9-8BB0-4415-A785-9BF86AAF4E99}']
    property Count: LongWord readonly dispid 1;
    function GetValAsString(const fieldName: WideString): WideString; dispid 2;
    function GetValAsStringByIndex(fieldIndex: LongWord): WideString; dispid 3;
    function GetValAsLong(const fieldName: WideString): Integer; dispid 4;
    function GetValAsLongByIndex(fieldIndex: LongWord): Integer; dispid 5;
    function GetValAsShort(const fieldName: WideString): Smallint; dispid 6;
    function GetValAsShortByIndex(fieldIndex: LongWord): Smallint; dispid 7;
    function GetValAsVariant(const fieldName: WideString): OleVariant; dispid 8;
    function GetValAsVariantByIndex(fieldIndex: LongWord): OleVariant; dispid 9;
    function GetObjID: Largeuint; dispid 10;
  end;

// *********************************************************************//
// Interface: IP2DataStream
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C14F57CF-BEE6-4ADB-85D0-44A307AD9444}
// *********************************************************************//
  IP2DataStream = interface(IDispatch)
    ['{C14F57CF-BEE6-4ADB-85D0-44A307AD9444}']
    function Get_TableSet: IP2TableSet; safecall;
    procedure Set_TableSet(const pVal: IP2TableSet); safecall;
    function Get_StreamName: WideString; safecall;
    procedure Set_StreamName(const pVal: WideString); safecall;
    function Get_DBConnString: WideString; safecall;
    procedure Set_DBConnString(const pVal: WideString); safecall;
    function Get_type_: TRequestType; safecall;
    procedure Set_type_(pVal: TRequestType); safecall;
    function Get_State: TDataStreamState; safecall;
    procedure Open(const conn: IP2Connection); safecall;
    procedure Close; safecall;
    function GetObjID: Largeuint; safecall;
    procedure OpenWithBlob(const conn: IP2Connection; const blob: WideString); safecall;
    procedure OpenWithBlobFile(const conn: IP2Connection; const blobFileName: WideString); safecall;
    function Get_blob: WideString; safecall;
    procedure SaveBlob(const blobFileName: WideString); safecall;
    procedure Resume(const conn: IP2Connection; const blob: WideString); safecall;
    procedure ResumeFromFile(const conn: IP2Connection; const blobFileName: WideString); safecall;
    property TableSet: IP2TableSet read Get_TableSet write Set_TableSet;
    property StreamName: WideString read Get_StreamName write Set_StreamName;
    property DBConnString: WideString read Get_DBConnString write Set_DBConnString;
    property type_: TRequestType read Get_type_ write Set_type_;
    property State: TDataStreamState read Get_State;
    property blob: WideString read Get_blob;
  end;

// *********************************************************************//
// DispIntf:  IP2DataStreamDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C14F57CF-BEE6-4ADB-85D0-44A307AD9444}
// *********************************************************************//
  IP2DataStreamDisp = dispinterface
    ['{C14F57CF-BEE6-4ADB-85D0-44A307AD9444}']
    property TableSet: IP2TableSet dispid 1;
    property StreamName: WideString dispid 2;
    property DBConnString: WideString dispid 3;
    property type_: TRequestType dispid 4;
    property State: TDataStreamState readonly dispid 5;
    procedure Open(const conn: IP2Connection); dispid 6;
    procedure Close; dispid 7;
    function GetObjID: Largeuint; dispid 8;
    procedure OpenWithBlob(const conn: IP2Connection; const blob: WideString); dispid 9;
    procedure OpenWithBlobFile(const conn: IP2Connection; const blobFileName: WideString); dispid 10;
    property blob: WideString readonly dispid 11;
    procedure SaveBlob(const blobFileName: WideString); dispid 12;
    procedure Resume(const conn: IP2Connection; const blob: WideString); dispid 13;
    procedure ResumeFromFile(const conn: IP2Connection; const blobFileName: WideString); dispid 14;
  end;

// *********************************************************************//
// Interface: IP2DataStreamEvents
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7F23A734-2F90-4DC6-9007-E5DC27FDC613}
// *********************************************************************//
  IP2DataStreamEvents = interface(IDispatch)
    ['{7F23A734-2F90-4DC6-9007-E5DC27FDC613}']
    procedure StreamStateChanged(const stream: IP2DataStream; newState: TDataStreamState); safecall;
    procedure StreamDataInserted(const stream: IP2DataStream; const tableName: WideString; 
                                 const rec: IP2Record); safecall;
    procedure StreamDataUpdated(const stream: IP2DataStream; const tableName: WideString; 
                                Id: Int64; const rec: IP2Record); safecall;
    procedure StreamDataDeleted(const stream: IP2DataStream; const tableName: WideString; 
                                Id: Int64; const rec: IP2Record); safecall;
    procedure StreamDatumDeleted(const stream: IP2DataStream; const tableName: WideString; 
                                 rev: Int64); safecall;
    procedure StreamDBWillBeDeleted(const stream: IP2DataStream); safecall;
    procedure StreamLifeNumChanged(const stream: IP2DataStream; LifeNum: Integer); safecall;
    procedure StreamDataBegin(const stream: IP2DataStream); safecall;
    procedure StreamDataEnd(const stream: IP2DataStream); safecall;
  end;

// *********************************************************************//
// DispIntf:  IP2DataStreamEventsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {7F23A734-2F90-4DC6-9007-E5DC27FDC613}
// *********************************************************************//
  IP2DataStreamEventsDisp = dispinterface
    ['{7F23A734-2F90-4DC6-9007-E5DC27FDC613}']
    procedure StreamStateChanged(const stream: IP2DataStream; newState: TDataStreamState); dispid 1;
    procedure StreamDataInserted(const stream: IP2DataStream; const tableName: WideString; 
                                 const rec: IP2Record); dispid 2;
    procedure StreamDataUpdated(const stream: IP2DataStream; const tableName: WideString; 
                                Id: Int64; const rec: IP2Record); dispid 3;
    procedure StreamDataDeleted(const stream: IP2DataStream; const tableName: WideString; 
                                Id: Int64; const rec: IP2Record); dispid 4;
    procedure StreamDatumDeleted(const stream: IP2DataStream; const tableName: WideString; 
                                 rev: Int64); dispid 5;
    procedure StreamDBWillBeDeleted(const stream: IP2DataStream); dispid 6;
    procedure StreamLifeNumChanged(const stream: IP2DataStream; LifeNum: Integer); dispid 7;
    procedure StreamDataBegin(const stream: IP2DataStream); dispid 8;
    procedure StreamDataEnd(const stream: IP2DataStream); dispid 9;
  end;

// *********************************************************************//
// Interface: IP2DataStreamEvents2
// Flags:     (0)
// GUID:      {C0027852-2CDC-41FB-A68F-97DD0F9EE368}
// *********************************************************************//
  IP2DataStreamEvents2 = interface(IUnknown)
    ['{C0027852-2CDC-41FB-A68F-97DD0F9EE368}']
    function StreamStateChanged(const stream: IP2DataStream; newState: TDataStreamState): HResult; stdcall;
    function StreamDataInserted(const stream: IP2DataStream; const tableName: WideString; 
                                const rec: IP2Record): HResult; stdcall;
    function StreamDataUpdated(const stream: IP2DataStream; const tableName: WideString; Id: Int64; 
                               const rec: IP2Record): HResult; stdcall;
    function StreamDataDeleted(const stream: IP2DataStream; const tableName: WideString; Id: Int64; 
                               const rec: IP2Record): HResult; stdcall;
    function StreamDatumDeleted(const stream: IP2DataStream; const tableName: WideString; rev: Int64): HResult; stdcall;
    function StreamDBWillBeDeleted(const stream: IP2DataStream): HResult; stdcall;
    function StreamLifeNumChanged(const stream: IP2DataStream; LifeNum: Integer): HResult; stdcall;
    function StreamDataBegin(const stream: IP2DataStream): HResult; stdcall;
    function StreamDataEnd(const stream: IP2DataStream): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IP2DataBuffer
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {48169C9B-8E7F-427C-836B-8BB168CA8042}
// *********************************************************************//
  IP2DataBuffer = interface(IDispatch)
    ['{48169C9B-8E7F-427C-836B-8BB168CA8042}']
    procedure AttachToStream(const stream: IP2DataStream); safecall;
    procedure DetachFromStream; safecall;
    function CountTables: Integer; safecall;
    function Get_TableRecords(const tableName: WideString): IP2TableRecords; safecall;
    function Count(const tableName: WideString): Integer; safecall;
    procedure Clear(const tableName: WideString); safecall;
    procedure ClearAll; safecall;
    function _NewEnum: IUnknown; safecall;
    property TableRecords[const tableName: WideString]: IP2TableRecords read Get_TableRecords;
  end;

// *********************************************************************//
// DispIntf:  IP2DataBufferDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {48169C9B-8E7F-427C-836B-8BB168CA8042}
// *********************************************************************//
  IP2DataBufferDisp = dispinterface
    ['{48169C9B-8E7F-427C-836B-8BB168CA8042}']
    procedure AttachToStream(const stream: IP2DataStream); dispid 1;
    procedure DetachFromStream; dispid 2;
    function CountTables: Integer; dispid 3;
    property TableRecords[const tableName: WideString]: IP2TableRecords readonly dispid 4;
    function Count(const tableName: WideString): Integer; dispid 5;
    procedure Clear(const tableName: WideString); dispid 6;
    procedure ClearAll; dispid 8;
    function _NewEnum: IUnknown; dispid -4;
  end;

// *********************************************************************//
// Interface: IP2TableRecords
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3A19BD02-482B-4A7A-AE9F-AD879BE573C8}
// *********************************************************************//
  IP2TableRecords = interface(IDispatch)
    ['{3A19BD02-482B-4A7A-AE9F-AD879BE573C8}']
    function _NewEnum: IUnknown; safecall;
  end;

// *********************************************************************//
// DispIntf:  IP2TableRecordsDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {3A19BD02-482B-4A7A-AE9F-AD879BE573C8}
// *********************************************************************//
  IP2TableRecordsDisp = dispinterface
    ['{3A19BD02-482B-4A7A-AE9F-AD879BE573C8}']
    function _NewEnum: IUnknown; dispid -4;
  end;

// *********************************************************************//
// Interface: IP2Application
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1F5B9148-F21F-4980-A5D5-23CD5F9CA0A5}
// *********************************************************************//
  IP2Application = interface(IDispatch)
    ['{1F5B9148-F21F-4980-A5D5-23CD5F9CA0A5}']
    procedure StartUp(const iniFileName: WideString); safecall;
    procedure CleanUp; safecall;
    function Get_ParserType: LongWord; safecall;
    procedure Set_ParserType(pVal: LongWord); safecall;
    function CreateP2Connection: IP2Connection; safecall;
    function CreateP2BLMessage: IP2BLMessage; safecall;
    function CreateP2BLMessageFactory: IP2BLMessageFactory; safecall;
    function CreateP2DataBuffer: IP2DataBuffer; safecall;
    function CreateP2DataStream: IP2DataStream; safecall;
    function CreateP2TableSet: IP2TableSet; safecall;
    property ParserType: LongWord read Get_ParserType write Set_ParserType;
  end;

// *********************************************************************//
// DispIntf:  IP2ApplicationDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1F5B9148-F21F-4980-A5D5-23CD5F9CA0A5}
// *********************************************************************//
  IP2ApplicationDisp = dispinterface
    ['{1F5B9148-F21F-4980-A5D5-23CD5F9CA0A5}']
    procedure StartUp(const iniFileName: WideString); dispid 1;
    procedure CleanUp; dispid 2;
    property ParserType: LongWord dispid 3;
    function CreateP2Connection: IP2Connection; dispid 4;
    function CreateP2BLMessage: IP2BLMessage; dispid 5;
    function CreateP2BLMessageFactory: IP2BLMessageFactory; dispid 6;
    function CreateP2DataBuffer: IP2DataBuffer; dispid 7;
    function CreateP2DataStream: IP2DataStream; dispid 8;
    function CreateP2TableSet: IP2TableSet; dispid 9;
  end;

// *********************************************************************//
// The Class CoCP2Connection provides a Create and CreateRemote method to          
// create instances of the default interface IP2Connection exposed by              
// the CoClass CP2Connection. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCP2Connection = class
    class function Create: IP2Connection;
    class function CreateRemote(const MachineName: string): IP2Connection;
  end;

  TCP2ConnectionConnectionStatusChanged = procedure(ASender: TObject; const conn: IP2Connection; 
                                                                      newStatus: TConnectionStatus) of object;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCP2Connection
// Help String      : P2ConnectionMTA Class
// Default Interface: IP2Connection
// Def. Intf. DISP? : No
// Event   Interface: IP2ConnectionEvent
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TCP2Connection = class(TOleServer)
  private
    FOnConnectionStatusChanged: TCP2ConnectionConnectionStatusChanged;
    FIntf: IP2Connection;
    function GetDefaultInterface: IP2Connection;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function Get_Status: Integer;
    function Get_AppName: WideString;
    procedure Set_AppName(const pVal: WideString);
    function Get_NodeName: WideString;
    function Get_Host: WideString;
    procedure Set_Host(const pVal: WideString);
    function Get_Port: LongWord;
    procedure Set_Port(pVal: LongWord);
    procedure Set_Password(const Param1: WideString);
    function Get_Timeout: LongWord;
    procedure Set_Timeout(pVal: LongWord);
    function Get_LoginStr: WideString;
    procedure Set_LoginStr(const pVal: WideString);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IP2Connection);
    procedure Disconnect; override;
    function Connect1: LongWord;
    procedure Disconnect1;
    procedure Login;
    procedure Logout;
    procedure ProcessMessage(out cookie: LongWord; pollTimeout: LongWord);
    function RegisterReceiver(const newReceiver: IP2MessageReceiver): LongWord;
    procedure UnRegisterReceiver(cookie: LongWord);
    function ResolveService(const service: WideString): WideString;
    function ProcessMessage2(pollTimeout: LongWord): LongWord;
    function Connect2(const connStr: WideString): LongWord;
    function ProcessMessage3(pollTimeout: LongWord): LongWord;
    function GetObjID: Largeuint;
    property DefaultInterface: IP2Connection read GetDefaultInterface;
    property Status: Integer read Get_Status;
    property NodeName: WideString read Get_NodeName;
    property Password: WideString write Set_Password;
    property AppName: WideString read Get_AppName write Set_AppName;
    property Host: WideString read Get_Host write Set_Host;
    property Port: LongWord read Get_Port write Set_Port;
    property Timeout: LongWord read Get_Timeout write Set_Timeout;
    property LoginStr: WideString read Get_LoginStr write Set_LoginStr;
  published
    property OnConnectionStatusChanged: TCP2ConnectionConnectionStatusChanged read FOnConnectionStatusChanged write FOnConnectionStatusChanged;
  end;

// *********************************************************************//
// The Class CoCP2BLMessage provides a Create and CreateRemote method to          
// create instances of the default interface IP2BLMessage exposed by              
// the CoClass CP2BLMessage. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCP2BLMessage = class
    class function Create: IP2BLMessage;
    class function CreateRemote(const MachineName: string): IP2BLMessage;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCP2BLMessage
// Help String      : P2BLMessageMTA Class
// Default Interface: IP2BLMessage
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TCP2BLMessage = class(TOleServer)
  private
    FIntf: IP2BLMessage;
    function GetDefaultInterface: IP2BLMessage;
  protected
    procedure InitServerData; override;
    function Get_Name: WideString;
    function Get_Id: LongWord;
    function Get_Version: WideString;
    function Get_DestAddr: WideString;
    procedure Set_DestAddr(const pVal: WideString);
    function Get_Field(const Name: WideString): OleVariant;
    procedure Set_Field(const Name: WideString; pVal: OleVariant);
    function Get_FieldAsLONGLONG(const Name: WideString): Int64;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IP2BLMessage);
    procedure Disconnect; override;
    function Send(const conn: IP2Connection; Timeout: LongWord): IP2BLMessage;
    procedure Post(const conn: IP2Connection);
    procedure SendAsync(const conn: IP2Connection; Timeout: LongWord; const event: IDispatch);
    procedure SendAsync2(const conn: IP2Connection; Timeout: LongWord; const event: IDispatch; 
                         eventParam: Int64);
    property DefaultInterface: IP2BLMessage read GetDefaultInterface;
    property Name: WideString read Get_Name;
    property Id: LongWord read Get_Id;
    property Version: WideString read Get_Version;
    property Field[const Name: WideString]: OleVariant read Get_Field write Set_Field;
    property FieldAsLONGLONG[const Name: WideString]: Int64 read Get_FieldAsLONGLONG;
    property DestAddr: WideString read Get_DestAddr write Set_DestAddr;
  published
  end;

// *********************************************************************//
// The Class CoCP2BLMessageFactory provides a Create and CreateRemote method to          
// create instances of the default interface IP2BLMessageFactory exposed by              
// the CoClass CP2BLMessageFactory. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCP2BLMessageFactory = class
    class function Create: IP2BLMessageFactory;
    class function CreateRemote(const MachineName: string): IP2BLMessageFactory;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCP2BLMessageFactory
// Help String      : P2BLMessageFactoryMTA Class
// Default Interface: IP2BLMessageFactory
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TCP2BLMessageFactory = class(TOleServer)
  private
    FIntf: IP2BLMessageFactory;
    function GetDefaultInterface: IP2BLMessageFactory;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IP2BLMessageFactory);
    procedure Disconnect; override;
    procedure Init(const structFile: WideString; const signFile: WideString);
    function CreateMessageByName(const msgName: WideString): IP2BLMessage;
    function CreateMessageById(msgId: LongWord): IP2BLMessage;
    property DefaultInterface: IP2BLMessageFactory read GetDefaultInterface;
  published
  end;

// *********************************************************************//
// The Class CoCP2TableSet provides a Create and CreateRemote method to          
// create instances of the default interface IP2TableSet exposed by              
// the CoClass CP2TableSet. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCP2TableSet = class
    class function Create: IP2TableSet;
    class function CreateRemote(const MachineName: string): IP2TableSet;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCP2TableSet
// Help String      : P2TableSetMTA Class
// Default Interface: IP2TableSet
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TCP2TableSet = class(TOleServer)
  private
    FIntf: IP2TableSet;
    function GetDefaultInterface: IP2TableSet;
  protected
    procedure InitServerData; override;
    function Get_FieldList(const tableName: WideString): WideString;
    procedure Set_FieldList(const tableName: WideString; const pVal: WideString);
    function Get_rev(const tableName: WideString): Int64;
    procedure Set_rev(const tableName: WideString; pVal: Int64);
    function Get_Count: Integer;
    function Get_FieldTypes(const tableName: WideString): WideString;
    function Get_LifeNum: Integer;
    procedure Set_LifeNum(pVal: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IP2TableSet);
    procedure Disconnect; override;
    procedure InitFromIni(const structFile: WideString; const signFile: WideString);
    procedure InitFromDB(const connectString: WideString; const signFile: WideString);
    procedure AddTable(const tableName: WideString; const fieldlList: WideString; rev: Largeuint);
    procedure DeleteTable(const tableName: WideString);
    procedure InitFromIni2(const iniFileName: WideString; const schemeName: WideString);
    procedure SetLifeNumToIni(const iniFileName: WideString);
    function GetObjID: Largeuint;
    property DefaultInterface: IP2TableSet read GetDefaultInterface;
    property FieldList[const tableName: WideString]: WideString read Get_FieldList write Set_FieldList;
    property rev[const tableName: WideString]: Int64 read Get_rev write Set_rev;
    property Count: Integer read Get_Count;
    property FieldTypes[const tableName: WideString]: WideString read Get_FieldTypes;
    property LifeNum: Integer read Get_LifeNum write Set_LifeNum;
  published
  end;

// *********************************************************************//
// The Class CoCP2Record provides a Create and CreateRemote method to          
// create instances of the default interface IP2Record exposed by              
// the CoClass CP2Record. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCP2Record = class
    class function Create: IP2Record;
    class function CreateRemote(const MachineName: string): IP2Record;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCP2Record
// Help String      : P2RecordMTA Class
// Default Interface: IP2Record
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (0)
// *********************************************************************//
  TCP2Record = class(TOleServer)
  private
    FIntf: IP2Record;
    function GetDefaultInterface: IP2Record;
  protected
    procedure InitServerData; override;
    function Get_Count: LongWord;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IP2Record);
    procedure Disconnect; override;
    function GetValAsString(const fieldName: WideString): WideString;
    function GetValAsStringByIndex(fieldIndex: LongWord): WideString;
    function GetValAsLong(const fieldName: WideString): Integer;
    function GetValAsLongByIndex(fieldIndex: LongWord): Integer;
    function GetValAsShort(const fieldName: WideString): Smallint;
    function GetValAsShortByIndex(fieldIndex: LongWord): Smallint;
    function GetValAsVariant(const fieldName: WideString): OleVariant;
    function GetValAsVariantByIndex(fieldIndex: LongWord): OleVariant;
    function GetObjID: Largeuint;
    property DefaultInterface: IP2Record read GetDefaultInterface;
    property Count: LongWord read Get_Count;
  published
  end;

// *********************************************************************//
// The Class CoCP2DataStream provides a Create and CreateRemote method to          
// create instances of the default interface IP2DataStream exposed by              
// the CoClass CP2DataStream. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCP2DataStream = class
    class function Create: IP2DataStream;
    class function CreateRemote(const MachineName: string): IP2DataStream;
  end;

  TCP2DataStreamStreamStateChanged = procedure(ASender: TObject; const stream: IP2DataStream; 
                                                                 newState: TDataStreamState) of object;
  TCP2DataStreamStreamDataInserted = procedure(ASender: TObject; const stream: IP2DataStream; 
                                                                 const tableName: WideString; 
                                                                 const rec: IP2Record) of object;
  TCP2DataStreamStreamDataUpdated = procedure(ASender: TObject; const stream: IP2DataStream; 
                                                                const tableName: WideString; 
                                                                Id: Int64; const rec: IP2Record) of object;
  TCP2DataStreamStreamDataDeleted = procedure(ASender: TObject; const stream: IP2DataStream; 
                                                                const tableName: WideString; 
                                                                Id: Int64; const rec: IP2Record) of object;
  TCP2DataStreamStreamDatumDeleted = procedure(ASender: TObject; const stream: IP2DataStream; 
                                                                 const tableName: WideString; 
                                                                 rev: Int64) of object;
  TCP2DataStreamStreamDBWillBeDeleted = procedure(ASender: TObject; const stream: IP2DataStream) of object;
  TCP2DataStreamStreamLifeNumChanged = procedure(ASender: TObject; const stream: IP2DataStream; 
                                                                   LifeNum: Integer) of object;
  TCP2DataStreamStreamDataBegin = procedure(ASender: TObject; const stream: IP2DataStream) of object;
  TCP2DataStreamStreamDataEnd = procedure(ASender: TObject; const stream: IP2DataStream) of object;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCP2DataStream
// Help String      : P2DataStreamMTA Class
// Default Interface: IP2DataStream
// Def. Intf. DISP? : No
// Event   Interface: IP2DataStreamEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TCP2DataStream = class(TOleServer)
  private
    FOnStreamStateChanged: TCP2DataStreamStreamStateChanged;
    FOnStreamDataInserted: TCP2DataStreamStreamDataInserted;
    FOnStreamDataUpdated: TCP2DataStreamStreamDataUpdated;
    FOnStreamDataDeleted: TCP2DataStreamStreamDataDeleted;
    FOnStreamDatumDeleted: TCP2DataStreamStreamDatumDeleted;
    FOnStreamDBWillBeDeleted: TCP2DataStreamStreamDBWillBeDeleted;
    FOnStreamLifeNumChanged: TCP2DataStreamStreamLifeNumChanged;
    FOnStreamDataBegin: TCP2DataStreamStreamDataBegin;
    FOnStreamDataEnd: TCP2DataStreamStreamDataEnd;
    FIntf: IP2DataStream;
    function GetDefaultInterface: IP2DataStream;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function Get_TableSet: IP2TableSet;
    procedure Set_TableSet(const pVal: IP2TableSet);
    function Get_StreamName: WideString;
    procedure Set_StreamName(const pVal: WideString);
    function Get_DBConnString: WideString;
    procedure Set_DBConnString(const pVal: WideString);
    function Get_type_: TRequestType;
    procedure Set_type_(pVal: TRequestType);
    function Get_State: TDataStreamState;
    function Get_blob: WideString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IP2DataStream);
    procedure Disconnect; override;
    procedure Open(const conn: IP2Connection);
    procedure Close;
    function GetObjID: Largeuint;
    procedure OpenWithBlob(const conn: IP2Connection; const blob: WideString);
    procedure OpenWithBlobFile(const conn: IP2Connection; const blobFileName: WideString);
    procedure SaveBlob(const blobFileName: WideString);
    procedure Resume(const conn: IP2Connection; const blob: WideString);
    procedure ResumeFromFile(const conn: IP2Connection; const blobFileName: WideString);
    property DefaultInterface: IP2DataStream read GetDefaultInterface;
    property State: TDataStreamState read Get_State;
    property blob: WideString read Get_blob;
    property TableSet: IP2TableSet read Get_TableSet write Set_TableSet;
    property StreamName: WideString read Get_StreamName write Set_StreamName;
    property DBConnString: WideString read Get_DBConnString write Set_DBConnString;
    property type_: TRequestType read Get_type_ write Set_type_;
  published
    property OnStreamStateChanged: TCP2DataStreamStreamStateChanged read FOnStreamStateChanged write FOnStreamStateChanged;
    property OnStreamDataInserted: TCP2DataStreamStreamDataInserted read FOnStreamDataInserted write FOnStreamDataInserted;
    property OnStreamDataUpdated: TCP2DataStreamStreamDataUpdated read FOnStreamDataUpdated write FOnStreamDataUpdated;
    property OnStreamDataDeleted: TCP2DataStreamStreamDataDeleted read FOnStreamDataDeleted write FOnStreamDataDeleted;
    property OnStreamDatumDeleted: TCP2DataStreamStreamDatumDeleted read FOnStreamDatumDeleted write FOnStreamDatumDeleted;
    property OnStreamDBWillBeDeleted: TCP2DataStreamStreamDBWillBeDeleted read FOnStreamDBWillBeDeleted write FOnStreamDBWillBeDeleted;
    property OnStreamLifeNumChanged: TCP2DataStreamStreamLifeNumChanged read FOnStreamLifeNumChanged write FOnStreamLifeNumChanged;
    property OnStreamDataBegin: TCP2DataStreamStreamDataBegin read FOnStreamDataBegin write FOnStreamDataBegin;
    property OnStreamDataEnd: TCP2DataStreamStreamDataEnd read FOnStreamDataEnd write FOnStreamDataEnd;
  end;

// *********************************************************************//
// The Class CoCP2DataBuffer provides a Create and CreateRemote method to          
// create instances of the default interface IP2DataBuffer exposed by              
// the CoClass CP2DataBuffer. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCP2DataBuffer = class
    class function Create: IP2DataBuffer;
    class function CreateRemote(const MachineName: string): IP2DataBuffer;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCP2DataBuffer
// Help String      : P2DataBufferMTA Class
// Default Interface: IP2DataBuffer
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TCP2DataBuffer = class(TOleServer)
  private
    FIntf: IP2DataBuffer;
    function GetDefaultInterface: IP2DataBuffer;
  protected
    procedure InitServerData; override;
    function Get_TableRecords(const tableName: WideString): IP2TableRecords;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IP2DataBuffer);
    procedure Disconnect; override;
    procedure AttachToStream(const stream: IP2DataStream);
    procedure DetachFromStream;
    function CountTables: Integer;
    function Count(const tableName: WideString): Integer;
    procedure Clear(const tableName: WideString);
    procedure ClearAll;
    property DefaultInterface: IP2DataBuffer read GetDefaultInterface;
    property TableRecords[const tableName: WideString]: IP2TableRecords read Get_TableRecords;
  published
  end;

// *********************************************************************//
// The Class CoCP2Application provides a Create and CreateRemote method to          
// create instances of the default interface IP2Application exposed by              
// the CoClass CP2Application. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCP2Application = class
    class function Create: IP2Application;
    class function CreateRemote(const MachineName: string): IP2Application;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCP2Application
// Help String      : P2ApplicationMTA Class
// Default Interface: IP2Application
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
  TCP2Application = class(TOleServer)
  private
    FIntf: IP2Application;
    function GetDefaultInterface: IP2Application;
  protected
    procedure InitServerData; override;
    function Get_ParserType: LongWord;
    procedure Set_ParserType(pVal: LongWord);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IP2Application);
    procedure Disconnect; override;
    procedure StartUp(const iniFileName: WideString);
    procedure CleanUp;
    function CreateP2Connection: IP2Connection;
    function CreateP2BLMessage: IP2BLMessage;
    function CreateP2BLMessageFactory: IP2BLMessageFactory;
    function CreateP2DataBuffer: IP2DataBuffer;
    function CreateP2DataStream: IP2DataStream;
    function CreateP2TableSet: IP2TableSet;
    property DefaultInterface: IP2Application read GetDefaultInterface;
    property ParserType: LongWord read Get_ParserType write Set_ParserType;
  published
  end;

// *********************************************************************//
// The Class CoCP2TableEnum provides a Create and CreateRemote method to          
// create instances of the default interface IUnknown exposed by              
// the CoClass CP2TableEnum. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCP2TableEnum = class
    class function Create: IUnknown;
    class function CreateRemote(const MachineName: string): IUnknown;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCP2TableEnum
// Help String      : P2TableEnumMTA Class
// Default Interface: IUnknown
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (0)
// *********************************************************************//
  TCP2TableEnum = class(TOleServer)
  private
    FIntf: IUnknown;
    function GetDefaultInterface: IUnknown;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IUnknown);
    procedure Disconnect; override;
    property DefaultInterface: IUnknown read GetDefaultInterface;
  published
  end;

// *********************************************************************//
// The Class CoCP2RecordEnum provides a Create and CreateRemote method to          
// create instances of the default interface IP2TableRecords exposed by              
// the CoClass CP2RecordEnum. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCP2RecordEnum = class
    class function Create: IP2TableRecords;
    class function CreateRemote(const MachineName: string): IP2TableRecords;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCP2RecordEnum
// Help String      : P2RecordEnumMTA Class
// Default Interface: IP2TableRecords
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (0)
// *********************************************************************//
  TCP2RecordEnum = class(TOleServer)
  private
    FIntf: IP2TableRecords;
    function GetDefaultInterface: IP2TableRecords;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IP2TableRecords);
    procedure Disconnect; override;
    property DefaultInterface: IP2TableRecords read GetDefaultInterface;
  published
  end;

// *********************************************************************//
// The Class Co__Impl_IP2DataStreamEvents provides a Create and CreateRemote method to          
// create instances of the default interface IP2DataStreamEvents exposed by              
// the CoClass __Impl_IP2DataStreamEvents. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  Co__Impl_IP2DataStreamEvents = class
    class function Create: IP2DataStreamEvents;
    class function CreateRemote(const MachineName: string): IP2DataStreamEvents;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : T__Impl_IP2DataStreamEvents
// Help String      : 
// Default Interface: IP2DataStreamEvents
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (0)
// *********************************************************************//
  T__Impl_IP2DataStreamEvents = class(TOleServer)
  private
    FIntf: IP2DataStreamEvents;
    function GetDefaultInterface: IP2DataStreamEvents;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IP2DataStreamEvents);
    procedure Disconnect; override;
    procedure StreamStateChanged(const stream: IP2DataStream; newState: TDataStreamState);
    procedure StreamDataInserted(const stream: IP2DataStream; const tableName: WideString; 
                                 const rec: IP2Record);
    procedure StreamDataUpdated(const stream: IP2DataStream; const tableName: WideString; 
                                Id: Int64; const rec: IP2Record);
    procedure StreamDataDeleted(const stream: IP2DataStream; const tableName: WideString; 
                                Id: Int64; const rec: IP2Record);
    procedure StreamDatumDeleted(const stream: IP2DataStream; const tableName: WideString; 
                                 rev: Int64);
    procedure StreamDBWillBeDeleted(const stream: IP2DataStream);
    procedure StreamLifeNumChanged(const stream: IP2DataStream; LifeNum: Integer);
    procedure StreamDataBegin(const stream: IP2DataStream);
    procedure StreamDataEnd(const stream: IP2DataStream);
    property DefaultInterface: IP2DataStreamEvents read GetDefaultInterface;
  published
  end;

procedure Register;

resourcestring
  dtlServerPage = 'ActiveX';

  dtlOcxPage = 'ActiveX';

implementation

uses ComObj;

class function CoCP2Connection.Create: IP2Connection;
begin
  Result := CreateComObject(CLASS_CP2Connection) as IP2Connection;
end;

class function CoCP2Connection.CreateRemote(const MachineName: string): IP2Connection;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CP2Connection) as IP2Connection;
end;

procedure TCP2Connection.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{43FB494A-620B-4588-A9DD-DB0BE4B6694A}';
    IntfIID:   '{1E7F68E9-DCA1-44E6-91CB-8B8C17D5601B}';
    EventIID:  '{4EF00151-3499-46D3-BC33-9A1763827D89}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCP2Connection.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as IP2Connection;
  end;
end;

procedure TCP2Connection.ConnectTo(svrIntf: IP2Connection);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TCP2Connection.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TCP2Connection.GetDefaultInterface: IP2Connection;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCP2Connection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCP2Connection.Destroy;
begin
  inherited Destroy;
end;

procedure TCP2Connection.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    1: if Assigned(FOnConnectionStatusChanged) then
         FOnConnectionStatusChanged(Self,
                                    IUnknown(TVarData(Params[0]).VPointer) as IP2Connection {const IP2Connection},
                                    Params[1] {TConnectionStatus});
  end; {case DispID}
end;

function TCP2Connection.Get_Status: Integer;
begin
  Result := DefaultInterface.Status;
end;

function TCP2Connection.Get_AppName: WideString;
begin
  Result := DefaultInterface.AppName;
end;

procedure TCP2Connection.Set_AppName(const pVal: WideString);
begin
  DefaultInterface.AppName := pVal;
end;

function TCP2Connection.Get_NodeName: WideString;
begin
  Result := DefaultInterface.NodeName;
end;

function TCP2Connection.Get_Host: WideString;
begin
  Result := DefaultInterface.Host;
end;

procedure TCP2Connection.Set_Host(const pVal: WideString);
begin
  DefaultInterface.Host := pVal;
end;

function TCP2Connection.Get_Port: LongWord;
begin
  Result := DefaultInterface.Port;
end;

procedure TCP2Connection.Set_Port(pVal: LongWord);
begin
  DefaultInterface.Port := pVal;
end;

procedure TCP2Connection.Set_Password(const Param1: WideString);
begin
  DefaultInterface.Password := Param1;
end;

function TCP2Connection.Get_Timeout: LongWord;
begin
  Result := DefaultInterface.Timeout;
end;

procedure TCP2Connection.Set_Timeout(pVal: LongWord);
begin
  DefaultInterface.Timeout := pVal;
end;

function TCP2Connection.Get_LoginStr: WideString;
begin
  Result := DefaultInterface.LoginStr;
end;

procedure TCP2Connection.Set_LoginStr(const pVal: WideString);
begin
  DefaultInterface.LoginStr := pVal;
end;

function TCP2Connection.Connect1: LongWord;
begin
  Result := DefaultInterface.Connect;
end;

procedure TCP2Connection.Disconnect1;
begin
  DefaultInterface.Disconnect;
end;

procedure TCP2Connection.Login;
begin
  DefaultInterface.Login;
end;

procedure TCP2Connection.Logout;
begin
  DefaultInterface.Logout;
end;

procedure TCP2Connection.ProcessMessage(out cookie: LongWord; pollTimeout: LongWord);
begin
  DefaultInterface.ProcessMessage(cookie, pollTimeout);
end;

function TCP2Connection.RegisterReceiver(const newReceiver: IP2MessageReceiver): LongWord;
begin
  Result := DefaultInterface.RegisterReceiver(newReceiver);
end;

procedure TCP2Connection.UnRegisterReceiver(cookie: LongWord);
begin
  DefaultInterface.UnRegisterReceiver(cookie);
end;

function TCP2Connection.ResolveService(const service: WideString): WideString;
begin
  Result := DefaultInterface.ResolveService(service);
end;

function TCP2Connection.ProcessMessage2(pollTimeout: LongWord): LongWord;
begin
  Result := DefaultInterface.ProcessMessage2(pollTimeout);
end;

function TCP2Connection.Connect2(const connStr: WideString): LongWord;
begin
  Result := DefaultInterface.Connect2(connStr);
end;

function TCP2Connection.ProcessMessage3(pollTimeout: LongWord): LongWord;
begin
  Result := DefaultInterface.ProcessMessage3(pollTimeout);
end;

function TCP2Connection.GetObjID: Largeuint;
begin
  Result := DefaultInterface.GetObjID;
end;

class function CoCP2BLMessage.Create: IP2BLMessage;
begin
  Result := CreateComObject(CLASS_CP2BLMessage) as IP2BLMessage;
end;

class function CoCP2BLMessage.CreateRemote(const MachineName: string): IP2BLMessage;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CP2BLMessage) as IP2BLMessage;
end;

procedure TCP2BLMessage.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{2281C925-902A-4612-B80D-414ED8F62C0D}';
    IntfIID:   '{A85BC7A3-9CE4-429E-A370-5875DEFEB3E9}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCP2BLMessage.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IP2BLMessage;
  end;
end;

procedure TCP2BLMessage.ConnectTo(svrIntf: IP2BLMessage);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCP2BLMessage.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCP2BLMessage.GetDefaultInterface: IP2BLMessage;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCP2BLMessage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCP2BLMessage.Destroy;
begin
  inherited Destroy;
end;

function TCP2BLMessage.Get_Name: WideString;
begin
  Result := DefaultInterface.Name;
end;

function TCP2BLMessage.Get_Id: LongWord;
begin
  Result := DefaultInterface.Id;
end;

function TCP2BLMessage.Get_Version: WideString;
begin
  Result := DefaultInterface.Version;
end;

function TCP2BLMessage.Get_DestAddr: WideString;
begin
  Result := DefaultInterface.DestAddr;
end;

procedure TCP2BLMessage.Set_DestAddr(const pVal: WideString);
begin
  DefaultInterface.DestAddr := pVal;
end;

function TCP2BLMessage.Get_Field(const Name: WideString): OleVariant;
begin
  Result := DefaultInterface.Field[Name];
end;

procedure TCP2BLMessage.Set_Field(const Name: WideString; pVal: OleVariant);
begin
  DefaultInterface.Field[Name] := pVal;
end;

function TCP2BLMessage.Get_FieldAsLONGLONG(const Name: WideString): Int64;
begin
  Result := DefaultInterface.FieldAsLONGLONG[Name];
end;

function TCP2BLMessage.Send(const conn: IP2Connection; Timeout: LongWord): IP2BLMessage;
begin
  Result := DefaultInterface.Send(conn, Timeout);
end;

procedure TCP2BLMessage.Post(const conn: IP2Connection);
begin
  DefaultInterface.Post(conn);
end;

procedure TCP2BLMessage.SendAsync(const conn: IP2Connection; Timeout: LongWord; 
                                  const event: IDispatch);
begin
  DefaultInterface.SendAsync(conn, Timeout, event);
end;

procedure TCP2BLMessage.SendAsync2(const conn: IP2Connection; Timeout: LongWord; 
                                   const event: IDispatch; eventParam: Int64);
begin
  DefaultInterface.SendAsync2(conn, Timeout, event, eventParam);
end;

class function CoCP2BLMessageFactory.Create: IP2BLMessageFactory;
begin
  Result := CreateComObject(CLASS_CP2BLMessageFactory) as IP2BLMessageFactory;
end;

class function CoCP2BLMessageFactory.CreateRemote(const MachineName: string): IP2BLMessageFactory;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CP2BLMessageFactory) as IP2BLMessageFactory;
end;

procedure TCP2BLMessageFactory.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{EAB94E8C-7106-42F7-BB04-C57ACD895DE3}';
    IntfIID:   '{0F3918B5-0FA6-4483-9261-EF64A019FA39}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCP2BLMessageFactory.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IP2BLMessageFactory;
  end;
end;

procedure TCP2BLMessageFactory.ConnectTo(svrIntf: IP2BLMessageFactory);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCP2BLMessageFactory.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCP2BLMessageFactory.GetDefaultInterface: IP2BLMessageFactory;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCP2BLMessageFactory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCP2BLMessageFactory.Destroy;
begin
  inherited Destroy;
end;

procedure TCP2BLMessageFactory.Init(const structFile: WideString; const signFile: WideString);
begin
  DefaultInterface.Init(structFile, signFile);
end;

function TCP2BLMessageFactory.CreateMessageByName(const msgName: WideString): IP2BLMessage;
begin
  Result := DefaultInterface.CreateMessageByName(msgName);
end;

function TCP2BLMessageFactory.CreateMessageById(msgId: LongWord): IP2BLMessage;
begin
  Result := DefaultInterface.CreateMessageById(msgId);
end;

class function CoCP2TableSet.Create: IP2TableSet;
begin
  Result := CreateComObject(CLASS_CP2TableSet) as IP2TableSet;
end;

class function CoCP2TableSet.CreateRemote(const MachineName: string): IP2TableSet;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CP2TableSet) as IP2TableSet;
end;

procedure TCP2TableSet.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{7AA8F6D7-8375-4345-9010-6F0DB3E08A51}';
    IntfIID:   '{CBE24405-8376-4B25-8C31-F4F4268EC7A1}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCP2TableSet.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IP2TableSet;
  end;
end;

procedure TCP2TableSet.ConnectTo(svrIntf: IP2TableSet);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCP2TableSet.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCP2TableSet.GetDefaultInterface: IP2TableSet;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCP2TableSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCP2TableSet.Destroy;
begin
  inherited Destroy;
end;

function TCP2TableSet.Get_FieldList(const tableName: WideString): WideString;
begin
  Result := DefaultInterface.FieldList[tableName];
end;

procedure TCP2TableSet.Set_FieldList(const tableName: WideString; const pVal: WideString);
begin
  DefaultInterface.FieldList[tableName] := pVal;
end;

function TCP2TableSet.Get_rev(const tableName: WideString): Int64;
begin
  Result := DefaultInterface.rev[tableName];
end;

procedure TCP2TableSet.Set_rev(const tableName: WideString; pVal: Int64);
begin
  DefaultInterface.rev[tableName] := pVal;
end;

function TCP2TableSet.Get_Count: Integer;
begin
  Result := DefaultInterface.Count;
end;

function TCP2TableSet.Get_FieldTypes(const tableName: WideString): WideString;
begin
  Result := DefaultInterface.FieldTypes[tableName];
end;

function TCP2TableSet.Get_LifeNum: Integer;
begin
  Result := DefaultInterface.LifeNum;
end;

procedure TCP2TableSet.Set_LifeNum(pVal: Integer);
begin
  DefaultInterface.LifeNum := pVal;
end;

procedure TCP2TableSet.InitFromIni(const structFile: WideString; const signFile: WideString);
begin
  DefaultInterface.InitFromIni(structFile, signFile);
end;

procedure TCP2TableSet.InitFromDB(const connectString: WideString; const signFile: WideString);
begin
  DefaultInterface.InitFromDB(connectString, signFile);
end;

procedure TCP2TableSet.AddTable(const tableName: WideString; const fieldlList: WideString; 
                                rev: Largeuint);
begin
  DefaultInterface.AddTable(tableName, fieldlList, rev);
end;

procedure TCP2TableSet.DeleteTable(const tableName: WideString);
begin
  DefaultInterface.DeleteTable(tableName);
end;

procedure TCP2TableSet.InitFromIni2(const iniFileName: WideString; const schemeName: WideString);
begin
  DefaultInterface.InitFromIni2(iniFileName, schemeName);
end;

procedure TCP2TableSet.SetLifeNumToIni(const iniFileName: WideString);
begin
  DefaultInterface.SetLifeNumToIni(iniFileName);
end;

function TCP2TableSet.GetObjID: Largeuint;
begin
  Result := DefaultInterface.GetObjID;
end;

class function CoCP2Record.Create: IP2Record;
begin
  Result := CreateComObject(CLASS_CP2Record) as IP2Record;
end;

class function CoCP2Record.CreateRemote(const MachineName: string): IP2Record;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CP2Record) as IP2Record;
end;

procedure TCP2Record.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{9A69614A-BC25-4A97-922B-3AFB993144B0}';
    IntfIID:   '{76033FC9-8BB0-4415-A785-9BF86AAF4E99}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCP2Record.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IP2Record;
  end;
end;

procedure TCP2Record.ConnectTo(svrIntf: IP2Record);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCP2Record.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCP2Record.GetDefaultInterface: IP2Record;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCP2Record.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCP2Record.Destroy;
begin
  inherited Destroy;
end;

function TCP2Record.Get_Count: LongWord;
begin
  Result := DefaultInterface.Count;
end;

function TCP2Record.GetValAsString(const fieldName: WideString): WideString;
begin
  Result := DefaultInterface.GetValAsString(fieldName);
end;

function TCP2Record.GetValAsStringByIndex(fieldIndex: LongWord): WideString;
begin
  Result := DefaultInterface.GetValAsStringByIndex(fieldIndex);
end;

function TCP2Record.GetValAsLong(const fieldName: WideString): Integer;
begin
  Result := DefaultInterface.GetValAsLong(fieldName);
end;

function TCP2Record.GetValAsLongByIndex(fieldIndex: LongWord): Integer;
begin
  Result := DefaultInterface.GetValAsLongByIndex(fieldIndex);
end;

function TCP2Record.GetValAsShort(const fieldName: WideString): Smallint;
begin
  Result := DefaultInterface.GetValAsShort(fieldName);
end;

function TCP2Record.GetValAsShortByIndex(fieldIndex: LongWord): Smallint;
begin
  Result := DefaultInterface.GetValAsShortByIndex(fieldIndex);
end;

function TCP2Record.GetValAsVariant(const fieldName: WideString): OleVariant;
begin
  Result := DefaultInterface.GetValAsVariant(fieldName);
end;

function TCP2Record.GetValAsVariantByIndex(fieldIndex: LongWord): OleVariant;
begin
  Result := DefaultInterface.GetValAsVariantByIndex(fieldIndex);
end;

function TCP2Record.GetObjID: Largeuint;
begin
  Result := DefaultInterface.GetObjID;
end;

class function CoCP2DataStream.Create: IP2DataStream;
begin
  Result := CreateComObject(CLASS_CP2DataStream) as IP2DataStream;
end;

class function CoCP2DataStream.CreateRemote(const MachineName: string): IP2DataStream;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CP2DataStream) as IP2DataStream;
end;

procedure TCP2DataStream.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{70F3774E-E66C-47DE-A47B-5AD3ECA67AF5}';
    IntfIID:   '{C14F57CF-BEE6-4ADB-85D0-44A307AD9444}';
    EventIID:  '{7F23A734-2F90-4DC6-9007-E5DC27FDC613}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCP2DataStream.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as IP2DataStream;
  end;
end;

procedure TCP2DataStream.ConnectTo(svrIntf: IP2DataStream);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TCP2DataStream.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TCP2DataStream.GetDefaultInterface: IP2DataStream;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCP2DataStream.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCP2DataStream.Destroy;
begin
  inherited Destroy;
end;

procedure TCP2DataStream.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    1: if Assigned(FOnStreamStateChanged) then
         FOnStreamStateChanged(Self,
                               IUnknown(TVarData(Params[0]).VPointer) as IP2DataStream {const IP2DataStream},
                               Params[1] {TDataStreamState});
    2: if Assigned(FOnStreamDataInserted) then
         FOnStreamDataInserted(Self,
                               IUnknown(TVarData(Params[0]).VPointer) as IP2DataStream {const IP2DataStream},
                               Params[1] {const WideString},
                               IUnknown(TVarData(Params[2]).VPointer) as IP2Record {const IP2Record});
    3: if Assigned(FOnStreamDataUpdated) then
         FOnStreamDataUpdated(Self,
                              IUnknown(TVarData(Params[0]).VPointer) as IP2DataStream {const IP2DataStream},
                              Params[1] {const WideString},
                              Params[2] {Int64},
                              IUnknown(TVarData(Params[3]).VPointer) as IP2Record {const IP2Record});
    4: if Assigned(FOnStreamDataDeleted) then
         FOnStreamDataDeleted(Self,
                              IUnknown(TVarData(Params[0]).VPointer) as IP2DataStream {const IP2DataStream},
                              Params[1] {const WideString},
                              Params[2] {Int64},
                              IUnknown(TVarData(Params[3]).VPointer) as IP2Record {const IP2Record});
    5: if Assigned(FOnStreamDatumDeleted) then
         FOnStreamDatumDeleted(Self,
                               IUnknown(TVarData(Params[0]).VPointer) as IP2DataStream {const IP2DataStream},
                               Params[1] {const WideString},
                               Params[2] {Int64});
    6: if Assigned(FOnStreamDBWillBeDeleted) then
         FOnStreamDBWillBeDeleted(Self, IUnknown(TVarData(Params[0]).VPointer) as IP2DataStream {const IP2DataStream});
    7: if Assigned(FOnStreamLifeNumChanged) then
         FOnStreamLifeNumChanged(Self,
                                 IUnknown(TVarData(Params[0]).VPointer) as IP2DataStream {const IP2DataStream},
                                 Params[1] {Integer});
    8: if Assigned(FOnStreamDataBegin) then
         FOnStreamDataBegin(Self, IUnknown(TVarData(Params[0]).VPointer) as IP2DataStream {const IP2DataStream});
    9: if Assigned(FOnStreamDataEnd) then
         FOnStreamDataEnd(Self, IUnknown(TVarData(Params[0]).VPointer) as IP2DataStream {const IP2DataStream});
  end; {case DispID}
end;

function TCP2DataStream.Get_TableSet: IP2TableSet;
begin
  Result := DefaultInterface.TableSet;
end;

procedure TCP2DataStream.Set_TableSet(const pVal: IP2TableSet);
begin
  DefaultInterface.TableSet := pVal;
end;

function TCP2DataStream.Get_StreamName: WideString;
begin
  Result := DefaultInterface.StreamName;
end;

procedure TCP2DataStream.Set_StreamName(const pVal: WideString);
begin
  DefaultInterface.StreamName := pVal;
end;

function TCP2DataStream.Get_DBConnString: WideString;
begin
  Result := DefaultInterface.DBConnString;
end;

procedure TCP2DataStream.Set_DBConnString(const pVal: WideString);
begin
  DefaultInterface.DBConnString := pVal;
end;

function TCP2DataStream.Get_type_: TRequestType;
begin
  Result := DefaultInterface.type_;
end;

procedure TCP2DataStream.Set_type_(pVal: TRequestType);
begin
  DefaultInterface.type_ := pVal;
end;

function TCP2DataStream.Get_State: TDataStreamState;
begin
  Result := DefaultInterface.State;
end;

function TCP2DataStream.Get_blob: WideString;
begin
  Result := DefaultInterface.blob;
end;

procedure TCP2DataStream.Open(const conn: IP2Connection);
begin
  DefaultInterface.Open(conn);
end;

procedure TCP2DataStream.Close;
begin
  DefaultInterface.Close;
end;

function TCP2DataStream.GetObjID: Largeuint;
begin
  Result := DefaultInterface.GetObjID;
end;

procedure TCP2DataStream.OpenWithBlob(const conn: IP2Connection; const blob: WideString);
begin
  DefaultInterface.OpenWithBlob(conn, blob);
end;

procedure TCP2DataStream.OpenWithBlobFile(const conn: IP2Connection; const blobFileName: WideString);
begin
  DefaultInterface.OpenWithBlobFile(conn, blobFileName);
end;

procedure TCP2DataStream.SaveBlob(const blobFileName: WideString);
begin
  DefaultInterface.SaveBlob(blobFileName);
end;

procedure TCP2DataStream.Resume(const conn: IP2Connection; const blob: WideString);
begin
  DefaultInterface.Resume(conn, blob);
end;

procedure TCP2DataStream.ResumeFromFile(const conn: IP2Connection; const blobFileName: WideString);
begin
  DefaultInterface.ResumeFromFile(conn, blobFileName);
end;

class function CoCP2DataBuffer.Create: IP2DataBuffer;
begin
  Result := CreateComObject(CLASS_CP2DataBuffer) as IP2DataBuffer;
end;

class function CoCP2DataBuffer.CreateRemote(const MachineName: string): IP2DataBuffer;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CP2DataBuffer) as IP2DataBuffer;
end;

procedure TCP2DataBuffer.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{9791418D-3998-4CAC-BA23-87E9EDA14FEB}';
    IntfIID:   '{48169C9B-8E7F-427C-836B-8BB168CA8042}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCP2DataBuffer.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IP2DataBuffer;
  end;
end;

procedure TCP2DataBuffer.ConnectTo(svrIntf: IP2DataBuffer);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCP2DataBuffer.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCP2DataBuffer.GetDefaultInterface: IP2DataBuffer;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCP2DataBuffer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCP2DataBuffer.Destroy;
begin
  inherited Destroy;
end;

function TCP2DataBuffer.Get_TableRecords(const tableName: WideString): IP2TableRecords;
begin
  Result := DefaultInterface.TableRecords[tableName];
end;

procedure TCP2DataBuffer.AttachToStream(const stream: IP2DataStream);
begin
  DefaultInterface.AttachToStream(stream);
end;

procedure TCP2DataBuffer.DetachFromStream;
begin
  DefaultInterface.DetachFromStream;
end;

function TCP2DataBuffer.CountTables: Integer;
begin
  Result := DefaultInterface.CountTables;
end;

function TCP2DataBuffer.Count(const tableName: WideString): Integer;
begin
  Result := DefaultInterface.Count(tableName);
end;

procedure TCP2DataBuffer.Clear(const tableName: WideString);
begin
  DefaultInterface.Clear(tableName);
end;

procedure TCP2DataBuffer.ClearAll;
begin
  DefaultInterface.ClearAll;
end;

class function CoCP2Application.Create: IP2Application;
begin
  Result := CreateComObject(CLASS_CP2Application) as IP2Application;
end;

class function CoCP2Application.CreateRemote(const MachineName: string): IP2Application;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CP2Application) as IP2Application;
end;

procedure TCP2Application.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{D42CDADE-DA8E-4133-844B-7602659D4F12}';
    IntfIID:   '{1F5B9148-F21F-4980-A5D5-23CD5F9CA0A5}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCP2Application.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IP2Application;
  end;
end;

procedure TCP2Application.ConnectTo(svrIntf: IP2Application);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCP2Application.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCP2Application.GetDefaultInterface: IP2Application;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCP2Application.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCP2Application.Destroy;
begin
  inherited Destroy;
end;

function TCP2Application.Get_ParserType: LongWord;
begin
  Result := DefaultInterface.ParserType;
end;

procedure TCP2Application.Set_ParserType(pVal: LongWord);
begin
  DefaultInterface.ParserType := pVal;
end;

procedure TCP2Application.StartUp(const iniFileName: WideString);
begin
  DefaultInterface.StartUp(iniFileName);
end;

procedure TCP2Application.CleanUp;
begin
  DefaultInterface.CleanUp;
end;

function TCP2Application.CreateP2Connection: IP2Connection;
begin
  Result := DefaultInterface.CreateP2Connection;
end;

function TCP2Application.CreateP2BLMessage: IP2BLMessage;
begin
  Result := DefaultInterface.CreateP2BLMessage;
end;

function TCP2Application.CreateP2BLMessageFactory: IP2BLMessageFactory;
begin
  Result := DefaultInterface.CreateP2BLMessageFactory;
end;

function TCP2Application.CreateP2DataBuffer: IP2DataBuffer;
begin
  Result := DefaultInterface.CreateP2DataBuffer;
end;

function TCP2Application.CreateP2DataStream: IP2DataStream;
begin
  Result := DefaultInterface.CreateP2DataStream;
end;

function TCP2Application.CreateP2TableSet: IP2TableSet;
begin
  Result := DefaultInterface.CreateP2TableSet;
end;

class function CoCP2TableEnum.Create: IUnknown;
begin
  Result := CreateComObject(CLASS_CP2TableEnum) as IUnknown;
end;

class function CoCP2TableEnum.CreateRemote(const MachineName: string): IUnknown;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CP2TableEnum) as IUnknown;
end;

procedure TCP2TableEnum.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{44B26EE1-964F-4643-BFEE-16A8E3C8CE71}';
    IntfIID:   '{00000000-0000-0000-C000-000000000046}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCP2TableEnum.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IUnknown;
  end;
end;

procedure TCP2TableEnum.ConnectTo(svrIntf: IUnknown);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCP2TableEnum.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCP2TableEnum.GetDefaultInterface: IUnknown;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCP2TableEnum.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCP2TableEnum.Destroy;
begin
  inherited Destroy;
end;

class function CoCP2RecordEnum.Create: IP2TableRecords;
begin
  Result := CreateComObject(CLASS_CP2RecordEnum) as IP2TableRecords;
end;

class function CoCP2RecordEnum.CreateRemote(const MachineName: string): IP2TableRecords;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CP2RecordEnum) as IP2TableRecords;
end;

procedure TCP2RecordEnum.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{D444BF81-1360-4836-AFD7-5DF15A3199DE}';
    IntfIID:   '{3A19BD02-482B-4A7A-AE9F-AD879BE573C8}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCP2RecordEnum.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IP2TableRecords;
  end;
end;

procedure TCP2RecordEnum.ConnectTo(svrIntf: IP2TableRecords);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCP2RecordEnum.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCP2RecordEnum.GetDefaultInterface: IP2TableRecords;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCP2RecordEnum.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCP2RecordEnum.Destroy;
begin
  inherited Destroy;
end;

class function Co__Impl_IP2DataStreamEvents.Create: IP2DataStreamEvents;
begin
  Result := CreateComObject(CLASS___Impl_IP2DataStreamEvents) as IP2DataStreamEvents;
end;

class function Co__Impl_IP2DataStreamEvents.CreateRemote(const MachineName: string): IP2DataStreamEvents;
begin
  Result := CreateRemoteComObject(MachineName, CLASS___Impl_IP2DataStreamEvents) as IP2DataStreamEvents;
end;

procedure T__Impl_IP2DataStreamEvents.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{31CD73F0-719C-3992-AA71-96149583D1FC}';
    IntfIID:   '{7F23A734-2F90-4DC6-9007-E5DC27FDC613}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure T__Impl_IP2DataStreamEvents.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IP2DataStreamEvents;
  end;
end;

procedure T__Impl_IP2DataStreamEvents.ConnectTo(svrIntf: IP2DataStreamEvents);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure T__Impl_IP2DataStreamEvents.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function T__Impl_IP2DataStreamEvents.GetDefaultInterface: IP2DataStreamEvents;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor T__Impl_IP2DataStreamEvents.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor T__Impl_IP2DataStreamEvents.Destroy;
begin
  inherited Destroy;
end;

procedure T__Impl_IP2DataStreamEvents.StreamStateChanged(const stream: IP2DataStream; 
                                                         newState: TDataStreamState);
begin
  DefaultInterface.StreamStateChanged(stream, newState);
end;

procedure T__Impl_IP2DataStreamEvents.StreamDataInserted(const stream: IP2DataStream; 
                                                         const tableName: WideString; 
                                                         const rec: IP2Record);
begin
  DefaultInterface.StreamDataInserted(stream, tableName, rec);
end;

procedure T__Impl_IP2DataStreamEvents.StreamDataUpdated(const stream: IP2DataStream; 
                                                        const tableName: WideString; Id: Int64; 
                                                        const rec: IP2Record);
begin
  DefaultInterface.StreamDataUpdated(stream, tableName, Id, rec);
end;

procedure T__Impl_IP2DataStreamEvents.StreamDataDeleted(const stream: IP2DataStream; 
                                                        const tableName: WideString; Id: Int64; 
                                                        const rec: IP2Record);
begin
  DefaultInterface.StreamDataDeleted(stream, tableName, Id, rec);
end;

procedure T__Impl_IP2DataStreamEvents.StreamDatumDeleted(const stream: IP2DataStream; 
                                                         const tableName: WideString; rev: Int64);
begin
  DefaultInterface.StreamDatumDeleted(stream, tableName, rev);
end;

procedure T__Impl_IP2DataStreamEvents.StreamDBWillBeDeleted(const stream: IP2DataStream);
begin
  DefaultInterface.StreamDBWillBeDeleted(stream);
end;

procedure T__Impl_IP2DataStreamEvents.StreamLifeNumChanged(const stream: IP2DataStream; 
                                                           LifeNum: Integer);
begin
  DefaultInterface.StreamLifeNumChanged(stream, LifeNum);
end;

procedure T__Impl_IP2DataStreamEvents.StreamDataBegin(const stream: IP2DataStream);
begin
  DefaultInterface.StreamDataBegin(stream);
end;

procedure T__Impl_IP2DataStreamEvents.StreamDataEnd(const stream: IP2DataStream);
begin
  DefaultInterface.StreamDataEnd(stream);
end;

procedure Register;
begin
  RegisterComponents(dtlServerPage, [TCP2Connection, TCP2BLMessage, TCP2BLMessageFactory, TCP2TableSet, 
    TCP2Record, TCP2DataStream, TCP2DataBuffer, TCP2Application, TCP2TableEnum, 
    TCP2RecordEnum, T__Impl_IP2DataStreamEvents]);
end;

end.
