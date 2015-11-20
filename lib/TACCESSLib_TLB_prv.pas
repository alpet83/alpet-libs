unit TACCESSLib_TLB;

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

// $Rev: 17252 $
// File generated on 20.08.2010 11:33:51 from Type Library described below.

// ************************************************************************  //
// Type Lib: c:\Trade\TAccess\TAccess.exe (1)
// LIBID: {01BB1EB7-9701-4178-883E-F54032FC04C8}
// LCID: 0
// Helpfile: 
// HelpString: TAccess 1.0 Type Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
// Errors:
//   Hint: Parameter 'Type' of ITATrader.GetSecurity changed to 'Type_'
//   Hint: Parameter 'Type' of ITATrader.GetSecListAsFields changed to 'Type_'
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
  TACCESSLibMajorVersion = 1;
  TACCESSLibMinorVersion = 0;

  LIBID_TACCESSLib: TGUID = '{01BB1EB7-9701-4178-883E-F54032FC04C8}';

  IID_ITAServer: TGUID = '{6BBEE3D0-7E1E-4CB1-BECD-61E84D052D97}';
  CLASS_TAServer: TGUID = '{040DBC51-C27E-4EA5-9855-81A84450D25F}';
  IID_ITATrader: TGUID = '{F29795C3-9BD9-493E-8E18-99D02248443F}';
  IID_ITARowset: TGUID = '{51C5E01C-1617-473C-B418-69B0EA7B4F77}';
  IID_ITARow: TGUID = '{F35F14DE-4B23-40B0-A5E5-734A32AE8639}';
  CLASS_TATrader: TGUID = '{53657E1F-8B95-412D-9ECE-C51F16549130}';
  CLASS_TARowset: TGUID = '{36EAFB71-57C6-4763-AB8F-4AEA05686A9F}';
  CLASS_TARow: TGUID = '{DE66A700-8ABC-43F8-9258-89FC29CD637D}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum _enum_OrderProperty
type
  _enum_OrderProperty = TOleEnum;
const
  PutInQueue = $00000000;
  CancelBalance = $00000001;
  ImmOrCancel = $00000002;

// Constants for enum _enum_Condition
type
  _enum_Condition = TOleEnum;
const
  None = $00000000;
  Bid = $00000001;
  BidOrLast = $00000002;
  Ask = $00000003;
  AskOrLast = $00000004;
  Time = $00000005;
  CovDown = $00000006;
  CovUp = $00000007;
  LastUp = $00000008;
  LastDown = $00000009;

// Constants for enum _enum_ChangeType
type
  _enum_ChangeType = TOleEnum;
const
  Absolute = $00000000;
  Percentage = $00000001;
  Combi = $00000002;

// Constants for enum _enum_MoveOrderFlag
type
  _enum_MoveOrderFlag = TOleEnum;
const
  DontAlterQuantity = $00000000;
  SetNewQuantity = $00000001;
  CancelIfQuantityChanged = $00000002;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  ITAServer = interface;
  ITAServerDisp = dispinterface;
  ITATrader = interface;
  ITATraderDisp = dispinterface;
  ITARowset = interface;
  ITARowsetDisp = dispinterface;
  ITARow = interface;
  ITARowDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  TAServer = ITAServer;
  TATrader = ITATrader;
  TARowset = ITARowset;
  TARow = ITARow;


// *********************************************************************//
// Interface: ITAServer
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6BBEE3D0-7E1E-4CB1-BECD-61E84D052D97}
// *********************************************************************//
  ITAServer = interface(IDispatch)
    ['{6BBEE3D0-7E1E-4CB1-BECD-61E84D052D97}']
    function NewTrader(ServerSection: SYSINT; const bstrBadge: WideString; 
                       const bstrClientList: WideString; var piResult: OleVariant; 
                       var pvarResult: OleVariant): ITATrader; safecall;
  end;

// *********************************************************************//
// DispIntf:  ITAServerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {6BBEE3D0-7E1E-4CB1-BECD-61E84D052D97}
// *********************************************************************//
  ITAServerDisp = dispinterface
    ['{6BBEE3D0-7E1E-4CB1-BECD-61E84D052D97}']
    function NewTrader(ServerSection: SYSINT; const bstrBadge: WideString; 
                       const bstrClientList: WideString; var piResult: OleVariant; 
                       var pvarResult: OleVariant): ITATrader; dispid 1;
  end;

// *********************************************************************//
// Interface: ITATrader
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F29795C3-9BD9-493E-8E18-99D02248443F}
// *********************************************************************//
  ITATrader = interface(IDispatch)
    ['{F29795C3-9BD9-493E-8E18-99D02248443F}']
    function GetQuotes(const bstrSecBoard: WideString; const bstrSecCode: WideString; 
                       nMaxRow: SYSINT; var pbUpdated: OleVariant; var piResult: OleVariant; 
                       var pvarResult: OleVariant): ITARowset; safecall;
    procedure NewOrder(const bstrBoard: WideString; const bstrSecCode: WideString; 
                       const bstrClient: WideString; varPrice: OleVariant; varVolume: OleVariant; 
                       bBuy: WordBool; bMarketPrice: WordBool; const bstrBrokerref: WideString; 
                       nOrderProp: _enum_OrderProperty; bSplit: WordBool; bUseCredit: WordBool; 
                       bUseAltAcc: WordBool; CancelAfterTime: OleVariant; var piResult: OleVariant; 
                       var pvarResult: OleVariant; var pTransactionID: OleVariant); safecall;
    procedure NewConditionalOrder(const bstrBoard: WideString; const bstrSecCode: WideString; 
                                  const bstrClient: WideString; varPrice: OleVariant; 
                                  varVolume: OleVariant; bBuy: WordBool; bMarketPrice: WordBool; 
                                  const bstrBrokerref: WideString; nOrderProp: _enum_OrderProperty; 
                                  bSplit: WordBool; bUseCredit: WordBool; bUseAltAcc: WordBool; 
                                  CancelAfterTime: OleVariant; condition: _enum_Condition; 
                                  varConditionValue: OleVariant; varValidAfterTime: OleVariant; 
                                  varValidBeforeTime: OleVariant; bWithinPos: WordBool; 
                                  var piResult: OleVariant; var pvarResult: OleVariant; 
                                  var pTransactionID: OleVariant); safecall;
    procedure CancelOrder(TransactionID: Integer; var piResult: OleVariant; 
                          var pvarResult: OleVariant); safecall;
    function GetOrders(const bstrClient: WideString; const bstrSecBoard: WideString; 
                       const bstrSecCode: WideString; bActive: WordBool; bMatched: WordBool; 
                       bCancelled: WordBool; var pbUpdated: OleVariant; var piResult: OleVariant; 
                       var pvarResult: OleVariant): ITARowset; safecall;
    function GetTrades(const bstrClient: WideString; const bstrSecBoard: WideString; 
                       const bstrSecCode: WideString; var pbUpdated: OleVariant; 
                       var piResult: OleVariant; var pvarResult: OleVariant): ITARowset; safecall;
    function GetSecurity(const bstrSecBoard: WideString; const bstrSecCode: WideString; 
                         Type_: _enum_ChangeType; var pbUpdated: OleVariant; 
                         var piResult: OleVariant; var pvarResult: OleVariant): ITARowset; safecall;
    function GetMarginPos(const bstrSecBoard: WideString; const bstrSecCode: WideString; 
                          const bstrClient: WideString; bIncludeConditional: WordBool; 
                          var pbDerivatives: OleVariant; var pType: OleVariant; 
                          var piResult: OleVariant; var pvarResult: OleVariant): ITARowset; safecall;
    function GetClientPos(const bstrClient: WideString; bIncludeConditional: WordBool; 
                          var piResult: OleVariant; var pvarResult: OleVariant): ITARowset; safecall;
    function Get_ServerStatus: Integer; safecall;
    function GetMarginPosList(const bstrSecList: WideString; const bstrClient: WideString; 
                              bIncludeConditional: WordBool; var piResult: OleVariant; 
                              var pvarResult: OleVariant): ITARowset; safecall;
    function GetClientFortsPos(const bstrClient: WideString; bIncludeConditional: WordBool; 
                               var piResult: OleVariant; var pvarResult: OleVariant): ITARowset; safecall;
    function GetSecList(var pbUpdated: OleVariant; var piResult: OleVariant; 
                        var pvarResult: OleVariant): ITARowset; safecall;
    function GetAllMoneyPos(var piResult: OleVariant; var pvarResult: OleVariant): ITARowset; safecall;
    function GetAllClientPos(var piResult: OleVariant; var pvarResult: OleVariant): ITARowset; safecall;
    function GetMessage(var pbUpdated: OleVariant; var piResult: OleVariant; 
                        var pvarResult: OleVariant): ITARowset; safecall;
    function GetExchIndex(var piResult: OleVariant; var pvarResult: OleVariant): ITARowset; safecall;
    function GetClientParams(const bstrClient: WideString; var iResult: OleVariant; 
                             var pvarResult: OleVariant): ITARowset; safecall;
    function GetFortsLimits(const bstrClient: WideString; var piResult: OleVariant; 
                            var pvarResult: OleVariant): ITARowset; safecall;
    function GetFondPortfolio(const bstrClient: WideString; var piResult: OleVariant; 
                              var pvarResult: OleVariant): ITARowset; safecall;
    function GetTransReject(var piResult: OleVariant; var pvarResult: OleVariant): ITARowset; safecall;
    function GetSecurityExt(const bstrSecBoard: WideString; const bstrSecCode: WideString; 
                            var piResult: OleVariant; var pvarResult: OleVariant): ITARowset; safecall;
    procedure AddSecToQuoteList(const bstrSecBoard: WideString; const bstrSecCode: WideString; 
                                nSecID: OleVariant; var piResult: OleVariant; 
                                var pvarResult: OleVariant); safecall;
    procedure DelSecFromQuoteList(nSecID: OleVariant; var piResult: OleVariant; 
                                  var pvarResult: OleVariant); safecall;
    procedure GetQuoteListAsArray(nMaxRow: OleVariant; var pbUpdated: OleVariant; 
                                  var piResult: OleVariant; var pvarResult: OleVariant; 
                                  var pVar: OleVariant); safecall;
    function GetQuoteListAsFields(nMaxRow: OleVariant; var pbUpdated: OleVariant; 
                                  var piResult: OleVariant; var pvarResult: OleVariant): OleVariant; safecall;
    function GetSecListAsFields(Type_: _enum_ChangeType; var pbUpdated: OleVariant; 
                                var piResult: OleVariant; var pvarResult: OleVariant): OleVariant; safecall;
    procedure AddSecToSecList(const bstrSecBoard: WideString; const bstrSecCode: WideString; 
                              nSecID: OleVariant; var piResult: OleVariant; 
                              var pvarResult: OleVariant); safecall;
    procedure DelSecFromSecList(nSecID: OleVariant; var piResult: OleVariant; 
                                var pvarResult: OleVariant); safecall;
    procedure GetOrdersAsArray(const bstrClient: WideString; const bstrSecBoard: WideString; 
                               const bstrSecCode: WideString; const bstrColumns: WideString; 
                               bActive: WordBool; bMatched: WordBool; bCancelled: WordBool; 
                               var pbUpdated: OleVariant; var piResult: OleVariant; 
                               var pvarResult: OleVariant; var pVar: OleVariant); safecall;
    procedure GetQuotesAsArray(const bstrSecBoard: WideString; const bstrSecCode: WideString; 
                               const bstrColumns: WideString; nMaxRow: SYSINT; 
                               var pbUpdated: OleVariant; var piResult: OleVariant; 
                               var pvarResult: OleVariant; var pVar: OleVariant); safecall;
    procedure GetMarginPosAsArray(const bstrSecBoard: WideString; const bstrSecCode: WideString; 
                                  const bstrClient: WideString; const bstrColumns: WideString; 
                                  bIncludeConditional: WordBool; var pbDerivatives: OleVariant; 
                                  var pType: OleVariant; var piResult: OleVariant; 
                                  var pvarResult: OleVariant; var pVar: OleVariant); safecall;
    procedure GetMarginPosListAsArray(const bstrSecList: WideString; const bstrClient: WideString; 
                                      const bstrColumns: WideString; bIncludeConditional: WordBool; 
                                      var piResult: OleVariant; var pvarResult: OleVariant; 
                                      var pVar: OleVariant); safecall;
    procedure MoveOrder(TransactionID: Integer; varPrice: OleVariant; varVolume: OleVariant; 
                        nMoveOrderFlag: _enum_MoveOrderFlag; var piResult: OleVariant; 
                        var pvarResult: OleVariant); safecall;
    property ServerStatus: Integer read Get_ServerStatus;
  end;

// *********************************************************************//
// DispIntf:  ITATraderDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F29795C3-9BD9-493E-8E18-99D02248443F}
// *********************************************************************//
  ITATraderDisp = dispinterface
    ['{F29795C3-9BD9-493E-8E18-99D02248443F}']
    function GetQuotes(const bstrSecBoard: WideString; const bstrSecCode: WideString; 
                       nMaxRow: SYSINT; var pbUpdated: OleVariant; var piResult: OleVariant; 
                       var pvarResult: OleVariant): ITARowset; dispid 1;
    procedure NewOrder(const bstrBoard: WideString; const bstrSecCode: WideString; 
                       const bstrClient: WideString; varPrice: OleVariant; varVolume: OleVariant; 
                       bBuy: WordBool; bMarketPrice: WordBool; const bstrBrokerref: WideString; 
                       nOrderProp: _enum_OrderProperty; bSplit: WordBool; bUseCredit: WordBool; 
                       bUseAltAcc: WordBool; CancelAfterTime: OleVariant; var piResult: OleVariant; 
                       var pvarResult: OleVariant; var pTransactionID: OleVariant); dispid 2;
    procedure NewConditionalOrder(const bstrBoard: WideString; const bstrSecCode: WideString; 
                                  const bstrClient: WideString; varPrice: OleVariant; 
                                  varVolume: OleVariant; bBuy: WordBool; bMarketPrice: WordBool; 
                                  const bstrBrokerref: WideString; nOrderProp: _enum_OrderProperty; 
                                  bSplit: WordBool; bUseCredit: WordBool; bUseAltAcc: WordBool; 
                                  CancelAfterTime: OleVariant; condition: _enum_Condition; 
                                  varConditionValue: OleVariant; varValidAfterTime: OleVariant; 
                                  varValidBeforeTime: OleVariant; bWithinPos: WordBool; 
                                  var piResult: OleVariant; var pvarResult: OleVariant; 
                                  var pTransactionID: OleVariant); dispid 3;
    procedure CancelOrder(TransactionID: Integer; var piResult: OleVariant; 
                          var pvarResult: OleVariant); dispid 4;
    function GetOrders(const bstrClient: WideString; const bstrSecBoard: WideString; 
                       const bstrSecCode: WideString; bActive: WordBool; bMatched: WordBool; 
                       bCancelled: WordBool; var pbUpdated: OleVariant; var piResult: OleVariant; 
                       var pvarResult: OleVariant): ITARowset; dispid 5;
    function GetTrades(const bstrClient: WideString; const bstrSecBoard: WideString; 
                       const bstrSecCode: WideString; var pbUpdated: OleVariant; 
                       var piResult: OleVariant; var pvarResult: OleVariant): ITARowset; dispid 6;
    function GetSecurity(const bstrSecBoard: WideString; const bstrSecCode: WideString; 
                         Type_: _enum_ChangeType; var pbUpdated: OleVariant; 
                         var piResult: OleVariant; var pvarResult: OleVariant): ITARowset; dispid 7;
    function GetMarginPos(const bstrSecBoard: WideString; const bstrSecCode: WideString; 
                          const bstrClient: WideString; bIncludeConditional: WordBool; 
                          var pbDerivatives: OleVariant; var pType: OleVariant; 
                          var piResult: OleVariant; var pvarResult: OleVariant): ITARowset; dispid 8;
    function GetClientPos(const bstrClient: WideString; bIncludeConditional: WordBool; 
                          var piResult: OleVariant; var pvarResult: OleVariant): ITARowset; dispid 9;
    property ServerStatus: Integer readonly dispid 10;
    function GetMarginPosList(const bstrSecList: WideString; const bstrClient: WideString; 
                              bIncludeConditional: WordBool; var piResult: OleVariant; 
                              var pvarResult: OleVariant): ITARowset; dispid 11;
    function GetClientFortsPos(const bstrClient: WideString; bIncludeConditional: WordBool; 
                               var piResult: OleVariant; var pvarResult: OleVariant): ITARowset; dispid 12;
    function GetSecList(var pbUpdated: OleVariant; var piResult: OleVariant; 
                        var pvarResult: OleVariant): ITARowset; dispid 13;
    function GetAllMoneyPos(var piResult: OleVariant; var pvarResult: OleVariant): ITARowset; dispid 14;
    function GetAllClientPos(var piResult: OleVariant; var pvarResult: OleVariant): ITARowset; dispid 15;
    function GetMessage(var pbUpdated: OleVariant; var piResult: OleVariant; 
                        var pvarResult: OleVariant): ITARowset; dispid 16;
    function GetExchIndex(var piResult: OleVariant; var pvarResult: OleVariant): ITARowset; dispid 17;
    function GetClientParams(const bstrClient: WideString; var iResult: OleVariant; 
                             var pvarResult: OleVariant): ITARowset; dispid 18;
    function GetFortsLimits(const bstrClient: WideString; var piResult: OleVariant; 
                            var pvarResult: OleVariant): ITARowset; dispid 19;
    function GetFondPortfolio(const bstrClient: WideString; var piResult: OleVariant; 
                              var pvarResult: OleVariant): ITARowset; dispid 20;
    function GetTransReject(var piResult: OleVariant; var pvarResult: OleVariant): ITARowset; dispid 21;
    function GetSecurityExt(const bstrSecBoard: WideString; const bstrSecCode: WideString; 
                            var piResult: OleVariant; var pvarResult: OleVariant): ITARowset; dispid 22;
    procedure AddSecToQuoteList(const bstrSecBoard: WideString; const bstrSecCode: WideString; 
                                nSecID: OleVariant; var piResult: OleVariant; 
                                var pvarResult: OleVariant); dispid 23;
    procedure DelSecFromQuoteList(nSecID: OleVariant; var piResult: OleVariant; 
                                  var pvarResult: OleVariant); dispid 24;
    procedure GetQuoteListAsArray(nMaxRow: OleVariant; var pbUpdated: OleVariant; 
                                  var piResult: OleVariant; var pvarResult: OleVariant; 
                                  var pVar: OleVariant); dispid 25;
    function GetQuoteListAsFields(nMaxRow: OleVariant; var pbUpdated: OleVariant; 
                                  var piResult: OleVariant; var pvarResult: OleVariant): OleVariant; dispid 26;
    function GetSecListAsFields(Type_: _enum_ChangeType; var pbUpdated: OleVariant; 
                                var piResult: OleVariant; var pvarResult: OleVariant): OleVariant; dispid 27;
    procedure AddSecToSecList(const bstrSecBoard: WideString; const bstrSecCode: WideString; 
                              nSecID: OleVariant; var piResult: OleVariant; 
                              var pvarResult: OleVariant); dispid 28;
    procedure DelSecFromSecList(nSecID: OleVariant; var piResult: OleVariant; 
                                var pvarResult: OleVariant); dispid 29;
    procedure GetOrdersAsArray(const bstrClient: WideString; const bstrSecBoard: WideString; 
                               const bstrSecCode: WideString; const bstrColumns: WideString; 
                               bActive: WordBool; bMatched: WordBool; bCancelled: WordBool; 
                               var pbUpdated: OleVariant; var piResult: OleVariant; 
                               var pvarResult: OleVariant; var pVar: OleVariant); dispid 30;
    procedure GetQuotesAsArray(const bstrSecBoard: WideString; const bstrSecCode: WideString; 
                               const bstrColumns: WideString; nMaxRow: SYSINT; 
                               var pbUpdated: OleVariant; var piResult: OleVariant; 
                               var pvarResult: OleVariant; var pVar: OleVariant); dispid 31;
    procedure GetMarginPosAsArray(const bstrSecBoard: WideString; const bstrSecCode: WideString; 
                                  const bstrClient: WideString; const bstrColumns: WideString; 
                                  bIncludeConditional: WordBool; var pbDerivatives: OleVariant; 
                                  var pType: OleVariant; var piResult: OleVariant; 
                                  var pvarResult: OleVariant; var pVar: OleVariant); dispid 32;
    procedure GetMarginPosListAsArray(const bstrSecList: WideString; const bstrClient: WideString; 
                                      const bstrColumns: WideString; bIncludeConditional: WordBool; 
                                      var piResult: OleVariant; var pvarResult: OleVariant; 
                                      var pVar: OleVariant); dispid 33;
    procedure MoveOrder(TransactionID: Integer; varPrice: OleVariant; varVolume: OleVariant; 
                        nMoveOrderFlag: _enum_MoveOrderFlag; var piResult: OleVariant; 
                        var pvarResult: OleVariant); dispid 34;
  end;

// *********************************************************************//
// Interface: ITARowset
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {51C5E01C-1617-473C-B418-69B0EA7B4F77}
// *********************************************************************//
  ITARowset = interface(IDispatch)
    ['{51C5E01C-1617-473C-B418-69B0EA7B4F77}']
    function Item(nItem: Integer): ITARow; safecall;
    function Get_Count: Integer; safecall;
    procedure GetAsSafeArray(const bstrColumns: WideString; var pVar: OleVariant); safecall;
    function GetItemAllFields(nItem: Integer; bDigitalBool: OleVariant; const bstrSV1: WideString; 
                              const bstrSV2: WideString): OleVariant; safecall;
    function GetRowsetAllFields(bDigitalBool: OleVariant; const bstrSV1: WideString; 
                                const bstrSV2: WideString; const bstrSV3: WideString): OleVariant; safecall;
    function GetRowsetAsSVString(const bstrColumns: WideString; bDigitalBool: OleVariant; 
                                 const bstrSV1: WideString; const bstrSV2: WideString): OleVariant; safecall;
    function GetItemAsSVString(const bstrColumns: WideString; nItem: Integer; 
                               bDigitalBool: OleVariant; const bstrSV1: WideString): OleVariant; safecall;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  ITARowsetDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {51C5E01C-1617-473C-B418-69B0EA7B4F77}
// *********************************************************************//
  ITARowsetDisp = dispinterface
    ['{51C5E01C-1617-473C-B418-69B0EA7B4F77}']
    function Item(nItem: Integer): ITARow; dispid 1;
    property Count: Integer readonly dispid 2;
    procedure GetAsSafeArray(const bstrColumns: WideString; var pVar: OleVariant); dispid 3;
    function GetItemAllFields(nItem: Integer; bDigitalBool: OleVariant; const bstrSV1: WideString; 
                              const bstrSV2: WideString): OleVariant; dispid 4;
    function GetRowsetAllFields(bDigitalBool: OleVariant; const bstrSV1: WideString; 
                                const bstrSV2: WideString; const bstrSV3: WideString): OleVariant; dispid 5;
    function GetRowsetAsSVString(const bstrColumns: WideString; bDigitalBool: OleVariant; 
                                 const bstrSV1: WideString; const bstrSV2: WideString): OleVariant; dispid 6;
    function GetItemAsSVString(const bstrColumns: WideString; nItem: Integer; 
                               bDigitalBool: OleVariant; const bstrSV1: WideString): OleVariant; dispid 7;
  end;

// *********************************************************************//
// Interface: ITARow
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F35F14DE-4B23-40B0-A5E5-734A32AE8639}
// *********************************************************************//
  ITARow = interface(IDispatch)
    ['{F35F14DE-4B23-40B0-A5E5-734A32AE8639}']
    function Column(const bstrColumn: WideString): OleVariant; safecall;
    function GetAllFields(bDigitalBool: OleVariant; const bstrSV1: WideString; 
                          const bstrSV2: WideString): OleVariant; safecall;
  end;

// *********************************************************************//
// DispIntf:  ITARowDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F35F14DE-4B23-40B0-A5E5-734A32AE8639}
// *********************************************************************//
  ITARowDisp = dispinterface
    ['{F35F14DE-4B23-40B0-A5E5-734A32AE8639}']
    function Column(const bstrColumn: WideString): OleVariant; dispid 1;
    function GetAllFields(bDigitalBool: OleVariant; const bstrSV1: WideString; 
                          const bstrSV2: WideString): OleVariant; dispid 2;
  end;

// *********************************************************************//
// The Class CoTAServer provides a Create and CreateRemote method to          
// create instances of the default interface ITAServer exposed by              
// the CoClass TAServer. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoTAServer = class
    class function Create: ITAServer;
    class function CreateRemote(const MachineName: string): ITAServer;
  end;

// *********************************************************************//
// The Class CoTATrader provides a Create and CreateRemote method to          
// create instances of the default interface ITATrader exposed by              
// the CoClass TATrader. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoTATrader = class
    class function Create: ITATrader;
    class function CreateRemote(const MachineName: string): ITATrader;
  end;

// *********************************************************************//
// The Class CoTARowset provides a Create and CreateRemote method to          
// create instances of the default interface ITARowset exposed by              
// the CoClass TARowset. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoTARowset = class
    class function Create: ITARowset;
    class function CreateRemote(const MachineName: string): ITARowset;
  end;

// *********************************************************************//
// The Class CoTARow provides a Create and CreateRemote method to          
// create instances of the default interface ITARow exposed by              
// the CoClass TARow. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoTARow = class
    class function Create: ITARow;
    class function CreateRemote(const MachineName: string): ITARow;
  end;

implementation

uses ComObj;

class function CoTAServer.Create: ITAServer;
begin
  Result := CreateComObject(CLASS_TAServer) as ITAServer;
end;

class function CoTAServer.CreateRemote(const MachineName: string): ITAServer;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_TAServer) as ITAServer;
end;

class function CoTATrader.Create: ITATrader;
begin
  Result := CreateComObject(CLASS_TATrader) as ITATrader;
end;

class function CoTATrader.CreateRemote(const MachineName: string): ITATrader;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_TATrader) as ITATrader;
end;

class function CoTARowset.Create: ITARowset;
begin
  Result := CreateComObject(CLASS_TARowset) as ITARowset;
end;

class function CoTARowset.CreateRemote(const MachineName: string): ITARowset;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_TARowset) as ITARowset;
end;

class function CoTARow.Create: ITARow;
begin
  Result := CreateComObject(CLASS_TARow) as ITARow;
end;

class function CoTARow.CreateRemote(const MachineName: string): ITARow;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_TARow) as ITARow;
end;

end.
