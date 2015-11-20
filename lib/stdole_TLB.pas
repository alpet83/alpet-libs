unit stdole_TLB;

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
// File generated on 11.08.2011 13:55:17 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\WINDOWS\system32\stdole2.tlb (1)
// LIBID: {00020430-0000-0000-C000-000000000046}
// LCID: 0
// Helpfile: 
// HelpString: OLE Automation
// DepndLst: None
// Errors:
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Symbol 'Type' renamed to 'type_'
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
  stdoleMajorVersion = 2;
  stdoleMinorVersion = 0;

  LIBID_stdole: TGUID = '{00020430-0000-0000-C000-000000000046}';

  IID_IEnumVARIANT: TGUID = '{00020404-0000-0000-C000-000000000046}';
  IID_IFont: TGUID = '{BEF6E002-A874-101A-8BBA-00AA00300CAB}';
  DIID_Font: TGUID = '{BEF6E003-A874-101A-8BBA-00AA00300CAB}';
  DIID_FontEvents: TGUID = '{4EF6100A-AF88-11D0-9846-00C04FC29993}';
  IID_IPicture: TGUID = '{7BF80980-BF32-101A-8BBB-00AA00300CAB}';
  DIID_Picture: TGUID = '{7BF80981-BF32-101A-8BBB-00AA00300CAB}';
  CLASS_StdPicture: TGUID = '{0BE35204-8F91-11CE-9DE3-00AA004BB851}';
  CLASS_StdFont: TGUID = '{0BE35203-8F91-11CE-9DE3-00AA004BB851}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum OLE_TRISTATE
type
  OLE_TRISTATE = TOleEnum;
const
  Unchecked = $00000000;
  Checked = $00000001;
  Gray = $00000002;

// Constants for enum LoadPictureConstants
type
  LoadPictureConstants = TOleEnum;
const
  Default = $00000000;
  Monochrome = $00000001;
  VgaColor = $00000002;
  Color = $00000004;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IEnumVARIANT = interface;
  IFont = interface;
  Font = dispinterface;
  FontEvents = dispinterface;
  IPicture = interface;
  Picture = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  StdPicture = Picture;
  StdFont = Font;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PUserType1 = ^TGUID; {*}
  PShortint1 = ^Shortint; {*}
  PPShortint1 = ^PShortint1; {*}
  PUserType2 = ^DISPPARAMS; {*}
  POleVariant1 = ^OleVariant; {*}

  GUID = record
    Data1: LongWord;
    Data2: Word;
    Data3: Word;
    Data4: array[0..7] of Byte;
  end;

  DISPPARAMS = record
    rgvarg: ^OleVariant;
    rgdispidNamedArgs: ^Integer;
    cArgs: SYSUINT;
    cNamedArgs: SYSUINT;
  end;

  EXCEPINFO = record
    wCode: Word;
    wReserved: Word;
    bstrSource: WideString;
    bstrDescription: WideString;
    bstrHelpFile: WideString;
    dwHelpContext: LongWord;
    pvReserved: Pointer;
    pfnDeferredFillIn: Pointer;
    scode: SCODE;
  end;

  OLE_COLOR = LongWord; 
  OLE_XPOS_PIXELS = Integer; 
  OLE_YPOS_PIXELS = Integer; 
  OLE_XSIZE_PIXELS = Integer; 
  OLE_YSIZE_PIXELS = Integer; 
  OLE_XPOS_HIMETRIC = Integer; 
  OLE_YPOS_HIMETRIC = Integer; 
  OLE_XSIZE_HIMETRIC = Integer; 
  OLE_YSIZE_HIMETRIC = Integer; 
  OLE_XPOS_CONTAINER = Single; 
  OLE_YPOS_CONTAINER = Single; 
  OLE_XSIZE_CONTAINER = Single; 
  OLE_YSIZE_CONTAINER = Single; 
  OLE_HANDLE = SYSINT; 
  OLE_OPTEXCLUSIVE = WordBool; 
  OLE_CANCELBOOL = WordBool; 
  OLE_ENABLEDEFAULTBOOL = WordBool; 
  FONTNAME = WideString; 
  FONTSIZE = Currency; 
  FONTBOLD = WordBool; 
  FONTITALIC = WordBool; 
  FONTUNDERSCORE = WordBool; 
  FONTSTRIKETHROUGH = WordBool; 
  IFontEventsDisp = FontEvents; 

// *********************************************************************//
// Interface: IEnumVARIANT
// Flags:     (16) Hidden
// GUID:      {00020404-0000-0000-C000-000000000046}
// *********************************************************************//
  IEnumVARIANT = interface(IUnknown)
    ['{00020404-0000-0000-C000-000000000046}']
    function Next(celt: LongWord; var rgvar: OleVariant; out pceltFetched: LongWord): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppenum: IEnumVARIANT): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IFont
// Flags:     (16) Hidden
// GUID:      {BEF6E002-A874-101A-8BBA-00AA00300CAB}
// *********************************************************************//
  IFont = interface(IUnknown)
    ['{BEF6E002-A874-101A-8BBA-00AA00300CAB}']
    function Get_Name(out pname: WideString): HResult; stdcall;
    function Set_Name(const pname: WideString): HResult; stdcall;
    function Get_Size(out psize: Currency): HResult; stdcall;
    function Set_Size(psize: Currency): HResult; stdcall;
    function Get_Bold(out pbold: WordBool): HResult; stdcall;
    function Set_Bold(pbold: WordBool): HResult; stdcall;
    function Get_Italic(out pitalic: WordBool): HResult; stdcall;
    function Set_Italic(pitalic: WordBool): HResult; stdcall;
    function Get_Underline(out punderline: WordBool): HResult; stdcall;
    function Set_Underline(punderline: WordBool): HResult; stdcall;
    function Get_Strikethrough(out pstrikethrough: WordBool): HResult; stdcall;
    function Set_Strikethrough(pstrikethrough: WordBool): HResult; stdcall;
    function Get_Weight(out pweight: Smallint): HResult; stdcall;
    function Set_Weight(pweight: Smallint): HResult; stdcall;
    function Get_Charset(out pcharset: Smallint): HResult; stdcall;
    function Set_Charset(pcharset: Smallint): HResult; stdcall;
    function Get_hFont(out phfont: OLE_HANDLE): HResult; stdcall;
    function Clone(out ppfont: IFont): HResult; stdcall;
    function IsEqual(const pfontOther: IFont): HResult; stdcall;
    function SetRatio(cyLogical: Integer; cyHimetric: Integer): HResult; stdcall;
    function AddRefHfont(hFont: OLE_HANDLE): HResult; stdcall;
    function ReleaseHfont(hFont: OLE_HANDLE): HResult; stdcall;
  end;

// *********************************************************************//
// DispIntf:  Font
// Flags:     (4096) Dispatchable
// GUID:      {BEF6E003-A874-101A-8BBA-00AA00300CAB}
// *********************************************************************//
  Font = dispinterface
    ['{BEF6E003-A874-101A-8BBA-00AA00300CAB}']
    property Name: WideString dispid 0;
    property Size: Currency dispid 2;
    property Bold: WordBool dispid 3;
    property Italic: WordBool dispid 4;
    property Underline: WordBool dispid 5;
    property Strikethrough: WordBool dispid 6;
    property Weight: Smallint dispid 7;
    property Charset: Smallint dispid 8;
  end;

// *********************************************************************//
// DispIntf:  FontEvents
// Flags:     (4112) Hidden Dispatchable
// GUID:      {4EF6100A-AF88-11D0-9846-00C04FC29993}
// *********************************************************************//
  FontEvents = dispinterface
    ['{4EF6100A-AF88-11D0-9846-00C04FC29993}']
    procedure FontChanged(const PropertyName: WideString); dispid 9;
  end;

// *********************************************************************//
// Interface: IPicture
// Flags:     (16) Hidden
// GUID:      {7BF80980-BF32-101A-8BBB-00AA00300CAB}
// *********************************************************************//
  IPicture = interface(IUnknown)
    ['{7BF80980-BF32-101A-8BBB-00AA00300CAB}']
    function Get_Handle(out phandle: OLE_HANDLE): HResult; stdcall;
    function Get_hPal(out phpal: OLE_HANDLE): HResult; stdcall;
    function Get_type_(out ptype: Smallint): HResult; stdcall;
    function Get_Width(out pwidth: OLE_XSIZE_HIMETRIC): HResult; stdcall;
    function Get_Height(out pheight: OLE_YSIZE_HIMETRIC): HResult; stdcall;
    function Render(hdc: SYSINT; x: Integer; y: Integer; cx: Integer; cy: Integer; 
                    xSrc: OLE_XPOS_HIMETRIC; ySrc: OLE_YPOS_HIMETRIC; cxSrc: OLE_XSIZE_HIMETRIC; 
                    cySrc: OLE_YSIZE_HIMETRIC; var prcWBounds: Pointer): HResult; stdcall;
    function Set_hPal(phpal: OLE_HANDLE): HResult; stdcall;
    function Get_CurDC(out phdcOut: SYSINT): HResult; stdcall;
    function SelectPicture(hdcIn: SYSINT; out phdcOut: SYSINT; out phbmpOut: OLE_HANDLE): HResult; stdcall;
    function Get_KeepOriginalFormat(out pfkeep: WordBool): HResult; stdcall;
    function Set_KeepOriginalFormat(pfkeep: WordBool): HResult; stdcall;
    function PictureChanged: HResult; stdcall;
    function SaveAsFile(var pstm: Pointer; fSaveMemCopy: WordBool; out pcbSize: Integer): HResult; stdcall;
    function Get_Attributes(out pdwAttr: Integer): HResult; stdcall;
    function SetHdc(hdc: OLE_HANDLE): HResult; stdcall;
  end;

// *********************************************************************//
// DispIntf:  Picture
// Flags:     (4096) Dispatchable
// GUID:      {7BF80981-BF32-101A-8BBB-00AA00300CAB}
// *********************************************************************//
  Picture = dispinterface
    ['{7BF80981-BF32-101A-8BBB-00AA00300CAB}']
    procedure Render(hdc: SYSINT; x: Integer; y: Integer; cx: Integer; cy: Integer; 
                     xSrc: OLE_XPOS_HIMETRIC; ySrc: OLE_YPOS_HIMETRIC; cxSrc: OLE_XSIZE_HIMETRIC; 
                     cySrc: OLE_YSIZE_HIMETRIC; var prcWBounds: {??Pointer}OleVariant); dispid 6;
    property hPal: OLE_HANDLE dispid 2;
    property type_: Smallint readonly dispid 3;
    property Handle: OLE_HANDLE readonly dispid 0;
    property Width: OLE_XSIZE_HIMETRIC readonly dispid 4;
    property Height: OLE_YSIZE_HIMETRIC readonly dispid 5;
  end;

// *********************************************************************//
// The Class CoStdPicture provides a Create and CreateRemote method to          
// create instances of the default interface Picture exposed by              
// the CoClass StdPicture. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoStdPicture = class
    class function Create: Picture;
    class function CreateRemote(const MachineName: string): Picture;
  end;

// *********************************************************************//
// The Class CoStdFont provides a Create and CreateRemote method to          
// create instances of the default interface Font exposed by              
// the CoClass StdFont. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoStdFont = class
    class function Create: Font;
    class function CreateRemote(const MachineName: string): Font;
  end;

implementation

uses ComObj;

class function CoStdPicture.Create: Picture;
begin
  Result := CreateComObject(CLASS_StdPicture) as Picture;
end;

class function CoStdPicture.CreateRemote(const MachineName: string): Picture;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_StdPicture) as Picture;
end;

class function CoStdFont.Create: Font;
begin
  Result := CreateComObject(CLASS_StdFont) as Font;
end;

class function CoStdFont.CreateRemote(const MachineName: string): Font;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_StdFont) as Font;
end;

end.
