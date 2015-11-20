unit FastDraw;

interface
uses Windows, SysUtils, Classes, Graphics, Misc, FastSync;

 const
      DEFPAL_8BIT: array [BYTE] of DWORD =
     ($000000,$000080,$008000,$008080,$800000,$800080,$808000,$C0C0C0,
      $C0DCC0,$F0CAA6,$002040,$002060,$002080,$0020A0,$0020C0,$0020E0,
      $004000,$004020,$004040,$004060,$004080,$0040A0,$0040C0,$0040E0,
      $006000,$006020,$006040,$006060,$006080,$0060A0,$0060C0,$0060E0,
      $008000,$008020,$008040,$008060,$008080,$0080A0,$0080C0,$0080E0,
      $00A000,$00A020,$00A040,$00A060,$00A080,$00A0A0,$00A0C0,$00A0E0,
      $00C000,$00C020,$00C040,$00C060,$00C080,$00C0A0,$00C0C0,$00C0E0,
      $00E000,$00E020,$00E040,$00E060,$00E080,$00E0A0,$00E0C0,$00E0E0,
      $400000,$400020,$400040,$400060,$400080,$4000A0,$4000C0,$4000E0,
      $402000,$402020,$402040,$402060,$402080,$4020A0,$4020C0,$4020E0,
      $404000,$404020,$404040,$404060,$404080,$4040A0,$4040C0,$4040E0,
      $406000,$406020,$406040,$406060,$46080,$4060A0,$4060C0,$4060E0,
      $408000,$408020,$408040,$408060,$408080,$4080A0,$4080C0,$4080E0,
      $40A000,$40A020,$40A040,$40A060,$40A080,$40A0A0,$40A0C0,$40A0E0,
      $40C000,$40C020,$40C040,$40C060,$40C080,$40C0A0,$40C0C0,$40C0E0,
      $40E000,$40E020,$40E040,$40E060,$40E080,$40E0A0,$40E0C0,$40E0E0,
      $800000,$800020,$800040,$800060,$800080,$8000A0,$8000C0,$8000E0,
      $802000,$802020,$802040,$802060,$802080,$8020A0,$8020C0,$8020E0,
      $804000,$804020,$804040,$804060,$804080,$8040A0,$8040C0,$8040E0,
      $806000,$806020,$806040,$806060,$806080,$8060A0,$8060C0,$8060E0,
      $808000,$808020,$808040,$808060,$808080,$8080A0,$8080C0,$8080E0,
      $80A000,$80A020,$80A040,$80A060,$80A080,$80A0A0,$80A0C0,$80A0E0,
      $80C000,$80C020,$80C040,$80C060,$80C080,$80C0A0,$80C0C0,$80C0E0,
      $80E000,$80E020,$80E040,$80E060,$80E080,$80E0A0,$80E0C0,$80E0E0,
      $C00000,$C00020,$C00040,$C00060,$C00080,$C000A0,$C000C0,$C000E0,
      $C02000,$C02020,$C02040,$C02060,$C02080,$C020A0,$C020C0,$C020E0,
      $C04000,$C04020,$C04040,$C04060,$C04080,$C040A0,$C040C0,$C040E0,
      $C06000,$C06020,$C06040,$C06060,$C06080,$C060A0,$C060C0,$C060E0,
      $C08000,$C08020,$C08040,$C08060,$C08080,$C080A0,$C080C0,$C080E0,
      $C0A000,$C0A020,$C0A040,$C0A060,$C0A080,$C0A0A0,$C0A0C0,$C0A0E0,
      $C0C000,$C0C020,$C0C040,$C0C060,$C0C080,$C0C0A0,$F0FBFF,$A4A0A0,
      $808080,$0000FF,$00FF00,$00FFFF,$FF0000,$FF00FF,$FFFF00,$FFFFFF);


    PIF_FILL = $0001;


type
    TPenAttrs = record
     color: TColor;
     width: Integer;
     style: TPenStyle;
    end;

    PPenAttrs = ^TPenAttrs;

    TBrushAttrs = record
     color: TColor;
     style: TBrushStyle;
    end;

    PBrushAttrs = ^TBrushAttrs;

    TFontAttrs = record
     color: TColor;
     size: Integer;
     style: TFontStyle;
    end;

    PFontAttrs = ^TFontAttrs;


    TPaintItemCode = (PIC_FREE, PIC_PIXEL, PIC_LINE, PIC_POLYGONE, PIC_ELLIPSE, PIC_TEXT);

    TPaintItem = packed record
     flags: DWORD;
     icode: TPaintItemCode;
     pa: TPenAttrs;
     fa: TFontAttrs;
     ba: TBrushAttrs;
     pts: array [0..3] of TPoint;    // also rectangle area for text/ellipse
     n_str: Integer;                 // string index
    end; // TPaintItem

    PPaintItem = ^TPaintItem;


    TGDITool = record
     hObj: HGDIOBJ;
     ttyp: CHAR;        // #0 B F P
     last_used: TDateTime;

     case BYTE of
      0: (pa: TPenAttrs);
      1: (fa: TFontAttrs);
      2: (ba: TBrushAttrs);

    end; // TGDITool

    PGDITool = ^TGDITool;

    // базовый класс пр€моугольной области дл€ рисовани€
    TFastDrawBox = class
    private
     procedure SetGrayScale(const Value: Boolean);
    procedure SetMaxCacheItems(const Value: Integer);
    protected
     FWidth, FHeight: Integer;
     FBPP: Integer;
     FGrayScale: Boolean;
     FPenAttrs: TPenAttrs;
     FDrawOrigin: TPoint;
     sc_share: TCritSection;
     FBuffer: array of TPaintItem;
     FPaintItems: TList;
     FStrings: TStrings;
     FMaxCacheItems: Integer;

    public

     { props }
     property           BPP: Integer read FBPP;
     property           GrayScale: Boolean read FGrayScale write SetGrayScale;

     property           MaxCacheItems: Integer read FMaxCacheItems write SetMaxCacheItems;
     property           PenAttrs: TPenAttrs read FPenAttrs write FPenAttrs;

      property           Height: Integer read FHeight;
      property           Width: Integer read FWidth;

     { C & D }
     constructor        Create (AWidth, AHeight, ABPP: Integer);
     destructor         Destroy; override;

     { methods }
     procedure          Flush; virtual;

     procedure          SetSize(AWidth, AHeight: Integer); virtual;
    end; // TFastDrawBox


    TGDIFastDrawBox = class (TFastDrawBox)
    protected
     hSection: THandle; // file-mapping section
     hDIB: HBITMAP;
     hPal: HPALETTE;
     tool_cache: array [0..255] of TGDITool;

     pLogPal: PLOGPALETTE;
     FDC: HDC;
     FBits: PByteArray;

     function           AllocateDIB: Boolean;
     procedure          ReleaseDIB;
    public

     property           DC: HDC read FDC;
     { C & D }
     constructor        Create (AWidth, AHeight, ABPP: Integer);
     { methods }

     procedure          Flush; override;
     procedure          SetSize (AWidth, AHeight: Integer); override;
    end;

implementation
uses Math;

procedure GDIRelease(var hObj: HGDIOBJ);
begin
 if hObj = 0 then exit;
 DeleteObject (hObj);
 hObj := 0;
end;


function CreateBMPInfo (Width, Height, BPP: SmallInt): PBitmapInfo;
var clrcnt, szInfo: Integer;
begin
 clrcnt := 0;
 if BPP <= 8  then clrcnt := 256;
 if BPP <= 4  then clrcnt := 16;
 if BPP <= 1  then clrcnt := 2;

 szinfo := sizeof(TBitmapInfo) + sizeof(TRGBQuad) * clrcnt + 2048;
 GetMem(Result, szinfo);
 ZeroMemory(Result,  sizeof(TBitmapInfo));
 with Result^ do
 begin
  bmiHeader.biSize     := sizeof(bmiHeader);
  bmiHeader.biWidth    := Width;
  bmiHeader.biHeight   := Height;
  bmiHeader.biPlanes   := 1;
  bmiHeader.biBitCount := BPP;
  bmiHeader.biCompression := BI_RGB;
  bmiHeader.biClrUsed := clrcnt;
  // bmiHeader.biSizeImage   := 0;
 end;

end; // CreateBMPInfo



procedure ConvertPalette (const pal: LOGPALETTE; var coltab: array of RGBQUAD; Count: Integer);
var n: Integer;
begin
 for n := 0 to Count - 1 do
 with pal.palPalEntry [n], coltab [n] do
 begin
  rgbRed := peRed;
  rgbBlue := peBlue;
  rgbGreen := peGreen;
  rgbReserved := 0;
 end;
end; // ConvertPalette


{ TGDIFastDrawBox }

function TGDIFastDrawBox.AllocateDIB;
var szdata: Integer;
    lcurr, lstep: Integer;
    n, clrused: Integer;
    px_bytes: Single;
    info: PBitmapInfo;
    sdc: HDC;
begin
 result := FALSE;
 if (Width <= 0) or (Height <= 0) or (BPP <= 0) then exit;
 sdc := GetDC (0);

 FDC := CreateCompatibleDC (sdc);
 if DC = 0 then
   begin
    PrintError('CreateCompatibleDC returned 0');
    exit;
   end;

 px_bytes := 1;
 // определение кол-ва байт на пиксел
 if BPP <= 32 then px_bytes := 4.0;
 if BPP <= 16 then px_bytes := 2.0;
 if BPP <= 8 then px_bytes := 1.0;
 if BPP <= 4 then px_bytes := 0.5;
 if BPP = 1 then px_bytes := 1 / 8;


 szdata := Round (px_bytes * Width * Height) or $FFF + 1; // 4k boundary

 hSection := CreateFileMapping (INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, szdata, nil);
 if hSection = 0 then
  begin
   PrintError('Ќе удалось создать секцию, функци€ CreateFileMapping вернула 0. LastError = ' + Err2Str (GetLastError) );
   exit;
  end;

 info := CreateBMPInfo (Width, Height, BPP);
 clrUsed := info.bmiHeader.biClrUsed;

 if clrUsed > 0 then
  begin
   // локальна€ палитра
   szdata := sizeof (LOGPALETTE) + sizeof (PALETTEENTRY) * clrUsed;
   GetMem (pLogPal, szdata);
   FillChar (pLogPal^, szdata, 0);

   // TODO: генераци€ палитры
   lstep := 256 div (clrUsed - 1);
   lcurr := 0;
   if GrayScale or (clrUsed = 2) then
    for n := 1 to clrUsed do
     begin
      lcurr := Min(255, lcurr);
      pLogPal.palPalEntry[n].peRed := lcurr;
      pLogPal.palPalEntry[n].peGreen := lcurr;
      pLogPal.palPalEntry[n].peBlue := lcurr;
      Inc (lcurr, lstep);
     end
   else
    case clrUsed of
     256: SafeMove (DEFPAL_8BIT, pLogPal.palPalEntry[0], sizeof (PALETTEENTRY) * 256);
    end;
   // применение палитры к контексту
   hpal := CreatePalette (pLogPal^);
   SelectObject (DC, hPal);
   RealizePalette (DC);
   ConvertPalette (pLogPal^, info.bmiColors, clrUsed);
  end;

 // создание DIB-секции
 hDIB := CreateDIBSection (sdc, info^, DIB_RGB_COLORS, Pointer (FBits), hSection, 0);

 result := (hDIB <> 0);
 if result then
    result := ( SelectObject (DC, hDIB) <> 0 )
 else
    PrintError('Ќе удалось создать DIB-секцию.');


 ReleaseDC (0, sdc);
 FreeMem (info);
end;

constructor TGDIFastDrawBox.Create(AWidth, AHeight, ABPP: Integer);
begin
 inherited Create (AWidth, AHeight, ABPP);

 GDISetBatchLimit (1000);
end;

procedure TGDIFastDrawBox.Flush;
var n, i: Integer;
    ppi: PPaintItem;
    ptl: PGDITool;
    sel: Boolean;
    ATools: array [0..2] of HGDIOBJ;


begin
 if FPaintItems.Count = 0 then exit;
 sc_share.Lock('Flush');
 FillChar (ATools, sizeof (ATools), 0);
 ptl := nil;

 try
  // TODO: optimize for similars
  for n := 0 to FPaintItems.Count - 1 do
   begin
    ppi := FPaintItems [n];
    // ptl := FindCreateTool ( ppi ); // поиск или создание инструмента
    if ptl = nil then continue;
    sel := TRUE;
    for i := 0 to High (ATools) do
        if ATools[i] = ptl.hObj then sel := FALSE;
    // активаци€ инструмента, если он ещЄ не активирован
    if sel then
     begin
      case ptl.ttyp of
       'B': ATools[0] := ptl.hObj;
       'F': ATools[1] := ptl.hObj;
       'P': ATools[2] := ptl.hObj;
      end;
      // ActivateTool (ptl);
     end; // case



    ppi.icode := PIC_FREE;
   end;


  FPaintItems.Clear;
  FStrings.Clear;
 finally
  sc_share.Unlock;
 end;
 GDIFlush ();


end; // Flush

procedure TGDIFastDrawBox.ReleaseDIB;
begin
 if DC <> 0 then DeleteDC ( DC );
 GDIRelease ( HGDIOBJ (hDIB) );

 if hSection <> 0 then CloseHandle (hSection);

 if pLogPal <> nil then FreeMem (pLogPal);

 FDC := 0;
 hSection := 0;

end;

procedure TGDIFastDrawBox.SetSize(AWidth, AHeight: Integer);
begin
 inherited;
 sc_share.Lock ('ReSizing');
 try
  ReleaseDIB;
  AllocateDIB;
 finally
  sc_share.Unlock;
 end;
end; // SetSize;

{ TFastDrawBox }

constructor TFastDrawBox.Create(AWidth, AHeight, ABPP: Integer);
begin
 FBPP := ABPP;
 sc_share := TCritSection.Create(ClassName + '.sc_share');
 FPaintItems := TList.Create;
 FStrings := TStringList.Create;
 self.MaxCacheItems := 200;
 SetSize(AWidth, AHeight);
end;

destructor TFastDrawBox.Destroy;
begin
 MaxCacheItems := 0;
 FPaintItems.Free;
 FStrings.Free;
 sc_share.Free;
 inherited;
end;

procedure TFastDrawBox.Flush;
begin
 FPaintItems.Clear;
 FStrings.Clear;
end;

procedure TFastDrawBox.SetGrayScale(const Value: Boolean);
begin
 if GrayScale = Value then exit;
 FGrayScale := Value;
 SetSize(Width, Height);
end;

procedure TFastDrawBox.SetMaxCacheItems(const Value: Integer);
begin
 sc_share.Lock('Cache-Resize');
 try
  if FPaintItems.Count > 0 then
     Flush;
  FMaxCacheItems := Value;
  SetLength (FBuffer, Value);
 finally
  sc_share.Unlock;
 end;
end; // SetMaxCacheItems

procedure TFastDrawBox.SetSize(AWidth, AHeight: Integer);
begin
 FWidth := AWidth;
 FHeight := AHeight;
end;

end.
