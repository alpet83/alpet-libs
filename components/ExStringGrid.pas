unit ExStringGrid;

interface

uses
  Windows, SysUtils, Classes, Controls, Grids, UniArray,
  Variants, Graphics, Menus, Forms, StdCtrls, Mask, ContNrs;

const
   TF_ALIGN_RIGHT = 1;
   TF_ALIGN_CENTER = 2;

   clTransparent: TColor = -1;

type
  TFontAttr = packed record
   size: BYTE;
   style: TFontStyles;
  end; // TFontAttr

  TCellAttr = packed record
   font_index: Integer;
   font_attr: TFontAttr;
   text_format: TTextFormat;
   bg_color, bg_scolor, fg_color, fg_scolor: TColor;
  end;

  PCellAttr = ^TCellAttr;
  TCellsAttrs = packed array [0..65535] of TCellAttr;
  PCellsAttrs = ^TCellsAttrs;


  TRowAttrs = class (TMatrixArray)
  protected
   FSize: Integer;
   FDefaultAttr: PCellAttr;

   function   GetCellAttr(n_cell: Integer): PCellAttr;
  public



   { props }
   property     CellsAttrs [index: Integer]: PCellAttr read GetCellAttr;
   property     DefaultAttr: PCellAttr read FDefaultAttr write FDefaultAttr;
   { C & D }
   constructor  Create; override;

  end; // TRowAttrs



  TStringGridEx = class(TStringGrid)
  private
    { Private declarations }
    FRowsAttrs: TObjectList;
    FDefaultAttr: TCellAttr;
    FCurrentAttr: TCellAttr;
    FNegValuesAttr: TCellAttr;
    FDrawHighlight: Boolean;
    FHighlightColor: TColor;
    FSelNegValues: Boolean;
    inside_call: Boolean;
    procedure    SetHighlightColor(const Value: TColor);
    function     GetDefaultAttr: PCellAttr;
    function     GetNegValuesAttr: PCellAttr;

  protected
    { Protected declarations }
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;

    function GetCellAttr(ACol, ARow: Longint): PCellAttr;
  public
    { Public declarations }
    property     CellsAttrs[ACol, ARow: Integer]: PCellAttr read GetCellAttr;
    property     DefaultAttr: PCellAttr read GetDefaultAttr;
    property     NegValuesAttr: PCellAttr read GetNegValuesAttr;
    { C & D }

    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    { Published declarations }

    property     SelNegValues: Boolean read FSelNegValues write FSelNegValues default FALSE;
    property     DrawHighlight: Boolean read FDrawHighlight write FDrawHighlight default TRUE;
    property     HighlightColor: TColor read FHighlightColor write SetHighlightColor default clHighlight;
  end;

procedure Register;

function GetFontIndexByName(const sFontName: TFontName): Integer;

implementation
Uses Math, Misc;


var
   gFontList: TStrings = nil;


procedure Register;
begin
  RegisterComponents('Samples', [TStringGridEx]);
end;

function RegFontProc( plf: PENUMLOGFONTEX;  pntm: PNEWTEXTMETRICEX; dwType: DWORD; pList: Pointer): Integer; stdcall;
var s: String;
    lst: TStrings;
begin
 result := 1;
 s := Trim(plf.elfLogFont.lfFaceName);
 lst := pList;
 if lst.IndexOf(s) < 0 then
        lst.Add(s);
end; // RegFontProc

procedure InitFontList;
var lf: TLogFont;
    dc: HDC;
begin
 FillChar(lf, sizeof(lf), 0);
 lf.lfCharSet := DEFAULT_CHARSET;
 gFontList := TStringList.Create;
 with TStringList (gFontList) do
  begin
   Sorted := TRUE;
   CaseSensitive :=FALSE;
  end;
 dc := GetDC(0);
 try
  EnumFontFamiliesEx ( dc, lf, @RegFontProc, DWORD (gFontList), 0);
 except
  on E: Exception do
     Assert(FALSE, 'Exception catched in InitFontList: ' + E.Message);
 end;
 ReleaseDC (0, dc);
end; // InitFontList

function GetFontIndexByName(const sFontName: TFontName): Integer;
begin
 if gFontList = nil then InitFontList;
 result := gFontList.IndexOf ( Trim(sFontName) );
end; // GetFontIndexByName

function GetFontNameByIndex(nIndex: Integer): TFontName;

begin
 result := 'Arial';
 if gFontList = nil then InitFontList;
 if (nIndex < 0) or (nIndex >= gFontList.Count) then exit;
 result := gFontList[nIndex];
end; // getFontNameByIndex


function ApplyAttr(caller: TStringGridEx; pOldAttr, pNewAttr: PCellAttr; dstate: TGridDrawState; bUpdate: Boolean = TRUE): TTextFormat;
begin
 result := [];
 if not Assigned (pNewAttr) then exit;

 with caller, pNewAttr^ do
  begin
   // if pOldAttr.font_index <> font_index then
   Canvas.Font.Size :=  font_attr.size;
   Canvas.Font.Style := font_attr.Style;
   Canvas.Font.Name :=  GetFontNameByIndex (pNewAttr.font_index);
   Canvas.Font.Pitch := fpFixed;

   if (gdSelected in dstate) and
       ( // фокус рисуется в случае не активности компонента, или разрешении рисования фокуса и выделенной линии
         not (gdFocused in dstate) or ( [goDrawFocusSelected, goRowSelect] * Options <> [] )
       ) and (DrawHighlight) then
     begin
      Canvas.Brush.Color := bg_scolor;
      Canvas.Pen.Color := fg_scolor;
      Canvas.Font.Color := fg_scolor;
     end
   else
     begin
      Canvas.Brush.Color := bg_color;
      Canvas.Pen.Color := fg_color;
      Canvas.Font.Color := fg_color;
     end;

   result := text_format;

  end;
  if bUpdate then  pOldAttr^ := pNewAttr^;
end; // ApplyAttr

{ TStringGridEx }

constructor TStringGridEx.Create(AOwner: TComponent);
begin
  inherited;
  FRowsAttrs := TObjectList.Create(TRUE);
  FHighlightColor := clHighlight;
  DrawHighlight := True;

  with FDefaultAttr do
   begin
    font_index := GetFontIndexByName('Arial');
    font_attr.size := 10;
    font_attr.style := [];
    bg_color := Color;
    bg_scolor := HighlightColor;
    fg_color := Font.Color;
    fg_scolor := clHighlightText;
   end;
 NegValuesAttr^ := FDefaultAttr;
 NegValuesAttr.fg_color  := clRed;
 NegValuesAttr.fg_scolor := $FFA0A0;
end; // Create


destructor TStringGridEx.Destroy;
begin
  try
   FRowsAttrs.Clear;
  except
   on E: Exception do
     OnExceptLog ('FRowsAttrs.Clear', E);
  end;

  FreeAndNil (FRowsAttrs);
  inherited;
end;

procedure TStringGridEx.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
var pca: PCellAttr;
    tf: TTextFormat;
    s: String;
    tmp: TCellAttr;
begin
  inside_call := TRUE;
  pca := GetCellAttr(ACol, ARow);
  tf := [];
  if (pca <> nil) and (ACol >= FixedCols) and (ARow >= FixedRows) then
    begin
     tf := ApplyAttr(self, @FCurrentAttr, pca, AState);
     // Canvas.FillRect (ARect);
    end;


 s := Cells [ACol, ARow];
 if (ACol >= FixedCols) and (ARow >= FixedRows) then
     Canvas.FillRect (ARect);

 if SelNegValues and IsNumber(s) and (atof (s) < 0) then
  begin
   tmp := NegValuesAttr^;
   NegValuesAttr^ := FCurrentAttr;
   NegValuesAttr.fg_color := tmp.fg_color;
   NegValuesAttr.fg_scolor := tmp.fg_scolor;
   tf := ApplyAttr(self, @FCurrentAttr, NegValuesAttr, AState);
  end;
 Inc (ARect.Left, 2);
 Inc (ARect.Top, 2);
 Dec (ARect.Right, 2);
 Dec (ARect.Bottom, 2);
 Canvas.TextRect (ARect, s, tf);
 inside_call := FALSE;

 // inherited;
end;


function TStringGridEx.GetCellAttr(ACol, ARow: Integer): PCellAttr;
var ra: TRowAttrs;
begin
 // выделение дополнительных строк и ячеек, в случае нехватки оных
 ACol := Max(0, Min(ACol, ColCount));
 ARow := Max(0, Min(ARow, RowCount));
 while ( ARow >= FRowsAttrs.Count ) do
        begin
         ra := TRowAttrs.Create;
         ra.DefaultAttr := @FDefaultAttr;
         ra.SetSize(ColCount * ra.RowSize);
         ra.GetCellAttr(ColCount - 1);
         FRowsAttrs.Add (ra);
        end;

 ra := TRowAttrs ( FRowsAttrs[ARow] );

 ASSERT (ra <> nil, 'RowAttrs [' + IntToStr(AROw) + '] = nil');
 ASSERT (ra.memory <> nil, 'RowAttrs [' + IntToStr(AROw) + '].Memory = nil');

 result := ra.GetCellAttr(ACol);

 if not inside_call then
    InvalidateCell(ACol, ARow);
end;

function TStringGridEx.GetDefaultAttr: PCellAttr;
begin
 result := @FDefaultAttr;
end;

function TStringGridEx.GetNegValuesAttr: PCellAttr;
begin
 result := @FNegValuesAttr;
end;

procedure TStringGridEx.SetHighlightColor(const Value: TColor);
begin
 FHighlightColor := Value;
 FDefaultAttr.bg_scolor := Value;
end;

{ TRowAttrs }

constructor TRowAttrs.Create;
begin
 inherited Create (sizeof (TCellAttr));
 grow_step := 1;
 SetSize(16);
 Clear;
end;


function TRowAttrs.GetCellAttr(n_cell: Integer): PCellAttr;
var n, newc, oldc: Integer;
    prows: PCellsAttrs;
begin
 result := nil;
 if (n_cell < 0) then exit;
 if n_cell >= Count then
   begin
    oldc := Count;
    newc := n_cell + 1;
    AddRows (newc);
    prows := Memory;
    if DefaultAttr = nil then
     for n := oldc to Count - 1 do
       FillChar(prows[n], sizeof (TCellAttr), $FF)
    else
      for n := oldc to Count - 1 do
                 prows[n] := DefaultAttr^;
   end;
 prows := Memory;
 result := @prows [n_cell];
end; // GetCellAttr


initialization
finalization
 gFontList.Free;
end.
