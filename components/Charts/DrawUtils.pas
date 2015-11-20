unit DrawUtils;

interface
uses Windows, SysUtils, Classes, Controls, Graphics, UTITypes, ExtCtrls, ContNrs, UNIArray;

type
    TChartBox = class (TControl)
    protected
     FLineColor, FBgColor: TColor;
     FPaintZone: TRect;


     procedure                  SetBgColor (cl: TColor); virtual;
     procedure                  SetLineColor (cl: TColor); virtual;

    public
     bAbsoluteTop: Boolean;


     property                   BackgroundColor: TColor read FBgColor write SetBgColor;
     property                   LineColor: TColor read FLineColor write SetLineColor;
     property                   PaintZone: TRect read FPaintZone;

     procedure                  RecalcRect (const prvRect: TRect); virtual;

     constructor                Create (AOwner: TComponent); override;

     procedure                  CalcPaintZone;
     procedure                  DrawBackground (Canvas: TCanvas); virtual;
    end;

implementation

{ TChartBox }

constructor TChartBox.Create(AOwner: TComponent);
begin
 ASSERT (AOwner is TControl, 'AOwner must be TWinControl');
 inherited;
 BackgroundColor := clNavy;
 LineColor := clYellow;
 bAbsoluteTop := TRUE;
end;

procedure TChartBox.DrawBackground;
begin
 with Canvas do
  begin
   Brush.Style := bsSolid;
   Brush.Color := BackgroundColor;
   Pen.Style := psSolid;
   Pen.Color := LineColor;
   with PaintZone do
        Rectangle (Left, Top, Left + Width, Top + Height);
  end;
end; // DrawBackground

procedure TChartBox.RecalcRect;
begin
 // no actions
 CalcPaintZone;
end;

procedure TChartBox.SetBgColor(cl: TColor);
begin
 FBgColor := cl;
end;

procedure TChartBox.SetLineColor(cl: TColor);
begin
 FLineColor := cl;
end;


procedure TChartBox.CalcPaintZone;
begin
 FPaintZone.Left := Left;
 FPaintZone.Right := Left + Width - 1;
 if bAbsoluteTop then
    FPaintZone.Top := Top
 else
    FPaintZone.Top := 0;

 FPaintZone.Bottom := FPaintZone.Top + Height - 1;
end; // PaintZone

end.
