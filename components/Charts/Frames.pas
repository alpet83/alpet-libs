unit Frames;

interface
uses Windows, SysUtils, Classes, ContNrs, Controls, ExtCtrls, Graphics, Axis, Indicators, DrawUtils, ChartData;

type
    TChartFrame = class (TChartBox)        // default = картинко
    private

    protected
     FIndicators: TIndicatorList;

     FLeftAxis, FRightAxis: TChartAxis;

     FCalcOrder: Integer;
     FGridColor: TColor;

     FImage: TImage;
     FName: String;
     prev_size: SIZE;

     procedure                  SetParent(AParent: TWinControl); override;
    public
     RelHeight: Single; // относительная высота
     VSpace: Integer;


     property                   CalcOrder: Integer read FCalcOrder write FCalcOrder;
     property                   GridColor: TColor read FGridColor write FGridColor;
     property                   Indicators: TIndicatorList read FIndicators;
     property                   LeftAxis: TChartAxis read FLeftAxis;
     property                   RightAxis: TChartAxis read FRightAxis;
     property                   Name: String read FName write FName;

     { C & D }

     constructor                Create(AOwner: TComponent); override;
     destructor                 Destroy; override;

     { Methods }
     procedure                  CalcFrameData();
     procedure                  DrawFrame();
     procedure                  DrawBackground (Canvas: TCanvas); override;
     procedure                  DrawGrid (Canvas: TCanvas);

     function                   HitObject(x, y: Integer): String;
     procedure                  PaintTo(Canvas: TCanvas);       // отображение фрейма отрисованного в буфере
     procedure                  RecalcRect (const prvRect: TRect); override;
     

    end; // TChartFrame

implementation
uses FinCharts, Misc;

{ TChartFrame }

constructor TChartFrame.Create;
begin
 ASSERT (AOwner is TFinChart, 'Owner must be TFinChart control');
 inherited Create (AOwner);
 FName := 'noname';
 FIndicators := TIndicatorList.Create;
 // ----------------------------------- //
 VSpace := 10;
 FLeftAxis := TChartAxis.Create(self);
 FRightAxis := TChartAxis.Create(self);
 FGridColor := clNavy + $101010;
 FCalcOrder := 10;
 LeftAxis.AxisType := atLeft;
 RightAxis.AxisType := atRight;
 Width := TControl (AOwner).Width - (LeftAxis.Width + RightAxis.Height + 2);
 FImage := TImage.Create(nil);
 bAbsoluteTop := FALSE;
 Top := 0;
 Left := 0;
end;


destructor TChartFrame.Destroy;
begin
 FIndicators.Clear;
 FImage.Free;
 FIndicators.Free;
 ODS (Format ('[~T]. #MSG: Frame $%p destroyed', [Pointer (self)]) );
 inherited;
end;


procedure TChartFrame.CalcFrameData;
var
   n: Integer;
   chart: TFinChart;
   ind: TIndicator;

begin
 chart := TFinChart (Owner);
 CalcPaintZone;
 InitRange ( LeftAxis.ValRange );
 InitRange ( RightAxis.ValRange );

 with chart do
 if Assigned (Data) and
    (VisRange.iFirst >= 0) and
    (VisRange.iFirst <= VisRange.iLast)  then
   for n := 0 to Indicators.Count - 1 do
     begin
      ind := TIndicator (Indicators [n]);
      ind.TimeAxis := TimeAxis;
      ind.CalcData (Data, VisRange.iFirst, VisRange.iLast);
      if ind.Axis <> nil then
         ExtentRange (ind.Axis.ValRange, ind.ValRange);

     end;
end;

procedure TChartFrame.DrawFrame;
var
   n: Integer;
   chart: TFinChart;
   ind: TIndicator;
begin
 chart := TFinChart (Owner);
 DrawBackground (FImage.Canvas);
 DrawGrid (FImage.Canvas);

 with chart do
 if Assigned (Data) and
    (VisRange.iFirst >= 0) and
    (VisRange.iFirst <= VisRange.iLast)  then
 begin
   for n := 0 to Indicators.Count - 1 do
     begin
      ind := TIndicator (Indicators [n]);
      ind.Draw(FImage.Canvas);
     end;
 end;

 FImage.Canvas.Font.Color := clWhite;
 FImage.Canvas.TextFlags := 0;
 FImage.Canvas.TextOut (PaintZone.Left + 20, PaintZone.Top + 3, 'frm@' + Name);

 if LeftAxis.Visible  then LeftAxis.DrawAxis (FImage.Canvas);
 if RightAxis.Visible then RightAxis.DrawAxis (FImage.Canvas);


end; // DrawFrame

procedure TChartFrame.DrawBackground;
begin
 inherited;
end; // DrawBackground

procedure TChartFrame.DrawGrid;
var n: Integer;
    chart: TFinChart;
    y1, y2: Integer;
begin
 //
 Canvas.Pen.Style := psDash;
 Canvas.Pen.Color := GridColor;
 chart := TFinChart (Owner);
 y1 := PaintZone.Top + 1;
 y2 := PaintZone.Top + Height - 1;
 with chart.TimeAxis do
  for n := 0 to PTMap.Count - 2 do
   with PTMap.Points [n]^ do
    if flags and 1 <> 0 then
      begin
       Canvas.MoveTo (x_pos, y1);
       Canvas.LineTo (x_pos, y2);
      end;
end;

procedure TChartFrame.RecalcRect;
var
   chart: TFinChart;
   w, h: Integer;
   lAxisW: Integer;
begin
 chart := TFinChart (Owner);
 Height := Round (RelHeight * chart.ChartZoneHeight);
 Width := chart.Width - (LeftAxis.Width + RightAxis.Width);
 Top := prvRect.Bottom;
 Left := 0;
 lAxisW := 0;

 if LeftAxis.Visible then
   begin
    Left := LeftAxis.Width;
    LeftAxis.Left := 0;
    LeftAxis.Top := Top;
    LeftAxis.Height := Height;
    lAxisW := LeftAxis.Width;
    LeftAxis.CalcPaintZone;
    chart.LeftAxisVisible := TRUE;
   end;

 if RightAxis.Visible then
   begin
    RightAxis.Left := chart.Width - lAxisW;
    RightAxis.Top := Top;
    RightAxis.Height := Height;
    RightAxis.CalcPaintZone;
    chart.RightAxisVisible := TRUE;
   end;

 w := chart.Width;
 h := Height;
 if (prev_size.cx <> Width) or (prev_size.cy <> Height) then
  begin
   FImage.Width := w;
   FImage.Height := h;
   with FImage.Picture do
    begin
     if Bitmap.Width < w then Bitmap.Width := w;
     if Bitmap.Height < h then Bitmap.Height := h;
    end;
   //FImage.ClientWidth := chart.Width;
   //FImage.ClientHeight := Height;
   // FImage.Picture.Height := Height;
   // FImage.SetBounds(0, 0, chart.Width, chart.Height );
   {FImage.Width := chart.Width;
   FImage.Height := Height;}
   prev_size.cx := Width;
   prev_size.cy := Height;
  end;
 inherited;
end; // RecalcSize

procedure TChartFrame.SetParent(AParent: TWinControl);
begin
 inherited;
 LeftAxis.Parent := Parent;
 RightAxis.Parent := Parent;
end;


procedure TChartFrame.PaintTo(Canvas: TCanvas);
var rdst, rsrc: TRect;
    chart: TFinChart;
begin
 chart := TFinChart (Owner);
 SetRect (rsrc, 0, 0, chart.Width - 1, Height);
 SetRect (rdst, 0, Top, chart.Width - 1, Top + Height);
 Canvas.CopyRect(rdst, FImage.Canvas, rsrc);
end;

function TChartFrame.HitObject(x, y: Integer): String;
begin
 //
 result := 'MISS';
 if (y > 3) and InBound (y, Top - 1, Top + 1) then result := 'FRAME_TOP';
end;


end.
