unit Axis;
{ Математика рассчета осей, механизмы их отрисовки }
interface
uses Windows, SysUtils, Classes, Controls, Graphics, UTITypes, ExtCtrls, ContNrs, UNIArray,
         DateTimeTools, ChartData, DrawUtils, Misc, ArrayTypes;


const
     TIME_AXIS_HEIGHT = 50;
     CHART_AXIS_WIDTH = 70;                

     DT_LABEL_WIDTH = 50; // минимальное место для рядом стоящих меток



type

    TTimePoint = packed record
       x_pos: Integer;            // graphic position
     n_index: Integer;            // index of bar
      o_time: TSeconds;
      c_time: TSeconds;
      n_date: TIntDate;
       flags: DWORD;
    end; // TTimePoint

    PTimePoint = ^TTimePoint;

    TAxisType = (atNone, atLeft, atRight);

    TPosTimeMap = class (TMatrixArray)
    private
     function                   GetTimePoint (nPoint: Integer): PTimePoint;
    public

     property                   Points[Index: Integer]: PTimePoint read GetTimePoint;

     constructor                Create; override;
    end; // TPosTimeMap



    TTimeAxis = class (TChartBox)
    protected
    public

     l_time: Integer;
     x_step: Single;
     LeftSpace, RightSpace: Integer;
     PTMap: TPosTimeMap;

     { C & D }

     constructor                Create(AOwner: TComponent); override;
     destructor                 Destroy; override;

     { Methods }


     procedure                  Calculate (Data: TDataMap; iFirst, iLast: Integer);

     procedure                  DrawAxis (Canvas: TCanvas);

     function                   GetIndexByPos (xpos: Integer): Integer;
     function                   GetPosByIndex (nIndex: Integer): Integer;

     procedure                  RecalcRect (const prvRect: TRect); override;

    end; // TTimeAxis

    TChartAxis = class (TChartBox) // вертикальная ось
    protected
    public

      AxisType: TAxisType;
      VSpace: Integer;
      ValRange: TFloatRange;

      constructor                Create(AOwner: TComponent); override;
      destructor                 Destroy; override;

      { Methods }

      function                   CalcYPos (v: Single): Integer;
      procedure                  DrawAxis (Canvas: TCanvas);

    end; // TChartAxis

function CalcYPosEx(v: Single; const ValRange: TFloatRange; Top, Height, VSpace: Integer): Integer;

implementation
uses FinCharts, Frames, Math;

{ TPosTimeMap }

constructor TPosTimeMap.Create;
begin
 inherited              Create (sizeof (TTimePoint));
end;

function TPosTimeMap.GetTimePoint(nPoint: Integer): PTimePoint;
begin
 result := PTimePoint ( Rows [nPoint] );
end; // GetTimePoint

{ TTimeAxis }

procedure TTimeAxis.Calculate;
var
   cnt, n, i: Integer;
   o_time, c_time: TArray32;
   ptp: PTimePoint;
   dmap: TDateMap;
   x_pos: Single;
   chart: TFinChart;

begin
 if (iFirst > iLast) or (iFirst < 0) or
    not Assigned (Data) then Exit;

 chart := TFinChart (Owner);
 o_time := data['TIME_OPEN'];
 c_time := data['TIME_CLOSE'];

 dmap := TDateMap (data.Arrays ['DATE']);
 PTMap.Clear;
 cnt := iLast - iFirst + 1;
 PTMap.AddRows( cnt );
 if PTMap.Count <= 0 then exit;
 x_pos := LeftSpace;
 x_step := ( Width - (LeftSpace + RightSpace) ) / ( chart.ShowBarsCount - 1 );
 i := 0;
 for n := iFirst to iLast do
 begin
  if i >= cnt then break;
  ptp := PTMap.Points [i];
  ptp.n_index := n;
  ptp.x_pos := Round (x_pos);
  if (o_time.Count <= n) or (c_time.Count <= n) then exit;
  ptp.o_time := o_time.IntItems^ [n];
  ptp.c_time := c_time.IntItems^ [n];
  ptp.n_date := dmap.FindDate(n);
  ptp.flags := 0;
  x_pos := x_pos + x_step;
  Inc (i);
 end;
 l_time := c_time.IntItems [iLast];
end; // Calculate

constructor TTimeAxis.Create(AOwner: TComponent);
begin
 inherited;
 Width := TControl (AOwner).Width - 50;
 Height := TIME_AXIS_HEIGHT;
 LeftSpace := CHART_AXIS_WIDTH;
 RightSpace := CHART_AXIS_WIDTH;
 Constraints.MaxHeight := TIME_AXIS_HEIGHT;
 Constraints.MinHeight := TIME_AXIS_HEIGHT;{}
 PTMap := TPosTimeMap.Create;
end;

destructor TTimeAxis.Destroy;
begin
 PTMap.Free;
 inherited;
end; // Destroy

procedure TTimeAxis.DrawAxis;

var
   n, ydta: Integer;
   rcout: TRect;
   cnt_days, tcurr, tprv: Integer;
   tmrng, cx, cy: Integer;
   mm, prvd: Integer;
   med_step: Integer;   // in seconds
   dash_step, txt_step: Integer;
   llpt: Integer;
   last: Boolean;
   S: String;
begin
 DrawBackground (Canvas);
 // предварительно должна быть рассчитана карта
 if PTMap.Count <= 2 then exit;
 Canvas.Pen.Color := LineColor;
 Canvas.Font.Color := clLime;
 med_step := 0;

 // вычисление среднего временного шага (точный интервал)
 tprv := PTMap.Points[0].o_time;
 tmrng := 0;
 cnt_days := 1;

 for n := 1 to PTMap.Count - 1 do
  with PTMap do
   begin
    tcurr := Points[n].o_time;
    if tcurr > tprv then
       begin
        Inc (tmrng, 60);
        med_step := med_step + (tcurr - tprv);
       end
    else
       begin
        if Points[n].n_date > Points[n - 1].n_date then Inc (cnt_days);
       end;
    tprv := tcurr;
   end;

 med_step := med_step div Max(1, tmrng);       // средний размер бара в минутах


 if med_step <= 0 then med_step := 1;


 cx := Width - DT_LABEL_WIDTH * cnt_days; // TODO: Считается, что даты рисуются всегда
 txt_step := med_step;                    // метка на каждый бар
 tmrng := PTMap.Count;                    // временной диапазон в барах
 dash_step  := 1;

 // max_lbls := cx div DT_LABEL_WIDTH;

 while ( ( DT_LABEL_WIDTH * tmrng * med_step)  div txt_step > cx ) do
  begin
   dash_step := txt_step;
   if txt_step <= 01 then txt_step := 5 else
   if txt_step <= 05 then txt_step := 10 else
   if txt_step <= 15 then txt_step := 30 else
   if txt_step <= 30 then txt_step := 60 else
   if txt_step <= 60 then txt_step := 180 else
   if txt_step <= 180 then txt_step := 16 * 60 else
      begin
       txt_step := 24 * 60;
       break;
      end;
  end;

 prvd := 0;
 llpt := -1;



 for n := 0 to PTMap.Count - 1 do
 with PTMap.Points [n]^ do
  begin
   ydta := 0;
   mm := o_time div 60; // minutes from day begins
   mm := (mm div med_step) * med_step; // to rounds
   last := (n = PTMap.Count - 1);

   // критерий увеличения длины штриха
   if ( (mm mod dash_step) = 0) then ydta := 5;
   if ( (mm mod txt_step) = 0 ) or (prvd <> n_date) then ydta := 10;
   if (txt_step > 1) and (llpt = n - 1) and (ydta > 5) then ydta := 0;

   if last then ydta := 30;
   Canvas.MoveTo (x_pos, Top);
   Canvas.LineTo (x_pos, Top + ydta);
   // рисование временных меток

   if (ydta > 5) and (o_time >= 0) then
     try
      flags := flags or 1;
      llpt := n;
      if last then
        s := FormatTime ('hh:nn:ss', l_time * 1000)
      else
        s := FormatTime ('hh:nn', o_time * 1000);
      if (prvd <> n_date) and (n_date > 0) then
          s := s + #13#10 + FormatDate ('d.mm.yy', n_date);
      prvd := n_date;
      SetRect (rcout, 0, 0, 0, 0);

      DrawText ( Canvas.Handle, PChar (s), Length(s), rcout, DT_CENTER or DT_VCENTER or DT_CALCRECT);
      cx := rcout.Right shr 1 + 5;
      cy := rcout.Bottom;
      SetRect (rcout, x_pos - cx, Top + ydta + 2, x_pos + cx, Top + ydta + cy + 2);
      with rcout do
        Canvas.FillRect (rcout); // Left - 2, Top - 1, Right + 3, Bottom + 2
      DrawText ( Canvas.Handle, PChar (s), Length(s), rcout, DT_CENTER or DT_VCENTER);
     except
      on E: Exception do
         PrintError ('TTimeAxis.DrawAxis: Exception catched ' + E.Message);
     end;
  end;
end; // DrawAxis

function TTimeAxis.GetIndexByPos(xpos: Integer): Integer;
var n, x, xd: Integer;
begin
 result := -1;
 xd := Round (x_step / 2);
 if (xpos < Left) or (xpos > Left + Width) then exit;
 for n := 0 to PTMap.Count - 1 do
  begin
   x := PTMap.Points [n].x_pos;
   if (x - xd <= xpos) and (xpos <= x + xd) then else continue;
   result := n;
   break;
  end;
end;

function TTimeAxis.GetPosByIndex(nIndex: Integer): Integer;
begin
 result := 0;
 // TODO: may be need binary searching
 if nIndex < PTMap.Count then
 with  PTMap.Points [nIndex]^ do
       result := x_pos;
end; // GetPosByIndex

procedure TTimeAxis.RecalcRect(const prvRect: TRect);
var chart: TFinChart;
begin
 chart := TFinChart (Owner);
 LeftSpace := IfV (chart.LeftAxisVisible, CHART_AXIS_WIDTH, 0);
 RightSpace := IfV (chart.RightAxisVisible, CHART_AXIS_WIDTH, 0);
 Left := 0;
 Top := chart.Height - chart.HScrollBar.Height - TIME_AXIS_HEIGHT - 1;
 Width := chart.Width;
 inherited;
end;

function CalcYPosEx(v: Single; const ValRange: TFloatRange; Top, Height, VSpace: Integer): Integer;
var
   rt, rh: Integer;
begin
 result := Top + Height div 2;  // error-value
 if (ValRange.minv <= v) and (v <= ValRange.maxv) and
    (ValRange.minv < ValRange.maxv) then else exit;
 // min @ Bottom, max @ Top
 v := ValRange.maxv - v;        // relative max & Top
 v := v / (ValRange.maxv - ValRange.minv); // part of Height!

 rt := Top + VSpace;         // Real-bottom
 rh := Height - VSpace * 2;     // Real-Height
 result := Round(rt + v * rh);
end;


{ TChartAxis }

function TChartAxis.CalcYPos(v: Single): Integer;
begin
 result := CalcYPosEx (v, ValRange, PaintZone.Top, Height, VSpace);
end;

constructor TChartAxis.Create(AOwner: TComponent);
begin
 inherited;
 Height := TControl (AOwner).Height;
 Width := CHART_AXIS_WIDTH;
 VSpace := TChartFrame (Owner).VSpace;  // отступы сверху и снизу
 BackgroundColor := clBlack;
 Constraints.MaxWidth := CHART_AXIS_WIDTH;
 Constraints.MinWidth := CHART_AXIS_WIDTH; {}
 bAbsoluteTop := FALSE; // рисуется в буфер
end;

destructor TChartAxis.Destroy;
begin

 inherited;
end;

procedure TChartAxis.DrawAxis;
var
   vdta, fval, step, scale: Double;
 
   ypos, vofs, xdta: Integer;
   step_cnt, decs: Integer;
   sFmt: String;
begin
 DrawBackground (Canvas);
 VSpace := TChartFrame (Owner).VSpace;
 Canvas.Font.Color := clLime;
 vdta := ValRange.maxv - ValRange.minv;
 if vdta <= 0 then exit;
 fval := ValRange.minv;
 step_cnt := (Height - vspace * 2) div 10;
 if (step_cnt <= 1) then exit;
 step := vdta / step_cnt;
 scale := 1;
 decs := 0;
 // подгонка масштаба, для осуществления округления шага
 while (step < 10) do
  begin
   Inc (decs);
   step := step * 10;
   scale := scale * 0.1;
  end;
 while (step > 10) do
  begin
   Dec (decs);
   step := step * 0.1;
   scale := scale * 10;
  end;
 if (decs <= 0) then decs := 0;
 sFmt := '%.' + IntToStr (decs) + 'f';
 if step > 5 then step := 5 else
 if step > 2 then step := 2 else step := 1;
 step := step * scale;
 if step <= 0 then exit;
 // сделать исходное значение кратным полученному шагу
 if step > 0 then
    fval := Int (fval / step) * step
 else
    fval := Int (fval * step) / step;

 while (fval < ValRange.minv) do fval := fval + step;
 vofs := Abs ( Canvas.Font.Height ) shr 1;
 while (fval < ValRange.maxv) do
  begin
   ypos := CalcYPos (fval);
   vdta := fval / step; // для оценки вывода текста-цифры
   xdta := 5;
   if Round (vdta) mod 5 = 0 then xdta := 10;    // каждый 10 шаг
   // if vdta <= 0 then break;
   if AxisType = atLeft then
    begin
     Canvas.MoveTo (Left + Height, ypos);
     Canvas.LineTo (Left + Height - xdta, ypos);
    end;
   if AxisType = atRight then
    begin
     Canvas.MoveTo (Left, ypos);
     Canvas.LineTo (Left + xdta, ypos);
     if xdta > 5 then
        Canvas.TextOut (Left + xdta + 4, ypos - vofs, Format(sFmt, [fval]));
    end;
   fval := fval + step;
  end;
 {
    Общая логика разбивки оси на отрезки:
      * короткие черточки рисуются не чаще, чем через 5 пикселов.
      * длинные с цифрой рисуются не чаще, чем через 25 пикселов
 }

end;

end.
