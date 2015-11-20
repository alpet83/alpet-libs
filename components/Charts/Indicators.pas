unit Indicators;

interface
uses Windows, SysUtils, Classes, Misc, StrClasses, Graphics, ChartData, Axis,
     FinMath, UNIArray, DrawUtils, ContNrs, TradeBase, SMTypes, ArrayTypes;

type
    { Базовый индикатор имеет несколько режимов отображения -
      линия (заданного стиля), гистограмма, инвизибл.
    }

    TSimpleDraw = (sdInvisible, sdHistogram, sdLine);

    TIndicatorLine = class;
    TIndicator = class;

    TRender = procedure (Canvas: TCanvas; nData: Integer; ln: TIndicatorLine) of object;


    TIndicatorLine = class
    protected
     FName: String;
     FColor: TColor;
     FPenStyle: TPenStyle;
     FPenWidth: Integer;
     FRender: TRender;
     FExtremData: String;
     FScaling: Single;
    public

     property                   Color: TColor read FColor write FColor;
     property                   Name: String read FName write FName;
     property                   PenStyle: TPenStyle read FPenStyle write FPenStyle;
     property                   PenWidth: Integer read FPenWidth write FPenWidth;
     property                   Render: TRender read FRender write FRender;
     property                   Scaling: Single read FScaling write FScaling;
     property                   ExtremData: String read FExtremData write FExtremData;       // данные для рассчета экстремумов

     { C & D }
     constructor                Create (const sName: String);
     { Methods }
     procedure                  ConfigurePen (pen: TPen);
    end; // TIndicatorLine


   TIndicator = class (TTradeSystem)
   private
     // tmpl: TStrMap;
     procedure CalcExtremums(var e: TFloatRange; src: TArray32; iFirst, iLast: Integer);
     function  CalcPoint(nIndex: Integer; fval, scal: Single): TPoint;
     function  FirstPoint(data: TArray32) : Integer;
   protected
     FSrcDataSet: DWORD;        // требуемые стандартные данные
     FExtraData: TStrMap;       // зависимые данные - мувинги к примеру
     FExtremums: TFloatRange;
     // FLocalData: TDataMap;
     // FJobList: TStrMap;
     FValRange: TFloatRange;
     FAxis: TChartAxis;
     FTimeAxis: TTimeAxis;
     FOwner: TChartBox;
     FDisplayCount: Integer;    // сколько элементов отрисовать
     FLines: TObjectList;


     function           GetLine (nIndex: Integer): TIndicatorLine;
    public


     property           Axis: TChartAxis read FAxis write FAxis;
     property           TimeAxis: TTimeAxis read FTimeAxis write FTimeAxis;

     property           ExtraData: TStrMap read FExtraData;
     // property           JobList: TStrMap read FJobList;

     property           Lines [Index: Integer]: TIndicatorLine read GetLine;
     // property           LocalData: TDataMap read FLocalData;
     property           Owner: TChartBox read FOwner;

     property           ValRange: TFloatRange read FValRange;


     { C & D }

     constructor        Create (AOwner: TChartBox); virtual;
     destructor         Destroy; override;

     { Methods }

     function           AddLine (const DataName: String; render: TRender;
                                  cl: TColor = clYellow;
                                  ps: TPenStyle = psSolid;
                                  pw: Integer = 1;
                                  sc: Single = 1.0): TIndicatorLine;

     function           AddCalcJob (const name, rqs: String; srcj: TDataCalcJob = nil ): TDataCalcJob;

     procedure          CalcData (src: TDataMap; iFirst, iLast: Integer);  override;

     procedure          Draw (Canvas: TCanvas); virtual;

     procedure          DrawLinear (Canvas: TCanvas; nData: Integer; ln: TIndicatorLine);  // отрисовка линейного графика
     procedure          DrawHistogram (Canvas: TCanvas; nData: Integer; ln: TIndicatorLine);  // отрисовка гистограммы
    end; // TIndicator

   TBarIndicator = class (TIndicator)
   protected
   public
    constructor         Create (AOwner: TChartBox); override;
    procedure           DrawBars(Canvas: TCanvas; nData: Integer; ln: TIndicatorLine);
    procedure           SetDrawLinear (bDrawLinear: Boolean);
   end; // TBarIndicator

   TCustomIndicator = class (TIndicator)
   protected
    FText: TStrMap;            // script text
   public
   end; // TCustomIndicator

   TIndicatorList = class (TObjectList)
   protected
    function            GetIndicator (nIndex: Integer): TIndicator;
   public
    property            Items [Index: Integer]: TIndicator read GetIndicator; default;
    { C & D }
    constructor         Create;
    { Methods }
    function            Find(const sName: String): TIndicator;
   end;

implementation
uses Math, FinCharts, Frames;

{ TIndicator }

function TIndicator.AddCalcJob(const name, rqs: String; srcj: TDataCalcJob): TDataCalcJob;
begin
 result := srcj;
 if result = nil then
    result := TDataCalcJob.Create (self);
 JobList.AddObject (name, result);
 if rqs <> '' then
    result.AddParam('RQS=' + rqs);
end; // AddCalcJob

function TIndicator.AddLine;
var
    newLine: TIndicatorLine;
begin
 AddData (DataName, TRUE);
 newLine := TIndicatorLine.Create (DataName);
 newLine.Render := Render;
 newLine.Color := cl;
 newLine.PenStyle := ps;
 newLine.PenWidth := pw;
 newLine.ExtremData := DataName;
 newLine.Scaling := sc;
 FLines.Add (newLine);
 result := newLine;
end;  // AddLine


procedure TIndicator.CalcData;
var
   n, i: Integer;
   a32: TArray32;
   iFirstE, iLastE: Integer;
begin
 InitRange (FValRange);
 Assert(Assigned(LocalData), 'TIndicator.CalcData: LocalData is not Assigned!');
 LocalData.Lock;
 try
   FDisplayCount := iLast - iFirst + 1;
   inherited CalcData (src, iFirst, iLast);
   // пост-обработка данных
   for n := 0 to LocalData.Count - 1 do
     if LocalData.ArrayByIndex(n) is TArray32 then
      begin
       a32 := LocalData.A32 (n);
       if a32.iLastValid = 0 then
          a32.iLastValid := a32.Count - 1;
       // a32.iFirstValid := ;
      end;
   // рассчет экстремумов
   for n := 0 to FLines.Count - 1 do
    begin
     tmpl.Split(',', Lines [n].ExtremData);
     for i := 0 to tmpl.Count - 1 do
       if (tmpl [i] <> '') then
         begin
          a32 := LocalData.Arrays32[ tmpl[i] ];
          // поиск лучшего ограничения
          iFirstE := Max (a32.iFirstValid, a32.iLeft );
          iLastE := Min (a32.iLastValid, a32.iRight );
          if Pos ('.VOLUME', UpperCase(a32.Name)) > 0  then
             FValRange.minv := Min (FValRange.minv, 0);
          CalcExtremums (FValRange, a32, iFirstE, iLastE);
         end;
    end;
 finally
  LocalData.Unlock;
 end;
end; // CalcData

procedure   TIndicator.CalcExtremums (var e: TFloatRange; src: TArray32; iFirst, iLast: Integer);
var
     n: Integer;
begin
 if (src.RowSize = 4) and (src.flags and DSF_DISPLAYED <> 0) then
   with src do
    for n := iFirst to iLast do
             ExtentRange (e, FloatItems [n])


end; // CalcExtremums

function    TIndicator.CalcPoint (nIndex: Integer; fval, scal: Single): TPoint;
begin
 result.x := TimeAxis.GetPosByIndex(nIndex);
 if Assigned (Axis) then
    result.y := Axis.CalcYPos(fval * scal)
 else
  with TChartFrame(Owner) do
    result.y := CalcYPosEx (fval * scal, FValRange, Top, Height, VSpace);
end;

constructor TIndicator.Create;
var frm: TChartFrame;
begin
 inherited Create ('');
 FLines := TObjectList.Create (TRUE);
 FExtraData := TStrMap.Create;
 FOwner := AOwner;
 WorkCount := 7500000;
 ReservCount := WorkCount; // просчитывать надо ещё и EQUITY!!!
 LocalData.maxItems := 7500000;

 frm := TChartFrame (FOwner);
 frm.Indicators.Add(self);
 AttachConfig ( TFinChart (frm.Owner).ActiveStg );
 Axis := nil;
end;

destructor TIndicator.Destroy;
begin
 FExtraData.Free;
 FLines.Free;
 // ODS('[~T]. Indicator object [' + Name + '] destroyed', 1);
 inherited;
end;

procedure TIndicator.Draw(Canvas: TCanvas);
var n: Integer;
begin
 //
 for n := 0 to FLines.Count - 1 do
   with Lines [n] do
   if Assigned (Render) then
     begin
      ConfigurePen (Canvas.Pen);
      Render ( Canvas,  LocalData.IndexOf (Name), Lines[n] );
     end;
end;


procedure TIndicator.DrawHistogram(Canvas: TCanvas; nData: Integer; ln: TIndicatorLine);
var n, tp: Integer;
    pt: TPoint;
    data: TArray32;
    minv, z_point: Single;
    iFirst, iValidLeft: Integer;
begin
 data := LocalData.A32 (nData);
 iFirst := data.iLeft;
 if (iFirst > data.iLastValid) then exit;
 tp := Max (0, data.iFirstValid - iFirst);   // номер крайнего левого бара на графике
 // ----------------------------------------------------- //
 minv := ValRange.minv; // от нуля объём
 if Axis <> nil then minv := Min (minv, Axis.ValRange.minv);
 z_point := Max (minv, 0); // отсчет от минимум точки

 iValidLeft := Max (data.iFirstValid, data.iLeft);

 with data do
 for n := iValidLeft to iLastValid do
  begin
   pt := CalcPoint (tp, z_point, ln.Scaling);
   Canvas.MoveTo (pt.X, pt.Y);
   pt := CalcPoint (tp, FloatItems [n], ln.Scaling);
   Canvas.LineTo(pt.X, pt.Y);
   Inc (tp);
   if FloatItems [n] <> 0 then
      Canvas.Ellipse(pt.X - 2, pt.Y - 2, pt.X + 2, pt.y + 2);
  end;
end;

procedure TIndicator.DrawLinear;
var n, tp, prvx: Integer;
    pt: TPoint;
    data: TArray32;
    iFirst, iValidLeft: Integer;
begin
 data := LocalData.A32 (nData);
 ASSERT (data <> nil, 'TIndicator.DrawLinear: data unassigned!');
 // диапазоны могут быть не пересекаться по данным
 { Правый и левый ориджины формируют окно просмотра в массиве данных.
   По диапазону они соответствовать должны количеству баров заданых к отображению
 }
 //  iFirst := Max (data.iFirstValid, data.iLastValid - (FDisplayCount - 1) );
 // TODO: Сведение индексных пространств по данным
 iFirst := data.iLeft;
 if (iFirst > data.iLastValid) then exit;
 tp := Max (0, data.iFirstValid - iFirst);   // номер крайнего левого бара на графике

 // tp := FirstPoint (data);
 iValidLeft := Max (data.iFirstValid + 1, data.iLeft + 1);


 pt := CalcPoint (tp, data.FloatItems[iValidLeft], ln.Scaling );
 Canvas.MoveTo (pt.X, pt.Y);
 Inc (tp);
 prvx := -1;
 with data do
 for n := iValidLeft to iLastValid do
  begin
   pt := CalcPoint (tp, FloatItems [n], ln.Scaling);
   if pt.x < prvx then break;
   Canvas.LineTo(pt.X, pt.Y);
   prvx := pt.x;
   Inc (tp);
   // Canvas.Ellipse(pt.X - 2, pt.Y - 2, pt.X + 2, pt.y + 2);
  end;
end; // DrawLinear

function TIndicator.FirstPoint (data: TArray32): Integer;
var draw_cnt: Integer;
begin
 draw_cnt := data.iLastValid - data.iFirstValid + 1;
 result := 0;
 if (draw_cnt < TimeAxis.PTMap.Count) then
     result := TimeAxis.PTMap.Count - draw_cnt;
end; // FirstPoint

function TIndicator.GetLine(nIndex: Integer): TIndicatorLine;
begin
 result := TIndicatorLine ( FLines[nIndex] );
end;

{ TBarIndicator }

constructor TBarIndicator.Create(AOwner: TChartBox);
begin
 inherited;
 AddData ('OPEN,CLOSE', FALSE);
 AddLine ('HIGH,LOW', DrawBars).Name := 'BARS';   // нулевой рендер
 // два массива оцениваются в экстремумах!
 with AddCalcJob('DUMMY', 'COPY') do AddFuncParams('', 'OPEN,HIGH,LOW,CLOSE', 'OPEN,HIGH,LOW,CLOSE');;
end;

procedure TBarIndicator.DrawBars;
var
   dx, n, tp: Integer;
   pt: TPoint;
   o, h, l, c: TArray32;
   iFirst: Integer;
begin
 o := LocalData['OPEN'];
 h := LocalData['HIGH'];
 l := LocalData['LOW'];
 c := LocalData['CLOSE'];
 tp := FirstPoint (o);
 iFirst := Max (0, c.iLastValid - (FDisplayCount - 1) );
 // -------------------------------------------------- //
 dx := Round ( TimeAxis.x_step / 4 );
 ASSERT (c.iLastValid < c.Count, 'Data range outbound');
 with pt do
 for n := iFirst to c.iLastValid do
  begin
   pt := CalcPoint (tp, h.FloatItems [n], ln.Scaling);
   Canvas.MoveTo (x, y);
   pt := CalcPoint (tp, l.FloatItems [n], ln.Scaling);
   Canvas.LineTo (x, y + 1); // тело свечи

   pt := CalcPoint (tp, o.FloatItems [n], ln.Scaling);
   Canvas.MoveTo(x - dx, y);
   Canvas.LineTo(x, y); // левый штрих (open)
   pt := CalcPoint (tp, c.FloatItems [n], ln.Scaling);
   Canvas.MoveTo(x + 1, y);
   Canvas.LineTo(x + dx + 1, y); // правый штрих (close)
   Inc (tp);
  end;
end;

procedure TBarIndicator.SetDrawLinear(bDrawLinear: Boolean);
begin
 if bDrawLinear then
   begin
    if FLines.Count > 1 then exit;
    FLines.Clear;
    AddLine ('OPEN',  DrawLinear, clYellow, psDot).ExtremData := '';
    AddLine ('HIGH',  DrawLinear, clRed,    psDot);
    AddLine ('LOW',   DrawLinear, clLime,   psDot);
    AddLine ('CLOSE', DrawLinear, clAqua,   psSolid).ExtremData := '';
   end
 else
   begin
    if FLines.Count = 1 then exit;
    FLines.Clear;
    AddLine ('HIGH,LOW', DrawBars, clYellow).Name := 'BARS';
   end;
end;

{ TIndicatorList }

constructor TIndicatorList.Create;
begin
 inherited Create (TRUE);
end;

function TIndicatorList.Find(const sName: String): TIndicator;
var n: Integer;
begin
 result := nil;
 for n := 0 to Count - 1 do
  if Items[n].Name = sName then
   begin
    result := Items[n];
    break;
   end;
end;

function TIndicatorList.GetIndicator(nIndex: Integer): TIndicator;
begin
 result := TIndicator (TObjectList(self).Items [nIndex]);
end;

{ TIndicatorLine }

procedure TIndicatorLine.ConfigurePen(pen: TPen);
begin
 Pen.Color := Color;
 Pen.Style := PenStyle;
 Pen.Width := PenWidth;
end;

constructor TIndicatorLine.Create(const sName: String);
begin
 FName := sName;
 PenWidth := 1;
 PenStyle := psSolid;
 Color := clBlack;
end;

end.
