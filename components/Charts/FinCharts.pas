unit FinCharts;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Dialogs, ExtCtrls, StdCtrls, Forms, Graphics,  UTITypes,
  ContNrs, Axis, Frames, Indicators, ChartData, UNIArray, StrClasses, TradeBase, SMTypes;

type
  TFinChart = class(TCustomControl)
  private

    // procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure OnHScroll (Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    function HitObject(x, y: Integer): String;
    procedure MoveSplitter(yPos: Integer);
    { Private declarations }
  protected
    { Protected declarations }
    FBarsCount: Integer;
    FFrames: TStrMap;
    FSortedFrames: TStrings;
    FTimeAxis: TTimeAxis;
    FHScrollBar: TScrollBar;
    FData: TDataMap;
    FDataAttached: Boolean;
    FShowBarsCount: Integer;
    mouse_action: String;
    mouse_target: String;
    hit_frame: TChartFrame;
    target_frame: TChartFrame;
    FActiveStg: TStrategyConfig;


    procedure                Paint; override;
    function                 GetFrame (nFrame: Integer): TChartFrame;
  public
    { Public declarations }
    LeftAxisVisible: Boolean;
    RightAxisVisible: Boolean;
    VisRange: TDataRange;
    need_repaint: Boolean;
    dep_ind_list: TObjectList; // список зависимых индикаторов, которых надо убивать в первую очередь
    chart_config: String;       // обобщеное название конфигурации индюков



    property                 ActiveStg: TStrategyConfig read FActiveStg write FActiveStg;
    property                 BarsCount: Integer read FBarsCount;
    property                 Constraints; // no design-time modifications


    property                 Data: TDataMap read FData;
    property                 Frames[Index: Integer]: TChartFrame read GetFrame;
    property                 TimeAxis: TTimeAxis read FTimeAxis;
    property                 HScrollBar: TScrollBar read FHScrollBar;
    property                 ShowBarsCount: Integer read FShowBarsCount write FShowBarsCount;

    { C & D }

    constructor              Create (AOwner: TComponent); override;
    destructor               Destroy; override;

    { Methods }
    procedure                AttachData (dm: TDataMap);
    function                 AddFrame (ypos: Integer; rel_h: Single; const sName: String): TChartFrame;
    function                 ChartZoneHeight: Integer;
    procedure                DetachData;
    function                 FramesCount: Integer;
    function                 FrameByName(const sName: String): TChartFrame;
    procedure                ProcessMouse (const evt: String; btn: TMouseButton; shift: TShiftState; x, y: Integer);
    procedure                RecalcSize;
    procedure                RecalcView;
    procedure                RemoveIndicators;
    procedure                SetFrameName (nFrame: Integer; const sName: String);     
    procedure                Update; override;

  published
    { Published declarations }
    property                 Anchors;
    property                 OnMouseDown;
    property                 OnMouseUp;
    property                 OnMouseMove;
  end;

procedure Register;

implementation
uses Misc, Math;

procedure Register;
begin
  RegisterComponents('Samples', [TFinChart]);
end;

{ TFinChart }

function TFinChart.AddFrame(ypos: Integer; rel_h: Single; const sName: String): TChartFrame;
var
    srcf: TChartFrame;
    hh, n, ipos: Integer;

begin
 result := TChartFrame.Create (self);
 result.Name := sName;
 srcf := nil;
 ipos := 0;
 if FramesCount > 0 then
   begin
     // поиск фрейма для разбиения на два
     for n := 0 to FramesCount - 1 do
     with Frames [n] do
      if (Top <= ypos) and (ypos <= Top + Height) then
       begin
        srcf := Frames [n];
        ipos := n;
        break;
       end;
       
     if srcf = nil then
        ipos := 0
      else
       begin
        hh := srcf.Top + srcf.Height shr 1;
        if ypos > hh then Inc (ipos);
       end;
   end;
 FFrames.InsertObject (ipos, sName, result);

 if srcf <> nil then
   begin
    result.RelHeight := srcf.relHeight * rel_h;         // размер нового, относительно исходного
    srcf.RelHeight := srcf.relHeight * (1 - rel_h);     // размер поделенного фрейма
   end
 else
   result.RelHeight := rel_h;
 // FFrames.Sort(CmpFrameTop) 
 RecalcSize;  
end; // AddFrame

procedure TFinChart.AttachData(dm: TDataMap);
begin
 if (not FDataAttached) and Assigned (FData) and (FData.Owner = self) then
   begin
    FData.Lock;
    FData.Free;
   end;
 FDataAttached := TRUE;
 FData := dm;
 FData.OnAttach (self, Format('TFinChart[$%p]', [Pointer(self)]));
end;

function TFinChart.ChartZoneHeight: Integer;
begin
 result := Height - TimeAxis.Height - FHScrollBar.Height - 1;
end;

constructor TFinChart.Create(AOwner: TComponent);

begin
 ASSERT (AOwner is TControl, 'Owner must be TControl');
 inherited Create (AOwner);
 ControlStyle := [csCaptureMouse, csClickEvents,
                  csOpaque, csDoubleClicks, csReplicatable];

 { if Parent is TWinControl then
    Parent := TWinControl (AOwner);}
 Width := 300;
 Constraints.MinWidth := 200;
 Constraints.MinHeight := 150;
 FFrames := TStrMap.Create;
 FFrames.CaseSensitive := FALSE;
 dep_ind_list := TObjectList.Create(FALSE);
 FSortedFrames := TStringList.Create;
 with TStringList (FSortedFrames) do
  begin
   Sorted := TRUE;
   Duplicates := dupAccept;	
  end;
 // -- TimeAxis --
 FTimeAxis := TTimeAxis.Create(self);
 FTimeAxis.BackgroundColor := $303030;
 FData := MakeInstrData (self);
 FData.Name := Format('TFinChart($%p).FData', [Pointer(self)]);
 FData.maxItems := 120000;
 FHScrollBar := TScrollBar.Create(self);
 FHScrollBar.Parent := self;
 with FHScrollBar do
  begin
   Kind := sbHorizontal;
   Align := alBottom;
   Show;
   OnScroll := OnHScroll;
   Position := Max;
  end;
 AddFrame (1, 1, 'main');
end;

destructor TFinChart.Destroy;
begin
 RemoveIndicators;
 if (not FDataAttached) and (FData.Owner = self) then
         begin
          FData.Free;
          FData := nil;
         end;
 FSortedFrames.Free;
 dep_ind_list.Free;
 FFrames.Free;
 inherited;
end;


procedure TFinChart.DetachData;
begin
 FData.OnDetach (self, Format('T[$%p]', [Pointer(self)]));
 FDataAttached := FALSE;

 FData := MakeInstrData (self);
 FData.Name := Format('TFinChart($%p).FData', [Pointer(self)]);
 FData.maxItems := 120000;
end;

function TFinChart.FrameByName(const sName: String): TChartFrame;
var n: Integer;
begin
 result := nil;
 n := FFrames.IndexOf (sName);
 if n >= 0 then
   result := TChartFrame (FFrames.Objects [n]);
end;

function TFinChart.FramesCount: Integer;
begin
 result := FFrames.Count;
end;

function TFinChart.GetFrame(nFrame: Integer): TChartFrame;
begin
 result := TChartFrame ( FFrames.Objects [nFrame] );
end;

function  TFinChart.HitObject (x, y: Integer): String;
var n: Integer;
    s: String;
    frm: TChartFrame;
begin
 result := 'SPACE';
 hit_frame := nil;
 for n := 0 to FramesCount - 1 do
  begin
   frm := Frames[n];
   if (frm.Top - 1 <= y) and (frm.Top + frm.Height - 1 > y) then
      hit_frame := frm;
   s := frm.HitObject (x, y);
   if s = 'MISS' then continue;
   result := s;
   break;
  end;
end; // HitObject

procedure TFinChart.OnHScroll(Sender: TObject; ScrollCode: TScrollCode;  var ScrollPos: Integer);
begin
 RecalcView;
end;

procedure TFinChart.Paint;
var
   frame: TChartFrame;
   n, data_org: Integer;
   br: TRect;
   show_last: Boolean;
begin
 need_repaint := FALSE;
 try
  RecalcSize; // TODO: Optimize
  if Assigned (Data) and Assigned (ActiveStg) then
    begin
     show_last := ( HScrollBar.Max = HScrollBar.Position );

     FBarsCount := Data['CLOSE'].Count;
     data_org := ActiveStg.ExtraConfig.data_origin;
     if FBarsCount < data_org then exit;


     n := FBarsCount - ShowBarsCount + 10;


     HScrollBar.SetParams(HScrollBar.Position,
                          Min (n, data_org), Max (HScrollBar.Min, n) ); // до 10 баров отступа



     if show_last and (HScrollBar.Position < HScrollBar.Max) then
        HScrollBar.Position := HScrollBar.Max;
     if VisRange.iFirst <> HScrollBar.Position then
        self.RecalcView;

    end;


   if Assigned (Data) then
     TimeAxis.Calculate (Data, VisRange.iFirst, VisRange.iLast);

   TimeAxis.DrawAxis (Canvas);

   // добавление фреймов в сортированный список
   FSortedFrames.Clear;
   for n := 0 to FramesCount - 1 do
     begin
      frame := Frames [n];
      FSortedFrames.AddObject(IntToHex (frame.CalcOrder, 4), frame);
     end;

   // ODS ('[~T]. #DBG: Drawing chart ');
   for n := 0 to FSortedFrames.Count - 1 do
    begin
     frame := TChartFrame ( FSortedFrames.Objects [n] );
     frame.CalcFrameData;
    end;


   for n := 0 to FSortedFrames.Count - 1 do
    begin
     frame := TChartFrame ( FSortedFrames.Objects [n] );
     frame.CalcFrameData;       // TODO: второй проход - надо оптимизировать по зависимостям!   
     frame.DrawFrame ();
     // frame.PaintTo(Canvas);
    end;

 
   for n := 0 to FSortedFrames.Count - 1 do
            TChartFrame ( FSortedFrames.Objects [n] ).PaintTo(Canvas);{}

   br := BoundsRect;
   // br.Bottom := br.Bottom - HScrollBar.Height;

   HScrollBar.Repaint;
   ValidateRect (Handle, @br);
 except
  on E: Exception do
     PrintError ('TFinChart.Paint: Exception catched ' + E.Message);
 end;
 //  HScrollBar.PaintTo(Canvas, 0, HScrollBar.Top);
 // Sleep (10);
end;

procedure TFinChart.MoveSplitter(yPos: Integer);
var
   i, btm, new_h, new_hp: Integer;
   prvf: TChartFrame;
begin
 if target_frame = nil then exit;
 i := FFrames.IndexOfObject (target_frame); // предполагается, что фреймы отсортированы по вертикали
 if (i <= 0) then exit;               // самый первый фрейм, при этом не приемлится
 prvf := Frames[i - 1];
 btm := target_frame.Top + target_frame.Height;
 new_h := btm - yPos;            // новая высота фрейма
 new_hp := yPos - prvf.Top; 
 if (new_hp < 50) or (new_h < 50) then exit; // слишком малый размер в результате
 prvf.RelHeight := new_hp / ChartZoneHeight;
 target_frame.RelHeight := new_h / ChartZoneHeight;
 Paint;
end;

procedure TFinChart.ProcessMouse(const evt: String; btn: TMouseButton; shift: TShiftState; x, y: Integer);
var
   sTarget: String;
begin
 sTarget := HitObject (x, y);
 if (evt = 'DOWN') then
   begin
    if sTarget = 'FRAME_TOP' then
     begin
      mouse_action := 'MOVE';
      mouse_target := 'SPLITTER';
      target_frame := hit_frame; 
     end;
   end;
 if (evt = 'MOVE') then
  begin
   if mouse_action = '' then
     begin
      if sTarget = 'FRAME_TOP' then Cursor := crSizeNS
       else
      if sTarget = 'SPACE' then Cursor := crCross;
     end;
   // обработка перетаскивания объектов  
   if mouse_action = 'MOVE' then
     begin
      if mouse_target = 'SPLITTER' then MoveSplitter (y);
     end;
  end;
 if (evt = 'UP') then
   begin
    mouse_action := '';
    mouse_target := '';
    target_frame := nil;
   end;
 if (evt = 'LEAVE') and (mouse_action = '') then Cursor := crDefault;
end; // ProcessMouse

procedure TFinChart.RecalcSize;
var n: Integer;
    frame: TChartFrame;
    rc: TRect;
    w: Integer;
begin
 LeftAxisVisible := FALSE;
 RightAxisVisible := FALSE;
 w := Width;
 SetRect (rc, 0, 0, w, 0);
 for n := 0 to FramesCount - 1 do
  begin
   frame := Frames [n];
   frame.RecalcRect (rc);
   rc := frame.BoundsRect;
  end;
 TimeAxis.RecalcRect (rc);
end;

procedure TFinChart.RecalcView;
begin
 VisRange.iFirst := HScrollBar.Position; // in range = 0 .. BarsCount - 10 typicaly
 VisRange.iLast := Min (BarsCount - 1, VisRange.iFirst + ShowBarsCount);
 need_repaint := TRUE;
end;

procedure TFinChart.RemoveIndicators;
var n, i: Integer;

begin
 // Сначала убираются индикаторы, которые данные подключили у соседей
 for n := 0 to FramesCount - 1 do
 with Frames[n] do
  begin
   for i := Indicators.Count - 1 downto 0 do
    if dep_ind_list.IndexOf(Indicators[i]) >= 0 then
       Indicators.Delete(i);
  end;
 dep_ind_list.Clear;
 for n := 0 to FramesCount - 1 do
     Frames[n].Indicators.Clear;
end;

procedure TFinChart.SetFrameName(nFrame: Integer; const sName: String);
begin
 if nFrame >= FramesCount then exit;
 FFrames[nFrame] := sName;
 Frames[nFrame].Name := sName;
end;

// RemoveIndicators

{

procedure TFinChart.WMWindowPosChanged(var Message: TWMWindowPosChanged);
var Rect: TRect;
begin
 Rect.Right := Width;
 Rect.Bottom := Height;
  if Message.WindowPos^.cx <> Rect.Right then
  begin
    Rect.Top := 0;
    Rect.Left := Rect.Right -  1;
    InvalidateRect(Handle, @Rect, False);
  end;
  if Message.WindowPos^.cy <> Rect.Bottom then
  begin
    Rect.Left := 0;
    Rect.Top := Rect.Bottom - 1;
    InvalidateRect(Handle, @Rect, False);
  end;
 inherited;
end;                {}

procedure TFinChart.Update;
begin
  inherited;
end;

procedure TFinChart.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
 exit;
end;

procedure InitModule;

begin
 if pos ('bds.exe', ExeFileName) > 0 then HideConsole;
end;

initialization
 InitModule;
finalization

end.
