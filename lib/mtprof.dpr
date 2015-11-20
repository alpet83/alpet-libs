program mtprof;

{  multi-thread library profiling}

{$APPTYPE CONSOLE}

{$R *.res}

uses
  FastMM4 in 'FastMM4.pas',
  FastMM4Messages in 'FastMM4Messages.pas',
  Windows, DateTimeTools,
  System.SysUtils,
  Misc, FastSync,
  WThreads;

type
   TProfThread = class (TWorkerThread)
   protected
    function            ProcessRequest(const rqs: String; rqobj: TObject): Integer; override;
   public

    index: Integer;

    procedure           WorkProc; override;
   end;


// ------------------------------------------------------------------
const
   cnt = 8;

var
   gTests: Integer = 0;

   ts: array [0..65535] of TDateTime;

   tm: array [1..cnt] of Double;
   cc: array [1..cnt] of Int64;

   tl: array [1..cnt] of TProfThread;

{ TProfThread }

function TProfThread.ProcessRequest(const rqs: String; rqobj: TObject): Integer;

var
   tt: TProfThread;

   df: Double;
    n: Integer;
begin

 result := 0;

 if rqs = 'ping' then
   begin
    InterlockedIncrement (gTests);
    n := Integer (rqobj);

    if n <= High(ts) then
       begin
        df := ( g_timer.GetTime - ts [n] ) / DT_ONE_MSEC;
        tm [index] := tm [index] + df;
        Inc (cc[index]);
       end;
   end;



 if rqs = 'test' then
   begin
    InterlockedIncrement (gTests);
    repeat
     n := Random (cnt) + 1;
    until (n <> index);

    tt := tl [n];
    tt.AddRequest('test');
   end;


 if rqs = 'PRQS' then exit;

 if rqs = 'nope' then asm nop end;


 result := inherited ProcessRequest (rqs, rqobj);

end;

procedure TProfThread.Workproc;
begin
 inherited;
 if wp_exec_cnt mod 100000 = 0 then
   ODS('[~T/~I]. #DBG: wp_exec_cnt = ' + IntToStr(wp_exec_cnt))
end;


// globals

procedure Main;
const
    msk = cnt - 1;
var

     pt: TProfileTimer;
     el: Double;
    lat: Double;
     ti: Integer;

a, b, n: Integer;


begin
 for n := 1 to cnt do
   begin
    tl [n] := TProfThread.Create(FALSE, 'ProfThread#' + IntToStr(n) );
    tl [n].index := n;
    // tl [n].wait_func := nil;
   end;


 // while not tobj.Terminated do





 for n := 1 to cnt do
     tl [n].WaitStart;

 Sleep(150);


 b := 0;
 pt := TProfileTimer.Create;
 pt.StartOne(5);

 ODS('[~T/~B]. #DBG: flood begins');

 {
 for n := 0 to cnt * 2 - 1 do
   begin
    tl [n and msk + 1].AddRequest('test');
   end;
 }


 a := 0;

 ti := 0;


 while TRUE do
 begin
  el := 0;
  Inc (a);

  for n := 0 to 1023 do
    with tl [n and msk + 1] do
     begin
      // ProfTimer.StartOne(28);

      ts [ti] := g_timer.GetTime;

      AddRequest('ping', Ptr(ti) );

      ti := (ti + 1) and High (ts);
     end;

  if a and $1F = 0 then el := pt.Elapsed(5);


  if el >= 5000 then
   begin

    b := 0;

    for n := 1 to cnt do Inc ( b, tl[n].GetLocksCount(TRUE) );

    lat := 0;

    for n := 1 to cnt do
     begin
      if cc [n] > 0 then lat := lat + tm [n] / cc [n];
      tm [n] := 0;
      cc [n] := 0;
     end;

    lat := 1000 * lat / cnt;


    ODS('[~T]. #DBG: locks = ' + IntToStr(b) + ', msgs per ' + ftow(el, '%.1f') + ' msec = ' + IntToStr(gTests) +
                     ', msgs in sec per thread  = ' + ftow (1000.0 * gTests / el / cnt, '%.1f') +
                     ', latency = ' + ftow(lat, '%.1f qs') );

    InterlockedAdd ( gTests, - gTests );

    pt.StartOne(5);
   end;

 end; // while


 ODS('[~T/~B]. #DBG: flood ends');

 ReadLn;

 for n := 1 to cnt do tl [n].WaitRequests();
 ODS('[~T/~B]. #DBG: waitings ends');

 for n := 1 to cnt do tl [n].StopThread();
 for n := 1 to cnt do tl [n].WaitStop();
 for n := 1 to cnt do tl [n].Free;

 pt.Free;
end;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    StartLogging('');
    ShowConsole;
    Main;
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
