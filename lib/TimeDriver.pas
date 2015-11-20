unit TimeDriver;

interface
uses Windows, SysUtils, Classes, Misc, DateTimeTools, WThreads;


type
   TTimeUpdater = class (TWorkerThread)
   private

    time_dev: TTimeDeviator;

   protected
    procedure     ProcessInit; override;
    procedure     ProcessThreadStop; override;

   public


    procedure   WorkProc; override;
   end;

var
 gUpdater: TTimeUpdater = nil;

implementation

{ TTimeUpdater }
{$R-}

procedure TTimeUpdater.ProcessInit;
begin
 inherited;
 wait_time := 0;
 time_dev := TTimeDeviator.Create;
 Garbage.Add (time_dev);
 time_dev.Patch;

end;

procedure TTimeUpdater.ProcessThreadStop;
begin
 inherited;
 time_dev.Patch (FALSE);
end;

procedure TTimeUpdater.WorkProc;
var
   dt, diff: TDateTime;
begin
 inherited;
 if Terminated then exit;
 Sleep (1);

 try
  dt := PreciseTime;
  diff := dt - Now;
  if diff = 0 then exit;

  time_dev.DateTime := dt;
  p_shared_vars.local_dt := dt;

 except
  on E: Exception do
     PrintError ( ClassName + '.WorkProc. Exception catched: ' + E.Message );
 end;

end;

end.
