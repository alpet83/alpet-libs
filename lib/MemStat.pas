{$WARN SYMBOL_DEPRECATED OFF}
{$WARN SYMBOL_PLATFORM OFF}
unit MemStat;
interface
uses Windows, SysUtils, Classes, StrClasses, ContNrs, FastSync, Math, Wthreads;

type
    TMemStatItem = class
    public
        total_alloc: Uint64; // allocated summary
        total_freed: Uint64;

     mem_usage_peak: NativeUInt;
       alloc_events: Integer;
        free_events: Integer;
    end; // TMemStatItem

    TMemStatCollector = class(TWorkerThread)
    protected
      FList: TStrMap;

     function          GetItem(const index: String): TMemStatItem; virtual;

     procedure         ProcessInit; override;
     function          ProcessRequest (const rqs: String; rqobj: TObject): Integer; override;

    public
     { C & D }

     { methods }
     procedure         DumpStat (const ctx: String = ''); virtual;
     procedure         LogAlloc (const index: String; n_bytes: Int64); virtual;
     procedure         LogFree  (const index: String; n_bytes: Int64); virtual;
    end; // TMemStatCollector


    TMMTracker = class (TWorkerThread)
    private
     alloc_prv: Int64;
          flog: Text;
    protected

     procedure        ProcessInit; override;
     procedure        ProcessThreadStop; override;
    public

     log_diff: Integer;

     procedure        WorkProc; override;
    end;

var

       gMMTrack: TMMTracker = nil;
   GetCollector:  function: TMemStatCollector = nil; // dynamic proc



procedure DbgLogAlloc(const index: String; n_bytes: Int64); inline;
procedure DbgLogFree (const index: String; n_bytes: Int64); inline;

function  GetMemCollector: TMemStatCollector; // original for export

implementation
uses Misc, ModuleMgr;

{$Include FastMM4Options.inc}

const
  GIG = 1024 * 1024 * 1024;


type
   TMemStatRqs = class
   public
      cb_chg: NativeUInt;
    cat_name: array [0..251] of CHAR;

    { C & D }
    constructor  Create (const ACat: String; cb: NativeUInt);

   end;


var
   gMemStat: TMemStatCollector = nil;

function  GetMemCollector: TMemStatCollector;
begin
 if gMemStat = nil then
    __nop;
 result := gMemStat;
end;

{ TMemStatCollector }

procedure TMemStatCollector.DumpStat;
const
     MIB = 1048576.0;
var n: Integer;
    mi: TMemStatItem;
    hs: THeapStatus;
    fa: Single;
    ff: Single;
    fp: Single;
    fu: Single;



   sum: Int64;
    mu: Int64;

begin


 ODS('[~T]. #DEBUG: Dumping memory usage statistic ' + ctx);
 sum := 0;

 FList.Lock ('DumpStat');
 for n := 0 to FList.Count - 1 do
  begin
   mi := TMemStatItem( FList.Objects[n] );
   if mi = nil then continue;


   mu := ( mi.total_alloc - mi.total_freed );

   fa := mi.total_alloc / MiB;
   ff := mi.total_freed / MiB;
   fp := mi.mem_usage_peak / MiB;
   fu := mu / MiB;

   sum := sum + mu;

   ODS(CFormat('%40s ALLOCATED = %10.3f MiB, FREED = %10.3f MiB  USAGE = %7.3f MiB,  MAX-USAGE = %7.3f MiB, AEVTS = %7d, FEVTS = %7d ', '~C07',
                [ FList [n], fa, ff, fu, fp, mi.alloc_events, mi.free_events ]) );
  end;


 FList.Unlock;


 fu := sum / MiB;
 fp := GetAddressSpaceUsed / MiB;

 ODS( CFormat('~C0E %40s USAGE = %10.3f MiB,    from ALL = %10.3f MiB ~C07', '~C0E',
              ['Summary', fu, fp ]));

 hs := GetHeapStatus;

 if hs.TotalAddrSpace > 0 then
     ODS( CFormat(#9#9#9'~C0F Heap Status: TotalAddrSpace = %d KiB, TotalAllocated = %d KiB ~C07', '~C0F',
                  [hs.TotalAddrSpace div 1024, hs.TotalAllocated div 1024]));
end;

function TMemStatCollector.GetItem(const index: String): TMemStatItem;
var i: Integer;
begin
 result := nil;
 if Trim(index) = '' then exit;

 FList.Lock ('LogAlloc', 150);
 try
  i := FList.IndexOf(index);
  if i < 0 then
   begin
    result := TMemStatItem.Create;
    result.total_alloc := 0;
    result.total_freed := 0;

    FList.AddObject(index, result);
   end
  else
    result := TMemStatItem(FList.Objects[i]);
 finally
  FList.Unlock;
 end;
end; // GetItem

procedure TMemStatCollector.LogAlloc(const index: String; n_bytes: Int64);
begin
 if self = nil then exit;
 if n_bytes < 0 then
    begin
     LogFree (index, - n_bytes);
     exit;
    end;

 if self <> nil then
   begin
    if ( n_bytes > GIG ) then
      __int3;

    AddRequest ( 'LOG_ALLOC', TMemStatRqs.Create (index,  n_bytes) );
   end;

end;

procedure TMemStatCollector.LogFree(const index: String; n_bytes: Int64);
begin
 Assert ( n_bytes > 0, 'LogFree for ' + index + ' n_bytes <= 0 ' );
 Assert ( n_bytes < GIG, 'LogFree for ' + index + ' n_bytes = ' + IntToStr(n_bytes) );

 if self <> nil then
    AddRequest ( 'LOG_FREE' , TMemStatRqs.Create (index,  n_bytes) );
end;


procedure TMemStatCollector.ProcessInit;
begin
 inherited;
 FList := TStrMap.Create (self);
 FList.Sorted := TRUE;
 FList.OwnsObjects := TRUE;
 Garbage.Add (FList);
end;

function TMemStatCollector.ProcessRequest(const rqs: String; rqobj: TObject): Integer;
var
   mr: TMemStatRqs;
   mi: TMemStatItem;


begin
 result := inherited ProcessRequest (rqs, rqobj);

 if  Pos('LOG_ALLOC', rqs) + Pos('LOG_FREE', rqs) > 0  then
    begin
     mr := TMemStatRqs (rqobj);
     if mr = nil then exit;

     mi := GetItem (mr.cat_name);
     if mi = nil then exit; // so strange!!!

     if mr.cb_chg > GIG then
        __nop;


     if rqs = 'LOG_ALLOC' then
       begin
        mi.total_alloc := mi.total_alloc + mr.cb_chg;
        Inc (mi.alloc_events);
        mi.mem_usage_peak := Max(mi.mem_usage_peak, mi.total_alloc - mi.total_freed );
       end
     else
       begin
        mi.total_freed := mi.total_freed + mr.cb_chg;
        Inc (mi.free_events);
       end;

     if mi.total_alloc > MAXINT div 4 then
        __nop;
     mr.Free;
    end;

end;


function OnModuleRqs (md: TModuleDescriptor; rqs, flags: DWORD): Boolean;
var
  mkobj: Boolean;
   hEXE: NativeUInt;
   buff: array [0..MAX_PATH + 1] of CHAR;
    exe: String;

begin
 result := FALSE;
 case rqs of
  MRQ_INITIALIZE:
   begin
    result := (gMemStat = nil);
    if not result then exit;
    FillChar ( buff, sizeof(buff), 0 );
    GetModuleFileName ( 0, buff, sizeof(buff) );
    exe := ExtractFileName ( buff );
    hEXE := GetModuleHandle ( PChar (exe) );

    if HInstance = hExe then
      mkobj := TRUE
    else
      begin
       GetCollector := GetProcAddress (hExe, 'GetMemCollector');
       mkobj := not Assigned (GetCollector);
      end;

    if mkobj then
     begin
      gMemStat := TMemStatCollector.Create (FALSE, 'MemStatCollector');
      md.Globals.Add (gMemStat);
      GetCollector := GetMemCollector;
     end;


   end;
  MRQ_FINALIZE:
   begin
    result := (gMemStat <> nil);
    if not result then exit;
    gMemStat.DumpStat;
    gMemStat.StopThread;
    gMemStat.WaitStop;
    gMemStat := nil;
   end;
  end; // case
end; // OnModuleRqs;

{ TMMTracker }

procedure TMMTracker.ProcessInit;
begin
  inherited;
  log_diff := 10240;

  FreeOnTerminate := TRUE;
  AssignFile ( flog, gLogPath + '\mmtrack.log');
  {$I-}
  ReWrite ( flog );

end;

procedure TMMTracker.ProcessThreadStop;
begin
  inherited;
  CloseFile ( flog );
end;

procedure TMMTracker.WorkProc;
var
    mms: TMemoryManagerState;
   atot: Int64;
   diff: Integer;
     sb: TSmallBlockTypeState;
      s: String;
begin
 inherited;
 // FastMM4.GetMemoryManagerUsageSummary ( mmus );
 GetMemoryManagerState ( mms );
 atot := mms.TotalAllocatedMediumBlockSize + mms.TotalAllocatedLargeBlockSize;

 for sb in mms.SmallBlockTypeStates do
     atot := atot + sb.UseableBlockSize * sb.AllocatedBlockCount;

 diff := atot - alloc_prv;

 if Abs(diff) > log_diff then
   begin
    s := InfoFmt ( Format('[~T/~U/~B]. #MMT: Allocated = %.3f MiB, diff = %.3f MiB ', [atot / MIB_BYTES, diff / MIB_BYTES]) );
    WriteLn ( flog, s );
    alloc_prv := atot;
   end;

end;

{$IFDEF FullDebugMode}

{$ELSE}
// Wow dude!

{$ENDIF}

procedure DbgLogAlloc(const index: String; n_bytes: Int64);
begin
 {$IFOPT D+}
 GetCollector.LogAlloc (index, n_bytes);
 {$ENDIF}
end;

procedure DbgLogFree (const index: String; n_bytes: Int64);
begin
 {$IFOPT D+}
 GetCollector.LogFree (index, n_bytes);
 {$ENDIF}
end;




{ TMemStatInfo }

constructor TMemStatRqs.Create(const ACat: String; cb: NativeUInt);
begin
 SetStrWZ ( cat_name, ACat, 250 );
 cb_chg := cb;
end;

initialization
 GetCollector := GetMemCollector;
 RegModule ( 'MemStat', 'Misc,ModuleMgr', OnModuleRqs );
 InitializeModule ('MemStat');
finalization
 FinalizeModule ('MemStat');
end.