unit FastSync;

interface
uses Windows, SysUtils, Classes, Math, SyncObjs, ContNrs;

{ Набор классов, для эффективной синхронизации, с возможностью отладки
}

{.$DEFINE SAVE_LOCK_CONTEXT}

const MUTEX_STATES_COUNT = 32;
      SZ_LAST_LOCKER = 1024;


type
    TSafeStr = WideString;

    TFastSyncObject = class
    protected
     FName: String;
     fpt: TObject;      // TProfileTimer

     slHistory: TStrings;
     maxHistLines: Integer;
     procedure                  SaveMsg (const msg: WideString); // saving message to the history list
    public

     bSaveHistory: Boolean;
     msg_pass: Boolean;

     property                   History: TStrings read slHistory;

     property                   Name: String read FName write FName;

     constructor                Create (const sc_name: String);
     destructor                 Destroy; override;

     function                   Lock (const ctx_msg: TSafeStr; warnTimeOut: Integer = 10000): Double; virtual; abstract;
     function                   TryLock (const ctx_msg: TSafeStr; timeOut: Integer):  Boolean; virtual; abstract;
     procedure                  Unlock; virtual; abstract;
    end; // TFastSyncObject

    TCritSection = class (TFastSyncObject)
    private
     function GetLastLocker: String;
     procedure SetLastLocker(const Value: String);
    protected
     FSect: RTL_CRITICAL_SECTION;
     evt_unlock: THandle;
     FLockCount: Integer;
     FLastLocker: array [0..SZ_LAST_LOCKER - 1] of CHAR;
     procedure                  OnLock(const ctx_msg: WideString); virtual;
    public
       EnterTimeOut: Integer;
          WaitDelay: Integer;
             tmLock: TDateTime;
        LockTimeout: Integer;

     property                   LastLocker: String read GetLastLocker write SetLastLocker;

     { C & D }
     constructor                Create (const sc_name: String; nSpinCount: DWORD = 0);
     destructor                 Destroy; override;
     { Methods }
     function                   Dump: String;

     function                   Lock (const ctx_msg: TSafeStr; warnTimeOut: Integer = 100): Double; override;
     function                   TryLock (const ctx_msg: TSafeStr; timeOut: Integer):  Boolean; override;
     procedure                  Unlock; override;

    end; // TCritSection

    TProtectedList = class (TList)
    protected
     sc_share: TCritSection;

     function                   GetIntItem (Index: Integer): Integer;
     procedure                  SetIntItem (Index, Value: Integer);
    public

     property                   IntItems[Index: Integer]: Integer read GetIntItem write SetIntItem; default;

     constructor                Create;
     destructor                 Destroy; override;

     function                   Add(Item: Integer): Integer; virtual;
     procedure                  Delete(Index: Integer); virtual;
     function                   Remove(Item: Integer): Integer; virtual;
    end; // TProtectedList

    TStateMutex = class
    protected
     evt_unlock: THandle;
     FLockState: Integer;
     FLockCount: Integer;
     FQueue: TProtectedList;
     FContext: String;
     sc_share: TCritSection;
    public

     property   CurrentState: Integer read FLockState;


     constructor                Create;
     destructor                 Destroy; override;

     procedure                  Lock (nState: Integer; const ctx: String = '');
     function                   TryLock (nState, timeOut: Integer; const ctx: String = ''): Boolean;
     procedure                  Unlock;
    end;

    TCSInvader = class
    public
     _cs: TCritSection;
     _id: DWORD;  // thread ID
     _wt: DWORD;  // warning timeout (ts)

     constructor    Create (cs: TCritSection; tid, wtd: DWORD);

    end;


    TNotifyParams = packed record
     i_params: array [0..7] of Int64;
     f_params: array [0..7] of Double;
    case BYTE of
     0: (p_params: array [0..7] of Pointer);
     1: (o_params: array [0..7] of TObject);
     2: (s_params: array [0..7] of PAnsiChar);
     3: (w_params: array [0..7] of PWideChar);
    end; // TNotifyParams

    PNotifyParams = ^TNotifyParams;

    TNotifyEventHandler = procedure ( pnp: PNotifyParams ) of object;

    TNotifyEventInfo = class
    public
     ID: Integer;
     params: TNotifyParams;             // для многократной (регулярной) нотификации
     Simple: TNotifyEvent;               // для простой отработки
     Handler: TNotifyEventHandler;      // для сложной отработки

    end; // TNotifyEventInfo

    // малый список евентов
    TNotifyEventList = class (TObjectList)
    private
     function       GetParams(index: Integer): PNotifyParams;
    protected


    public

     property       Params[index: Integer]: PNotifyParams read GetParams;

     { C & D }
     constructor    Create;
     { Methods }
     procedure      AddHandler ( neh: TNotifyEventHandler; id: Integer = - 1 );
     procedure      AddSimple  ( ne: TNotifyEvent; id: Integer = - 1 );


     procedure      NotifyAll ( Sender: TObject = nil; AParams: PNotifyParams = nil; mask: Integer = -1 ); virtual;

    end;


threadvar
   lock_trap: Boolean;

var
   main_thread_id: DWORD = 0;
      cs_watchdog: procedure (cs: TCSInvader; add: Boolean) = nil;

   GOnLockTimeout: procedure (Sender: Pointer) = nil;

implementation
uses Misc, DateTimeTools, WThreads;


var
   log_lines: Integer = 0;


{.$DEFINE NO_SPIN}
{.$DEFINE HANG_LOG}

function DTPrefix: String;
begin
 result := Format( '%-28s', [ InfoFmt('[~T/~I/~B]') ] )
end;

procedure HangLog (const msg: String);
var
  fname: String;
   ftxt: Text;
begin
 fname := gLogPath + 'app_hang.log';

 if InterlockedIncrement (log_lines) = 1 then DeleteFile ( fname );

 {$I-}
 AssignFile ( ftxt, fname );
 if FileExists (fname) then
    Append ( ftxt )
 else
    ReWrite ( ftxt );
 if IOresult = 0 then
    WriteLn ( ftxt, msg );
 CloseFile ( ftxt );
 {$I+}
end;

{ TCritSection }

constructor TCritSection.Create(const sc_name: String; nSpinCount: DWORD);
begin
 inherited Create (sc_name);
 evt_unlock := CreateEvent (nil, TRUE, FALSE, nil);
 EnterTimeOut := 250;
 WaitDelay := 1;
 LastLocker := 'NOBODY';
 LockTimeout := MAXINT;

 {$IFDEF NO_SPIN}
 nSpinCount := 0;
 {$ENDIF}

 if nSpinCount = 0 then
    InitializeCriticalSection (FSect)
 else
    InitializeCriticalSectionAndSpinCount (FSect, nSpinCount);
end; // Create

destructor TCritSection.Destroy;
begin
 try

  if TryLock ('Destroy', 1500) then  Unlock;
  DeleteCriticalSection (FSect);
  // ==================
  if evt_unlock <> 0 then
     CloseHandle (evt_unlock);


 except
  on E: Exception do
     OnExceptLog ( ClassName + '.Destroy', E );
 end;

 inherited;
end;

function TCritSection.Dump: String;
begin
 result := Format ( '%-40s owner %5d lock count %4d, recursion count %4d debug info  $%p last locker %s ',
                    [Name, FSect.OwningThread, FSect.LockCount, FSect.RecursionCount, FSect.DebugInfo, LastLocker ] );

end;

function TCritSection.GetLastLocker: String;
begin
 result := FLastLocker;
end;

// Destroy



function TCritSection.Lock(const ctx_msg: TSafeStr; warnTimeOut: Integer): Double;
var
    n_pass: Integer;
     s, ll: String;
       tid: DWORD;
        pt: TProfileTimer;
      {$IFDEF PERF_CHECK}
       csi: TCSInvader;
      {$ENDIF}

begin
 Assert( DWORD(self) > $10000, Format('TCritSection.Lock - self unassigned! = $%p', [Pointer(self)]) );
 n_pass := 0;
 ll := LastLocker;
 pt := TProfileTimer (fpt);
 pt.StartOne (21);

 tid := GetCurrentThreadID;


 {$IFDEF PERF_CHECK}
 csi := nil;
 if Assigned (cs_watchdog) then
    csi := TCSInvader.Create (self, tid, warnTimeout * 10);
 if Assigned (csi) then
    cs_watchdog (csi, TRUE);
 {$ENDIF}

 {$IFDEF HANG_LOG}
 s :=  DTPrefix + '#ENTER: EntLock ' + Dump;
 HangLog ( s );
 {$ENDIF}

 //

 {$IFDEF PERF_CHECK}
 DbgEnterCS (FSect, ctx_msg);
 if Assigned (cs_watchdog) and Assigned (csi) then
    cs_watchdog (csi, FALSE); // invader record freed by watchdog thread
 {$ELSE}
 EnterCriticalSection (FSect);
 {$ENDIF}

 if evt_unlock <> 0 then ResetEvent (evt_unlock);
 result := pt.Elapsed (21);




 if result > 1.0 * warnTimeOut then
   try
    s := '[~T/~B].~C0C #WARN(~I): TCritSection(' + Name + ').Lock - time of locking (ms) =~C0F ' +
         FormatFloat('0.##', result) + '~C0C, context =~C0F ' + ctx_msg +
         '~C0C, Pass of tryings =~C0F ' + IntToStr(n_pass);
    s := s + ',~C0C FSect.LockCount =~C0F ' + IntToStr (FSect.LockCount);
    s := s + ',~C0C LastLocker =~C0F ' + ll;

    {$IFDEF SAVE_LOCK_CONTEXT}
    if Assigned (slHistory) and (slHistory.Count > 0) then
       s := s + '~C0C, Lock-History: ~C0F '#13#10 + slHistory.Text;
    {$ENDIF}
    ODS(s  + '~C07');

   except
    on E: Exception do
       OutputDebugString (PChar ('Exception catched in TCritSection.Lock ' + E.Message));
   end;
 {$IFDEF SAVE_LOCK_CONTEXT}
 OnLock (ctx_msg + ftow (result, ' [%.3f ms]'));
 {$ELSE}
 LastLocker := ctx_msg + ' ' + IntToStr(tid) + '@' + FormatDateTime('nn:ss.zzz', Now);
 {$ENDIF}
end; // TCritSection.Lock

procedure TCritSection.OnLock;
begin
 TProfileTimer (fpt).StartOne(22);

 InterlockedIncrement (FLockCount);
 try
  SaveMsg (InfoFmt ('[~TP]. "' + FName + '" locked by thread ~I, context: ' + ctx_msg +
                    ', ' + Dump ) );
  LastLocker := ctx_msg + ':' + IntToStr(GetCurrentThreadId);
  tmLock := local_time;

  if lock_trap then
     raise Exception.Create ('lock_trap');


 except
  on E: Exception do
     OnExceptLog ('OnLock', E);
 end;

 TProfileTimer (fpt).StartOne(23);
end;

procedure TCritSection.SetLastLocker(const Value: String);
begin
 if FSect.LockCount >= -2 then
    SetStrWZ (FLastLocker, Value, SZ_LAST_LOCKER)
 else
    SetStrWZ (FLastLocker, LastLocker + #13#10 + Value, SZ_LAST_LOCKER)
end;

// Lock

function TCritSection.TryLock(const ctx_msg: TSafeStr; timeOut: Integer): Boolean;
var pt: TProfileTimer;
    dw: Integer;
     s: String;
begin
 // циклическая попытка блокировки объекта
 pt := TProfileTimer (fpt);
 pt.StartOne (29);
 dw := 0;
 {$IFDEF HANG_LOG}
 s := DTPrefix + '#ENTER: TryLock ' + Dump;
 HangLog ( s );
 {$ENDIF}

 repeat
  ResetEvent (evt_unlock);
  result := TryEnterCriticalSection (FSect);

  if not result then
    begin
     Inc (dw);

     if dw and $FF = 0 then
        WaitForSingleObject ( evt_unlock, 5 )
     else
        SwitchToThread ();

     if ( GetCurrentThreadId = main_thread_id )  then
       begin
        if msg_pass and  Assigned (GOnLockTimeout) then
           GOnLockTimeout ( self )
        else
          begin
           s := DTPrefix + ' #TIMEOUT: TryLock ' + Dump;
           HangLog ( s );
          end;
       end
    end;


  if ( pt.Elapsed (29) > timeOut ) then break;
 until result;
 {$IFDEF SAVE_LOCK_CONTEXT}
 if result then OnLock (ctx_msg);
 {$ENDIF}
end; // TryLock

procedure TCritSection.Unlock;
var
   l, n: Integer;
   {$IFDEF SAVE_LOCK_CONTEXT}
     pt: TProfileTimer;
      s: String;
      t: Double;
   {$ENDIF}
begin
 {$IFDEF SAVE_LOCK_CONTEXT}
 pt := TProfileTimer (fpt);

 s := InfoFmt ('[~TP].~C08 "' + FName + '"~C07 unlocked by thread ~I. LockTime = ');
 if tmLock = 0 then
    t := 0
 else
    t := pt.Elapsed (22);

 if t > 50 then s := s + '~C0C' else
   if t > 10 then s := s + '~C0E' else
    if t >  1 then s := s + '~C0A' else
     if t > 0.1 then s := s + '~C09';

 s := s + Format ( '%.3f~C07 ms, CPUtime =~C0D %.3f~C07 ms, intersync =~C0D %.3f ms ~C07', [t, pt.CPUElapsed (22), pt.Elapsed(23)] );
 SaveMsg ( s );

 if ( t > LockTimeout ) and IsKeyPressed (VK_LSHIFT) then
    ODS ( History.Text );
 {$ENDIF}


 {$IFDEF HANG_LOG}
 s := DTPrefix + '#EXIT.1: Unlock ' + Dump;

 Assert ( FSect.LockCount <> -1, 'Excess unlocking attempt. ' + s );
 {$ENDIF}
 LeaveCriticalSection (FSect);
 {$IFDEF HANG_LOG}
 s := s + #13#10#9#9#9'    #EXIT.2: Unlock ' + Dump;
 HangLog ( s );
 {$ENDIF}

 l := StrLen (FLastLocker);
 for n := l - 1 downto 0 do
  if FLastLocker[n] = #13 then
    begin
     FLastLocker[n] := #0;
     break;
    end;

 // LastLocker := '~' + IntToStr(GetCurrentThreadId) + ': ' + FLastLocker;

 if FLockCount > 0 then
    InterlockedDecrement (FLockCount);

 if FSect.LockCount = -1 then
    FLastLocker := 'NOBODY'#0;

 if evt_unlock <> 0 then SetEvent (evt_unlock);
end; // UnLock

{ TFastSyncObject }

constructor TFastSyncObject.Create(const sc_name: String);
begin
 MaxHistLines := 20;
 fpt := TProfileTimer.Create();
 TProfileTimer (fpt).EnableWarnings := FALSE;
 {$IFOPT D+}
 bSaveHistory := TRUE;
 {$ELSE}
 bSaveHistory := FALSE;
 {$ENDIF}
 FName := sc_name;
 slHistory := TStringList.Create;
end;

destructor TFastSyncObject.Destroy;
begin
 slHistory.Clear;
 slHistory.Capacity := 0;
 slHistory.Free;
 slHistory := nil;
 fpt.Free;
end;

procedure TFastSyncObject.SaveMsg(const msg: WideString);
begin
 if not bSaveHistory then exit;
 if Assigned (slHistory) then slHistory.Add(msg);
 while (slHistory.Count > MaxHistLines) do slHistory.Delete (0);
end; // SaveMsg

{ TStateMutex }

var gEvtsCount: Integer = 0;

constructor TStateMutex.Create;
begin
 FLockState := -1;
 FLockCount := 0;
 sc_share := TCritSection.Create ('TStateMutex.sc_share');
 FQueue := TProtectedList.Create;
 evt_unlock := CreateEvent(nil, TRUE, FALSE, nil);
 // TEvent.Create(nil, TRUE, FALSE, '');
 Inc (gEvtsCount);
end;

destructor TStateMutex.Destroy;
begin
 sc_share.Free;
 CloseHandle (evt_unlock);
 Dec (gEvtsCount);
 FQueue.Free;
 inherited;
end;

procedure TStateMutex.Lock;
begin
 while not TryLock (nState, 1000, ctx) do;
end; // Lock, hard...

function TStateMutex.TryLock;
var
    Elapsed, t_start: Int64;
    p: Integer;
begin
 result := FALSE;
 Assert ( (nState >=0) and (nState < MUTEX_STATES_COUNT),
           Format ('TStateMutex.TryLock: outbound state %d used. MaxState = %d', [nState, MUTEX_STATES_COUNT - 1]));

 sc_share.Lock ('TryLock/1');
 try
  if nState = FLockState then
   begin // вторичная блокировка
    result := TRUE;
    Inc (FLockCount);
   end
  else
   begin
    p := FQueue.IndexOf (Ptr(nState));
    if p < 0 then
       FQueue.Add (nState);

   end;
 finally
  sc_share.Unlock;
 end;
 t_start := GetTickCount();
 Elapsed := 0;
 if not result then
 repeat
   if (FLockState < 0) and (FQueue.Count > 0) and (FQueue[0] = nState) then
    begin
     FLockState := nState;
     FLockCount := 1;
     result := TRUE;
     break;
    end;
   Elapsed := GetTickCount - t_start;
   if timeOut > 0 then
      WaitForSingleObject (evt_unlock, 50 );
 until (Elapsed > Timeout);
 ResetEvent (evt_unlock);
 if not result then
  begin
   { Если блокировка не удалась - надо себя из очереди вычеркнуть }
   FQueue.Remove (nState)
  end
 else FContext := ctx;

end; // TryLock

procedure TStateMutex.Unlock;
begin
 if (FLockState >= 0) then
  begin
   sc_share.Lock('.Unlock');
   try
     Dec (FLockCount);
     if FLockCount <= 0 then
        begin
         FQueue.Remove(FLockState);
         FLockState := -1;
         SetEvent (evt_unlock);
        end;
   finally
    sc_share.Unlock;
   end;
  end;

end; // Unlock

{ TProtectedList }

function TProtectedList.Add;
begin
 sc_share.Lock('.Add');
 try
  result := inherited Add( Ptr(Item));
 finally
  sc_share.Unlock;
 end;
end; // Add

constructor TProtectedList.Create;
begin
 sc_share := TCritSection.Create('TProtectedList.sc_share')
end;

procedure TProtectedList.Delete(Index: Integer);
begin
 sc_share.Lock('.Delete');
 try
  inherited Delete(Index);
 finally
  sc_share.Unlock;
 end;
end; // Delete

destructor TProtectedList.Destroy;
begin
 inherited;
 sc_share.Free;
end;

function TProtectedList.GetIntItem(Index: Integer): Integer;
begin
 result := Integer(Items [Index]);
end;

function TProtectedList.Remove;
begin
 sc_share.Lock('.Remove');
 try
  result := inherited Remove ( Ptr (Item) );
 finally
  sc_share.Unlock;
 end;
end;

procedure TProtectedList.SetIntItem(Index, Value: Integer);
begin
 Items [Index] := Ptr(Value); 
end;

{ TNotifyEventList }

procedure TNotifyEventList.AddHandler;
var
   einf: TNotifyEventInfo;
begin
 einf := TNotifyEventInfo.Create;
 einf.Handler := neh;
 einf.ID := id;
end;

procedure TNotifyEventList.AddSimple;
var
   einf: TNotifyEventInfo;
begin
 einf := TNotifyEventInfo.Create;
 einf.Simple := ne;
 einf.ID := id;
 Add (einf);
end;

constructor TNotifyEventList.Create;
begin
 inherited Create (TRUE); // owns
end;

function TNotifyEventList.GetParams(index: Integer): PNotifyParams;
var
   einf: TNotifyEventInfo;
begin
 result := nil;
 einf := TNotifyEventInfo ( Items [index] );
 if einf <> nil then
    result := @einf.Params;
end; // GetParams

procedure TNotifyEventList.NotifyAll( Sender: TObject; AParams: PNotifyParams; mask: Integer);
var
   n: Integer;
   einf: TNotifyEventInfo;
begin
 for n := 0 to Count - 1 do
  begin
   einf := TNotifyEventInfo ( Items [n] );
   if Assigned (einf.Handler) then
     begin
       if AParams = nil then
          einf.Handler ( @einf.Params )
       else
          einf.Handler ( AParams );
     end;
   if Assigned (einf.Simple) then einf.Simple ( Sender );
  end;

end; // NotifyAll


{ TCSInvader }

constructor TCSInvader.Create(cs: TCritSection; tid, wtd: DWORD);
begin
 _cs := cs;
 _id := tid;
 _wt := DWORD ( CurrentTime.Time ) + wtd;
end;

initialization
 main_thread_id := GetCurrentThreadID;


end.
