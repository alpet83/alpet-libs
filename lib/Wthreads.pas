////////////////////////////////////////////////////////////////////////////
//  Module 	    : WThread.pas
//  Modified 	    : 25.10.2014
//  Author	    : Alexander Petrov
//  Description     : Класс рабочих потоков, и потокового управляемого пула
////////////////////////////////////////////////////////////////////////////
unit WThreads;

interface
uses Windows, SysUtils, Classes, Vcl.Dialogs, ShellAPI, FastSync, StrClasses, ComObj, ActiveX, SyncObjs,
     Messages, Misc, ContNrs, DateTimeTools, Math, madExcept;


{$DEFINE ASYNC_RQS}

const
    RQSR_STOPTHREAD = $400;
    MSG_HANDLE_DEFAULT   = $7CEFBAD;
    UICMD_SHOW_CONSOLE   = $1030;
    UICMD_STOP_THREAD    = $CCCC;


    WAITING_MESSAGES = $0001;
    WAITING_RQ_EVENT = $0002;



    TST_STOPPING = 'STOPPING';
    TST_SHUTDOWN = 'SHUTDOWN';
    TST_TERMINATING = 'TERMINATING';

    OP_WAIT_START   = $0001;
    OP_WAIT_RQS     = $0002;

    OP_THREAD_STOP  = $0010;
    OP_WAIT_STOP    = $0011;

    OP_FREE_NIL     = $8000;


    THREAD_SUSPEND_RESUME       = $000002;
    THREAD_TERMINATE            = $000001;
    THREAD_GET_CONTEXT          = $000008;
    THREAD_SET_CONTEXT          = $000010;
    THREAD_QUERY_INFORMATION    = $000040;
    THREAD_ALL_ACCESS           = $1F03FF;



type
    TWaitFunc = function (timeOut: DWORD): DWORD of object;
    TRqsHandler = function (const rqs: String; rqobj: TObject): Integer of object;
    TObjProc = procedure of object;

    TSmallBuff4K = packed record
     used: Integer;
     data: array [0..4095 - 4] of BYTE;
    end;

    TBuffersList16x4K = array [0..15] of TSmallBuff4K;
    PBuffersList16x4K = ^TBuffersList16x4K;


    PRequestSlot = ^TRequestSlot;

    TWTRequestCallback = function ( prs: PRequestSlot ): Integer;

    TRequestSlot = packed record
       cl_data: PBuffersList16x4K;
         index: Integer;
        ownerf: Integer;
        ownert: DWORD;
           rqs: array [0..23] of CHAR;
           obj: TObject; // Debugging: может быть затерто плохим кодом
      callback: TWTRequestCallback;


      p_params: array [0..3] of Pointer;
      i_params: array [0..3] of Int64;
      f_params: array [0..3] of Double;
     allocated: array [0..3] of Integer;

     procedure   SetRqs (const s: String); inline;

     function    TestSwitchState(new_st, old_st: Integer): Boolean; inline;

    end; // TRequestSlot





    TAQEvents = record
      ReadEvent: THandle;
      FreeEvent: THandle;
      PushEvent: THandle;
       hMapping: THandle;
       OwnerPID: DWORD;
    end;

    PAQEvents = ^TAQEvents;


    PAQData = ^TAQData;


    TAQData = record
     last_allocated: Integer;
      slot_wait_cnt: Integer;
          last_read: Integer;
         rq_flushed: Integer;
          aq_events: array [0..7] of TAQEvents;
          is_shared: Boolean;
          cnt_locks: Integer;
           rq_slots: array [0..1023] of TRequestSlot;
            rq_mask: Integer;
            sz_name: array [0..31] of CHAR;

        creator_pid: DWORD;
      buffers_count: Integer;
       buffers_list: TBuffersList16x4K;
    end;

    // класс асинхронной очереди, для множества записывающих потоков и одного читающего

    TAsyncQueue = class (TObject)
    private
     FLocks: Integer;
     FSlotWaitCount: Integer;

    protected
           FData: PAQData;
      push_event: THandle;



     procedure  AdjustPointers (prs: PRequestSlot);

     function   AllocBuff (cb: Integer): Pointer; virtual;

     procedure  InitEvents; virtual;
    public

     property   Locks:   Integer read FLocks;
     property   SlotWaitCount: Integer read FSlotWaitCount;
     property   PushEvent: THandle read push_event;

     { C & D }

     constructor   Create (const AName: String);
     destructor    Destroy; override;

     { methods }

     function    AllocSlot (wait_msec: Integer = 500): PRequestSlot;
     procedure   Init;

     procedure   Flush (prs: PRequestSlot);
     function    Flushed: Integer;

     function    GetEvents: PAQEvents;
     function    GetName: String;

     function    IsOwner: Boolean;

     function    PushDataRqs   ( const rqs: String; const buff; cbSize: Integer; dwTimeout: DWORD = 10000 ): Pointer; virtual;
     function    PushSimpleRqs ( const rqs: String; rqobj: TObject; dwTimeout: DWORD = 10000 ): Boolean; virtual;


     function    Read: PRequestSlot;
     function    ReadRelease: TRequestSlot;
     procedure   Release (prs: PRequestSlot);
     procedure   ReleaseData (p: Pointer); virtual;
     function    WaitForPush (dwTimeout: DWORD = 1000; bAlertable: Boolean = TRUE): DWORD;


    end; // TAsyncQueue

    TIPCAsyncQueue = class (TAsyncQueue)
    private
      FReceiverClosed: Boolean;
    protected

       function       AllocBuff (cb: Integer): Pointer; override;


    public

      property        ReceiverClosed: Boolean read FReceiverClosed;

      { C & D }
      constructor     Create (const AName: String);
      constructor     Open;

      destructor      Destroy; override;

      { methods }
      function        IsReceiverSide: Boolean;

      class function  MakeInstance (const AName: String): TIPCAsyncQueue;

      procedure       ReleaseData (p: Pointer); override;

    end;


    TWorkThreadPool = class;

    TWorkerThread = class (TThread)
    private
          rql: TStrMap;      // кэш запросов при выполнении
     rq_queue: TAsyncQueue;

  FNotifyIconData: TNotifyIconData;
     FHangTimeout: DWORD;
          FStage: Single;
     FThreadName: String;     // name of thread
   FThreadStatus: String;
   FMaxHangCount: Integer;
      FHangCount: Integer;
   FOnThreadHang: TObjProc;
     FWorkPaused: Boolean;
      FNeedBench: Boolean;
      FVerbosity: Integer;
        FStarted: Boolean;
     FWaitingFor: Integer;
 FWarningTimeout: Integer;
     FWarnIgnore: Integer;
       FWindowed: Boolean;



     procedure SetThreadName(const Value: String);
     procedure SetHangTimeout(const Value: DWORD);
     procedure SetCurrentRqs(const Value: String);
     function  GetCurrentRqs: String;
     function  GetRequestCount: Integer; inline;
     procedure SetWorkPaused(const Value: Boolean);
     procedure SetStage(const Value: Single);

    protected

     n_loops: Integer;

     // rq_share: TCritSection;
     DDE_events: Integer;
     alv_events: Integer;
     // evt_rqs_posted: TEvent;
     evt_rqs_handled: TEvent;

     evt_start: TEvent;
     evt_stop: TEvent;
     rq_list: TStrMap;
     dr_list: TStrMap;  // список отложенных запросов
     wait_time: DWORD;
     wait_func: TWaitFunc;
     hMsgWnd: HWND;     // message window
     hTrayMenu: HMENU;


     FPool: TWorkThreadPool;
     FBusy: Boolean;
     FRQSH: TRqsHandler;
     FUsingTimer: Integer;
     FAliveTimer: TProfileTimer;
      FProfTimer: TProfileTimer;
     FGarbage: TObjectList;
     FJobFunc: TNotifyEvent;
     FJobParam: TObject;
     FJobPeriod: Integer;
     FAllowRelease: Boolean;
     ptExec: TProfileTimer;


           ps_flag: Boolean;
        void_loops: Integer;
       wp_exec_cnt: Integer;
      wp_last_time: Integer; // milliseconds from midnight
     bWaitMessages: Boolean;
         stop_time: Integer;

     FRequestCount: Integer;
        FRQPSCount: Integer;       // processing rqs count
          FAllocES: Integer;         // сколько было наработано до аллокации в пуле
       FIsCritical: Boolean;
          FTerrain: Integer;
        stop_ready: Boolean;
     logging_flags: DWORD;         // консоль для вывода
     have_debugger: Boolean;

     cur_rqs: array [0..255] of CHAR;

     procedure                  AddDelayedRequests(rqlOut: TStrMap);
     function                   GetDelayedRequests(rqlOut: TStrMap): Integer;

     function                   CanHandleRequests: Boolean; virtual;
     procedure                  HandleRequests(bForce: Boolean);  virtual;
     procedure                  HandleMessages (waitMsg: Boolean = FALSE);

     procedure                  MakeIPCQueue (const ipc_name: String);

     procedure                  Execute; override;
     procedure                  OleExec; virtual;
     procedure                  OnTimer (idTimer: Integer); virtual;
     function                   NoWait (timeOut: DWORD): DWORD;
     function                   WaitRqsPosted (timeOut: DWORD): DWORD;
     procedure                  WorkCycle; virtual;


     function                   GetRequests(rqlOut: TStrMap): Integer; // выборка новых запросов, и очистка списка


     procedure                  ProcessInit; virtual;

     function                   ProcessCommand (wCmd: WORD): Integer; virtual; // обработка WM_COMMAND для hMsgWnd
     function                   ProcessMessage (uMsg: DWORD; wParam, lParam: Integer): Integer; virtual;
     function                   ProcessRequest (const rqs: String; rqobj: TObject): Integer; virtual;
     function                   ProcessRequestEx ( prs: PRequestSlot ): Integer; virtual;
     procedure                  ProcessRequestResult  (res: Integer); virtual;
     procedure                  ProcessTerminate; virtual;   // pre terminate actions & terminate
     procedure                  ProcessThreadStop; virtual;  // pre stopping

    public

     work_state: Integer;       // 0 = waiting, 1 = active
     bMsgForAnyWindow: Boolean;
     bUnicodeWindow: Boolean;

     dbg_info: String;


     property                   AliveTimer: TProfileTimer read FAliveTimer;
     property                   AllocES: Integer read FAllocES;
     property                   Busy: Boolean read FBusy;
     property                   Garbage: TObjectList read FGarbage;
     property                   CurrentRqs: String read GetCurrentRqs write SetCurrentRqs;
     property                   HangTimeout: DWORD read FHangTimeout write SetHangTimeout;
     property                   HangCount: Integer read FHangCount write FHangCount;
     property                   NeedBench: Boolean read FNeedBench write FNeedBench;
     property                   LoggingFlags: DWORD   read logging_flags;
     property                   MaxHangCount: Integer read FMaxHangCount write FMaxHangCount;
     property                   MsgHandle: HWND read hMsgWnd;
     property                   IsCritical: Boolean read FIsCritical write FIsCritical;
     property                   ThreadName: String read FThreadName write SetThreadName;
     property                   ThreadStatus: String read FThreadStatus write FThreadStatus;
     property                   ProfTimer: TProfileTimer read FProfTimer;
     property                   RQSH: TRqsHandler read FRQSH write FRQSH;
     property                   RequestCount: Integer read GetRequestCount;
     property                   RequestQueue: TAsyncQueue read rq_queue;
     property                   Stage: Single read FStage write SetStage;         // стадия выполнения операции
     property                   Started: Boolean read FStarted;
     property                   StopTime: Integer read stop_time write stop_time;
     property                   Terminated;
     property                   Terrain: Integer read FTerrain; // how to def, where now eip

     property                   OnThreadHang: TObjProc read FOnThreadHang write FOnThreadHang;
     property                   WaitingFor: Integer read FWaitingFor;
     property                   WarnIgnore: Integer read FWarnIgnore write FWarnIgnore;
     property                   WarningTimeout: Integer read FWarningTimeout write FWarningTimeout;
     property                   Windowed: Boolean read FWindowed;
     property                   WorkPaused: Boolean read FWorkPaused write SetWorkPaused;
     property                   Verbosity: Integer read FVerbosity write FVerbosity;

     { C & D }
     constructor                Create (CreateSuspended: Boolean; const sName: String; bWindowed: Boolean = FALSE );
     destructor                 Destroy; override;

     { Methods }

     function                   AddRequest (rqs: String; rqobj: TObject = nil; flags: DWORD = 0 ): Boolean;
     function                   AddRequestEx (rqs: String): PRequestSlot;

     function                   CPUTime: Integer; // сколько процессорных мсек скушано

     function                   GetLocksCount(bReset: Boolean): Integer; // debugging
     procedure                  SendRequest (prs: PRequestSlot);

     procedure                  HangStop;
     procedure                  NotifyAlive; virtual;
     procedure                  MakeTrayIcon (const sTip: String; hIcon, hMenu: DWORD);

     function                   RQCount: Integer;

     function                   PostMessage (uMsg: DWORD; wParam, lParam: Integer): Boolean; virtual;

     function                   ScheduleRequest (const rqs: String; at: Integer = 0; rqobj: TObject = nil; bSingle: Boolean = FALSE): Boolean;
     procedure                  SetJob (jFunc: TNotifyEvent; jParam: TObject; jPeriod: Integer = 0);
     function                   Stopped: Boolean;
     procedure                  StopThread (bNestedStop: Boolean = FALSE); virtual;
     function                   ThreadUID: String;
     function                   WaitRequests (timeOut: DWORD = 5300; bWarnTimeout: Boolean = TRUE): DWORD;
     function                   WaitStart (timeOut: DWORD = 1500): DWORD;
     function                   WaitStop  (timeOut: DWORD = 5200; bForceTerminate: Boolean = FALSE): DWORD;
     procedure                  WorkProc; virtual;


     procedure                  SetBusy (timer_start: Integer = 5); virtual;
     procedure                  Release; virtual; // освобождение потока от задания

    end; // TWorkerThread

    TWorkerCreator = function (const sName: String): TWorkerThread of object;

    TWorkerThreadList =  array of TWorkerThread;



    TWorkThreadPool = class (TObjectList)
    private
     FMaxWaitWorkers: Integer;
     FMaxWorkers: Integer;
    protected
     FCurrentWorker: Integer;
     FWorkerCreator: TWorkerCreator;
     FName: String;
     sc_share: TCritSection;
     FReadyCount: Integer;
     FStrictPool: Boolean; // стремится ограничивать размер пула по количеству доступны CPU

     function                   AddWorker(const sName: String): TWorkerThread;
     function                   GetWorker(nIndex: Integer): TWorkerThread;
    public

     property                   Items[Index: Integer]: TWorkerThread read GetWorker; default;
     property                   ReadyCount: Integer read FReadyCount;
     property                   StrictPool: Boolean read FStrictPool write FStrictPool;
     property                   Name: String read FName write FName;

     property                   MaxWaitWorkers: Integer read FMaxWaitWorkers write FMaxWaitWorkers;
     property                   MaxWorkers: Integer read FMaxWorkers write FMaxWorkers;


     { C&D }
     constructor                Create (wCreator: TWorkerCreator);
     destructor                 Destroy; override;
     { methods }
     function                   AllocateWorker(const sName: String = ''): TWorkerThread;
     function                   BusyCount: Integer;          
     function                   CreateWorker (const sName: String): TWorkerThread; // default unified
     function                   DispatchRequest (const rqs: String; rqobj: TObject): TWorkerThread;
     procedure                  StopAll;
    end; // TWorkThreadPool


    TThreadAliveContext = record
         ThreadID: DWORD;
           Handle: THandle;
        HangCount: Integer;
     MaxHangCount: Integer;
      HangTimeout: Integer;
       AliveTimer: TProfileTimer;
    end;

    PThreadAliveContext = ^TThreadAliveContext;


    TThreadWatchdog = class (TWorkerThread)
    private
      procedure ProcessThreadHang(wt: TWorkerThread; th: THandle);
    protected
     FThreadList: TObjectList;
       FTracking: String;
        FCSIList: TObjectList;


     FMainThread: TThreadAliveContext;


     procedure                  OnTimer (idTimer: Integer); override;
     procedure                  ProcessInit; override;
     procedure                  ProcessThreadStop; override;  // pre stopping

     function                   ProcessRequest (const rqs: String; rqobj: TObject): Integer; override;


    public

     property                   Tracking: String read FTracking;

     { C & D }

     constructor                Create;
     destructor                 Destroy; override;

     { methods }

     procedure                  MTNotifyAlive;
     procedure                  RegThread (wt: TWorkerThread; bRegister: Boolean);
     procedure                  WorkProc; override;

    end;


    EThreadHangException = class ( Exception )
    public
    end;
var
         g_verb_level: Integer = 5;
      gThreadWatchDog: TThreadWatchDog = nil;
          gTrapThread: DWORD = 0;
         gWaitHistory: String;

   SetThreadErrorMode: function ( newMode: DWORD; oldMode: PDWORD ): LongBool; stdcall;

procedure CoInitMT;
procedure RunMessageLoop (wt: TWorkerThread);

// operate thread list
function  OpThreadList (op, param: DWORD; tl: Pointer; ACount: Integer): Integer; overload;
procedure OpThreadList (op, param: DWORD; tl: TWorkerThreadList); overload; inline;
procedure OpThreadList (op, param: DWORD; tl: TObjectList); overload; inline;


procedure DbgEnterCS ( var sc: _RTL_CRITICAL_SECTION; const ctx: String );
function  DbgWaitForSO ( h: THandle; dwTimeout: DWORD; const ctx: String ): DWORD;




implementation
uses ModuleMgr, IPCUtils;


const
   TMR_REGJOB = 158;
   TMR_WTLIVE = 190;

   SLOT_FLUSHED  = Ord('f');
   SLOT_READ     = Ord('r');
   SLOT_WRITE    = Ord('w');
   SLOT_UNUSED   = Ord('u');
   MSGWND_CLASS = 'WT_MSG_WINDOW';

var
   gPID: DWORD = 0;

threadvar
   NewAQMapping: THandle;


function OpenThread ( dwDesiredAccess: DWORD; bInheritHandle: Boolean; dwThreadID: DWORD ): THandle; stdcall;  external kernel32;


procedure DbgEnterCS ( var sc: _RTL_CRITICAL_SECTION; const ctx: String );
var
   t: TDateTime;
begin
 t := g_time_func();
 EnterCriticalSection (sc);
 t := ( g_time_func() - t ) / DT_ONE_MSEC;
 if ( t > 10 ) and ( gTrapThread = GetCurrentThreadID ) then
    gWaitHistory := gWaitHistory + ctx + ftow (t, ':%.3f ms ');
end;


function DbgWaitForSO ( h: THandle; dwTimeout: DWORD; const ctx: String ): DWORD;
var
   t: TDateTime;
begin
 t := g_time_func();
 result := WaitForSingleObject (h, dwTimeout);
 t := ( g_time_func() - t ) / DT_ONE_MSEC;
 if ( t > 10 ) and ( gTrapThread = GetCurrentThreadID ) then
    gWaitHistory := gWaitHistory + ctx + ftow (t, '@%.3f ');

end;


procedure LockWatchdog(csi: TCSInvader; add: Boolean);
begin
 if not Assigned (gThreadWatchDog) or gThreadWatchDog.Terminated then exit;

 if add then
    gThreadWatchDog.AddRequest('ADD_CSI', csi)
 else
    gThreadWatchDog.AddRequest('RMV_CSI', csi);
end;

procedure CoInitMT;
var
   f: DWORD;
   r: HResult;
begin
 CoUninitialize;
 SetLastError (0);
 f := COINIT_MULTITHREADED or CoInitFlags;
 r := CoInitializeEx (nil, f);
 case r of
  S_OK: ;
  RPC_E_CHANGED_MODE:
      ODS ('[~T]. #DBG: CoInitializeEx returned~C0F RPC_E_CHANGED_MODE~C07');
  S_FALSE:
     PrintError ('CoInitializeEx returned S_FALSE, Flags = $' + IntToHex (f, 4) + ', Error: ' + Err2Str );
 end;

end;

function OpThreadList (op, param: DWORD; tl: Pointer; ACount: Integer): Integer; overload;
type
   TThreadList256 = array [0..256] of TWorkerThread;
   PThreadList256 = ^TThreadList256;

var
   ptl: PThreadList256;
    wt: TWorkerThread;
     n: Integer;

begin
 result := 0;
 ptl := tl;

 for n := 0 to ACount - 1 do
  begin
   wt := ptl [n];

   if Assigned (wt) then
     case op of
       OP_WAIT_START: wt.WaitStart (param);
         OP_WAIT_RQS:
            if wt.WaitRequests(param, param and 1 <> 0) = WAIT_OBJECT_0 then Inc (result);
      OP_THREAD_STOP: wt.StopThread();
        OP_WAIT_STOP:
            if wt.WaitStop (param, param and 1 <> 0) = WAIT_OBJECT_0 then Inc (result);
         OP_FREE_NIL:
           begin
            wt.Free;
            ptl [n] := nil;
           end;

     end; // case
  end; // for

end;

procedure OpThreadList (op, param: DWORD; tl: TWorkerThreadList); overload;
begin
 OpThreadList ( op, param, @tl[0], Length(tl) );
end;

procedure OpThreadList (op, param: DWORD; tl: TObjectList); overload;
begin
 OpThreadList ( op, param, @tl.List[0], tl.Count );
end;




procedure RunMessageLoop (wt: TWorkerThread);
var key: CHAR;
    stopped: Boolean;
begin
 repeat
  key := UpCase (ReadKey);
  case key of
    'E', 'Q':
       begin
        ShutdownReason := 'MANUAL STOP';
        wt.StopThread;
       end;
    'H': HideConsole;
    'V':
       begin
        Inc (g_verb_level);
        g_verb_level := g_verb_level and 7;
        ODS ('[~T]. #DBG: Global verbosity level = ~C0D' + IntToStr(g_verb_level) + '~C07' );
       end;
  end;

  stopped := ( DbgWaitForSO ( wt.Handle, 500, 'RML' ) = WAIT_OBJECT_0 );
  Sleep(5);
 until stopped;

end;
{ TWorkerThread }

function TWorkerThread.AddRequest(rqs: String; rqobj: TObject; flags: DWORD ): Boolean;
var
   res: Integer;
begin
 Assert (Assigned(self), 'TWorkerThread(?).AddRequest, self unassigned!');


 result := False;
 if ( flags and 1 <> 0 )  and (rqs = CurrentRqs) then exit; // already handling

 if Terminated then exit;

 if ( flags and 2 = 0 ) and ( ThreadId = GetCurrentThreadId ) then
   begin
    res := ProcessRequest(rqs, rqobj);
    ProcessRequestResult (res);
    exit;
   end;


 {$IFDEF ASYNC_RQS}
 result := rq_queue.PushSimpleRqs (rqs, rqobj, WarningTimeout * 4 );
 if not result then
      begin
       PrintError ( ThreadName + '. rq_queue.PushSimpleRqs returned FALSE' );
       exit;
      end;
 {$ELSE}
 rq_list.Lock('.AddRequest');

 try
  if Terminated then exit;  // после блокировки задача могла быть снята

  try
    if (not bSingle) or (rq_list.IndexOf(rqs) < 0) then
       begin
        rq_list.AddObject(rqs, rqobj);
        result := True;
       end;

    FRequestCount := rq_list.Count; // действительное количество запросов

  except
   on E: Exception do
      OnExceptLog ( ThreadName + Format ('.AddRequest ("%s", $%p)', [rqs, Pointer(rqobj) ]), E );
  end;

 finally
  rq_list.Unlock;
 end;

 {$ENDIF}


 // evt_rqs_posted.SetEvent;
 evt_rqs_handled.ResetEvent;

 WorkPaused := FALSE;

 if bWaitMessages then PostMessage (WM_TIMER, 1001, 0);
end;

function TWorkerThread.AddRequestEx (rqs: String): PRequestSlot;
var
   timeout: Integer;
begin
 timeout := 100;
 repeat
  result := rq_queue.AllocSlot;
  if result = nil then
     PrintError ( ThreadName + '.AddRequestEx, cannot allocate slot. Probably thread hang' );
  Dec (timeout);
 until ( result <> nil ) or ( timeout <= 0 );
 if result <> nil then
    result.SetRqs (rqs);
end;

// AddRequest

constructor TWorkerThread.Create;
begin
 try
  // ThreadName := ClassName + '(' + FormatPtr( Pointer(self) ) + ', ' + IntToStr(ThreadID) + ')';
  logging_flags := gThreadLogFlags;

  FWindowed := bWindowed;
  WarningTimeout := 2000;
  CoInitFlags := COINIT_MULTITHREADED;
  FThreadName := sName;
  FProfTimer := TProfileTimer.Create;
  // rq_share := TCritSection.Create('TWorkerThread(' + FThreadName + ').rq_share');
  // evt_rqs_posted :=  TEvent.Create (nil, FALSE, FALSE, '');
  evt_rqs_handled := TEvent.Create (nil, FALSE, FALSE, '');
  evt_stop :=  TEvent.Create(nil, TRUE, FALSE, '');
  evt_start := TEvent.Create(nil, TRUE, FALSE, '');
  rq_list := TStrMap.Create;
  dr_list := TStrMap.Create;

  if rq_queue = nil then
     rq_queue := TAsyncQueue.Create (sName + '.rq_queue');

  wait_time := 150;
  wait_func := WaitRqsPosted;
  stop_time := 25 * 3600;
 except
  on E: Exception do
   ods ('#Exception in TWorkerThread.Create: ' + E.Message);
 end;
 inherited Create(CreateSuspended);
end; // Create

destructor TWorkerThread.Destroy;
begin
 if not Terminated then Terminate;
 if WaitStop(1500) <> WAIT_OBJECT_0 then
     ODS('[~T]. #WARN: Ожидания завершения нити~C0A ' + ThreadName + '~C07 завершилоось таймаутом');

 rq_list.Free;
 dr_list.Free;
 rq_queue.Free;


 // evt_rqs_posted.Free;
 evt_rqs_handled.Free;
 evt_stop.Free;
 evt_start.Free;
 FProfTimer.Free;
 inherited;
end; // Destroy

procedure TWorkerThread.Execute;
begin
 gThreadLogFlags := logging_flags;

 OutputDebugString ( PChar ('TWorkerThread.Execute ->>> ENTER') );
 OutputDebugString ( PChar ('ThreadID = ' + IntToStr (ThreadID) ) );

 if FThreadName = '' then
    ThreadName := ThreadUID
 else
    ThreadName := FThreadName + ' ' + ThreadUID;


 CoInitMT;
 OleInitialize (nil);

 stop_ready := TRUE;
 rql := TStrMap.Create;

 FGarbage := TObjectList.Create ( TRUE );

 FAliveTimer := TProfileTimer.Create;
 FHangTimeout := 35000;


 if Assigned (gThreadWatchDog) then
    gThreadWatchDog.RegThread (self, TRUE);

 ptExec := TProfileTimer.Create;
 ptExec.Start($FFFFFFFFFF); // 32 timers

 FGarbage.Add (FAliveTimer);
 FGarbage.Add (rql);
 FGarbage.Add (ptExec);



 // NameThread (ThreadId, AnsiString(ThreadName) );
 try
   try
    SetLastError (0);

    if Windowed then
     begin

      if bUnicodeWindow then
         hMsgWnd := CreateWindowExW (WS_EX_APPWINDOW or WS_EX_NOPARENTNOTIFY,
                                     MSGWND_CLASS, 'MsgWnd', WS_OVERLAPPEDWINDOW,
                                     0, 0, 150, 30, 0, 0, hInstance, self)
      else
         hMsgWnd := CreateWindowExA (WS_EX_APPWINDOW or WS_EX_NOPARENTNOTIFY,
                                     MSGWND_CLASS, 'MsgWnd',  WS_OVERLAPPEDWINDOW,
                                     0, 0, 150, 30, 0, 0, hInstance, self);
      ASSERT ( hMsgWnd <> 0, 'function CreateWindowEx failed: ' + IntToStr (GetLastError));
     end;



    OleExec;
   except
    // TODO: exception handling
    on E: Exception do
      begin
       OnExceptLog (ThreadName + '.Execute', E, FALSE);
       Terminate;
       if IsCritical then TerminateProcess (GetCurrentProcess, $C0001401);
      end;
   end;
 finally
  if Assigned (ptExec) then
     ODS('[~T].~C0E #DBG: ' + ThreadName + ' finalization stage #3, run-time =~C0D ' + ftow(ptExec.Elapsed, '%.1f') +'~C0E ms ~C07');

  if FNotifyIconData.uID <> 0 then
     Shell_NotifyIcon (NIM_DELETE, @FNotifyIconData);

  if Assigned (gThreadWatchDog) and (gThreadWatchDog <> self) then
     gThreadWatchDog.RegThread (self, FALSE);

  ptExec := nil;
  FreeAndNil (FGarbage);
 end;
 OleUninitialize;
 evt_rqs_handled.SetEvent;
 evt_stop.SetEvent;
 ODS('[~T].~C0E #DBG: ' + ThreadName + ' finalization completed~C07');
end; // Execute

function TWorkerThread.GetCurrentRqs: String;
begin
 result := cur_rqs;
end;


procedure TWorkerThread.AddDelayedRequests(rqlOut: TStrMap);
begin
 dr_list.Lock('.AddRequests');
 try
  dr_list.AddStrings (rqlOut);
 finally
  dr_list.Unlock;
 end;

end;

function TWorkerThread.GetDelayedRequests(rqlOut: TStrMap): Integer;
begin
 result := 0;
 if dr_list.Count = 0 then exit;
 dr_list.Lock('.GetRequest');
 try
  Stage := 102.11;
  rqlOut.Assign (dr_list);
  dr_list.Clear;
  result := rqlOut.Count;
 finally
  dr_list.Unlock;
 end;

end;

function TWorkerThread.GetLocksCount(bReset: Boolean): Integer;
begin
 result := rq_queue.Locks;
 if bReset then rq_queue.FLocks := 0;
end;

function TWorkerThread.GetRequestCount: Integer;
begin
 result := FRequestCount + FRQPSCount + rq_queue.Flushed;
end;

function TWorkerThread.GetRequests(rqlOut: TStrMap): Integer;
begin
 result := 0;

 {$IFDEF ASYNC_RQS}
 rqlOut.Clear;
 {$ELSE}

 if (rq_list = nil) or (rq_list.Count = 0) then exit;
 try

  rq_list.Lock('.GetRequest');
  dbg_info := 'rq_list = ' + FormatPtr(rq_list);

   try
    rqlOut.Clear;
    rqlOut.Assign (rq_list);
    rq_list.Clear;
    result := rqlOut.Count;
   finally
    rq_list.Unlock;
   end;
 except
  on E: Exception do
    OnExceptLog (ClassName + '.GetRequests, ' + dbg_info, E);
 end;
 {$ENDIF}
end; // GetRequest

procedure TWorkerThread.HandleMessages;
var
    msg: tagMSG;
    wnd: HWND;
    mcnt: Integer;
begin
 FTerrain := 3;
 Stage := 101;


 if bMsgForAnyWindow then wnd := 0 else wnd := hMsgWnd;
 mcnt := 0;


 if waitMsg then
  try
   FWaitingFor := WAITING_MESSAGES;
   if GetMessage(msg, wnd, 0, 0) then
      begin
       TranslateMessage (msg);
       DispatchMessage (msg);
      end;
  finally
   FWaitingFor := 0;
  end;

 Stage := 102;

 // GetQueueStatus (1);




 while ( mcnt < 9 ) and PeekMessage (msg, wnd, 0, 0, PM_REMOVE or PM_NOYIELD ) do
  begin
   Stage := Stage + 0.01;
   if wnd > 0 then
    begin
     TranslateMessage (msg);
     Stage := Stage + 0.001;
     DispatchMessage (msg);
    end
   else
    ProcessMessage ( msg.message, msg.wParam, msg.lParam );
   Stage := Stage + 0.0001;
   Inc (mcnt);
   if IsCritical then break;
  end;

 Stage := -1;
 FTerrain := 1;
end; // HandleMessages

procedure TWorkerThread.HandleRequests;
var n, tc, tr, rres: Integer;
    r: String;
    t: String;
    robj: TObject;
     prs: PRequestSlot;
      wt: TWorkerThread;
begin
 Stage := 102;
 {$IFNDEF ASYNC_RQS}
 if (rq_list = nil) or (dr_list = nil) or ( (dr_list.Count = 0) and (rq_list.Count = 0) ) then
   begin
    FRequestCount := 0;
    exit;
   end;
 {$ENDIF}
 if bForce or CanHandleRequests then else exit; /// пропускать

 Stage := 102.1;

 // --------------------------------------------------------
 // Обработка отложенных запросов
 if GetDelayedRequests (rql) > 0 then
  begin
   n := 0;

   tc := CurrentTime.Time;

   while (n < rql.Count) do
    begin
     t := rql.ValueFromIndex [n];
     tr := StrTimeDecode (t).Time;
     // если время выполнения запроса пришло
     if ( tc >= tr ) or ( tr - tc > 23 * 3600 * 1000 ) then
        begin
         r := rql.Names [n];
         AddRequest ( r, rql.Objects [n], 2 );
         rql.Delete (n);
        end
     else
        Inc (n);
    end; // while

   Stage := 102.2 + 0.001 * n;
   if rql.Count > 0 then
      AddDelayedRequests (rql);
  end;

 if dr_list.Count > 0 then
    ODS('[~T]. #PERF: delayed requests rest ~C0F' + dr_list.CommaText + '~C07');



 Stage := 102.5;



 FTerrain := 2;

 GetRequests (rql);
 r := 'NOPE';
 robj := nil;

 // in order/processing request count
 FRQPSCount := rql.Count + rq_queue.Flushed;

 wt := self;
 // EXT REQUEST HANDLING LOOP
 for n := 0 to wt.rql.Count - 1 do
  begin
   if Terminated or (self = nil) then break;

   try
    Assert ( Assigned (self), 'HandleRequests self = nil. prv_rqs = ' + r );
    r := Trim ( wt.rql [n] );

    robj := wt.rql.Objects [n];
    Assert ( Length (r) < 128, 'Request length is long = ' + IntToStr ( Length(r) ) );
    CurrentRqs := r;

    Assert ( Assigned (self) and (self = wt), 'HandleRequest self is bad. Possible stack damaged.' );
    Assert ( not IsBadReadPtr ( self, 4 ), 'WorkerThread self is bad addr = ' + FormatPtr (self) );

    rres := wt.ProcessRequest (r, robj);
    wt.ProcessRequestResult (rres);
   except
    on E: Exception do
      OnExceptLog (ThreadName + Format('.HandleRequests, rqs = %s, robj = %p ', [r, Pointer(robj)]), E);
   end;

   CurrentRqs := '';
  end;
 rql.Clear;

 // MAIN LOOP request processing    =================  =================  =================  =================  =================  =================  =================  =================
 for n := 0 to 15 do
   begin
    prs := rq_queue.Read;
    if prs = nil then break;
    try
      try
       CurrentRqs := prs.rqs;
       rres := ProcessRequestEx ( prs );
       wt.ProcessRequestResult (rres);
      except
        on E: Exception do
          OnExceptLog (ThreadName + Format('.HandleRequests#2, rqs = %s, robj = %p ', [r, Pointer(robj)]), E);
      end;

    finally
     rq_queue.Release (prs);
    end;
   end; // for


 if Terminated or (rq_list = nil) then
  begin
   FRequestCount := 0;
   FRQPSCount := 0;
   evt_rqs_handled.SetEvent;
  end
 else
  begin
   FRequestCount := rq_list.Count + rq_queue.Flushed;
   FRQPSCount := 0;
   if RQCount = 0 then
      evt_rqs_handled.SetEvent;
  end;

 FTerrain := 1;
 Stage := -2;
end; // HandleRequests

procedure TWorkerThread.HangStop;
begin
 TerminateThread (Handle, $8840);
 if stop_ready then ProcessThreadStop;
end;



var
   gIconCounter: Integer = 0;

procedure TWorkerThread.MakeIPCQueue(const ipc_name: String);
begin
 rq_queue.Free;
 rq_queue := TIPCAsyncQueue.MakeInstance (ipc_name);
end;

procedure TWorkerThread.MakeTrayIcon(const sTip: String; hIcon, hMenu: DWORD);
const NOTIFYICON_VERSION = 3;
var
   nd: TNotifyIconData;
begin
 if hMsgWnd = 0 then exit;
 FillChar (nd, sizeof (nd), 0);
 nd.cbSize := sizeof(nd);
 nd.Wnd := hMsgWnd;
 nd.uFlags := 0;
 nd.uID := InterlockedIncrement (gIconCounter);
 nd.uCallbackMessage := WM_MOUSEMOVE;
 nd.hIcon := hIcon;
 nd.uTimeout := NOTIFYICON_VERSION;

 hTrayMenu := hMenu;
 StrPCopy (nd.szTip, sTip);
 if sTip <> '' then nd.uFlags := nd.uFlags or NIF_TIP;
 if hIcon <> 0 then nd.uFlags := nd.uFlags or NIF_ICON;
 if hMsgWnd <> 0 then nd.uFlags := nd.uFlags or NIF_MESSAGE;
 if Shell_NotifyIcon (NIM_ADD, @nd) then
    FNotifyIconData := nd;

end;





procedure TWorkerThread.NotifyAlive;
begin
 AliveTimer.StartPFC (35);
 Inc (alv_events);
end;

function TWorkerThread.NoWait(timeOut: DWORD): DWORD;
begin
 result := 0;
end;



function TWorkerThread.WaitRequests(timeOut: DWORD; bWarnTimeout: Boolean): DWORD;
var
   tmax: TDateTime;
   tout: Boolean;
begin
 result := 0;
 if Terminated then exit;

 Assert ( Assigned(self), '.WaitRequests: self unassigned!');
 tmax := TimeLine (timeOut);
 Assert ( Assigned (rq_list), ClassName + '.rq_list is unassigned!' );

 tout := FALSE;

 if ( WaitingFor = 0 ) or ( void_loops = 0 ) or ( RQCount > 0 ) or ( FTerrain = 2 ) then
 repeat
   result := DbgWaitForSO (evt_rqs_handled.Handle, timeOut, 'WRQ' );

   if ( RQCount > 0 ) and ( result = WAIT_OBJECT_0 ) then
     begin
      evt_rqs_handled.ResetEvent;
      ODS('[~T].~C0C #WARN (WaitRequest):~C07 ложное срабатывание evt_rqs_handled в нитке ~C0A' + self.ThreadName + '~C07' );
      continue;
     end;

   if (result = WAIT_TIMEOUT) and bWarnTimeout then
      begin
       try
        try
         if timeOut = 5300 then
          Assert(FALSE, '#WARN(WaitRequests): Timeout ~C0F' + IntToStr(timeOut) +
             '~C0C msec, for waiting requests in thread ~C0F' + ThreadName + '~C07')
         else
          ODS('[~T].~C0C #WARN(WaitRequests): Timeout ~C0F' + IntToStr(timeOut) +
             '~C0C msec, for waiting requests in thread ~C0F' + ThreadName + '~C07');
        except
         on E: Exception do
            OnExceptLog('WaitRequest.detection', E, TRUE);
        end;
       except
        on E: Exception do ;
       end;

       break;
      end;

   tout := ( Now > tmax );

   if tout then break;

   if (ps_flag) or (void_loops = 0) or ( WaitingFor <> 0 ) then continue;

 until ( rq_list = nil ) or ( RQCount = 0 );

 if (tout) and bWarnTimeout then
       ODS('[~T].~C0C #WARN(WaitRequests#2): Timeout ~C0F' + IntToStr(timeOut) +
           '~C0C msec, for waiting requests in thread ~C0F' + ThreadName + '~C07' );

end;


function  TWorkerThread.WaitRqsPosted (timeOut: DWORD): DWORD;
var
   tn: String;
begin
 result := WAIT_OBJECT_0;
 if Stopped then exit;

 result := DbgWaitForSO ( rq_queue.push_event, timeOut, 'WRP' );

 if (result = WAIT_ABANDONED) or (result = WAIT_FAILED) then
    begin
     tn := '~' + ThreadName;
     PrintError('Wait RQS posted - failed/abandoned. Thread = ' + tn + '. Error = ' + Err2Str(GetLastError));
     exit;
    end;

end;

procedure TWorkerThread.OleExec;
begin // Стандартный цикл обработки рабочего потока



 try
  ProcessRequest('INIT', nil);
 except
  on E: Exception do
     OnExceptLog ('TWorkerThread.OleExec', E);
 end;

 evt_start.SetEvent;
 FStarted := TRUE;


 n_loops := 0;

 Repeat
  WorkCycle;
 Until Terminated;

 FStarted := FALSE;
 ODS('[~T].~C0E #DBG: ' + ThreadName + ' finalization stage #2~C07');
 // ---- postpone processing --------
 HandleMessages(FALSE);
 //
 HandleRequests(TRUE);
 hMsgWnd := 0;
 ProcessRequest('DONE', nil);
 rq_list.Free;
 rq_list := nil;
end; // OleExec

procedure TWorkerThread.OnTimer(idTimer: Integer);
begin
 // nothing actions
 NotifyAlive ();  // OnTimer
 if (idTimer = TMR_WTLIVE) and Assigned (FPool) then
  begin
   Dec (FUsingTimer);
   if FUsingTimer = 0 then Release;
  end;
 if (idTimer = TMR_REGJOB) then
     AddRequest ('PS_JOB');
end; // OnTimer

// function GetConsoleWindow: DWORD; stdcall; external 'kernel32.dll';

function TWorkerThread.ProcessCommand(wCmd: WORD): Integer;
begin
 result := 0;
 case wCmd of
  UICMD_SHOW_CONSOLE:
        ShowWindow ( GetConsoleWindow, SW_SHOWNORMAL);
  UICMD_STOP_THREAD:
        StopThread;
 end; // case
end;

procedure TWorkerThread.ProcessInit;
var
   t_inf: String;
begin
 // any actions  on INIT
 ThreadStatus := 'INIT';
 MaxHangCount := 2;
 t_inf := Format ('~C0D %5d~C07 (~C0D$%04x~C07), Name = ~C0A%s~C07, Class = ~C0A%s~C07',
                        [ThreadId, ThreadID, ThreadName, ClassName]);

 ODS ('[~T]. #DBG: WorkerThread '  + t_inf  + ' started std-loop in OleExec');
end;

function TWorkerThread.ProcessMessage(uMsg: DWORD; wParam, lParam: Integer): Integer;
var mcp: TPoint;
// var rb_pressed: Boolean;
begin
 result := MSG_HANDLE_DEFAULT; //
 ps_flag := TRUE;

 case uMsg of
  WM_CREATE:
     begin
      ProcessRequest('WM_CREATE', nil);
      result := 0;
     end;
  WM_NCCREATE: result := 1;
  WM_CLOSE, WM_QUIT:
       begin
        ODS ('[~T]. #DBG: Received WM_CLOSE/WM_QUIT by thread ' + ThreadName);
        DestroyWindow (hMsgWnd);
        if (not Terminated) then
                StopThread (TRUE);
       end;
  WM_DESTROY:
       begin
        ODS ('[~T]. #DBG: Received WM_DESTROY by thread ' + ThreadName);
        hMsgWnd := 0;
       end;

      WM_TIMER: OnTimer (wParam);
    WM_COMMAND: result := ProcessCommand ( LoWord (wParam) );
  WM_MOUSEMOVE:
     if ( FNotifyIconData.Wnd = hMsgWnd ) and ( hTrayMenu <> 0 ) then
     with mcp do
      begin
       X := 0;
       Y := 0;
       case lParam of // детект выделения иконки
        NIN_KEYSELECT, NIN_SELECT, WM_RBUTTONUP, WM_CONTEXTMENU: GetCursorPos (mcp);
       end;
       // rb_pressed := GetAsyncKeyState (VK_RBUTTON) and $F000 <> 0; // ( wParam and MK_RBUTTON <> 0 );
       if X > 0 then
         begin
          TrackPopupMenu (hTrayMenu, TPM_LEFTALIGN  or  TPM_LEFTBUTTON, X, Y, 0, hMsgWnd, nil);
         end;
      end;
 end;
end; // ProcessMessage

function TWorkerThread.ProcessRequest(const rqs: String; rqobj: TObject): Integer;
begin
 result := 0;
 if rqs = 'INIT' then
    ProcessInit;

 if rqs = 'UPDATE_THREAD_NAME' then
     madExcept.NameThread ( ThreadId, FThreadName );

 if rqs = 'FREE_OBJ' then
   begin
    rqobj.Free;
    exit;
   end;

 if (rqs = 'PS_JOB') and Assigned (FJobFunc) then
   try
    FJobFunc (FJobParam);
    if log_verbose > 4 then
       ODS('[~T]. #DBG: Complete job in thread ~C0A' + ThreadName + '~C07');

    if FJobPeriod = 0 then
      begin
       if FPool <> nil then
          Release
       else
          result := RQSR_STOPTHREAD; // complete job!
      end;
   except
    on E: Exception do
       PrintError ('TWorkerThread(' + ThreadName + ').ProcessRequest / JobFunc caused exception = ' + e.Message);
   end;
 if Assigned (FRQSH) then
    result := FRQSH(rqs, rqobj);

 if ( rqs = 'STOPTHREAD' ) then
     begin
      if stop_ready then
        begin
         ODS('[~T].~C0E #DBG: ' + ThreadName + ' finalization started...~C07');
         ProcessThreadStop;
        end;
      result := RQSR_STOPTHREAD;
     end;

 if ( rqs = 'PAUSE_WORK' ) then FWorkPaused := TRUE;
 if FRequestCount > 0 then Dec (FRequestCount);
end; // ProcessRequest

function TWorkerThread.ProcessRequestEx(prs: PRequestSlot): Integer;
begin
 if ( prs.rqs = 'EXEC_CALLBACK' ) and Assigned (prs.callback) then
   result := prs.callback (prs)
 else
   result := ProcessRequest (prs.rqs, prs.obj);
end;



procedure TWorkerThread.ProcessRequestResult(res: Integer);
begin
 ps_flag := TRUE;

 case res of
   RQSR_STOPTHREAD:
           begin
            if stop_ready and (ThreadStatus <> TST_STOPPING) then ProcessThreadStop;
            ProcessTerminate;
           end;
 end;
end;


{
 if uMsg = WM_GETMINMAXINFO then
 with mmi^ do
  begin
    FillChar (mmi^, sizeof (MINMAXINFO), 0);
    mmi.ptMinTrackSize.X := 2;
    mmi.ptMinTrackSize.Y := 2;
    mmi.ptMaxSize.x := 2;
    mmi.ptMaxSize.y := 2;
    mmi.ptMaxPosition.X := 0;
    mmi.ptMaxPosition.Y := 0;
    mmi.ptMaxTrackSize.X := mmi.ptMaxSize.x + 24;
    mmi.ptMaxTrackSize.Y := mmi.ptMaxSize.y + 24;
  end;
}

function WrapWindowProc (hWnd: HWND; uMsg: DWORD; wParam, lParam: Integer): LRESULT; stdcall;
var wt: TWorkerThread;
    mmi: PMINMAXINFO absolute lParam;
    pcs: PCREATESTRUCT absolute lParam;
begin
 wt := Ptr ( GetWindowLong (hWnd, GWL_USERDATA) );
 result := MSG_HANDLE_DEFAULT;
 case uMsg of
   WM_CREATE, WM_NCCREATE:
    begin
     wt := pcs.lpCreateParams;
     SetWindowLong (hWnd, GWL_USERDATA, Integer (wt));
    end;
   WM_DDE_FIRST, WM_DDE_LAST:
    begin
     if Assigned(wt) then Inc (wt.DDE_events);
    end;
 end;

 if Assigned (wt) then
    result := wt.ProcessMessage (uMsg, wParam, lParam);
 if uMsg = WM_NCCREATE then
    result := 1; // TRUE

 if result = MSG_HANDLE_DEFAULT then
    result := DefWindowProc(hWnd, uMsg, wParam, lParam);
end; // WrapWindowProc

function RegMsgWndClass: Boolean;
var
   wc: WNDCLASSEXA;
   err: Integer;
begin
 FillChar (wc, sizeof (wc), 0);
 wc.cbSize := sizeof (wc);
 wc.style := 0;
 wc.lpfnWndProc := @WrapWindowProc;
 wc.hInstance := hInstance;
 wc.lpszClassName := MSGWND_CLASS;

 result := RegisterClassExA (wc) <> 0;
 if not result then
   begin
    err := GetLastError;
    if err <> ERROR_CLASS_ALREADY_EXISTS then
     ShowMessage ( '[WThreads.pas] RegisterClassEx returned error: ' + Err2Str (err));
   end;

end; // RegMsgWndClass


function TWorkerThread.Stopped: Boolean;
begin
 // TODO: detect thread terminated by handle
 result := (self = nil) or Terminated or (rq_list = nil); // or ( DbgWaitForSO (Handle, 1) = WAIT_OBJECT_0);
end;

procedure TWorkerThread.StopThread;
begin
 bWaitMessages := FALSE;
 wait_time := 5;
 if ( not Started ) or Terminated then exit;

 ptExec := nil;

 if Verbosity >= 3 then
    ODS('[~T]. #DBG: Trying StopThread request for ~C0A' + self.ThreadName + '~C07');


 if (not bNestedStop) then
  begin
   // чтобы небыло лишних сообщений
   if (hMsgWnd <> 0) then PostMessage (WM_CLOSE, 0, 0);

  end;

 AddRequest ('STOPTHREAD', nil, 1);
 {if (not FreeOnTerminate) and ( WaitStop (500) = WAIT_TIMEOUT ) then
    Terminate;}
end;

function TWorkerThread.ThreadUID: String;
begin
 result := '{ ' + ClassName + '@=$' + IntToHex( DWORD(self), 8) +':: ' + ModuleName;
 if ThreadID <> 0 then result := result + ' /ID=' + IntToStr(ThreadID);

 result := result  + ' }';
end;

procedure TWorkerThread.WorkCycle;
var
  elps, cpu: Double;
begin
 Stage := 99.9;
 if (not have_debugger) and IsDebuggerPresent then
   begin
    have_debugger := TRUE;
    SetThreadName (ThreadName);
   end;


 if (not Terminated) and (ThreadStatus <> TST_STOPPING) then
        begin
         if NeedBench then ptExec.StartOne (27); // work-proc time
         if not WorkPaused then WorkProc;
         if Terminated then exit;
        end;

  Inc (n_loops);

  // awaiting next loop
  if (wait_time > 0) and (not Busy) and Assigned (wait_func) and (rq_queue.Flushed = 0) then
    begin
     Stage := 98.0;
     work_state := 0;
     FWaitingFor := WAITING_RQ_EVENT;
     try
      wait_func(wait_time);
      Stage := 98.1;
     finally
      FWaitingFor := 0;
      work_state := 1;
     end;
    end;


  if ( not IsCritical ) and ( n_loops and $FFFFF = 0 ) then
   begin
    Stage := 97.0;
    elps := ProfTimer.Elapsed (26);
    cpu :=  ProfTimer.CPUElapsed (26);

    if ( elps >= 1000 ) and ( cpu > elps * 0.9 ) then
     begin
      PrintError ( ThreadName +  ': High CPU load = ' + ftow ( 100 * cpu / elps, '%.1f %%') );
      Sleep(100);
     end;

    ProfTimer.StartOne (26);
    n_loops := 0;
   end;


 FHangCount := 0;
end;

procedure TWorkerThread.WorkProc;
var
   ctime: TSeconds;
begin
 // default actions
 Stage := 0;

 ps_flag := FALSE;

 FTerrain := 1;
 if FAllowRelease then
  begin
   FAllowRelease := FALSE;
   Release;
   exit;
  end;

 try
  HandleMessages (bWaitMessages);
 except
  on E: Exception do
    OnExceptLog (ThreadName + '.WorkProc#HandleMessages call', E);
 end;

 try
  HandleRequests (False);
 except
  on E: Exception do
   OnExceptLog (ThreadName + '.WorkProc#HandleRequests call', E);
 end;

 if ps_flag then
    void_loops := 0
 else
    Inc (void_loops);



 Inc (wp_exec_cnt);

 if wp_exec_cnt and $F = 0 then
    gThreadLogFlags := logging_flags; // updated

 ctime := CurrentTime.Time;

 if Abs (ctime - wp_last_time) > 1000 then
  begin
   NotifyAlive (); // WorkProc
   wp_last_time := ctime;
  end;
 ctime := ctime div 1000;


 if InBound (ctime, stop_time, stop_time + 60) and (not Terminated) then
   begin
    ODS('[~T]. WorkThread(' + ThreadName + ') now stopped. Stop-Time = ' + FormatTime('hh:nn:ss', stop_time * 1000));
    StopThread;
    stop_time := 27 * 3600; // never again!
   end;
 FTerrain := 0;
 Stage := -3;
end;


function TWorkerThread.WaitStart(timeOut: DWORD): DWORD;
begin
 result := DbgWaitForSO ( evt_start.Handle, timeOut, 'WST' );
 if (result = WAIT_TIMEOUT) and (timeOut and $F <> 13) then
    PrintError (ClassName + '.WaitStart result = TIMEOUT for ' + IntToStr(timeOut) + ' msec');
end; // WaitStart


function TWorkerThread.WaitStop(timeOut: DWORD; bForceTerminate: Boolean): DWORD;
var
   w1, w2: DWORD;
begin
 stage := 0;
 SetEvent ( rq_queue.push_event );

 w1 := DbgWaitForSO ( evt_stop.Handle, timeOut, 'WSP' );
 w2 := w1;

 if (GetCurrentThreadId <> ThreadId) then
     w2 := DbgWaitForSO (Handle, IfV (w1 = WAIT_TIMEOUT, 1000, timeOut), 'WTerm' );


 result := Min (w1, w2);

 if ( w1 = WAIT_TIMEOUT ) and ( w2 = WAIT_TIMEOUT ) then
    ODS('~C0C[~T]. #WARN: Таймаут остановки нити ~C0C' + ThreadName + '~C07');

 if ( w2 = WAIT_TIMEOUT ) and (bForceTerminate) then
    begin
     ODS(Format('[~T/~I].~C0C #WARN: Принудительное завершение нити ID =~C0F %d~C0C, Handle =~C0F $%X~C07',
          [ThreadId, Handle] ));
     TerminateThread (Handle, $A0002000);
    end;
end;

procedure TWorkerThread.ProcessTerminate;
begin
 ThreadStatus := TST_TERMINATING;
 if not Terminated then Terminate;
 SetEvent (rq_queue.push_event); // loop
end;

procedure TWorkerThread.ProcessThreadStop;
begin
 // any actions before loop breaked.
 stop_ready := FALSE;
 ThreadStatus := TST_STOPPING;
 ODS ('[~T/~I]. #DBG: WorkerThread '  + ThreadName  +
      ' completed std-loop in OleExec. CPU Time = ' + IntToStr (CPUTime) );
end;

procedure TWorkerThread.Release;
begin
 if GetCurrentThreadId <> ThreadId then
   begin
    FAllowRelease := TRUE;
    exit;
   end;
 FBusy := FALSE;
 KillTimer(hMsgWnd, 190);
 if Assigned (FPool) then
    Inc (FPool.FReadyCount);
 Priority := tpNormal;
end;

function TWorkerThread.RQCount: Integer;
begin
 result := FRQPSCount;
 if rq_queue <> nil then
    Inc ( result, rq_queue.Flushed );

 if rq_list <> nil then
    Inc ( result, rq_list.Count );
end;

function TWorkerThread.ScheduleRequest(const rqs: String; at: Integer; rqobj: TObject; bSingle: Boolean): Boolean;
var
   tc: Integer;
begin
 result := FALSE;
 if Terminated then exit;
 tc := CurrentTime.Time;

 dr_list.Lock ('ScheduleRequest');
 try
   if bSingle and (dr_list.IndexOfName(rqs) >= 0) then exit;

   if at < 0 then
     dr_list.AddObject (rqs + '=' + FormatTime('hh:mm:ss.zzz', -at), rqobj)
   else
     dr_list.AddObject (rqs + '=' + FormatTime('hh:mm:ss.zzz', tc + at), rqobj);

  result := TRUE;
 finally
  dr_list.Unlock;
 end;
end; // ScheduleRequest

procedure TWorkerThread.SendRequest(prs: PRequestSlot);
begin
 rq_queue.Flush (prs);
end;

procedure TWorkerThread.SetBusy;
begin
 if FPool = nil then exit;
 wp_exec_cnt := 0;
 // FProfTimer.Start;
 FAllowRelease := FALSE;
 if ( not FBusy ) and ( Windowed ) then
    SetTimer (hMsgWnd, TMR_WTLIVE, 1000, nil); // alive timer
 FBusy := TRUE;
 FUsingTimer := timer_start;
end;

procedure TWorkerThread.SetCurrentRqs(const Value: String);
begin
 StrLCopy (@cur_rqs[0], PChar(Value), 255);
end;

procedure TWorkerThread.SetHangTimeout(const Value: DWORD);
begin
 FHangTimeout := Value;
 if Windowed then
    SetTimer (hMsgWnd, 13333, Value div 2, nil);
end;

function TWorkerThread.PostMessage(uMsg: DWORD; wParam,
  lParam: Integer): Boolean;
begin
 if Windowed then
    result := Windows.PostMessage (hMsgWnd, umsg, wParam, lParam)
 else
    result := PostThreadMessage ( ThreadID, Umsg, wParam, lParam );

 if result then
    SetEvent (rq_queue.push_event);
end;

procedure TWorkerThread.SetJob(jFunc: TNotifyEvent; jParam: TObject; jPeriod: Integer);
begin
 FJobFunc := jFunc;
 FJobParam := jParam;
 FJobPeriod := jPeriod;
 AddRequest ('PS_JOB');
 if ( jPeriod > 0 ) and Windowed then
    SetTimer (hMsgWnd, TMR_REGJOB, jPeriod, nil);
end;

procedure TWorkerThread.SetStage(const Value: Single);
var
   e: Double;
   r: Integer;
begin
 Assert ( self <> nil, 'SetStage: bad thread instance');
 // Assert ( not Terminated, 'SetStage: thread was terminated');

 if (self = nil) or (Value = FStage) or ( ThreadID <> GetCurrentThreadID ) then exit;

 if IsCritical and (ptExec <> nil) and ( WarnIgnore <= 0 ) then
  begin
   e := ptExec.Elapsed (29);
   r := PerfWarnRatio;

   if ( ( e > WarningTimeout * r * 1 ) and ( log_verbose >= 5 ) ) or
      ( ( e > WarningTimeout * r * 9 ) and ( log_verbose >= 3 ) )  then
      ODS ( CFormat ( '[~T/~B].~C0E #PERF(%s): Stage changed from %.5f to %.5f after %.3f ms (CPU = %.3f), wp_exec_cnt = $%x, thread ~I ~C07',
                        '~C0E', [ ThreadName, FStage, Value, e, ptExec.CPUElapsed (29), wp_exec_cnt ] ) );

  end;
 Dec ( FWarnIgnore );


 FStage := Value;

 if ptExec <> nil then
    ptExec.StartOne (29);
end;

procedure TWorkerThread.SetThreadName(const Value: String);
begin
  try
   FThreadName := Value;
   madExcept.NameThread( ThreadId, Value );
   NameThreadForDebugging ( AnsiString (value), ThreadId );

   if Assigned ( rq_list )  then
                 rq_list.SetSyncObjectName (FThreadName + '::rq_list.sc_share');
 except
  on E: Exception do
    OnExceptLog (ClassName + '.SetThreadName', E);
 end;

end;

procedure TWorkerThread.SetWorkPaused(const Value: Boolean);
begin
 if Value and (not WorkPaused) and ( ThreadId <> GetCurrentThreadId ) and ( not Suspended ) then
   begin
    AddRequest ('PAUSE_WORK', nil);
    WaitRequests (3350);
   end
 else
    FWorkPaused := Value;
end;

function TWorkerThread.CanHandleRequests: Boolean;
begin
 result := not Terminated;
end;

function TWorkerThread.CPUTime: Integer;
var
   ct, et, kt, ut: FILETIME;
   kti: Int64 absolute kt;
   uti: Int64 absolute ut;
begin
 //
 GetThreadTimes (Handle, ct, et, kt, ut);
 result := Round ( 1.0 * (kti + uti) / 10000 );
end;

{ TWorkThreadPool }

function TWorkThreadPool.AddWorker(const sName: String): TWorkerThread;

begin
 result := FWorkerCreator ( sName ) ;
 result.FPool := self;
 DbgWaitForSO ( result.evt_start.Handle, 1000, 'ADW' );
 Add (result);
 Inc (FReadyCount);
end;

constructor TWorkThreadPool.Create(wCreator: TWorkerCreator);
begin
 inherited Create (TRUE);
 sc_share := TCritSection.Create('TWorkThreadPool.sc_share');
 StrictPool := TRUE;
 MaxWaitWorkers := 5900;
 MaxWorkers := 32;

 if not Assigned (wCreator) then
    FWorkerCreator := CreateWorker
 else
    FWorkerCreator := wCreator;
 FCurrentWorker := 0;
 // initial count of workes = 1
 AddWorker ('+W#' + IntToStr(Count));
end;

destructor TWorkThreadPool.Destroy;

begin
 ODS('[~T]. #DBG: Destroying thread pool named ~C0A' + Name + '~C07');
 sc_share.Lock('Destroy pool');
 StopAll;
 sc_share.Unlock;
 sc_share.Free;
 inherited;
end; // Destroy

function TWorkThreadPool.DispatchRequest(const rqs: String;
  rqobj: TObject): TWorkerThread;

begin
 result := AllocateWorker;
 result.AddRequest(rqs, rqobj);
end;

function TWorkThreadPool.AllocateWorker(const sName: String): TWorkerThread;
var
   chk_cnt: Integer;
   pt: TProfileTimer;
begin
 sc_share.Lock('.FindUnused');
 pt := TProfileTimer.Create;
 try
   chk_cnt := 0;
   result := nil;
   // max_cpu := 32;

   if (FCurrentWorker >= Count) then FCurrentWorker := 0;

   // if StrictPool then max_cpu := GetCPUCount;


   // =========== looped wait unused
   if Count > 0 then
      Repeat

        if (pt.Elapsed > 2 * MaxWaitWorkers) then
          begin
           ODS('[~T]. #WARN: AllocateWorker~C0A ' + sName + '~C07  timeout = ' + ftow (pt.Elapsed, '%.0f ms'));
           pt.StartOne(1);
          end;


        result := Items[FCurrentWorker];
        Inc (FCurrentWorker);
        Inc (chk_cnt);

        if (FCurrentWorker >= Count) then FCurrentWorker := 0;

        if ( not result.Busy ) then break;

        if chk_cnt >= Count then
         begin
          {if (Count >= max_cpu + 3) then
             begin
              Sleep(20);
              chk_cnt := 0;
             end
           else
             break;}

          chk_cnt := 0;

          result.WaitRequests (50, FALSE);
         end;

         result := nil; // next loop
         // долждаться таймаута, если пул не заполнен
      Until (pt.Elapsed > MaxWaitWorkers) and ( Count < MaxWorkers );

   if (result = nil) or result.Busy then
       result := AddWorker(sName + '#' + IntToStr(Count));
   Dec (FReadyCount);    
   result.SetBusy;
   result.FProfTimer.Start;
   result.FAllocES := result.CPUTime;
 finally
  sc_share.Unlock;
  pt.Free;
 end;
end;

function TWorkThreadPool.BusyCount: Integer;
var n: Integer;
begin
 result := 0;
 for n := Count - 1 downto 0 do
  if Items[n].Busy then Inc(result);
end; // BusyCount

// FindUnused

function TWorkThreadPool.GetWorker(nIndex: Integer): TWorkerThread;
begin
 result := TWorkerThread ( TObjectList(self).Items [nIndex] );
end;

function TWorkThreadPool.CreateWorker(const sName: String): TWorkerThread;
begin
 result := TWorkerThread.Create (FALSE, sName);
end;

procedure TWorkThreadPool.StopAll;
var
   n: Integer;
   tl: array of DWORD;

begin
 if Count = 0 then exit;
 SetLength (tl, Count);
 try
   for n := 0 to Count - 1 do
     begin
      tl [n] := Items[n].Handle;
      Items [n].StopThread;
     end;


  WaitForMultipleObjects ( Count,   @tl, TRUE, 2000);

  Clear;
 except
  on E: Exception do
     OnExceptLog ('TWorkThreadPool.StopAll', E);
 end; // try-except-end
end;  // StopAll



var
   gHangThread: TWorkerThread = nil;

procedure _ThreadHangTrap;
begin

 if Assigned (gHangThread) then
   begin
    gHangThread.HangCount := 0;
    raise EThreadHangException.Create ('Достигнут таймаут ожидания потока ' + gHangThread.ThreadName )
   end
 else
    raise EThreadHangException.Create ('Достигнут таймаут ожидания потока.');

 __int3;
 PInteger(nil)^ := 123;

end;

{ TThreadWatchdog }

constructor TThreadWatchdog.Create;
begin
 inherited Create ( FALSE, 'THREAD_WATCHDOG' );
 FMainThread.ThreadID := GetCurrentThreadID;
 FMainThread.Handle := OpenThread ( THREAD_ALL_ACCESS, FALSE, FMainThread.ThreadID );
 FMainThread.AliveTimer := ProfTimer;
 FMainThread.MaxHangCount := 3;
 FMainThread.HangTimeout := 37000;
end;

destructor TThreadWatchdog.Destroy;
begin
 CloseHandle ( FMainThread.Handle );
 FCSIList.Free;
 inherited;
end;

procedure TThreadWatchdog.MTNotifyAlive;
begin
 FMainThread.AliveTimer.StartPFC (35);
 FMainThread.HangCount := 0;
end;

procedure TThreadWatchdog.OnTimer(idTimer: Integer);
var
    n: Integer;
   wt: TWorkerThread;
   ct: TContext;

{$IFDEF PERF_CHECK}
  csi: TCSInvader;
{$ENDIF}

begin
 inherited OnTimer (idTimer);
 if idTimer <> 1500 then exit;

 NotifyAlive (); // OnTimer WD

 if Assigned (g_timer) then g_timer.TestSync;

 if ( ProfTimer.Elapsed (35) > FMainThread.HangTimeout ) and ( FMainThread.Handle <> 0 ) then
   begin
    ProfTimer.StartOne (35);

    PrintError ( Format ('Превышен таймаут зависания (%d мс) для главной нити %d ', [FMainThread.HangTimeout, FMainThread.ThreadID ] ) );
    Inc ( FMainThread.HangCount );
    if FMainThread.HangCount = FMainThread.MaxHangCount then
       ProcessThreadHang ( nil, FMainThread.Handle );
   end;



 for n := 0 to FThreadList.Count - 1 do
    begin
     wt := TWorkerThread ( FThreadList [n] );
     if wt = nil then continue;

     if (wt.Terminated) or ( wt.AliveTimer.Elapsed (35) < wt.HangTimeout )  then continue;



     PrintError ( Format ('Превышен таймаут зависания (%d мс) для нити %s, hangCount = %d/%d, alive_events = %d, CurrentRqs = %s, Stage = %.5f ',
                           [wt.HangTimeout, wt.ThreadName, wt.HangCount, wt.MaxHangCount, wt.alv_events, wt.CurrentRqs, wt.Stage]  ) );

     FillChar (ct, sizeof (ct), 0);
     ct.ContextFlags := CONTEXT_FULL or CONTEXT_i486;


     if GetThreadContext ( wt.Handle, ct ) then
       begin
        ODS ( DumpRegisters (ct) );
       end
     else
       PrintError ('Невозможно получить контекст нити: ' + Err2Str (GetLastError));



     wt.HangCount := wt.HangCount  + 1;
     if Assigned (wt.OnThreadHang) then
                  wt.OnThreadHang ();

     if wt.HangCount = wt.MaxHangCount then
       ProcessThreadHang ( wt, wt.Handle );


      wt.NotifyAlive (); // cheat

     if wt.HangCount > wt.MaxHangCount then
      begin
       PrintError('Максимальное количество таймаутов превышено. Процесс будет завершен.');
       CBeep (2000, 2000);
       TerminateProcess (GetCurrentProcess, $10200);
      end;

    end; // for




 {$IFDEF PERF_CHECK}
 tms := CurrentTime.Time;
 for n := 0 to FCSIList.Count - 1 do
  begin
   csi := TCSInvader ( FCSIList [n] );

   s := String ( GetThreadName ( csi._id ) );

   dms := Integer (tms) - Integer (csi._wt);

   // 0:00:15 > 0:00:10 or 23:59:59 - 0:00:05 > 23h
   if ( dms > 0 ) or ( csi._wt - tms > 23 * 3600 * 1000 ) then
        ODS( CFormat('[~T].~C0C #WARN:~C07 Critical section %s lock timeout = %d ms.~n    Pretender = %s~n    Locker = %s', '~C07', [csi._cs.Name, dms, s, csi._cs.LastLocker]) );

  end;
 {$ENDIF}
end;

procedure TThreadWatchdog.ProcessInit;
begin
 inherited;
 FThreadList := TObjectList.Create (FALSE);
 FCSIList := TObjectList.Create (TRUE);
 Garbage.Add (FThreadList);

 SetTimer (hMsgWnd, 1500, 1500, nil);

 FastSync.cs_watchdog := LockWatchdog;

 wait_time := 1;
end;

function TThreadWatchdog.ProcessRequest(const rqs: String; rqobj: TObject): Integer;
begin
 result := inherited ProcessRequest (rqs, rqobj);

 if rqs = 'REG_THREAD' then
    FThreadList.Add (rqobj);

 if rqs = 'UNREG_THREAD' then
    FThreadList.Remove (rqobj);

 if rqs = 'ADD_CSI' then
   begin
    FCSIList.Add (rqobj);
    Assert (FCSIList.Count < 1000, ClassName + '.FCSIList overrun');
   end;

 if rqs = 'RMV_CSI' then
    FCSIList.Remove (rqobj);
end;

procedure TThreadWatchdog.ProcessThreadHang (wt: TWorkerThread; th: THandle);
var
   ct: TContext;
begin
 PrintError ('Вызов исключения в зависшей нитке...');

 gHangThread := wt;
 ct.ContextFlags := CONTEXT_CONTROL;
 GetThreadContext ( th, ct );

 {$IFDEF CPUX64}
 ct.Rip := NativeUInt ( @_ThreadHangTrap );
 {$ELSE}
 ct.Eip := NativeUInt ( @_ThreadHangTrap );
 {$ENDIF CPUX64}

 SuspendThread ( th );
 SetThreadContext ( th, ct );
 ResumeThread ( th );
// OnExceptLog ( 'ThreadHang', nil, TRUE, @ct );
end;

procedure TThreadWatchdog.ProcessThreadStop;
begin
 FastSync.cs_watchdog := nil;
 inherited;
end;

procedure TThreadWatchdog.RegThread(wt: TWorkerThread; bRegister: Boolean);
begin
 if wt = self then exit;
 if bRegister then
    AddRequest ('REG_THREAD', wt)
 else
    AddRequest ('UNREG_THREAD', wt);
end;


procedure TThreadWatchdog.WorkProc;
var
  wt: TWorkerThread;
   n: Integer;
begin
 inherited;
 //
 if gTrapThread = 0 then exit;

  for n := 0 to FThreadList.Count - 1 do
    begin
     wt := TWorkerThread ( FThreadList [n] );
     if ( wt = nil ) or ( wt.Terminated ) or ( wt.ThreadID <> gTrapThread ) then continue;

     FTracking := ftow ( wt.Stage, '%f<<' ) + FTracking;
     if Length ( FTracking ) > 1000 then
        FTracking := Copy ( FTracking, 1, 1000 );

     exit;
    end;




end;

function OnModuleRqs (md: TModuleDescriptor; rqs, flags: DWORD): Boolean;
var
   lib: HMODULE;
    wt: TWorkerThread;


begin
 result := FALSE;
 case rqs of
      MRQ_INITIALIZE:  // ==================================================================================================== //
          begin
           if gThreadWatchDog <> nil then exit;

           RegMsgWndClass;

           lib := GetModuleHandle('kernel32.dll');
           if lib > 0 then
              SetThreadErrorMode := GetProcAddress ( lib, 'SetThreadErrorMode' );

           gThreadWatchDog := TThreadWatchDog.Create;
           // gThreadWatchDog.WaitStart;
           result := TRUE;
          end;
      MRQ_FINALIZE: // ==================================================================================================== //
          begin
           wt := gThreadWatchDog;
           if wt = nil then exit;
           gThreadWatchDog := nil;


           wt.ThreadStatus := 'SHUTDOWN';
           wt.FreeOnTerminate := FALSE;            // $037D976C
           wt.StopThread;
           wt.WaitStop (1500); // Possible stopped from DLL
           wt.Free;

           result := TRUE;
          end;

 end; // case
end; // OnModuleRqs


{ TRequestSlot }

procedure TRequestSlot.SetRqs(const s: String);
begin
 SetStrWZ ( rqs, s, High (rqs) );
end;

function TRequestSlot.TestSwitchState(new_st, old_st: Integer): Boolean;
begin
 result := InterlockedCompareExchange (ownerf, new_st, old_st) = old_st;

 if result then
    ownert := IfV (new_st = SLOT_UNUSED, 0, GetCurrentThreadId);

end;

{ TAsyncQueue }

procedure TAsyncQueue.AdjustPointers(prs: PRequestSlot);
var
   rcv_base: Pointer;
       diff: NativeInt;
          n: Integer;
begin
 if ( not FData.is_shared ) or ( prs.cl_data = nil ) or ( FData.buffers_count = 0 ) then exit;
 rcv_base := @FData.buffers_list;
 diff := NativeInt (rcv_base) - NativeInt ( prs.cl_data );

 if diff = 0 then exit;

 if NativeInt(prs.obj) > $F0000 then
    Inc ( NativeInt(prs.obj), diff );

 for n := 0 to High (prs.p_params) do
  if prs.p_params [n] <> nil then
     Inc ( NativeInt(prs.p_params[n]), diff );
end;

function TAsyncQueue.AllocBuff(cb: Integer): Pointer;
begin
 result := AllocMem ( cb );
end;

function TAsyncQueue.AllocSlot(wait_msec: Integer): PRequestSlot;
var
   i, n: Integer;
   tresh: UINT64;
   ps: PRequestSlot;
   pe: PAQEvents;
begin
 result := nil;
 pe := GetEvents;
 // enum all slots

 tresh := GetTickCount + DWORD (wait_msec);

 i := FData.last_allocated;

 with FData^ do

 repeat
   for n := 0 to rq_mask do
    begin
     i := (i + 1) and rq_mask;

     ps := @rq_slots[i];

     if not ps.TestSwitchState (SLOT_WRITE, SLOT_UNUSED) then continue;
     ps.index := InterlockedIncrement (last_allocated) - 1; // ordered by index
     ps.cl_data := @FData.buffers_list;

     result := ps;
     break;
    end; // for

  if result = nil then
    begin
     Inc ( slot_wait_cnt );
     DbgWaitForSO ( pe.FreeEvent, Min(1000, tresh - GetTickCount), 'ALS' );
     InterlockedIncrement (cnt_locks);
     if IsKeyPressed (VK_MENU) then
        ODS('[~T]. #PERF: TAsyncQueue.AllocSlot waits for slot release');

    end;

 until (result <> nil) or ( GetTickCount > tresh ); // TODO: timeout 4GiB problem
end; // AllocSlot

constructor TAsyncQueue.Create;
var
   cb: Integer;
begin
 if FData = nil then
   begin
    cb := sizeof(TAQData) - sizeof ( TBuffersList16x4K );
    FData := AllocMem ( cb );
    FData.buffers_count := 0;
   end;


 Init; // 10 = 1024 slots

 with FData^ do
  begin
   rq_mask := High(rq_slots);
   SetStrWZ ( sz_name, AName, 31 );
   last_read := -1;
  end;

 InitEvents;
end;

destructor TAsyncQueue.Destroy;
var
   pe: PAQEvents;
begin
 if Assigned(FData) then
  begin
   pe := GetEvents;
   CloseHandle (pe.ReadEvent);
   CloseHandle (pe.FreeEvent);
   pe.ReadEvent := 0;
   pe.FreeEvent := 0;
   FreeMem (FData);
   FData := nil;
  end;
 inherited;
end;

procedure TAsyncQueue.Flush(prs: PRequestSlot);
begin
 InterlockedExchange ( prs.ownerf, SLOT_FLUSHED);
 InterlockedIncrement ( FData.rq_flushed );
 if push_event <> 0 then
    SetEvent (push_event);
end;

function TAsyncQueue.Flushed: Integer;
begin
 result := FData.rq_flushed;
end;

function TAsyncQueue.GetEvents: PAQEvents;
var
   n: Integer;
   s: String;
begin
 result := nil;
 s := 'CurrentPID = ' + IntToStr(gPID) + '. Avaible:';

 with FData^ do
   for n := 0 to High (aq_events) do
      if aq_events [n].OwnerPID = gPID then
        begin
         result := @aq_events [n];
         exit;
        end
      else
        s := s + ' ' + IntToStr (aq_events [n].OwnerPID);

 Assert ( FALSE, ClassName + '.GetEvents failed. ' + s );
end;

function TAsyncQueue.GetName: String;
begin
 result := FData.sz_name;
end;

function TAsyncQueue.PushDataRqs(const rqs: String; const buff; cbSize: Integer; dwTimeout: DWORD): Pointer;
var
   ps: PRequestSlot;
begin
 result := nil;
 ps := AllocSlot (dwTimeout);
 if ps = nil then exit; // timeout fatal
 result := AllocBuff ( cbSize );
 if result = nil then exit;
 Move ( buff, result^, cbSize );

 ps.allocated [0] := cbSize;
 ps.p_params [0] := result;
 ps.SetRqs(rqs);
 Flush(ps);
end;

function TAsyncQueue.PushSimpleRqs (const rqs: String; rqobj: TObject; dwTimeout: DWORD): Boolean;
var
   ps: PRequestSlot;
begin
 ps := AllocSlot (dwTimeout);
 result := (ps <> nil);
 if ps = nil then exit; // timeout fatal

 ps.SetRqs(rqs);
 ps.obj := rqobj;

 Flush(ps);
end;

function TAsyncQueue.Read: PRequestSlot;
var
   ps: PRequestSlot;
   n, i, ii: Integer;

begin
 result := nil;
 with FData^ do
  begin
   i := (last_read + 1);
   ii := i;

   // поиск слота по порядку чтения
   for n := 0 to rq_mask do
    begin
     ps := @rq_slots [ii and rq_mask];
     ii := ii + 1;
     if  (ps.index <> i) or ( not ps.TestSwitchState (SLOT_READ, SLOT_FLUSHED) ) then continue;

     last_read := ps.index;
     result := ps;
     AdjustPointers (result);

     exit;
   end;

  end;
end;


function TAsyncQueue.ReadRelease: TRequestSlot;
var
   prs: PRequestSlot;
begin
 FillChar(result, sizeof(result), 0);
 prs := Read();
 if prs <> nil then
   begin
    result := prs^;
    Release (prs);
   end;
end;

procedure TAsyncQueue.Release(prs: PRequestSlot);
var
   pe: PAQEvents;
begin
 prs.ownert := 0;
 InterlockedExchange (prs.ownerf, SLOT_UNUSED);
 InterlockedDecrement (FData.rq_flushed);
 pe := GetEvents;
 if pe <> nil then
    SetEvent ( pe.FreeEvent )
 else
    PrintError ( ClassName +  '.GetEvents returned nil' );
end;

procedure TAsyncQueue.ReleaseData (p: Pointer);
begin
  FreeMem ( p );
end;

function TAsyncQueue.WaitForPush(dwTimeout: DWORD; bAlertable: Boolean): DWORD;
begin
 result := WaitForSingleObjectEx (PushEvent, dwTimeout, bAlertable);
end;

procedure TAsyncQueue.Init;
var
   n: Integer;
begin
 with FData^ do
  for n := 0 to High (rq_slots) - 1 do
      rq_slots [n].ownerf := SLOT_UNUSED;
end;

procedure TAsyncQueue.InitEvents;
var
   pe: PAQEvents;
    s: String;
    n: Integer;
begin
 pe := nil;
 s := '';
 Assert ( gPID > 0 );


 with FData^ do
  for n := 0 to High (aq_events) do
   if aq_events [n].OwnerPID = 0 then
     begin
      pe := @aq_events[n];
      break;
     end
   else s := s + IntToStr (aq_events [n].OwnerPID) + ' ';


 Assert ( Assigned(pe), 'Cannot allocate events slot. Owners: ' + s );
 pe.OwnerPID := gPID;
 // WARN: все без исключения события создаются с автосбросом!
 if FData.is_shared then
  begin
   // события открываются глобальные для сессии, просто разные дескрипторы для разных процессоров (при шаринге)
   pe.ReadEvent := CreateEvent (nil, FALSE, FALSE, PChar(GetName + '_RE') );
   pe.FreeEvent := CreateEvent (nil, FALSE, FALSE, PChar(GetName + '_FE') );
   pe.PushEvent := CreateEvent (nil, FALSE, FALSE, PChar(GetName + '_PE') );
  end
 else
  begin
   pe.ReadEvent := CreateEvent ( nil, FALSE, FALSE, nil );
   pe.FreeEvent := CreateEvent ( nil, FALSE, FALSE, nil );
   pe.PushEvent := CreateEvent ( nil, FALSE, FALSE, nil );
  end;
  push_event := pe.PushEvent;
end;

function TAsyncQueue.IsOwner: Boolean;
begin
 result := Assigned (FData) and (FData.creator_pid = GetCurrentProcessId);
end;

{ TIPCAsyncQueue }

function TIPCAsyncQueue.AllocBuff(cb: Integer): Pointer;
var
   n: Integer;
   u: Integer;
begin
 result := nil;
 with FData^ do
  begin

   if ( cb < 0 ) or ( cb > sizeof ( buffers_list[0].data ) ) then exit;
     for n := 0 to buffers_count - 1 do
      begin
       u := InterlockedCompareExchange ( buffers_list[n].used, cb, 0 );
       if u > 0 then continue;
       result := @buffers_list [n].data;
       FillChar ( result^, cb, 0 );
       exit;
      end;

  end;
end;

constructor TIPCAsyncQueue.Create;
begin
 FData := MapViewOfFile ( NewAQMapping, FILE_MAP_ALL_ACCESS, 0, 0, sizeof (TAQData) );
 Assert ( Assigned(FData), 'Mapping failed ' + err2str );

 FData.buffers_count := High (FData.buffers_list) + 1;
 FData.is_shared := TRUE;
 FData.creator_pid := gPID;
 inherited Create (AName);
end;

destructor TIPCAsyncQueue.Destroy;
var
  pe: PAQEvents;
   h: THandle;

begin
 if IsReceiverSide then
    FReceiverClosed := TRUE;

 pe := GetEvents;
 h := pe.hMapping;
 FillChar (pe^, sizeof(TAQEvents), 0); // slot was released
 UnmapViewOfFile (FData);

 CloseHandle (h); //

 FData := nil;
 inherited;
end;


function TIPCAsyncQueue.IsReceiverSide: Boolean;
begin
 result := ( gPID = FData.creator_pid );
end;

class function TIPCAsyncQueue.MakeInstance(const AName: String): TIPCAsyncQueue;
var
   map: TFileMapping;

begin

 map.Init ( sizeof (TAQData) );

 if map.Open (AName) then
  begin
   NewAQMapping := map.hMapping;
   result := TIPCAsyncQueue.Open;
  end
 else
  begin
   wprintf ('[~T]. #DBG: Creating TIPCAsyncQueue %s ', [AName]);
   map.Create (AName, TRUE);
   NewAQMapping := map.hMapping;
   result := TIPCAsyncQueue.Create (AName);
  end;


 NewAQMapping := 0; // не доставайся ты никому!!!
end;

constructor TIPCAsyncQueue.Open;
begin
 FData := MapViewOfFile ( NewAQMapping, FILE_MAP_ALL_ACCESS, 0, 0, sizeof(TAQData) );
 FData.is_shared := TRUE;
 InitEvents;
end;

procedure TIPCAsyncQueue.ReleaseData (p: Pointer);
var
   n: Integer;
begin
 with FData^ do
  for n := 0 to buffers_count - 1 do
   if p = @buffers_list [n].data then
      begin
       InterlockedExchange ( buffers_list [n].used, 0 );
       break;
      end;
end;

initialization
 gPID := GetCurrentProcessID;
 CoInitFlags := COINIT_MULTITHREADED;
 RegModule ('WThreads', 'misc', OnModuleRqs);
 InitializeModule ('WThreads');
finalization
 FinalizeModule ('WThreads');
end.


