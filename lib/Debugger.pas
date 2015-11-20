unit Debugger;

interface
uses Windows, SysUtils, Classes, Misc, ContNrs, PSApi, TlHelp32, PsUtils;

const
    THREAD_ALL_ACCESS   = $1F03FF;
    THREAD_GET_CONTEXT  = $0008; // Required to read the context of a thread using GetThreadContext.
    THREAD_IMPERSONATE  = $0100; // Required to use a thread's security information directly without calling it by using a communication mechanism that provides impersonation services.
    THREAD_QUERY_INFORMATION = $0040; // Required to read certain information from the thread object, such as the exit code (see GetExitCodeThread).


type
   TThreadRecord = record
    ID: DWORD;
    h: THandle;
   end;


   TStackDumpFunc = function (hProcess: THandle; const ctx: TContext; E: Windows.PExceptionRecord ): String;


   TDebuggedProcess = class
   private
    he_lock: Boolean;
    FStackDumpFunc: TStackDumpFunc;
    FAttached: Boolean;
    FThreadID: DWORD;
    cThread: THandle;

    procedure ProcessDebugEvent (de: TDebugEvent);
    function GetHandle: THandle;

   protected
    FHandle: THandle;
    FThreads: array of TThreadRecord;
    FProcessID: DWORD;
    dbgreg: TContext;


    procedure           Cleanup;
    function            GetThreadHandle(dwThreadID: DWORD): THandle;
   public

    AV_count: Integer;  // exceptions class STATUS_ACCESS_VIOLATION

    { props }

    property            Attached: Boolean read FAttached;
    property            Handle: THandle read GetHandle;
    property            ProcessID: DWORD read FProcessID;
    property            ThreadID: DWORD read FThreadID;
    property            StackDumpFunc: TStackDumpFunc read FStackDumpFunc write FStackDumpFunc;



    { C & D }
    constructor         Create;
    destructor          Destroy; override;

    { methods }
    function            Attach (pid: DWORD): Boolean;
    procedure           Detach;
    procedure           HandleDebugEvents(msec: DWORD);
   end; // TDebuggedProcess


function DebugActiveProcessStop (pid: DWORD): Boolean; stdcall;
function OpenThread(dwDesiredAccess: DWORD; bInheritHandle: Boolean; dwThreadID: DWORD): THandle; stdcall;

implementation
uses BaseLib;

function DebugActiveProcessStop (pid: DWORD): Boolean; stdcall; external 'kernel32.dll';
function OpenThread(dwDesiredAccess: DWORD; bInheritHandle: Boolean; dwThreadID: DWORD): THandle; stdcall; external 'kernel32.dll';

{ TDebuggedProcess }

procedure TDebuggedProcess.ProcessDebugEvent (de: TDebugEvent);
var
    event : DWORD;
    ctx, dctx: TContext;
    s: string;
 // p: pointer;
    r: DWORD;
    pc: array [0..255] of char;
    hThread: THandle;
    is_trap: Boolean;
    dwContinueStatus: DWORD;

begin
 event := de.dwDebugEventCode;
 dwContinueStatus := DBG_CONTINUE;
 is_trap := FALSE;

 if Event = CREATE_PROCESS_DEBUG_EVENT then
     begin
      FHandle := de.CreateProcessInfo.hProcess;
      // r := Length ( FThreads );
      FThreadID := de.dwThreadID;
      if GetThreadHandle (ThreadID) = 0 then
       asm
        nop
       end;

      cThread := de.CreateProcessInfo.hThread;
      {hThread := ;
      iThread := de.dwThreadId;
      hPrcsModule := dword (de.CreateProcessInfo.lpBaseOfImage);
      inc (ThrdCount);
      cThread := hThread;
      ThIndex := 1;
      ThrdArray [ThrdCount].h := hThread;
      ThrdArray [ThrdCount].id := iThread;
      UpdateBtns;}
     end;
   case event of
    EXCEPTION_DEBUG_EVENT :
     begin
      s := '~C0C EDE:~C0F ';
      // p := de.Exception.ExceptionRecord.ExceptionAddress;

      if de.Exception.dwFirstChance = 0 then
         dwContinueStatus := DBG_EXCEPTION_NOT_HANDLED;

      // получаем доступ к процессу и потоку, контекст исполнения
      if Handle = 0 then
         PrintError ( Format ('OpenProcess fails for ProcessID = $%x :', [ProcessID] ) + Err2Str (GetLastError) );

      FThreadID := de.dwThreadID;
      hThread := GetThreadHandle ( FThreadID );

      FillChar (ctx, sizeof (ctx), 0);
      // SuspendThread (hThread);
      ctx.ContextFlags := CONTEXT_INTEGER or CONTEXT_CONTROL;
      GetThreadContext (hThread, ctx);
      SetLastError (0);
      if cThread <> 0 then
       begin
        dctx.ContextFlags := CONTEXT_DEBUG_REGISTERS;
        GetThreadContext (cThread, dctx);
       end;

      // ResumeThread (hThread);

      // ===========================================================
        with de.Exception.ExceptionRecord do
        case ExceptionCode of

         EXCEPTION_BREAKPOINT:
           with ctx do
            begin
             s := s + Format('~C0E BREAKPOINT at~C0D $%p', [ExceptionAddress]) + #13#10#9;
             is_trap := TRUE;
            end;
         EXCEPTION_ACCESS_VIOLATION :
           with ctx do
            begin
             s := s + Format('~C0C ACCESS_VOILATION: at~C0D $%p~C0E', [ExceptionAddress]);
             if NumberParameters >= 2 then
              begin
               if ExceptionInformation [0] = 0  then
                 s := s + ' read from address' else
                 s := s + ' write to address';
               s := s + ' ~C0D $' + IntToHex (ExceptionInformation [1], 7) + '~C07'#13#10#9;
              end;
             dwContinueStatus := DBG_EXCEPTION_NOT_HANDLED;
             Inc (AV_count);
             if AV_count > 25 then
                begin
                 PrintError ('Отлаживаемый процесс завершен из-за неустранимой ошибки.');
                 TerminateProcess ( Handle, STATUS_ACCESS_VIOLATION );
                 CBeep (300, 1000); CBeep (600, 1000); CBeep (900, 1000);
                end;
             // is_trap := TRUE;
             // MForm.cb_hev.Checked := false;
            end;

         EXCEPTION_ARRAY_BOUNDS_EXCEEDED : s := s + ' ARRAY_BOUNDS_EXCEEDED';
         EXCEPTION_SINGLE_STEP :
            begin
             s := s + ' SINGLE_STEP ';
             {
             c.ContextFlags := context_debug_registers;
             GetThreadContext (cThread, c);
             ShowContext (ThIndex);
             if MForm.cb_traceLog.checked then
                AddMaxInfo (cThread);
             bpn := 0;
             if c.dr6 and 1 <> 0 then bpn := 1;
             if c.dr6 and 2 <> 0 then bpn := 2;
             if c.dr6 and 4 <> 0 then bpn := 3;
             if c.dr6 and 8 <> 0 then bpn := 4;
             if bpn in [1..4] then
              begin
               v := 0;
               ReadProcessMemory (csm.SVars.alias, ptr (BreakPoints [bpn].addr),
                                @v, BreakPoints [bpn].size, R);
               AddPtr (p, v, BreakPoints [bpn].LastVal, bpn);
               if p2 <> nil then AddPtr (p2, v, BreakPoints [bpn].LastVal, bpn);
               BreakPoints [bpn].lastVal := v;
              end;
             }
           end;
         else
           s := s + Format(' Unknown ExceptionCode = $%X', [ExceptionCode]);
        end; // case ExceptionCode

       with ctx do
         s := s + CFormat('~C0E Thread context: ~C0B '#13#10#9 +
                        ' EAX = $%.8X EBX = $%.8X ECX = $%.8X EDX = $%.8X EDI = $%.8X ESI = $%.8X EBP = $%.8X ESP = $%.8X EIP = $%.8X ', '~C0B',
                        [eax, ebx, ecx, edx, edi, esi, ebp, esp, eip]) + #13#10#9;
       with dctx do
         s := s + CFormat(' DR0 = $%.8X DR1 = $%.8X DR2 = $%.8X DR3 = $%.8X DR6 = $%.8X DR7 = $%.8X', '~C0B',
                        [DR0, DR1, DR2, DR3, DR6, DR7]) + #13#10#9;

       if Assigned (StackDumpFunc) then
           s := s + StackDumpFunc ( Handle, ctx, nil );

     end;
    CREATE_PROCESS_DEBUG_EVENT : s := ' Запущен процесс';
    CREATE_THREAD_DEBUG_EVENT :
        begin
         s := Format ('Создан поток : $%x ', [de.CreateThread.hThread]);
         {inc (ThrdCount);
         ThrdArray [ThrdCount].h := de.CreateThread.hThread;
         ThrdArray [ThrdCount].id := de.dwThreadId;
         if SpyWait then
            hSpyThrd := de.CreateThread.hThread;
         SpyWait := false;
         ss := format ('$%x', [ThrdArray [ThrdCount].h]);
         s := s + ' Handle=' + ss + ' ID=$' + dword2hex (de.dwThreadId);
         ss := format ('$%p', [de.CreateThread.lpStartAddress]);
         if ss <> '$00000000' then
            s := s + ' START=' + ss;}
        end;
      LOAD_DLL_DEBUG_EVENT:
       begin
        s := 'Загружена DLL';
        FillChar (pc, 256, 0);
        r := dword (de.LoadDll.lpBaseOfDll);
        s := s + ' HINST:$' + dword2hex (r);
        GetModuleFileName (r, pc, 256);
        s := s + ' ' + pc;
        //ListModules (tvMemBlocks);
       end;
    UNLOAD_DLL_DEBUG_EVENT:
       begin
        FillChar (pc, 256, 0);
        r := dword (de.UnLoadDll.lpBaseOfDll);
        GetModuleFileName (r, pc, 256);
        s := 'Выгружена DLL HINST:$' + dword2hex (r)
                + ' ' + pc;
       end;
    EXIT_THREAD_DEBUG_EVENT :
        begin
         s := ' Поток завершен, ThreadID= $' + dword2hex (de.dwThreadId);
        end;
    EXIT_PROCESS_DEBUG_EVENT:
        begin
         s := ' Процесс завершен';
         FHandle := 0;
        end;
   OUTPUT_DEBUG_STRING_EVENT:
        begin
         {s := ' Сообщение отладки';
         p := de.DebugString.lpDebugStringData;
         FillChar (pc, 256, 0);
         ReadProcessMemory (csm.SVars.alias, p, @pc, Min (256, de.DebugString.nDebugStringLength), R);
         ss := pc;
         s := s + ': ' + ss;
         }
        end;
    else s := format ('$%x', [de.dwDebugEventCode])
   end;


  ODS  ('~C0B[~T]. #DBG_EVENT:~C0F ' + format ('$%X ', [de.dwDebugEventCode]) +
                 'TID: $~C0D' + format ('$%X ', [de.dwThreadID])+'~C07' +  s + '~C07');

  if is_trap then
     CBeep (1000, 100);
   // Режим остановки

  {  DisableBPS;
     if event = trapEvent then
      begin
       WaitEvent := false;
       EventCode := de.dwDebugEventCode;
      end;

     if (event = EXCEPTION_DEBUG_EVENT) and
        (de.Exception.ExceptionRecord.ExceptionCode =
         EXCEPTION_BREAKPOINT) then Sleep (10);
     EnableBPS;
    end;

 }


  ContinueDebugEvent (de.dwProcessId, de.dwThreadId, dwContinueStatus);
end;

function TDebuggedProcess.Attach(pid: DWORD): Boolean;
begin
 if Attached then Detach;

 result := DebugActiveProcess ( pid );
 if result then
   begin
    FAttached := TRUE;
    FProcessID := pid;
    // TODO: open handle
   end
 else
    PrintError ('Cannot debug active process: ' + Err2Str (GetLastError))
end;

procedure TDebuggedProcess.Cleanup;
var
   n: Integer;
begin
 if Attached then Detach;

 for n := 0 to Length (FThreads) - 1 do
     CloseHandle ( FThreads [n].h );

 SetLength (FThreads, 0);
 if Handle <> 0 then CloseHandle ( Handle );
end;

constructor TDebuggedProcess.Create;
begin
 // TODO: Init here
end;

destructor TDebuggedProcess.Destroy;
begin
 Cleanup;
 inherited;
end;

procedure TDebuggedProcess.Detach;
begin
 if ProcessID = 0 then exit;
 DebugActiveProcessStop (ProcessID);
 Cleanup;
 FProcessID := 0;
end;

function TDebuggedProcess.GetHandle: THandle;
begin
 if Attached and (FHandle = 0) then
    FHandle := OpenProcess ( PROCESS_ALL_ACCESS, TRUE, ProcessID );

 result := FHandle;
end;

function TDebuggedProcess.GetThreadHandle(dwThreadID: DWORD): THandle;
var n: Integer;
begin
 for n := 0 to Length (FThreads) - 1  do
  if FThreads[n].ID = dwThreadID then
   begin
    result := FThreads[n].h;
    exit;
   end;


 result := OpenThread ( THREAD_ALL_ACCESS, TRUE, dwThreadID );
 if result <> 0 then
  begin
   n := Length (FThreads);
   SetLength (FThreads, n + 1);
   FThreads [n].ID := dwThreadID;
   FThreads [n].h := result;
  end
 else
  PrintError ( Format ('OpenThread fails for ThreadID = $%x :', [dwThreadID] ) + Err2Str (GetLastError) );

end;

procedure TDebuggedProcess.HandleDebugEvents (msec: DWORD);
var
   de: TDebugEvent;
   // h: THandle;
begin
 if he_lock then exit;
 he_lock := true;
 FillChar (de, sizeof (de), 0);
 SetLastError (0);
 if ThreadID <> 0 then
   begin
    GetThreadHandle (ThreadID);
   end;

 while WaitForDebugEvent (de, msec) do
   try
    ProcessDebugEvent (de);
   except
    on E: Exception do
       PrintError('Exception catched in HandleDebugEvents: ' + E.Message);
   end;
 he_lock := false;
end;


end.
