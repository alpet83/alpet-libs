unit WSASock;

interface

uses Windows, SysUtils, FastSync, Misc, WinSock, Classes;

{

  *** Socket wrapper class ***
  created by alpet, at 7.12.2007

}

const
    FD_MAX_EVENTS = 10;
    SOCKET_BUFFER = 16 * 1024;

type
    TSocketState = (SST_ERROR, SST_NEWSOCK, SST_BINDED, SST_CONNECTED, SST_LISTENED, SST_SHUTDOWNED, SST_CLOSED );
    TWSAEvent = THandle;
    PWSAEvent = ^TWSAEvent;
    TByteSet = set of BYTE;

    TWSANetworkEvents = packed record
     lNetworkEvents: LongInt;
     iErrorCode: array [0..FD_MAX_EVENTS-1] of Integer;
    end;
    
    PWSANetworkEvents = ^TWSANetworkEvents;

    TWSASock = class
    protected
     FSocket: TSocket;
      FState: TSocketState;
    public
     property                   socket: TSocket read FSocket;
     property                   State: TSocketState read FState write FState;
    end; // TWSASock

    TWSAAsyncMode = (WSA_SYNC_MODE, WSA_ASYNC_MSG, WSA_ASYNC_EVT);

    TTCPSocket = class (TWSASock)
    protected
     FSockEvent: TWSAEvent;
     FAsyncMode: TWSAAsyncMode;
     FWSAError: Integer;
     FBuffSize: Integer;
     hMsgWnd: HWND;
     FastMode: Boolean;
     procedure                  MakeSocket;
     function                   WSATransfer (socket: TSocket; buff: Pointer; dsize: Integer;
                                        bRead: Boolean): Integer;
    public
     SurviveTime: Integer;
     recv_ctx: TStrings;
     send_ctx: TStrings;


     property                   AsyncMode: TWSAAsyncMode read FAsyncMode;
     property                   BuffSize: Integer read FBuffSize;
     property                   Error: Integer read FWSAError;

     constructor                Create(exists_socket: TSocket = 0);
     destructor                 Destroy; override;
     { methods }
     function                   accept: TTCPSocket;
     function                   bind (const Host: String; port: Integer): Boolean;
     function                   connect (const Host: String; port: Integer): Boolean;
     function                   connected: Boolean;
     function                   close: Boolean;
     function                   get_events: LongInt;
     function                   listen (backlog: Integer = 1): Boolean;
     function                   recv (buff: Pointer; nBytes: Integer;flags: Integer = 0): Integer;
     function                   ready_recv: Integer;                  // bytes count, ready to recieve
     function                   shutdown (how: Integer = SD_SEND): Boolean;
     function                   send (buff: Pointer; nBytes: Integer): Integer;
     function                   set_smode (fast: Boolean): Boolean; // выбор между скоростью и латентностью
     function                   set_buffsize (newSize: Integer; flags: BYTE = 3): Boolean;
     { extra methods }
     procedure                  set_event;
     function                   setopt_i (level, optn, optv: Integer): Boolean;
     procedure                  reset_event;
     procedure                  StartAsync (lEventSet: Integer = FD_READ or FD_CLOSE);
     procedure                  StartAsyncMsg(hWnd, uMsg: DWORD; lEventSet: Integer = FD_READ or FD_CLOSE);
     procedure                  StopAsync;
     function                   WaitFor (timeOut: Integer): DWORD;
    end; // TTCPSocket


function ResolveHost(Host: AnsiString): Integer;
function NetErrorCheck (iCode: Integer; ErrCode: PInteger = nil): Boolean;

implementation



function WSACreateEvent(): TWSAEvent; stdcall; external 'Ws2_32.dll' name 'WSACreateEvent';
function WSAEnumNetworkEvents(S: TSocket;
                              hEvent: TWSAEvent;
                              pResult: PWSANetworkEvents): Integer; stdcall; external 'Ws2_32.dll' name 'WSAEnumNetworkEvents';
function WSAEventSelect(S: TSocket;
                        hEvent: TWSAEvent;
                        lEventSet: LongInt): Integer; stdcall; external 'Ws2_32.dll' name 'WSAEventSelect';

function WSASetEvent(hEvent:TWSAEvent):BOOL;   stdcall; external 'Ws2_32.dll' name 'WSASetEvent';
function WSAResetEvent(hEvent:TWSAEvent):BOOL; stdcall; external 'Ws2_32.dll' name 'WSAResetEvent';
function WSAWaitForMultipleEvents(cEvents: DWORD;
                                  const phEvents: PWSAEvent;
                                  bWaitAll: Boolean; dwTimeOut: DWORD;
                                  fAlertable: Boolean): DWORD; stdcall; external 'Ws2_32.dll' name 'WSAWaitForMultipleEvents';
{ -------------------------------- -------------------------------- --------------------------------}
function NetErrorCheck;
begin
 // TODO: complete error check
 if ErrCode <> nil then ErrCode^ := 0;
 result := (iCode >= 0);
 if (not result) and (ErrCode <> nil) then
     ErrCode^ := WSAGetLastError;
end; // NetErrorCheck


function ResolveHost (Host: AnsiString): Integer ;
var
 HostEnt: PHostEnt;
begin
  Result := inet_addr( PAnsiChar(Host) );
  if DWORD(Result) = INADDR_NONE then
   begin
     HostEnt := gethostbyname( PAnsiChar (Host) );
     if (HostEnt = nil) and not NetErrorCheck (-1) then
        Result := 0 else
        Result := PLongint(HostEnt^.h_addr_list^)^;
   end;
end; // NetResolve


procedure InitWSA;
var wsd: TWSAData;
    err: Integer;
begin
 err := WSAStartup (MAKEWORD (2, 2), wsd);
 Assert (err = 0, Format('WSAStartup returned code %d ', [err]));
end; // InitWSA

function SetSockAddr (var saddr: TSockAddrIn; const Host: String; port: Integer): Boolean;
begin
 FillChar (saddr, sizeof (saddr), 0);
 with saddr do
  begin
   sin_family := AF_INET;
   sin_port :=  htons (port);
   sin_addr.S_addr := ResolveHost ( AnsiString (Host) );
   result := sin_addr.S_addr <> 0;
  end;
end;

{ TTCPSocket }

function TTCPSocket.accept: TTCPSocket;
var
   new_sock: TSocket;
begin
 result := nil;
 if FState <> SST_LISTENED then exit;
 new_sock := WinSock.accept (FSocket, nil, nil);
 if NetErrorCheck (new_sock, @FWSAError) then
   begin
    result := TTCPSocket.Create (new_sock);
    result.set_buffsize ( SOCKET_BUFFER, 3 );
   end;
end; // accept

function TTCPSocket.Bind(const Host: String; port: Integer): Boolean;
var
   saddr: TSockAddrIn;
   err: Integer;
begin
 result := SetSockAddr (saddr, Host, port);
 if not result then exit;
 err := WinSock.bind (FSocket, saddr, sizeof (saddr));
 result := NetErrorCheck (err, @FWSAError);
 if result then FState := SST_BINDED;
end; // bind

function TTCPSocket.close: Boolean;
begin
 result := NetErrorCheck ( closesocket (FSocket), @FWSAError );
 if result then FState := SST_CLOSED;
 FSocket := 0;
end;

function TTCPSocket.connect(const Host: String; port: Integer): Boolean;
var
   saddr: TSockAddr;
   err: Integer;
begin
 result := SetSockAddr (saddr, Host, port);
 if not result then exit;
 err := WinSock.connect (FSocket, saddr, sizeof (saddr));
 result := NetErrorCheck (err, @FWSAError);
 if result then
   begin
    FState := SST_CONNECTED;
    set_buffsize ( SOCKET_BUFFER, 3 );
   end;
end; // connect

function TTCPSocket.connected: Boolean;
begin
 result := (FState = SST_CONNECTED);
end;

constructor TTCPSocket.Create;
begin
 SurviveTime := 250;
 if exists_socket <> 0 then
   begin
    FSocket := exists_socket;
    FState := SST_CONNECTED;
   end
 else
    MakeSocket;
 recv_ctx := TStringList.Create;
 send_ctx := TStringList.Create;
end; // Create

destructor TTCPSocket.Destroy;
begin
 SurviveTime := 0;
 StopAsync;
 case State of
  SST_BINDED, SST_NEWSOCK, SST_SHUTDOWNED: close;
  SST_CONNECTED:
     begin
      shutdown;
      close;
     end;
 end;
 recv_ctx.Free;
 send_ctx.Free;
 inherited;
end; // Destroy

function TTCPSocket.get_events: LongInt;
var
   evts: TWSANetworkEvents;
   err, mask, n: Integer;
begin
 result := 0;
 if AsyncMode <> WSA_ASYNC_EVT then exit;
 FillChar (evts, sizeof (evts), 0);
 err := WSAEnumNetworkEvents (FSocket, FSockEvent, @evts);
 if not NetErrorCheck ( err, @FWSAError) then
   begin
    result := Integer ($80000000);
    exit;
   end;
 result := evts.lNetworkEvents;
 for n  := 0 to FD_MAX_EVENTS - 1 do
  begin
   mask := (1 shl n);
   if (result and mask <> 0) and
      ( not NetErrorCheck (evts.iErrorCode[n], @FWSAError) ) then
       result := result and (not mask); // masking - error action   
  end;

end;

function TTCPSocket.listen(backlog: Integer): Boolean;
var err: Integer;
begin
 result := FALSE;
 if FState <> SST_BINDED then exit;
 err := WinSock.listen (FSocket, backlog);
 result := NetErrorCheck (err, @FWSAError);
 if result then FState := SST_LISTENED;
end; // listen

procedure TTCPSocket.MakeSocket;
begin
 FSocket := WinSock.socket (AF_INET, SOCK_STREAM, 0);
 if NetErrorCheck (FSocket, @FWSAError) then
    FState := SST_NEWSOCK
 else
    FState := SST_ERROR;
end; // MakeSocket

function TTCPSocket.ready_recv: Integer;
begin
 FWSAError := 0;
 NetErrorCheck ( ioctlsocket (FSocket, FIONREAD, result),  @FWSAError);
 if FWSAError <> 0 then result := -1;
end;

function  TTCPSocket.WSATransfer (socket: TSocket; buff: Pointer; dsize: Integer;
                                        bRead: Boolean): Integer;
var
   ticks: Int64;
   pbuff: PAnsiChar;
   lr: Integer;
begin
 WSASetLastError (0);
 ticks := GetTickCount ();
 result := 0;
 pbuff := buff;
  Repeat
   // Циклическое считывание/запись данных из сокета
   if bRead then
     lr := WinSock.recv (FSocket, pbuff [result], dsize, 0)
   else
     lr := WinSock.send (FSocket, pbuff [result], dsize, 0);
   if lr > 0 then
    begin
     Inc (result, lr);   //{}
     Dec (dsize, lr);
    end;
  Until (lr < 0) or (dsize <= 0);

 if lr = SOCKET_ERROR then
    NetErrorCheck (lr, @FWSAError)
 else
  begin
   ticks := GetTickCount () - ticks;
   if ticks > 200 then
     ODS (format ('[~T]. #WARN: Transfer operation used %d msec',
                [ticks]));
  end;
end; // WSATransfer


function TTCPSocket.recv;
var
   buffx: PByteArray absolute buff;
   rb: Integer;
begin
 result := 0;
 FWSAError := WSAENOTCONN;
 if not Connected then exit;
 FWSAError := 0;
 repeat
  rb := WinSock.recv (FSocket, buffx^[result], nBytes, flags);
  if rb = 0 then SleepEx(1, FALSE);
  if rb > 0 then
    begin
     Inc (result, rb);
     Dec (nBytes, rb);
    end;
  if not NetErrorCheck(rb, @FWSAError) then
     begin
      if FWSAError <> WSAEWOULDBLOCK then
         ODS ('[~T]. WinSock.recv failed with msg:' + Err2Str (FWSAError));
      break;
     end;

 until (nBytes <= 0);
 NetErrorCheck (result, @FWSAError);
end; // recv

procedure TTCPSocket.reset_event;
begin
 if AsyncMode = WSA_ASYNC_EVT then WSAResetEvent (FSockEvent);
end;

function TTCPSocket.send(buff: Pointer; nBytes: Integer): Integer;
var
   buffx: PByteArray absolute buff;
   wb: Integer;
begin
 result := 0;
 FWSAError := WSAENOTCONN;
 if not Connected then exit;
 FWSAError := 0;
 repeat
  wb := WinSock.send (FSocket, buffx^[result], nBytes, 0);
  if wb = 0 then SleepEx(1, FALSE);
  if wb > 0 then
    begin
     Dec (nBytes, wb);
     Inc (result, wb);
    end;
  if not NetErrorCheck(wb, @FWSAError) then
     begin
      if FWSAError <> WSAEWOULDBLOCK then
         ODS ('[~T]. WinSock.send failed with msg: ' + Err2Str (FWSAError));
      break;
     end;
  until (nBytes <= 0);
end; // send               

function TTCPSocket.set_buffsize;
begin
 result := TRUE;
 FBuffSize := newSize;
 if flags and 1 <> 0 then
   result := result and setopt_i (SOL_SOCKET, SO_RCVBUF, newSize);
 if flags and 2 <> 0 then
   result := result and setopt_i (SOL_SOCKET, SO_SNDBUF, newSize);
end;

procedure TTCPSocket.set_event;
begin
 if AsyncMode = WSA_ASYNC_EVT then WSASetEvent (FSockEvent);
end;

function TTCPSocket.shutdown(how: Integer): Boolean;
begin
 ASSERT (Assigned(self), 'self unassigned!');
 result := NetErrorCheck ( WinSock.shutdown (FSocket, how), @FWSAError );
 if result then FState := SST_SHUTDOWNED;
end; // shutdown



procedure TTCPSocket.StartAsync;
begin
 if FSockEvent = 0 then
   begin
    FSockEvent := WSACreateEvent;
   end;
 if NetErrorCheck ( WSAEventSelect (FSocket, FSockEvent, lEventSet), @FWSAError) then
    FAsyncMode := WSA_ASYNC_EVT;
end; // StartAsync

procedure TTCPSocket.StopAsync;
begin
 if FAsyncMode = WSA_SYNC_MODE then exit;
 if (FSockEvent <> 0) then
  begin
   WSAEventSelect (FSocket, FSockEvent, 0);
   CloseHandle (FSockEvent);
   FSockEvent := 0;
  end;
 if FAsyncMode = WSA_ASYNC_MSG then
  begin
   WSAAsyncSelect (FSocket, hMsgWnd, 0, 0);
   hMsgWnd := 0;
  end;
 FAsyncMode := WSA_SYNC_MODE;
end; // StopAsync

function TTCPSocket.WaitFor;
var hEvents: array [0..3] of TWSAEvent;
begin
 result := 0;
 if FAsyncMode <> WSA_ASYNC_EVT then exit;
 hEvents [0] := FSockEvent;
 hEvents [1] := FD_CLOSE;
 if ready_recv = 0 then WSAResetEvent (FSockEvent);

 result := WSAWaitForMultipleEvents (2, @hEvents, FALSE, timeOut, TRUE);
 if result = WAIT_OBJECT_0 then WSAResetEvent (FSockEvent);
end; // WaitFor

procedure TTCPSocket.StartAsyncMsg(hWnd, uMsg: DWORD; lEventSet: Integer);
var err: Integer;
begin
 if not IsWindow (hWnd) then
  begin
   PrintError ('StartAsyncMsg: Wrong window handle used. hWnd = ' + IntToStr (hWnd));
   exit;
  end;
 hMsgWnd := hWnd;
 err := WSAAsyncSelect (FSocket, hWnd, uMsg, lEventSet);
 if NetErrorCheck (err, @FWSAError) then
    FAsyncMode := WSA_ASYNC_MSG
 else
    State := SST_ERROR;
end;

function TTCPSocket.set_smode(fast: Boolean): Boolean;
var
  err: Integer;
begin
 result := TRUE;
 if fast = FastMode then exit;
 FastMode := fast;
 err := setsockopt (socket, SOL_SOCKET, TCP_NODELAY, @fast, sizeof(DWORD));
 result := NetErrorCheck (err, @FWSAError);
end; // set_smode

function TTCPSocket.setopt_i(level, optn, optv: Integer): Boolean;
begin
 result := NetErrorCheck ( setsockopt (FSocket, level, optn, @optv, 4), @FWSAError );
end;

initialization
 InitWSA;
finalization
 WSACleanup;
end.
