unit JSONWeb.API;

interface
uses Windows, SysUtils, StrUtils, Classes, Lua5,
     // +alpet units
     StrClasses, DateTimeTools, LuaTypes, LuaEngine, Misc, JSONTools, BTIProto,
     DBXJSON,
     // +Indy
     IdStack, IdHTTP, IdSSLOpenSSL, IdAuthentication, IdFIPS, IdHMACSHA1, IdSSLOpenSSLHeaders, IdComponent, IdcTypes, IdException, IdIOHandler,
     // +sgcWebSockets
     sgcWebSocket_Classes, sgcWebSocket_Client, sgcWebSocket_CustomClient, sgcWebSocket_Client_SocketIO, sgcWebSocket, sgcWebSocket_Types;

type
    TSessionConnection = class;

    // declarations

    TConnectEvent       = procedure ( const AContext: String; AConnection: TSessionConnection ) of object;
    TJSONDataHandler    = procedure ( jo: TJSONObject; const lead: String ) of object;

    TSessionConnection = class
    private
       FOnConnect: TConnectEvent;
       FOnMessage: TJSONDataHandler;
           FTimer: TProfileTimer;
      FServerPort: WORD;
      FServerHost: String;
      FHttpServer: String;
     FOnHeartbeat: TNotifyEvent;
    FContext: TTradingAPIContext;
    protected

    FPingsReceived: Integer;
         FMsgCount: Integer;
        FConnected: Boolean;
        FLastError: String;
       FLastStatus: TIdStatus;
           FClient: TsgcWebSocketClient_SocketIO;
           FActive: Boolean;


     procedure    ConfigureSSL (const AContext: String; AHandler: TIdSSLIOHandlerSocketOpenSSL); virtual;
     procedure    ConfigureHttp (const AContext: String; AHttp: TIdHttp ); virtual;
     function     DefaultOrigin (const path: String = '/socket.io/1'): String; virtual;
     procedure    DoDisconnect; virtual;


     // event handlers
     procedure    socket_Message (Connection: TsgcWSConnection; const Text: string); virtual;
     procedure    socket_Connect (Connection: TsgcWSConnection); virtual;
     procedure    socket_Handshake (Connection: TsgcWSConnection; var Headers: TStringList); virtual;
     procedure    socket_Binary (Connection: TsgcWSConnection; const Data: TMemoryStream); virtual;
     procedure    socket_Error (Connection: TsgcWSConnection; const Error: string); virtual;
     procedure    socket_Disconnect (Connection: TsgcWSConnection; Code: Integer); virtual;
     procedure    socket_Exception (Connection: TsgcWSConnection; E: Exception); virtual;

     procedure    ProcessClientStatus (ASender: TObject; const AStatus: TIdStatus; const AStatusText: string); virtual;
     function     ProcessVerifyPeer   (Certificate: TIdX509; AOk: Boolean; ADepth, AError: Integer): Boolean; virtual;
     procedure    ProcessStatusInfo   (ASender : TObject; const AsslSocket: PSSL;
                                       const AWhere, Aret: TIdC_INT; const AType, AMsg : String ); virtual;

     procedure    ProcessMessage ( jo: TJSONObject ); virtual;

    public
     { props }
     property
     Client: TsgcWebSocketClient_SocketIO read FClient;
     property     Connected: Boolean read FConnected;
     property     Context: TTradingAPIContext read FContext write FContext;

     property     HttpServer: String read FHttpServer write FHttpServer;  // for sync requests

     property     MsgCount: Integer read FMsgCount;
     property     LastError: String read FLastError;
     property     LastStatus: TIdStatus read FLastStatus;
     property     OnConnect: TConnectEvent read FOnConnect write FOnConnect;
     property     OnHeartbeat: TNotifyEvent read FOnHeartbeat write FOnHeartbeat;
     property     OnMessage: TJSONDataHandler read FOnMessage write FOnMessage;
     property     Pings: Integer read FPingsReceived;
     property     ServerPort: WORD read FServerPort;
     property     ServerHost: String read FServerHost;
     property     Timer: TProfileTimer read FTimer;
     { C & D }
     constructor  Create (const AServerHost: String; AServerPort: WORD);
     destructor   Destroy; override;
     { methods }
     function     Connect: Boolean; virtual;
     procedure    Disconnect; virtual;
     procedure    OnMessageDefault ( jo: TJSONObject; const lead: String ); virtual; // вызывается если никакой обработчик не принял сообщение
     procedure    NotifyAlive; virtual;
     function     TCPRead: TThread;
    end; // TSessionConnection



implementation





{ TSessionConnection }

constructor TSessionConnection.Create(const AServerHost: String; AServerPort: WORD);
const
   SSL_PORT = 443;

var
   prefix: String;
begin
 OnMessage := OnMessageDefault;
 FServerHost := AServerHost;

 FTimer := TProfileTimer.Create;

 FClient := TsgcWebSocketClient_SocketIO.Create ( nil );
 FClient.Host := AServerHost;
 prefix := IfV ( AServerPort = SSL_PORT, 'https://', 'http://' );

 FClient.Port := AServerPort;
 FClient.Options.Origin := DefaultOrigin;
 FClient.SSL := ( AServerPort = SSL_PORT );
 FClient.SSLHandlerConfigure := ConfigureSSL;
 FClient.HttpConfigure := ConfigureHttp;


 with FClient.TCPClient do
  begin
   ConnectTimeout := 30000;
   ReadTimeout := 90000;
  end;

 FClient.OnConnect := socket_Connect;
 FClient.OnHandshake := socket_Handshake;
 FClient.OnException := socket_Exception;
 FClient.OnDisconnect := socket_Disconnect;
 FClient.OnMessage := socket_Message;
 FClient.OnError := socket_Error;
end;

procedure TSessionConnection.ConfigureSSL ( const AContext: String; AHandler: TIdSSLIOHandlerSocketOpenSSL );
begin
 //
 with AHandler do
 begin
  Context.LogMsg('[~T]. #SSL_CONFIG: Context = ' + AContext);
  SSLOptions.Mode := sslmClient;
  OnVerifyPeer := self.ProcessVerifyPeer;
  OnStatusInfoEx := self.ProcessStatusInfo;

  SSLOptions.SSLVersions := [sslvSSLv3, sslvSSLv2, sslvSSLv23, sslvTLSv1]; // sslvSSLv2, sslvSSLv23, sslvSSLv3,
  SSLOptions.Method := sslvSSLv3;
  // SSLOptions.Method := sslvSSLv2;
  SSLOptions.CipherList := 'AES:256:!aNULL:!eNULL:+RC4:@STRENGTH'; //

  SSLOptions.VerifyDepth := 10;
  SSLOptions.VerifyMode := [sslvrfPeer];

 end;
end;

function TSessionConnection.Connect: Boolean;
var
    cl: TsgcWSCustomClient;
     n: Integer;
begin
 FLastError := '';
 FMsgCount := 0;
 FPingsReceived := 0;
 result := FALSE;

 try
  Context.LogMsg ('[~T/~B]. #DBG: Starting connection to~C0A ' + DefaultOrigin + '~C07');
  FClient.Active := FALSE;
  FClient.Active := TRUE;
  FClient.HeartBeat.Interval := 15;
  FClient.HeartBeat.Enabled := TRUE;
  cl := FClient.TCPClient;

  for n := 1 to 100 do
    if cl.Connected then
      begin
       Context.LogMsg('[~T/~B]. #DBG: TCPClient connected (session)');
       break
      end
    else
      begin
       Sleep (1000); // 100 seconds for wait ?
       Context.LogMsg('[~T/~B]. #WAIT: TCPClient connecting...');
      end;


  result := cl.Connected;
  FConnected := result;
  if result then
    begin
     cl.OnStatus := self.ProcessClientStatus;
    end;

 except
  on E: EIdSocketError do
     FLastError := E.Message;
  on E: EIdConnClosedGracefully do
     FLastError := E.Message;
 end
end;

procedure TSessionConnection.ConfigureHttp ( const AContext: String; AHttp: TIdHttp );
begin
 with AHttp do
  begin
   ConnectTimeout := 0;
   ReadTimeout := 20000;
   HandleRedirects := TRUE;
   AllowCookies := FALSE;
   Request.ContentType := 'application/x-www-form-urlencoded';
   Context.LogMsg('[~T]. #HTTP_CONFIG: Context = ' + AContext);
   {
   if ( Client.TCPClient <> nil ) then
     begin
      Context.LogMsg ( #9'Client.TCPClient = ' + FormatPtr (Client.TCPClient) + '@' + Client.TCPClient.QualifiedClassName );
      if Client.TCPClient.IOHandler <> nil then
         begin
          Context.LogMsg ( #9'Client.TCPClient.IOHandler = ' + FormatPtr (Client.TCPClient.IOHandler) + '@' + Client.TCPClient.IOHandler.QualifiedClassName );
          Context.LogMsg ( #9'Client.TCPClient.Connected =' + IfV ( Client.TCPClient.Connected, '~C0A yes', '~C0F no' ) + '~C07' );
         end;
     end;
   // }

  end;
end;


function TSessionConnection.DefaultOrigin(const path: String): String;
var
   prefix: String;
        i: Integer;
begin
 Prefix := IfV ( FClient.Port = 443, 'https://', 'http://' ); // wss or ws?

 result := ServerHost + path;
 result := Prefix + AnsiReplaceStr (result, '//', '/');
end;

destructor TSessionConnection.Destroy;
begin
 if FClient.Active then
    Disconnect;

 Client.Free;
 Timer.Free;
 inherited;
end;

procedure TSessionConnection.Disconnect;
var
   t: TThread;
begin
 // FEngine.callFunction('disconnect');
 FActive := FALSE;

 if Connected then
   begin
    t := TCPRead;
    if t = nil then
       DoDisconnect
    else
      TThread.Synchronize ( t, DoDisconnect );
   end;
end;

procedure TSessionConnection.DoDisconnect;
begin
 Client.WriteData('0::');
 FConnected := FALSE;
 FClient.Active := FALSE;
end;

procedure TSessionConnection.NotifyAlive;
begin
 if Assigned (FClient) and ( FClient.Active )  and ( FTimer.Elapsed (35) > 15000 ) then
    begin
     FTimer.StartOne (35);
     FClient.WriteData ('2::'); // heartbeat
    end;
end;

procedure TSessionConnection.OnMessageDefault(jo: TJSONObject; const lead: String);
 var
    jp: TJSONPair;
    ja: TJSONArray;
    jv: TJSONValue;
    js: TJSONString;
    dm: Boolean;
     n: Integer;

begin
 //
 dm := ( log_verbose > 9 );
 if ( lead = #9 ) and dm then
     Context.LogMsg('[~T/~U/~I]. #MESSAGE:');
 for n := 0 to jo.Size - 1 do
  begin
   jp := jo.Get (n);
   if jp = nil then break;
   js := jp.JsonString;
   jv := jp.JsonValue;

   if jv is TJSONObject then
    begin
     if dm then Context.LogMsg ( lead + Format('~C0F %-20s: ~C07', [js.Value]) );
     OnMessageDefault ( TJSONObject (jp.JsonValue), lead + ' ');
    end
   else
   if jv is TJSONArray then
    begin
     ja := TJSONArray (jv);
     if dm then Context.LogMsg ( lead + Format('~C0F %-20s as array, items =~C0D %d ~C07', [js.Value, ja.size]) );
    end
   else
   if dm then
    begin
     if jv is TJSONValue then
       Context.LogMsg ( lead + CFormat(' %-20s = %-20s ', '~C07', [js.Value, jv.Value] ) )
     else
       Context.LogMsg ( lead + CFormat(' %-20s as %-20s ', '~C07', [js.Value, jv.QualifiedClassName] ) );
    end;
  end;
end;

procedure TSessionConnection.ProcessClientStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
begin
 FLastStatus := AStatus;
 case AStatus of
  hsConnected:
     FConnected := TRUE;
  hsDisconnected:
     FConnected := FALSE;
 end;
 Context.LogMsg('[~T]. #CLIENT_STATUS:~C0A ' + AStatusText + '~C07');
end;

procedure TSessionConnection.ProcessMessage(jo: TJSONObject);
begin
 // nothing
 OnMessage ( jo, #9 );
end;

procedure TSessionConnection.ProcessStatusInfo(ASender: TObject; const AsslSocket: PSSL; const AWhere, Aret: TIdC_INT; const AType, AMsg: String);
begin
 Context.LogMsg(  CFormat ( '[~T/~B].~C05 #SSL_STATUS: type = %-20s msg = %-50s where = %7d, ret = %5d   ',
                        '~C07', [AType, AMsg, AWhere, ARet] ) );

 if Pos ('close notify', LowerCase (AMsg)) > 0 then
   try
    FConnected := FALSE;
    // FClient.Active := FALSE;
   except
    on E: Exception do
      OnExceptLog ('ProcessStatusInfo', E, TRUE);
   end;
end;

function TSessionConnection.ProcessVerifyPeer(Certificate: TIdX509; AOk: Boolean; ADepth, AError: Integer): Boolean;
begin
 Context.LogMsg('[~T].~C0E #VERIFY_PEER:' + IfV (AOk, '~C0A Yep ', '~C0C Nope') +
                CFormat( '~C07 Depth = %7d, Error = %7d', '~C07', [ADepth, AError] ) );
 Sleep (1);
 result := TRUE;
end;

procedure TSessionConnection.socket_Binary(Connection: TsgcWSConnection; const Data: TMemoryStream);
begin
 //
 Context.LogMsg('[~T]. #SOCKET_BINARY: ' + IntToStr (Data.size) + ' bytes' );
end;

procedure TSessionConnection.socket_Connect(Connection: TsgcWSConnection);
begin
 Context.LogMsg('[~T/~B].~C0F #CONNECTED_SESSION. ~C07');
 if Assigned (OnConnect) then
    OnConnect ('SESSION', self);
 FMsgCount := 0;
 // SubList.Clear; #MOVE
end;

procedure TSessionConnection.socket_Disconnect(Connection: TsgcWSConnection; Code: Integer);
begin
 //
 FClient.Active := FALSE;
end;

procedure TSessionConnection.socket_Error(Connection: TsgcWSConnection; const Error: string);
begin
 Context.LogMsg('[~T/~B].~C0C #SOCKET_ERROR: ~C0F' + Error + '~C07');
 FLastError := '#SOCKET_ERROR:' + Error;
end;

procedure TSessionConnection.socket_Exception(Connection: TsgcWSConnection; E: Exception);
begin
 FLastError := '#EXCEPTION:' + E.ClassName + '=' + E.Message;
 PrintError ('TSessionConnection.socket_Exception catched ' + E.QualifiedClassName + ' = ' + E.Message );

 if ( E is EIdSocketError ) or ( E is EIdConnClosedGracefully ) then
  try
   FConnected := FALSE;
   FClient.Active := FALSE;
  except
   on E: Exception do
      OnExceptLog ( 'TSessionConnection.socket_Exception#2', E, TRUE );
  end;
end;

procedure TSessionConnection.socket_Handshake(Connection: TsgcWSConnection; var Headers: TStringList);
var
   t, s: String;

begin
 if Headers.Count = 0 then exit;
 s := Headers[0];
 if Pos('socket.io', s) = 0 then exit;
 t := StrTok ( s, [' '] );
 t := StrTok ( s, [' '] );
 Context.LogMsg('[~T/~I]. #HANDSHAKE: session url ~C0A ' + t + '~C07');

 // FSocket.Options.Origin := 'https://' + FSocket.Host + t; // TODO: need or not?
end;


procedure TSessionConnection.socket_Message(Connection: TsgcWSConnection; const Text: string);

var
    mt: Integer; // message type
    sm: String;
    rs: RawByteString;
    jo: TJSONObject;
     i: Integer;

begin
 sm := Text;
 if Pos(':', sm) < 2 then exit;

 mt := Ord (sm [1]) - $30;
 Delete (sm, 1, 3);

 case mt of
   0: sm := '%disconnect_ep%:' + sm;
   1: begin
       if Assigned (OnConnect) then OnConnect ( sm, self );
       sm := '%connect_ep%:' + sm;
       // Subscribe ('', CHID_TICKER, '');
       // Subscribe ('', CHID_TRADES, '');
       // Subscribe ('', CHID_DEPTH, '');
      end;
   2: sm := '%heartbeat%:' + sm;
   3: sm := '%message%:' + sm;
   4: sm := '%JSON_message%:' + sm;
   5: sm := '%event%:' + sm;
   6: sm := '%ACK%:' + sm;
   7: sm := '%error%:' + sm;
   8: sm := '%noop%:' + sm;
 end;

 // and ( MsgCount > 0 )
 if mt = 2 then
   begin
    NotifyAlive;
    Inc ( FPingsReceived );
    if Assigned (OnHeartbeat) then
       OnHeartbeat (self);
   end;

 if Pos('{', sm) = 0 then
   begin
    if IsKeyPressed (VK_CONTROL) then
       Context.LogMsg('[~T/~I]. #SHORT_MSG:~C0F ' + sm + '~C07, data MsgCount =~C0D ' + IntToStr(msgCount) + '~C07');
    exit;
   end;

 Inc ( FMsgCount ); // 1st large message
 rs := UTF8Encode (Text);
 jo := JSONParse (rs);
 if Assigned (jo) then
  try
   ProcessMessage ( jo );
  finally
   jo.Free;
  end;

end;


function TSessionConnection.TCPRead: TThread;
begin
 result := nil;
 if Assigned ( FClient ) and Assigned ( FClient.TCPClient ) then
     result := FClient.TCPClient.TCPRead;
end;

end.
