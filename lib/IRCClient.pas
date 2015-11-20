unit IRCClient;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, IdContext, Misc, StrClasses, DateTimeTools,
  Dialogs, IdIOHandler, IdGlobal, IdThread, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSLHeaders, IdSSLOpenSSL, IdBaseComponent, IdComponent, IdException, IdExceptionCore,
  IdTCPConnection, IdTCPClient, IdCmdTCPClient, IdIRC, IdStack, StdCtrls, ExtCtrls, ContNrs, WThreads;


type

  TIdIRCCon = class;
  TChatChannel = class;
  TClientIRCThread = class;

  TIRCEventHandler = function (FirstSender: TObject; con: TIdIRCCon; const sEvent: String; params: TStrMap): Boolean of object;


  TChatMsgInfo = class
  private
  public
   NNFrom, NNTo, Host, Text: String; // read FFrom write FFrom;
   IsPrivate: Boolean;
   IsIncoming: Boolean;
   ircc: TIdIRCCon;
   owner: TChatChannel;
  end;

  TChatChannel = class (TStrMap) // тупо список сообщений с дескрипторами
  private
    FChannelID: String;
  public

   property     ChannelID: String read FChannelID write FChannelID;
   { C & D }

   constructor   Create (AID: String; AOwner: TIdIRCCon);
   destructor    Destroy; override;
   { methods }
   function      AddMsg (const ANNFrom, AHost, ANNTo, AMsg: String): TChatMsgInfo;
   procedure     GetMsgs (dst: TStrings; bClear: Boolean = TRUE);
   function      IsPrivate: Boolean;
   procedure     TestJoin (con: TIdIRCCon);

  end; // TChatMessageInfo


  TIdIRCCon = class (TIdIRC)
  private
    FChnList: TStrMap;  // список каналов общения
    FEventParams: TStrMap;
    FOnChatEvent: TIRCEventHandler;
    FActiveChannel: TChatChannel;
    FAutoConnect: Boolean;
    FAuthorized: Boolean;
    FLastError: String;
    FID: String;
    pt: TProfileTimer;
    FReady: Boolean;
    FEchoRaw: Boolean;
    FLastConnected: Boolean; //
    FOwnerThread: TClientIRCThread;

    connect_times: Integer;
    FConnectorID: DWORD;
    FErrorCount: Integer;
    FUseSSL: Boolean;

    function  DefOnChatEvent (FirstSender: TObject; con: TIdIRCCon;  const sEvent: String; params: TStrMap): Boolean;
    function  GetChannel(const index: String): TChatChannel; inline;
    function  FindAddChannel(const AChannel: String): TChatChannel;
    procedure SetChannel(const Value: String);
    function  GetChannelID: String;
    function  GetChannelByIndex(index: Integer): TChatChannel;
    procedure SetLastConnected(const Value: Boolean);
    function GetLastConnected: Boolean;
    procedure LogMsg(const msg: String; prefix: String = '#DBG');


  protected

    procedure   _OnConnect(Sender: TObject);
    procedure   _OnDisconnect(Sender: TObject);
    procedure   _OnNotice (ASender: TIdContext; const ANicknameFrom, AHost, ANicknameTo, ANotice: String);
    procedure   _OnPart (ASender: TIdContext; const ANickname, AHost, AChannel, APartMessage: String);
    procedure   _OnPrivMsg (ASender: TIdContext; const ANicknameFrom, AHost, ANicknameTo, AMessage: String);
    function    _OnVerifyPeer (Certificate: TIdX509; AOk: Boolean; ADepth: Integer): Boolean;
    procedure   _OnWelcome (ASender: TIdContext; const AMsg: String);
    procedure   _OnJoin (ASender: TIdContext; const ANickname, AHost, AChannel: String);
    procedure   _OnRaw (ASender: TIdContext; AIn: Boolean; const AMessage: String);
    procedure   _OnStatusChanged (bReady: Boolean);
  public

   property     ActiveChannel: TChatChannel read FActiveChannel;
   property     AutoConnect: Boolean read FAutoConnect write FAutoConnect;
   property     Authorized: Boolean read FAuthorized write FAuthorized;
   property     Channel: String read GetChannelID write SetChannel;
   property     ChannelByIndex [index: Integer]: TChatChannel read GetChannelByIndex;
   property     Channels[const index: String]: TChatChannel read GetChannel;
   property     ConnectorID: DWORD read FConnectorID; // thread why called connect
   property     EchoRaw: Boolean read FEchoRaw write FEchoRaw;
   property     ErrorCount: Integer read FErrorCount write FErrorCount;
   property     EventParams: TStrMap read FEventParams;
   property     ID: String read FID write FID;
   property     LastConnected: Boolean read GetLastConnected write SetLastConnected;
   property     LastError: String read FLastError;
   property     OnChatEvent: TIRCEventHandler read FOnChatEvent write FOnChatEvent;
   property     OwnerThread: TClientIRCThread read FOwnerThread write FOwnerThread;
   property     Ready: Boolean read FReady;
   property     UseSSL: Boolean read FUseSSL write FUseSSL;
   { C & D }
   constructor  Create(AOwner: TComponent);
   destructor   Destroy; override;

   { methods }
   procedure    AlivePing;
   function     ChannelsCount: Integer; inline;
   procedure    CheckState;
   procedure    Connect; override;
   function     Connected: Boolean; override;
   procedure    Disconnect(const AReason: String = ''); reintroduce; virtual;
   function     IsReady: Boolean; inline;
   procedure    ReJoin;
   procedure    SafeDisconnect;

  end; // TIdIRCHack


  TClientIRCThread = class (TWorkerThread)
  private


   function GetCon(index: Integer): TIdIRCCon; inline;
   function _OnVerifyPeer (Certificate: TIdX509; AOk: Boolean; ADepth, AError: Integer): Boolean;
  protected
   FConList: TStrMap;  // cписок подключений
   procedure                  ProcessInit; override;
   function                   ProcessRequest (const rqs: String; rqobj: TObject): Integer; override;
   procedure                  ProcessThreadStop; override;

  public
   { props }
   property          ConList[index: Integer]: TIdIRCCon read GetCon;
   { methods }

   function          ConCount: Integer; inline;
   function          MakeCon (const AHost, ANick, APasswd: String; APort: WORD; bSSL: Boolean): TIdIRCCon;
   procedure         PrivMsg (const AMsg: String; ircc: TIdIRCCon; ATo: String = '');
   procedure         SetChannel (const ch: String; ircc: TIdIRCCon);

   procedure         WorkProc; override;
  end; // TClientIRCThread


var
   gIRCThread: TClientIRCThread = nil;
   RussianEnc: TEncoding;

implementation

{ TIdIRCCon }

procedure TIdIRCCon.AlivePing;
begin
 if not LastConnected then exit;

 if (GetCurrentThreadID = ConnectorID) or (OwnerThread = nil) then
    Ping(Host)
 else
    OwnerThread.AddRequest('ALIVE_PING', self)
end;

function TIdIRCCon.ChannelsCount: Integer;
begin
 result := FChnList.Count;
end;

procedure TIdIRCCon.CheckState;
var
   elps: Double;
begin
 try
   if (connect_times > 0) and (pt.Elapsed (1) > 10000) and IsReady then
    begin
     if Authorized then AlivePing;
     pt.StartOne (1);
    end;

   if AutoConnect and  ( pt.Elapsed(11) > 10000 ) and ( not FLastConnected ) then
     begin
      ODS('[~T]. #DBG(IRCC): Trying AutoConnect to server ~C0A' + Host + '~C07');
      FAuthorized := FALSE;
      Connect;
     end;

   elps := pt.Elapsed (2);
   if (not FReady) and (elps > 2000) and IsReady then
     begin
      FReady := TRUE;
      _OnStatusChanged (FReady);
     end;

 except
  On E: Exception do
    begin
     FAuthorized := FALSE;
     FLastConnected := FALSE;
     FReady := FALSE;
     Windows.Beep(2222,2000);
     OnExceptLog ('CheckState', E);
    end;
 end;
end;


procedure TIdIRCCon.Connect;
var
   try_again: Integer;
begin
 Inc (connect_times);
 try_again := 10;
 FErrorCount := 0;

 while (not LastConnected) and (try_again > 0) and (ErrorCount < 10) do
 try
  SleepEx (try_again, TRUE);
  FLastError := '';
  FAuthorized := FALSE;
  FConnectorID := GetCurrentThreadID;
  pt.StartOne (11);     // когда началось подключение

  LogMsg('Trying connect IRC, ErrorCount = ' + IntToStr(ErrorCount));

  inherited;

  if Connected then
    begin
     if IOHandler <> nil then
       begin
        IOHandler.DefStringEncoding := RussianEnc;
        // IOHandler.ReadTimeout := 5000;
        // IOHandler.ReadLnTimedout := FALSE;
       end;
     try_again := 0;
     pt.StartOne(35);
    end
  else
     FConnectorID := 0;

 except
  on E: EIdOSSLUnderlyingCryptoError do
    begin
     // Not use PrintError!
     // тупо не удалось подключиться
     LogMsg (' in function Connect SSLException catched, please check system date-time:~C0F ' + E.Message + '~C07', '~C0C#EXCEPT');
     Windows.Beep (1000, 500);
     FLastError := E.Message;
     LastConnected := FALSE;
     Disconnect;
     try_again := -1;
     Inc (FErrorCount);
    end;
  on E: EIdConnClosedGracefully do
    begin
     FLastConnected := FALSE;
     Inc (FErrorCount);
     Windows.Beep (2000, 500);
     LogMsg (' in function Connect:~C0C ' + E.Message + '~C07', '~C0C#EXCEPT');
     try_again := 5000;
    end;

  on E: EIdAlreadyConnected do
    begin
     LastConnected := FALSE;
     Disconnect;
     Inc (FErrorCount);
    end;

  on E: EIdSocketError do
    begin
     // тупо не удалось подключиться
     LogMsg (' in function Connect:~C0C ' + E.Message + '~C07', '~C0C#EXCEPT');
     FLastError := E.Message;
     LastConnected := FALSE;
     Inc (FErrorCount);
    end;

  on E: Exception do
    begin
     LastConnected := FALSE;
     FLastError := E.Message;
     try_again := 5000;
     LogMsg (' in function Connect:~C0C ' + E.Message + '~C07', '~C0C#EXCEPT');
     Inc (FErrorCount);
    end;


 end; // try-except

end; // Connect

function TIdIRCCon.Connected: Boolean;
begin
 // logic is simple - if ConnectorID = 0 then Connect never calls :)
 try
  if GetCurrentThreadId = ConnectorID then
     LastConnected := ( IOHandler <> nil ) and ( not IOHandler.ClosedGracefully ) and
                      ( inherited Connected );
 except
  on E: EIdSocketError do
    begin
     LastConnected := FALSE;
     ODS('[~T]. #EXCEPTION_INDY(' + ClassName + '.Connected): ' + E.Message );
     Windows.Beep (444, 200);
     pt.StartOne (11);
     Inc (FErrorCount);
    end;

  on E: EAccessViolation do
    begin
     LastConnected := FALSE;
     // Not use PrintError!
     OnExceptLog ('~C0A' + ClassName + '.Connected:~C0C '#13#10#9#9 + CFormat('~C0F host: %s, port: %d~C07', '~C0F', [Host, Port]), E);
     Windows.Beep (320, 200);
     Inc (FErrorCount);
    end;
  else
    begin
     LastConnected := FALSE;
     Windows.Beep (222, 200);
     ODS('[~T]. #EXCEPTION: unknown exeception cathed in TIdIRCCon.Connectd.');
     Inc (FErrorCount);
     raise;
    end;


 end;
 result := FLastConnected;
end;

// CheckState

constructor TIdIRCCon.Create(AOwner: TComponent);
begin
 inherited;
 FChnList := TStrMap.Create (self);
 FChnList.OwnsObjects := TRUE;

 pt := TProfileTimer.Create;
 pt.Start ($FFFF);

 FEventParams := TStrMap.Create (self);
 OnConnected := _OnConnect;
 OnDisconnected := _OnDisconnect;
 OnJoin := _OnJoin;
 OnNotice := _OnNotice;
 OnPart := _OnPart;
 OnPrivateMessage := _OnPrivMsg;
 OnServerWelcome :=  _OnWelcome;
 OnRaw := _OnRaw;

 FOnChatEvent := DefOnChatEvent;

 FActiveChannel := FindAddChannel('~private');
 EchoRaw := TRUE;
end;

function TIdIRCCon.DefOnChatEvent;
begin
 result := TRUE;
end;

destructor TIdIRCCon.Destroy;
begin
 if LastConnected then
    Disconnect(ClassName + '.Destroy');

 inherited;

 FreeAndNil (FChnList);
 FreeAndNil (FEventParams);
 pt.Free;
end;

procedure TIdIRCCon.Disconnect(const AReason: String);
var
   safe: Boolean;
   hssl: TIdSSLIOHandlerSocketOpenSSL;
begin
 if Assigned (OwnerThread) and (OwnerThread.ThreadID <> GetCurrentThreadID) then
  begin
   OwnerThread.AddRequest ('DISCONNECT', self);
   OwnerThread.WaitRequests;
   exit;
  end;

 try
  safe := Authorized and LastConnected and (IOHandler <> nil) and (not IOHandler.ClosedGracefully);

  hssl := TIdSSLIOHandlerSocketOpenSSL (IOHandler);

  if (IOHandler is TIdSSLIOHandlerSocketOpenSSL) and (hssl.SSLContext = nil) then
      safe := FALSE;

  if safe then Raw ('QUIT :' + AReason);


  FAutoConnect := FALSE;
  LastConnected := FALSE;
  FReady := FALSE;
  _OnStatusChanged (Ready);
  TIdTCPConnection(self).Disconnect;
 except
  on E: Exception do
     PrintError('Exception catched in ' + ClassName + '.Disconnect: ' + E.Message);
 end;

end;

function TIdIRCCon.GetChannel(const index: String): TChatChannel;
begin
 result := TChatChannel ( FChnList.FindObject (index) );
end;

function TIdIRCCon.GetChannelByIndex(index: Integer): TChatChannel;
begin
 result := TChatChannel ( FChnList.Objects[index] );
end;

function TIdIRCCon.GetChannelID: String;
begin
 result := ActiveChannel.ChannelID;
end;

function TIdIRCCon.GetLastConnected: Boolean;
begin
 result := Assigned (self) and self.FLastConnected;
end;

function TIdIRCCon.IsReady: Boolean;
begin
 result := FALSE;

 try
  result := LastConnected and Connected and Authorized;
 except
  on E: EIdSocketError do
     begin
      ODS('[~T]. #EXCEPTION_INDY(TIdIRCCon.IsReady): ' + E.Message);
      Windows.Beep(1000, 100);
     end;
 end;

 FReady := result;
end;

procedure TIdIRCCon.ReJoin;
var
   n: Integer;
   ch: TChatChannel;
begin
 for n := 0 to FChnList.Count - 1 do
  begin
   ch := TChatChannel ( FChnList.Objects [n] );
   Assert (ch <> nil, 'channel = nil');
   Join (ch.ChannelID, '');
  end;
end;

function TIdIRCCon.FindAddChannel(const AChannel: String): TChatChannel;
begin
 Assert (Assigned (FChnList), ClassName + '.FindAddChannel: FChnList unassigned !');
 result := Channels [AChannel];
 if result = nil then
     begin
      result := TChatChannel.Create (AChannel, self);
      FChnList.AddObject (AChannel, result);
     end;
end; // FindAddChannel

procedure TIdIRCCon._OnConnect(Sender: TObject);
begin
 if Connected then
    Sleep(5);
 EventParams.Lock('_OnConnect');
 try
  EventParams.Clear;
  UserName := UserName + 'ccc';
  OnChatEvent (Sender, self, 'CONNECTED', EventParams);
 finally
  EventParams.Unlock;
 end;

 if LastConnected then
    ODS('[~T]. #DBG(IRCC): Connected with ~C0A' + Host + ':' + IntToStr(Port) + '~C07');
end;

procedure TIdIRCCon._OnDisconnect(Sender: TObject);
begin
 FAuthorized := FALSE;
 LastConnected := FALSE;
 EventParams.Clear;
 OnChatEvent (Sender, self, 'DISCONNECTED', EventParams);
 ODS ('[~T]. #DBG(IRCC): Disconnected.')
end;

procedure TIdIRCCon._OnJoin(ASender: TIdContext; const ANickname, AHost, AChannel: String);
begin
 FActiveChannel := FindAddChannel (AChannel);
 EventParams.Lock('_OnJoin');
 try
  EventParams.Clear;
  EventParams.Add ('NICK=' + ANickname);;
  EventParams.Add ('HOST=' + AHost);
  EventParams.AddObject ('CHANNEL=' + AChannel, ActiveChannel);
  OnChatEvent (ASender, self, 'JOIN_CHANNEL', EventParams);
 finally
  EventParams.Unlock;
 end;
end;

procedure TIdIRCCon._OnNotice(ASender: TIdContext; const ANicknameFrom, AHost, ANicknameTo, ANotice: String);
begin
 EventParams.Lock('_OnNotice');
 try
  EventParams.Clear;
  EventParams.Add ('FROM=' + ANicknameFrom);;
  EventParams.Add ('HOST=' + AHost);
  EventParams.Add ('TO=' + ANicknameTo);;
  EventParams.Add ('NOTICE=' + ANotice);
  OnChatEvent (ASender, self, 'NOTICE', EventParams);
 finally
  EventParams.Unlock;
 end;
end;

procedure TIdIRCCon._OnPart(ASender: TIdContext; const ANickname, AHost, AChannel, APartMessage: String);
begin
 EventParams.Lock('_OnPart');
 try
  EventParams.Clear;
  EventParams.Add ('NICK=' + ANickname);;
  EventParams.Add ('HOST=' + AHost);
  EventParams.Add ('CHANNEL=' + AChannel);
  EventParams.Add ('PMSG=' + APartMessage);
  OnChatEvent (ASender, self, 'LEAVE_CHANNEL', EventParams);
 finally
  EventParams.Unlock;
 end;
end;

procedure TIdIRCCon._OnPrivMsg(ASender: TIdContext; const ANicknameFrom, AHost, ANicknameTo, AMessage: String);
var
   ch: TChatChannel;
   mi: TChatMsgInfo;
begin
 if Pos('#', ANicknameTo) = 1 then
    ch := FindAddChannel (ANicknameTo)
 else
    ch := Channels['~private'];

 mi := ch.AddMsg (ANicknameFrom, AHost, ANickNameTo, AMessage);
 mi.IsIncoming := TRUE;
 mi.IsPrivate := TRUE;
 EventParams.Lock('PM');
 try
  EventParams.Clear;
  EventParams.AddObject ('MSG_OBJ', mi);
  OnChatEvent (ASender, self, 'CHAT_MESSAGE', EventParams);
 finally
  EventParams.Unlock;
 end;
end;

procedure TIdIRCCon._OnRaw(ASender: TIdContext; AIn: Boolean; const AMessage: String);
begin
 if EchoRaw then
    ODS('[~T]. #RAW(IRCC' + '):~C0E ' + AMessage + '~C07');
 if Assigned (EventParams) then
 try
  EventParams.Lock ('OnRaw');
  try
   EventParams.Clear;
   EventParams.Add ('MSG=' + AMessage);
   OnChatEvent (ASender, self, 'RAW', EventParams);
  finally
   EventParams.Unlock;
  end;
 except
  on E: Exception do
    OnExceptLog(ClassName + '._OnRaw/OnChatEvent MSG = ' + AMessage , E);
 end;
end;

procedure TIdIRCCon._OnStatusChanged(bReady: Boolean);
var
   i: Integer;
   ch: TChatChannel;
begin
 if bReady then
  for i := 0 to FChnList.Count - 1 do
   begin
    ch := TChatChannel (FChnList.Objects[i]);
    ch.TestJoin(self);
   end;
 EventParams.Lock ('StCh');
 try
  EventParams.Clear;
  EventParams.Add ('READY=' + IfV (bReady, '1', '0'));
  OnChatEvent (nil, self, 'STATUS_CHANGED', EventParams);
 finally
  EventParams.Unlock;
 end;
end; // _OnStatusChanged

function TIdIRCCon._OnVerifyPeer(Certificate: TIdX509; AOk: Boolean; ADepth: Integer): Boolean;
begin
 EventParams.Lock ('VP');
 try
  EventParams.Clear;
  result := OnChatEvent (Certificate, self, 'VERIFY_PEER', EventParams);
 finally
  EventParams.Unlock;
 end;
end;

procedure TIdIRCCon._OnWelcome(ASender: TIdContext; const AMsg: String);
begin
 FAuthorized := TRUE;
 EventParams.Clear;
 EventParams.Add ('MSG=' + AMsg);
 ODS('[~T]. #DBG(IRCC): Server Welcome: ~C0A' + AMsg + '~C07');
 OnChatEvent (ASender, self, 'WELCOME', EventParams);
 pt.StartOne (2); // Ready timer
 AlivePing;
end;

// OnPrivateMessage
procedure TIdIRCCon.LogMsg(const msg: String; prefix: String);
begin
 ODS('[~d ~T]. ' + prefix + '(' + ClassName + '): ' + msg);
end;

procedure TIdIRCCon.SafeDisconnect;
var
   lt: TIdThread;
   th: THandle;
   s: String;
   i: Integer;
begin
 AutoConnect := FALSE;
 Authorized := FALSE;
 FReady := FALSE;
 _OnStatusChanged (Ready);

 lt := nil;
 if ( IOHandler <> nil ) and ( IOHandler.Connected ) then
 try
  try
   lt := FListeningThread;
   th := 0;

   if Assigned (lt) then
     begin
      // Raw('/ping ' + Host); // callback reaction needs
      th := lt.Handle;
      if lt.Loop then
         lt.Stop
      else
         lt.Terminate;

      Ping (Host);
     end;

   if th <> 0 then
    begin
     WaitForSingleObject (th, 500);
     Raw('QUIT :SafeDisconnect');
     s := IOHandler.InputBufferAsString;
     ODS ('[~T]. #DBG: Input buffer rest: ~C0F{ ' + s + ' }~C07');
     TIdTCPConnection (self).Disconnect;
    end;

   if th <> 0 then
     begin
      for i := 1 to 10 do
       if WaitForSingleObject (th, 500) = WAIT_TIMEOUT then
         begin
          ODS('[~T]. #DBG(IRCC): ListeningThread termination timeout = ' + IntToStr(i * 500) );
          Application.ProcessMessages;
          if i >= 9 then
             TerminateThread (th, $200); // forced terminate
         end
       else
          break;
     end;

   if (not IOHandler.ClosedGracefully) and IOHandler.Connected then
     begin
      IOHandler.CloseGracefully;
      IOHandler.Close;
      IOHandler.InputBuffer.Clear;
     end;

   // IOHandler := nil;



  except
   on E: Exception do
      OnExceptLog ('SafeDisconnect', E);
  end;
 finally
  if Assigned(FListeningThread) then
     FreeAndNil ( FListeningThread );
 end;
end; // SafeDisconnect

procedure TIdIRCCon.SetChannel(const Value: String);
begin
 Assert ( Assigned(self), ClassName + '.SetChannel self unassigned!');

 if Channel <> Value then
  try
    FActiveChannel  := FindAddChannel(Value);
  except
   on E: Exception do
      OnExceptLog ( ClassName + '.SetChannel', E );
  end;
end;

procedure TIdIRCCon.SetLastConnected(const Value: Boolean);
begin
  if Value = FLastConnected then exit;
  FLastConnected := Value;
  ODS('[~T/~I]. #DBG: SetLastConnected (' + IfV (Value, 'TRUE', 'FALSE') + '). conn_time = ' + ftow(pt.Elapsed(35) / 1000,  '%.1f sec'));
end;

{ TClientIRCThread }

function TClientIRCThread.ConCount: Integer;
begin
 result := FConList.Count;
end;

function TClientIRCThread.GetCon(index: Integer): TIdIRCCon;
begin
 result := TIdIRCCon (FConList.Objects [index]);
end;

function TClientIRCThread.MakeCon;
var
   iossl: TIdSSLIOHandlerSocketOpenSSL;
begin
 result := TIdIRCCon.Create (nil);
 if bSSL then
  try
   iossl := TIdSSLIOHandlerSocketOpenSSL.Create (result); // TODO: owning problems here?
   // iossl.Port := APort;
   iossl.OnVerifyPeer := _OnVerifyPeer;
   iossl.SSLOptions.SSLVersions := [sslvSSLv23];
   iossl.SSLOptions.Mode := sslmClient;
   result.IOHandler := iossl;
  except
   on E: Exception do
     OnExceptLog (ClassName + '.MakeCon', E);
  end;

 result.UseSSL := bSSL;
 result.Host := AHost;
 result.Nickname := ANick;
 result.Password := APasswd;
 result.Port := APort;
 if result.IOHandler <> nil then
    result.IOHandler.DefStringEncoding := RussianEnc;
 AddRequest('REG_CON', result);
end;

procedure TClientIRCThread.PrivMsg(const AMsg: String; ircc: TIdIRCCon; ATo: String = '');
var
   mi: TChatMsgInfo;
begin
 if (Assigned(ircc) and ircc.LastConnected and ircc.Authorized) then
   begin
    mi := TChatMsgInfo.Create;
    mi.owner := nil;
    mi.ircc := ircc;
    mi.Text := AMsg;
    mi.NNTo := ATo;
    mi.NNFrom := '$self$';
    AddRequest('CHAT_MSG', mi);
   end;

end;

procedure TClientIRCThread.ProcessInit;
begin
 inherited;
 FConList := TStrMap.Create (self);
 FConList.OwnsObjects := TRUE;
end;

function TClientIRCThread.ProcessRequest(const rqs: String; rqobj: TObject): Integer;
var
   con: TIdIRCCon;
   mi: TChatMsgInfo;
   a, b: String;
   i: Integer;
begin

 result := inherited ProcessRequest (rqs, rqobj);

 if not Assigned (rqobj) then exit;

 mi := nil;

 if ( Pos('CHAT_MSG', rqs) = 1 )  and Assigned (rqobj) then
    try
     mi := TChatMsgInfo (rqobj);
     con := mi.ircc;
     if (con.IsReady) then else exit;

     a := mi.NNTo;
     b := mi.Text;

     if a = '' then
        a := con.Channel;



     try
      if Pos('#NOTICE:', b) > 0 then
         con.Notice (a, b)
      else
         con.Say (a, b);
     except
      on E: Exception do
         OnExceptLog('CHAT_MSG#1 a = ' + a + ', b = ' + b, E);
     end;

    finally
     mi.Free;
    end;


 if not (rqobj is TIdIRCCon) then exit;

 con := TIdIRCCon (rqobj);

 if rqs = 'REG_CON' then
  begin
   FConList.Lock ('AddCon');
   FConList.AddObject(con.ID, con);
   FConList.Unlock;
  end;

 if rqs = 'FREE_CON' then
  try
   FConList.Lock ('FreeCon');
   i := FConList.IndexOfObject (con);
   if i >= 0 then
    begin
     FConList.Objects [i] := nil;
     FreeAndNil (con);

     FConList.Delete (i);
    end;
   FConList.Unlock;
   exit;
  except
   on E: Exception do
      OnExceptLog(ThreadName + '.ProcessRequests/FREE_CON', E);
  end;

 if con.LastConnected then
  begin
   SplitPair (':', rqs, a, b);


   if 'ALIVE_PING' = rqs then
       con.AlivePing;

   if ( Pos('PART:', rqs) = 1 )  then
    begin
     if ( con.ActiveChannel <> nil ) and ( Pos ('#', con.Channel) = 1 ) then
       for i := 0 to con.ChannelsCount - 1 do
         with con.ChannelByIndex [i] do
          if not IsPrivate then
              con.Part (ChannelID, b);
     con.FActiveChannel := nil;
    end;

   if ( Pos('SET_CHANNEL:', rqs) = 1 ) then
      con.Channel := b;

   if ( rqs = 'DISCONNECT' )  then
      con.SafeDisconnect;

  end
 else
  begin
   if ( rqs = 'CONNECT' ) then
        con.Connect;
  end;




end;

procedure TClientIRCThread.ProcessThreadStop;
begin
 FreeAndNil (FConList);
 inherited;
end;

procedure TClientIRCThread.SetChannel(const ch: String; ircc: TIdIRCCon);
begin
 AddRequest('SET_CHANNEL:' + ch, ircc);
end;

procedure TClientIRCThread.WorkProc;
var
   i: Integer;
   con: TIdIRCCon;
begin
 inherited;
 if (not Assigned(self)) or Stopped then exit;

 if Assigned (FConList) and (self.wp_exec_cnt mod 100 = 0) then
 for i := 0 to ConCount - 1 do
  begin
   con := ConList [i];
   con.CheckState;
  end;
end;

function TClientIRCThread._OnVerifyPeer(Certificate: TIdX509; AOk: Boolean; ADepth, AError: Integer): Boolean;
begin
 result := TRUE;
end;

{ TChatChannel }

function TChatChannel.AddMsg;
begin
 Lock ('AddMsg');
 try
  result := TChatMsgInfo.Create;
  result.NNFrom := ANNFrom;
  result.NNTo := ANNTo;
  result.Host := AHost;
  result.Text := AMsg;
  result.owner := self;
  AddObject ('#' + IntToStr(Count + 1), result);
 finally
  Unlock;
 end;
end;

constructor TChatChannel.Create(AID: String; AOwner: TIdIRCCon);
begin
 inherited Create (AOwner);
 ChannelID := AID;
 TestJoin (AOwner);
end;

destructor TChatChannel.Destroy;
begin
  FreeObjects;
  inherited;
end;

procedure TChatChannel.GetMsgs;
begin
 Lock ('GetMsgs');
 try
  dst.Assign (self);
  if bClear then Clear;
 finally
  Unlock;
 end;
end;

function TChatChannel.IsPrivate: Boolean;
begin
 result := Pos('#', ChannelID) <> 1;
end;

procedure TChatChannel.TestJoin(con: TIdIRCCon);
begin
 // ODS('[~T]. #DBG(IRCC): Trying join channel ~C0A' + ID + '~C07, con.Connected = ' + IfV( con.Connected, 'Yes', 'No') );
 with con do
  if Ready and ( Pos('#', ChannelID) = 1 ) then
     Join (ChannelID);
end;


initialization
 RussianEnc := TMBCSEncoding.Create (1251); // TUnicodeEncoding.Create;
finalization
 if Assigned( gIRCThread ) then
  begin
   gIRCThread.FreeOnTerminate := FALSE;
   gIRCThread.StopThread;
   gIRCThread.WaitStop(150);
   FreeAndNil (gIRCThread);
  end;
 FreeAndNil (RussianEnc);
end.
