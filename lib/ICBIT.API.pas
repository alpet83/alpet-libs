unit ICBIT.API;

interface

uses Windows, SysUtils, StrUtils, Classes, Misc, sgcWebSocket_Classes, sgcWebSocket_Client, sgcWebSocket_CustomClient, sgcWebSocket_Client_SocketIO, sgcWebSocket, DBXJSON,
                        IdComponent, IdSSLOpenSSLHeaders, IdSSLOpenSSL, IdcTypes, IdException, IdHttp, IdIOHandler, IdStack,
                                DateTimeTools, JSONBurse.API, AuthDlg;


type

  TIcbitConnection = class (TBurseConnection)
  private
      FSubList: TStrings;
    FOriginSfx: String;

  protected
   function     DefaultOrigin (const path: String = '/socket.io/1'): String; override;
   procedure    DoDisconnect; override;
   // event handler
   procedure    socket_Connect (Connection: TsgcWSConnection); override;
   procedure    HandleNewSession (Sender: TObject);
  public

   property     SubList: TStrings read FSubList;
   property     URLParams: String read FOriginSfx write FOriginSfx;
   { C & D }
   constructor  Create (const AServerHost: String; APort: WORD);
   destructor   Destroy; override;
   { methods }
   procedure    Subscribe (const channel, data, idKey: String); override;
   procedure    LoadAuthInfo; override;
  end;


implementation
uses Vcl.Forms, JSONTools, Soap.EncdDecd, Base64Tools, IniFiles;


{ TMtGoxConnector }



constructor TIcbitConnection.Create;
begin
 inherited Create ( AServerHost, Aport );
 BurseName := 'icbit';
 FSubList := TStringList.Create;
 URLParams := '?';
 Client.OnNewSession := HandleNewSession;
 ScriptName := AddSlash ( misc.DllPath ) + 'IcbitBTI.lua';

 ODS('[~T/~I]. #NEW_CONN_CREATE(ICBIT)');
end;

function TIcbitConnection.DefaultOrigin (const path: String): String;
begin
 result := inherited DefaultOrigin (path);

 if ( LastChar (result) <> '/' ) and ( Pos('/', URLParams) <> 1 ) then
      result := result + '/';

 result := result + URLParams;
 Assert ( result <> '' );
end;

destructor TIcbitConnection.Destroy;
begin
 FSubList.Free;
 inherited;
end;

procedure TIcbitConnection.DoDisconnect;
begin
 Client.WriteData( '0::/' + BurseName );
 inherited;
end;

procedure TIcbitConnection.HandleNewSession(Sender: TObject);
begin
 with Client.Options do
  if ( URLParams <> '' ) and ( Pos (URLParams, Parameters) = 0 ) then
      begin
       // Parameters := '/';
       Context.LogMsg('[~T]. #DBG: Client.Options.Origin     =~C0A ' + Origin + '~C07');
       Context.LogMsg('[~T]. #DBG: Client.Options.Parameters =~C0A ' + Parameters + '~C07');

      end;
end;

procedure TIcbitConnection.LoadAuthInfo;
 var
   fini: TIniFile;
  fconf: String;

      s: String;
      t: String;

begin
 inherited;
 fconf := FindConfigFile ( BurseName + '.conf' );
 if not FileExists (fconf) then exit;
 fini := TIniFile.Create (fconf);
 try
  s := fini.ReadString ('keys', 'TradeKey', '?');
  if not fini.SectionExists (s) then
     begin
      PrintError ( BurseName + '.conf parsing: section not found ' + s);
      exit;
     end;

  repeat
   t := fini.ReadString ( s, 'AuthKey', '?');
   if Pos('*', t) = 0 then break;

   AskAuthInfo ( 'icbit', s, fini );
  until FALSE;



  FAuthInfo.Values ['auth_key'] := t;
  t := fini.ReadString ( s, 'UserID', '?');
  FAuthInfo.Values ['user_id'] := t;
 finally
  fini.Free;
 end;

 URLParams := '?AuthKey=' + AuthInfo['auth_key'] + '&UserId=' + AuthInfo['user_id'] + '&t=' + IntToStr ( DateTimeToUnixTime32 ( Now ) );
 Client.Options.Origin := DefaultOrigin;
 Client.Options.Parameters := '/' + URLParams;
 { if Assigned (Client.Authentication) then
      FClient.Authentication.Password := OriginSfx; }
end;

procedure TIcbitConnection.socket_Connect(Connection: TsgcWSConnection);
begin
 inherited socket_Connect (Connection);
 Client.WriteData('1::/' + BurseName);
end;

procedure TIcbitConnection.Subscribe;
var
   rqs: String;
begin
 //
 rqs := '4::/' + BurseName + ':{"op":"subscribe"';
 rqs := rqs + ',"channel":"' + channel + '"';
 rqs := rqs + '}';
 Context.LogMsg('[~T].~C0F #SUBSCRIBE: rqs = ~C0E' + rqs + '~C07');
 FClient.WriteData (rqs); // will be autoencoded in UTF8
end;



initialization
 //
 IdSSLOpenSSLHeaders.Load;

end.
