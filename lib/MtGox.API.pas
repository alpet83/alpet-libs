unit MtGox.API;

interface

uses Windows, SysUtils, StrUtils, Classes, Misc, LuaTypes, Lua5, LuaTools, LuaWebPacket,
                        sgcWebSocket_Classes, sgcWebSocket_Client, sgcWebSocket_CustomClient, sgcWebSocket_Client_SocketIO, sgcWebSocket, DBXJSON,
                        IdComponent, IdSSLOpenSSLHeaders, IdSSLOpenSSL, IdcTypes, IdException, IdHttp, IdIOHandler, IdStack,
                                DateTimeTools, JSONBurse.API;

const
   CHID_TRADES = 'dbf1dee9-4f2e-4a08-8cb7-748919a71b21';
   CHID_TICKER = 'd5f06780-30a8-4a48-a2f8-7ed181b4a13f';
    CHID_DEPTH = '24e67e0d-1cad-4cc0-9e7a-f8523ef460fe';

type
  TMtGoxConnection = class;

  TMtGoxConnection = class (TBurseConnection)
  private
      FSubList: TStrings;
    FOriginSfx: String;
       FAPIKey: TBytes;
    function GetAPIkey: TBytes;    // same as PrivateKey, but binary


  protected
   function     DefaultOrigin (const path: String = 'socket.io/1/'): String; override;
   procedure    DoDisconnect; override;
   // event handler
   procedure    socket_Connect (Connection: TsgcWSConnection); override;
   procedure    HandleNewSession (Sender: TObject);
  public

   property     APIKey: TBytes read GetAPIkey;
   property     SubList: TStrings read FSubList;
   property     OriginSfx: String read FOriginSfx write FOriginSfx;
   { C & D }
   constructor  Create (const AHttpServer, AServerHost: String; APort: WORD);
   destructor   Destroy; override;
   { methods }
   procedure    Subscribe (const channel, data, idKey: String); override;
   procedure    LoadAuthInfo; override;
   procedure    PushAuthParams (L: lua_State); override;

  end;


implementation
uses Vcl.Forms, JSONTools, Soap.EncdDecd, Base64Tools, IniFiles;


{ TMtGoxConnector }



constructor TMtGoxConnection.Create;
var
   i: Integer;
begin
 inherited Create ( AServerHost, Aport );
 HttpServer := AHttpServer;
 BurseName := 'mtgox';
 i := Pos('.', HttpServer);
 if i > 0 then
    BurseName := Copy (HttpServer, 1, i - 1);

 FSubList := TStringList.Create;
 OriginSfx := '?Currency=EUR,USD,RUB';
 Client.OnNewSession := HandleNewSession;
 Context.LogMsg ('[~T/~I]. #NEW_SOCKET');
end;

function TMtGoxConnection.DefaultOrigin (const path: String): String;
begin
 result := inherited DefaultOrigin (path) + OriginSfx;
end;

destructor TMtGoxConnection.Destroy;
begin
 FSubList.Free;
 SetLength (FAPIkey, 0);
 inherited;
end;

procedure TMtGoxConnection.DoDisconnect;
begin
 Client.WriteData('0::/mtgox');
 inherited;
end;

function TMtGoxConnection.GetAPIkey: TBytes;
begin
 if Length(FAPIKey) = 0 then
    LoadAuthInfo;

 result := FAPIKey;
end;

procedure TMtGoxConnection.HandleNewSession(Sender: TObject);
begin
 with Client.Options do
  if ( OriginSfx <> '' ) and ( Pos (OriginSfx, Parameters) = 0 ) then
      begin
       Parameters := Parameters + OriginSfx;
       Context.LogMsg ('[~T]. #DBG: Client.Options.Parameters =~C0A ' + Parameters + '~C07');
      end;
end;

procedure TMtGoxConnection.LoadAuthInfo;
 var
   fini: TIniFile;
  fconf: String;
      s: String;
      t: String;
      i: Integer;

begin
 inherited;
 fconf := FindConfigFile('mtapi.conf');
 if not FileExists (fconf) then exit;
 fini := TIniFile.Create (fconf);
 try
  s := fini.ReadString ('keys', 'TradeKey', '?');
  if not fini.SectionExists (s) then
     begin
      PrintError ('mtapi.conf parsing: section not found ' + s);
      exit;
     end;
  t := fini.ReadString ( s, 'private', '?');
  FAuthInfo.Values ['private'] := t;
  t := fini.ReadString ( s, 'secret', '?');
  FAuthInfo.Values ['secret'] := t;
 finally
  fini.Free;
 end;


 s := AuthInfo ['private'];
 Assert ( s <> '', 'AuthInfo.private not assigned!');
 s := AnsiReplaceStr ( s, '-', '' );
 i := Length (s);
 SetLength ( FAPIKey, i div 2 );
 i := 0;

 while Length (s) > 0 do
  begin
   t := Copy (s, 1, 2);
   FAPIKey [i] := atoi ( '$' + t );
   Delete (s, 1, 2);
   Inc (i);
  end; // while


end;

procedure TMtGoxConnection.PushAuthParams(L: lua_State);
var
   sc: String;
   cb: Integer;

begin
 inherited;
  cb := Length (APIKey);
  if cb < 10 then
    begin
     PrintError('API Key size = ' + IntToStr( cb ) );
     exit;
    end;

 sc := AuthInfo ['Secret'];
 if Length(sc) > 5 then
   begin
    _buffer_instance (L, DecodeBase64( AnsiString(sc) ) );
    lua_sf (L, 'secret');
   end;

 _buffer_instance (L, APIKey);
 lua_sf (L, 'api_key');

end;

procedure TMtGoxConnection.socket_Connect(Connection: TsgcWSConnection);
begin
 inherited socket_Connect (Connection);
 Client.WriteData('1::/mtgox');
end;

procedure TMtGoxConnection.Subscribe(const channel, data, idKey: String);
var
   rqs: String;
begin
 rqs := '4::/' + BurseName + ':{"op":"' + BurseName + '.subscribe"';
 if data <> '' then
    rqs := rqs + ',"type":"' + data + '"';
 if channel <> '' then
    rqs := rqs + ',"channel":"' + channel + '"';
 if idKey <> '' then
    rqs := rqs + ',"key":"' + idKey + '"';
 //

 rqs := rqs + '}';
 Context.LogMsg ('[~T].~C0F #SUBSCRIBE: rqs = ~C0E' + rqs + '~C07');
 FClient.WriteData (rqs); // will be autoencoded in UTF8
end;



initialization
 //
 IdSSLOpenSSLHeaders.Load;

end.
