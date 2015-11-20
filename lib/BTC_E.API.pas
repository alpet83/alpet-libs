unit BTC_E.API;

interface
uses Windows, SysUtils, StrUtils, Classes, Misc, sgcWebSocket_Classes, sgcWebSocket_Client, sgcWebSocket_CustomClient, sgcWebSocket_Client_SocketIO, sgcWebSocket, DBXJSON,
                        IdComponent, IdSSLOpenSSLHeaders, IdSSLOpenSSL, IdcTypes, IdException, IdHttp, IdIOHandler, IdStack,
                        Lua5, LuaTypes, LuaTools, LuaWebPacket,
                                DateTimeTools, JSONBurse.API, AuthDlg;


type
    TBTCeConnection = class (TBurseConnection)
    protected

     procedure    socket_Connect (Connection: TsgcWSConnection); override;
    public

     { C & D }
     constructor  Create (const AServerHost: String; APort: WORD);
     destructor   Destroy; override;

     { methods }

     function     CommitRequest ( const rqs: String ): String; override;
     function     Connect: Boolean; override;

     procedure    LoadAuthInfo; override;
     procedure    PushAuthParams (L: lua_State); override;
    end;

implementation
uses IniFiles, Base64Tools, Soap.EncdDecd;

{ TBTCeConnection }

function TBTCeConnection.CommitRequest(const rqs: String): String;
begin
 Engine.Lock('CommitRqs');
 Engine.VarSet ('request', rqs);
 Engine.CallFuncEx ('commit_request', 'request', '');
 Engine.Unlock;
 result := 'OK';
end;

function TBTCeConnection.Connect: Boolean;
begin
 result := TRUE; // TODO: add auth-check
 FConnected := TRUE;
end;

constructor TBTCeConnection.Create(const AServerHost: String; APort: WORD);
begin
 inherited Create ( AServerHost, Aport );
 BurseName := 'btc-e';
 ScriptName := AddSlash ( misc.DllPath ) + 'btc-e.lua';

end;

destructor TBTCeConnection.Destroy;
begin

  inherited;
end;

procedure TBTCeConnection.LoadAuthInfo;
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
  t := fini.ReadString (s, 'API_Key', '?');
  FAuthInfo.Values ['API_key'] := t;
  t := fini.ReadString (s, 'secret', '?');
  FAuthInfo.Values ['secret'] := t;
 finally
  fini.Free;
 end;

end;


procedure TBTCeConnection.PushAuthParams(L: lua_State);
var
   sc: String;
   bs: TBytes;

begin
 sc := AuthInfo ['secret'];
 if Length(sc) > 7 then
   begin
    bs := Hex2Bytes ( sc );
    _buffer_instance (L, bs );
    lua_sf (L, 'secret');
   end;

 sc := AuthInfo ['API_key'];
 if Length(sc) > 10 then
   begin
    lua_pushwstr (L, sc);
    lua_sf (L, 'API_key');
   end;
end;

procedure TBTCeConnection.socket_Connect(Connection: TsgcWSConnection);
begin
 // inherited;
 FConnected := TRUE;

end;

end.
