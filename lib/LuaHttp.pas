unit LuaHttp;

interface
uses Windows, SysUtils, Classes, StrUtils, Misc, StrClasses, Lua5, LuaTypes, LuaTools, LuaWebPacket;


function _IdHttp_Index ( L: lua_State ): Integer; cdecl;
function _IdHttp_NewIndex ( L: lua_State ): Integer; cdecl;

implementation
uses IdComponent, IdSSLOpenSSLHeaders, IdSSLOpenSSL, IdcTypes, IdException, IdHttp, IdIOHandler, IdStack;

procedure MakeSSL(ih: TIdHttp);
begin
 if ( ih.IOHandler <> nil ) and ( ih.IOHandler is TIdSSLIOHandlerSocketOpenSSL ) then exit;
 ih.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create (ih);
end;


function _IdHttp_AddHeader ( L: lua_State ): Integer; cdecl;
const
   BAD_SUB = ':'#13#10' ';

var
   ih: TIdHttp;
   hn: String;
   hv: String;

begin
 ih := lua_objptr (L, 1);
 hn := LuaStrArg (L, 2);
 hv := LuaStrArg (L, 3);

 result := 0;

 if Assigned (ih.Request) and Assigned (ih.Request.CustomHeaders) then
 with ih.Request.CustomHeaders do
   begin
    AddValue (hn, hv);
    hv := Text;
    if Pos(BAD_SUB, hv) = 0 then exit;
    // wprintf('[~T]. #DBG: strange headers  "%s" ', [hv]);
    Text := AnsiReplaceStr(hv, BAD_SUB, ': ');
   end;


end;


function sync_http_rqs (L: lua_State; post_params: Boolean ): Integer;

var
   uri: String;
   rqs: UTF8String;
   rss: AnsiString;
   rqd: TStream;
   res: TStream;
    bs: TBytes;
    rr: String;
    ss: String;
    rs: String;
    ih: TIdHttp;
     i: Integer;


begin
 ih := lua_objptr (L, 1);
 uri := LuaStrArg (L, 2);


 rqd := TMemoryStream.Create;
 res := TMemoryStream.Create;
 try
  rr := '';
  i := lua_type (L, 3);
  if post_params then
    case i of
     LUA_TSTRING:
        begin
         rqs := UTF8Encode (  LuaStrArg(L, 3) );
         if rqs <> '' then rqd.Write ( rqs[1], Length(rqs) ); // pushing POST data
        end;
     LUA_TUSERDATA:
        begin
         bs := lua_tobytes (L, 3);
         if Length (bs) > 0 then
            rqd.Write ( bs [0], Length(bs) );
         SetLength ( bs, 0 );
        end
     else
        wprintf('[~T].~C0C #WARN(post):~C07 type of post-param =~C0D %d~C07', [i] );
    end;

  if Pos('https:', uri) > 0 then
     MakeSSL (ih);

  try
   if post_params then
     begin
      // wprintf('[~T].~CE0 #RAW_HEADERS: ~C0F '#13#10'%s~C07', [ih.Request.RawHeaders.Text] );
      // wprintf('[~T].~CF0 #USR_HEADERS: ~C0F '#13#10'%s~C07', [ih.Request.CustomHeaders.Text] );
      ih.Post (uri, rqd, res);
     end
   else
      ih.Get (uri, res);
  except
   on E: EAccessViolation do
      rr := '#FATAL: AV exception while TIdHttp.Get/Post ccall ' + E.Message;
   on E: EIdSocketError do
      rr := '#FATAL: Connection timeout to ' + uri;
   on E: EIdException do
      rr := '#EXCEPT: class = ' + E.QualifiedClassName + ', E.message: ' + E.Message + ', response: '#13#10 + ih.Response.RawHeaders.GetText;
  end;

  if ( res.Size > 0 ) then
     begin
      // loading string from stream
      SetLength ( rss, res.Size );
      res.Position := 0;
      res.Seek (0, soBeginning);
      res.ReadBuffer ( rss [1], res.Size );

      rr := AnsiReplaceStr ( UTF8ToWideString (rss), #0, '\0' );
      repeat
       i := Pos('\u0', rr);
       if i = 0 then break;
       ss := Copy ( rr, i + 2, 4 );
       rs := Char ( atoi ( '$' + ss ) );
       rr := AnsiReplaceStr ( rr, '\u' + ss, rs );
      until FALSE;

     end;

 finally
  rqd.Free;
  res.Free;
 end;

 lua_pushwstr (L, rr);
 result := 1;
end;

function _IdHttp_Get ( L: lua_State ): Integer; cdecl;
begin
 result := sync_http_rqs (L, FALSE);
end;


function _IdHttp_Post ( L: lua_State ): Integer; cdecl;
begin
 result := sync_http_rqs (L, TRUE);
end;

function _IdHttp_Index ( L: lua_State ): Integer; cdecl;
var
   key: String;
   res: String;
    rf: lua_CFunction;
    ih: TIdHttp;

begin
 result := 1;
 ih := lua_objptr (L, 1);
 key := LuaStrArg (L, 2);
 res := 'wrong key ' + key;

 rf := nil;
 if key = 'free_object' then
    rf := LuaFreeObject
 else
 if key = 'host' then
    res := ih.URL.Host
 else
 if key = 'URL' then
    res := ih.URL.URI
 else
 if key = 'add_header' then
    rf := _IdHttp_AddHeader
 else
 if key = 'get' then
    rf := _IdHttp_Get
 else
 if key = 'post' then
    rf := _IdHttp_Post;


 if @rf <> nil then
   lua_pushcfunction (L, rf)
 else
   lua_pushwstr (L, res);
end;


function _IdHttp_NewIndex ( L: lua_State ): Integer; cdecl;
var
   key: String;
   val: String;
    ih: TIdHttp;

begin
 result := 0;
 ih := lua_objptr (L, 1);
 key := LuaStrArg (L, 2);
 val := LuaStrArg (L, 3);

 if key = 'host' then
   begin
    ih.URL.Host := val;
    ih.Request.Host := val;
   end
 else
 if key = 'URL' then
    ih.URL.URI := val
 else
 if key = 'content_encoding' then
    ih.Request.ContentEncoding := val
 else
 if key = 'content_type' then
    ih.Request.ContentType := val
 else
 if key = 'connect_timeout' then
    ih.ConnectTimeout := atoi (val)
 else
 if key = 'read_timeout' then
    ih.ReadTimeout := atoi (val)
 else
 if key = 'user_agent' then
    ih.Request.UserAgent := val

end;

end.
