unit LuaWebPacket;

interface


// WARN: LuaImport must be defined as unit alias for Lua5 or XrayLua in project settings
uses Windows, SysUtils, Classes, StrUtils,
        Misc, StrClasses, DateTimeTools, LuaTypes,
        LuaImports, LuaTools, Base64Tools,
        IdGlobal, IdHMACSHA1;


type

    TWebPacket = class (TMemoryStream) // for sending messages in UTF8

    public


     procedure  PushInstance (L: lua_State; gc: Boolean ); // push object with metatable into lua stack

     function   WriteStr ( const ws: String ): LongInt; overload; inline;
     function   WriteStr ( const us: UTF8String ): LongInt; overload;

    end;


function Hex2Bytes ( s: String ): TIdBytes;
function  lua_tobytes(L: lua_State; idx: Integer): TIdBytes;
function  _base64_decode (L: lua_state): Integer; cdecl;
function  _base64_encode (L: lua_state): Integer; cdecl;
function  _buffer_index (L: lua_state): Integer; cdecl;
procedure _buffer_instance (L: lua_State; const b: TIdBytes);
function  _data_buffer (L: lua_state): Integer; cdecl;
function  _utf8_buffer (L: lua_state): Integer; cdecl;
function  _web_packet (L: lua_state): Integer; cdecl;

procedure ExportAll (L: lua_State; disp: Boolean);

implementation
uses Soap.EncdDecd;


function DecodeBase64 (ps: AnsiString): TIdBytes;
var
   tmp: TBytes;

begin
 tmp := Soap.EncdDecd.DecodeBase64(ps);
 SetLength (result, Length(tmp) + 1);
 Move ( tmp[0], result[0], Length(tmp));
 result[Length(tmp)] := 0;
end;


const
   BB_HEADER_SIZE = 2 * sizeof(NativeInt);
   BB_SUFFIX_SIZE = 4;


type
   TBytesBuffer = record
    size: NativeInt;
    used: NativeInt;
    data: packed array [0..MAXINT div 2 - 1] of BYTE;
   end;

   PBytesBuffer = ^TBytesBuffer;


function DumpBuffer ( pb: PBytesBuffer ): String;
var n: Integer;
begin
 result := '';
 for n := 0 to pb.used - 1 do
     result := result + IntToHex ( pb.data[n], 2 );
 result := LowerCase (result);
end;

function Hex2Bytes ( s: String ): TIdBytes;
var
   n, cnt: Integer;
begin
 cnt := Length(s) div 2;
 SetLength ( result, cnt );
 for n := 0 to cnt - 1 do
     result [n] := atoi ( '$' + Copy (s, 1 + n * 2, 2) );

end;

function _check_buffer (p: PBytesBuffer; ctx: String): Boolean;
var
   i: Integer;
   s: String;
   d: String;
begin
 result := FALSE;
 if (p <> nil) and not IsBadReadPtr(@p.data[p.size], 4) then
    try
     result := ( p.data[p.size + 2] = BYTE('O') ) and  ( p.data[p.size + 3] = BYTE('B') );
     if not result then
       begin
        d := '';
        for i := 0 to p.size + 3 do
         if p.data[i] in [$20..$B6] then
            d := d + WideChar(p.data[i])
         else
            d := d + '\' + IntToStr(p.data[i]);

        PrintError( FormatPtr(p) + ctx + ' damaged data buffer ' + ': ' + d);
       end;
    except
     on E: Exception do
        OnExceptLog('_check_buffer', E);
    end;

end;

function  lua_tobytes(L: lua_State; idx: Integer): TIdBytes;
var
   p: PBytesBuffer;
begin
 p := lua_objptr (L, idx);
 if _check_buffer(p, 'to_bytes') then
  begin
   SetLength(result, p.used);
   Move ( p.data, result[0], p.used );
  end
 else
  SetLength (result, 0);
end;


function _buffer_free (L: lua_state): Integer; cdecl;
var
   p: PBytesBuffer;
   s: String;
begin

 p := lua_objptr (L, 1, TRUE);
 s := '';
 if IsDebuggerPresent then
    s := LuaTraceBack (L, ' ');
 if (p <> nil) and not IsBadReadPtr(@p.data[p.size], 4) then
    try
     if _check_buffer(p, 'free()') then
       begin
        if log_verbose >= 7 then
           wprintf(' Releasing buffer $%p ', [p]);
        p.data[p.size + 1] := 1;
        p.data[p.size + 2] := 0;
        p.data[p.size + 3] := 0;
        if lua_type(L, 1) = LUA_TLUDATA then
           FreeMem (p, p.size + BB_HEADER_SIZE + BB_SUFFIX_SIZE);
       end;

    except
     on E: Exception do
       begin
        PrintError( Format('Exception catched in _buffer_free($%p): %s', [p, E.Message]));
        if Length (s) > 10 then ODS ('~C0F' + s + '~C07');
       end;
    end;
 result := 0;
end;


procedure _buffer_instance (L: lua_State; const b: TIdBytes);
var
   pb: PBytesBuffer;
   cb: Integer;
begin
 cb := Length (b);
 // pb := AllocMem ( cb + BB_HEADER_SIZE + BB_SUFFIX_SIZE );
 pb := lua_newuserdata (L, cb + BB_HEADER_SIZE + BB_SUFFIX_SIZE);
 if log_verbose >= 7 then
    wprintf('[~T]. #DBG: allocated buffer instance $%p ', [pb]);
 pb.Size := cb;
 pb.Used := cb;
 // termination tag
 pb.data [cb + 1] := BYTE('E');
 pb.data [cb + 2] := BYTE('O');
 pb.data [cb + 3] := BYTE('B');
 Move ( b[0], pb.data, cb );
 AssignMetaIndex (L, pb, _buffer_index, nil, 'gmt_DataBuffer', 0);
 _check_buffer(pb, '._instance()');
end;

function _base64_decode (L: lua_state): Integer; cdecl;
var
  res: TIdBytes;
   ps: PAnsiChar;
   sl: DWORD;

begin
 ps := lua_tolstring (L, 1, sl);
 res := DecodeBase64 ( ps );
 _buffer_instance (L, res);
 result := 1;
end;

function  _base64_encode (L: lua_state): Integer; cdecl;
var
   pb: PAnsiChar;
   sa: String;
   sz: Cardinal;
   rs: AnsiString;
begin
 pb := nil;
 sz := 0;
 case lua_type(L, 1) of
  LUA_TSTRING:
   pb := lua_tolstring (L, 1, sz);
  LUA_TLUDATA, LUA_TUSERDATA:
   begin
    pb := lua_objptr (L, 1);
    sz := lua_objlen (L, 1);
   end;

 end;
 if lua_gettop(L) > 1 then
    sz := lua_tointeger (L, 2);

 rs := EncodeBase64 (pb, sz);

 lua_pushstring (L, PAnsiChar(rs));
 result := 1;
end;


function _buffer_hash (L: lua_State): Integer; cdecl; // hash data of self
var
   hmac: TIdHMACSHA512;
   psec: PAnsiChar;
    res: TIdBytes;
     db: TIdBytes;
      s: String;
begin

 if lua_gettop(L) < 2 then
   begin
    PrintError('_buffer_hash required two params: this, secret');
    result := 0;
    exit;
   end;

 hmac := TIdHMACSHA512.Create;
 db := lua_tobytes (L, 1);  // this

 case lua_type(L, 2) of
  LUA_TSTRING:
   begin
    psec := lua_tostring (L, 2);
    s := String (psec);
    if Pos('==', s ) > 0 then
       hmac.Key := DecodeBase64 ( psec )
    else
       hmac.Key := Hex2Bytes ( s );
   end;
  LUA_TUSERDATA:
    hmac.Key := lua_tobytes (L, 2); // if type of secret = data-buffer
   else
     wprintf('[~T].~C0C #FATAL(_buffer_hash):~C07  invalid key type specified = %d ', [ lua_type(L, 2) ] );
 end; // case

 res := hmac.HashValue (db);
 _buffer_instance ( L, res );
 result := 1;
end;

function _buffer_index(L: lua_state): Integer; cdecl;
var
   p: PBytesBuffer;
   k: String;

begin
 p := lua_objptr (L, 1);
 k := LowerCase ( LuaStrArg (L, 2) );

 _check_buffer(p, '.' + k);
 // methods
 if k = 'dump' then
   lua_pushwstr ( L, DumpBuffer( p ) )
 else
 if k = 'free' then
   lua_pushcfunction (L, _buffer_free)
 else
 if k = 'hmac_sha512' then
   lua_pushcfunction (L, _buffer_hash) // return also data_buffer !
 else
 // props
 if k = 'base64enc' then
   lua_pushwstr ( L, Base64LineW( @p.data, p.size ) )
 else
 if k = 'ansi_str' then
   lua_pushstring (L, PAnsiChar (@p.data))
 else
 if ( k = 'uncode_str' ) or ( k = 'wide_str' ) then
   lua_pushwstr (L, PWideChar (@p.data))
 else
 if k = 'data' then
   lua_pushlightuserdata (L, @p.data )
 else
 if ( k = 'length' ) or ( k = 'size' ) then
    lua_pushinteger (L, p.size) else
 if ( k = 'used' ) then
    lua_pushinteger (L, p.used)
 else
    lua_pushnil (L);

 _check_buffer(p, '.~' + k);

 result := 1;
end;

function _data_buffer(L: lua_state): Integer; cdecl;
var
   b: TIdBytes;
   c: Integer;
begin
 result := 0;
 c := lua_tointeger (L, 1);
 if c <= 0 then exit;

 SetLength (b, c);
 _buffer_instance (L, b);
end;

function  _utf8_buffer (L: lua_state): Integer; cdecl;
var
   us: UTF8String;
    b: TIdBytes;
begin
 us := UTF8Encode ( lua_tostring (L, 1) );
 SetLength (b, Length(us));
 Move ( us[1], b[0], Length(us) );
 _buffer_instance ( L, b );
 SetLength (b, 0);
 result := 1;
end;

function _write_data (L: lua_state): Integer; cdecl;
const
   VIDX = 2;

var
   wp: TWebPacket;
   bf: TIdBytes;
    p: Pointer;
    c: Integer;
begin
 wp := lua_objptr (L, 1);
 c := lua_type(L, VIDX);
 result := 0;

 if not ( BYTE(c) in [LUA_TUSERDATA, LUA_TLUDATA] ) then exit;

 if lua_gettop(L) = VIDX then
   begin
    bf := lua_tobytes (L, VIDX);
    c := Length (bf);
    p := @bf [0];
   end
 else
   begin
    p := lua_objptr (L, VIDX);
    c := lua_tointeger (L, 3);
   end;

 if (wp <> nil) and (p <> nil) then
     c := wp.Write (p^, c)
 else
     c := 0;
 result := 1;
 lua_pushinteger (L, c);
end;

function _write_str (L: lua_state): Integer; cdecl;
var
   wp: TWebPacket;
    s: String;
    r: Integer;
begin
 wp := lua_objptr (L, 1);
 s := LuaStrArg (L, 2);
 r := 0;
 if (wp <> nil) then
     r := wp.WriteStr (s);
 result := 1;
 lua_pushinteger (L, r);
end;

function _write_i32 (L: lua_state): Integer; cdecl;
var
   wp: TWebPacket;
    i: LongInt;
    r: Integer;
begin
 wp := lua_objptr (L, 1);
 i := lua_tointeger (L, 2);
 if (wp <> nil) then r := wp.Write (i, 4) else r := 0;
 result := 1;
 lua_pushinteger (L, r);
end;

function _write_i64 (L: lua_state): Integer; cdecl;
var
   wp: TWebPacket;
    i: Int64;
    r: Integer;
begin
 wp := lua_objptr (L, 1);
 i := lua_toint64 (L, 2);
 if (wp <> nil) then r := wp.Write (i, 8) else r := 0;
 result := 1;
 lua_pushinteger (L, r);
end;

function _write_u32 (L: lua_state): Integer; cdecl;
var
   wp: TWebPacket;
    v: DWORD;
    r: Integer;
begin
 wp := lua_objptr (L, 1);
 v := lua_tointeger (L, 2);
 if (wp <> nil) then r := wp.Write (v, 4) else r := 0;
 result := 1;
 lua_pushinteger (L, r);
end;

function _write_u64 (L: lua_state): Integer; cdecl;
var
   wp: TWebPacket;
    v: UInt64;
    r: Integer;
begin
 wp := lua_objptr (L, 1);
 v := lua_toint64 (L, 2);
 if (wp <> nil) then r := wp.Write (v, 8) else r := 0;
 result := 1;
 lua_pushinteger (L, r);
end;

function _write_f32 (L: lua_state): Integer; cdecl;
var
   wp: TWebPacket;
    v: Single;
    r: Integer;
begin
 wp := lua_objptr (L, 1);
 v := lua_tonumber (L, 2);
 if (wp <> nil) then r := wp.Write (v, 4) else r := 0;
 result := 1;
 lua_pushinteger (L, r);
end;

function _write_f64 (L: lua_state): Integer; cdecl;
var
   wp: TWebPacket;
    v: Double;
    r: Integer;
begin
 wp := lua_objptr (L, 1);
 v := lua_tonumber (L, 2);
 if (wp <> nil) then r := wp.Write (v, 8) else r := 0;
 result := 1;
 lua_pushinteger (L, r);
end;


function _WebPacket_Index (L: lua_state): Integer; cdecl;
var
   wp: TWebPacket;
   sa: AnsiString;
    k: String;
begin
 wp := lua_objptr (L, 1);
 k := LowerCase ( LuaStrArg(L, 2) );

 if k = 'write_data' then
    lua_pushcfunction (L, _write_data)
 else
 if k = 'write_str' then
    lua_pushcfunction (L, _write_str)
 else
 if k = 'write_i32' then
    lua_pushcfunction (L, _write_i32)
 else
 if k = 'write_i64' then
    lua_pushcfunction (L, _write_i64)
 else
 if k = 'write_u32' then
    lua_pushcfunction (L, _write_u32)
 else
 if k = 'write_u64' then
    lua_pushcfunction (L, _write_u64)
 else
 if k = 'write_f32' then
    lua_pushcfunction (L, _write_f32)
 else
 if k = 'write_f64' then
    lua_pushcfunction (L, _write_f64)
 else
 if k = 'free_object' then
    lua_pushcfunction (L, LuaFreeObject)
 else
 if ( k = 'length' ) or ( k = 'size' ) then
    lua_pushnumber (L, wp.Size )
  else
 if k = 'base64' then
   begin
    sa := Base64LineA(wp.Memory, wp.Size);
    lua_pushstring (L, PAnsiChar (sa) )
   end
 else
    lua_pushnil (L);

 result := 1;
end;


function _web_packet (L: lua_state): Integer; cdecl;
var
   wp: TWebPacket;
   gc: Boolean;
begin
 gc := ( lua_gettop(L) = 0 ) or ( lua_toboolean (L, 1) );
 wp := TWebPacket.Create;
 wp.PushInstance ( L, gc );
 result := 1;
end;



{ TWebPacket }

procedure TWebPacket.PushInstance(L: lua_State; gc: Boolean);
begin
 AssignMetaIndex ( L, self, _WebPacket_Index, nil, 'gmt_WebPacket', MTF_OBJECT or IfV (gc, MTF_ADD_GC, 0) );
end;


function TWebPacket.WriteStr(const ws: String): LongInt;
begin
 result := WriteStr ( UTF8Encode (ws) );
end;

function TWebPacket.WriteStr(const us: UTF8String): LongInt;
begin
 result := Write ( us[1], Length (us) );
end;


procedure ExportAll (L: lua_State; disp: Boolean);
begin
 LuaRegFunc (L, 'base64_decode', _base64_decode, '(string) // convert base64 string to data_buffer object',                  disp);
 LuaRegFunc (L, 'base64_encode', _base64_encode, '(string|userdata, [size]) // convert string/userdata to base64 string ',   disp);
 LuaRegFunc (L, 'data_buffer',   _data_buffer,   '(size) // constructs data_buffer object',                                  disp);
 LuaRegFunc (L, 'utf8_buffer',   _utf8_buffer,   '(string) // constructs data_buffer object from string encoded UTF8',       disp);
 LuaRegFunc (L, 'web_packet',    _web_packet,    '([bool use_gc]) // constructs void web_packed object',                     disp);
end;


end.
