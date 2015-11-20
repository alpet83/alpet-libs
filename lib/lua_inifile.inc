
function __fini_read_bool(L: lua_State): Integer; cdecl;
var
   fini: TIniFile;
    scn: String;
    vln: String;
begin
 result := 1;
 fini := lua_objptr (L, 1);
 scn := LuaStrArg (L, 2);  vln := LuaStrArg (L, 3);
 lua_pushboolean (L, fini.ReadBool(scn, vln, lua_toboolean(L, 4)) );
end;

function __fini_read_int(L: lua_State): Integer; cdecl;
var
   fini: TIniFile;
    scn: String;
    vln: String;
begin
 result := 1;
 fini := lua_objptr (L, 1);
 scn := LuaStrArg (L, 2); vln := LuaStrArg (L, 3);
 lua_pushinteger (L, fini.ReadInteger (scn, vln, lua_tointeger(L, 4)) );
end;

function __fini_read_number(L: lua_State): Integer; cdecl;
var
   fini: TIniFile;
    scn: String;
    vln: String;
begin
 result := 1;
 fini := lua_objptr (L, 1);
 scn := LuaStrArg (L, 2); vln := LuaStrArg (L, 3);
 lua_pushnumber (L, fini.ReadFloat (scn, vln, lua_tonumber(L, 4)) );
end;

function __fini_read_section(L: lua_State): Integer; cdecl;
var
   fini: TIniFile;
    scn: String;
    tmp: TStrings;
      n: Integer;
begin
 result := 1;
 fini := lua_objptr (L, 1);
 scn := LuaStrArg (L, 2);

 tmp := TStringList.Create;
 fini.ReadSection(scn, tmp);
 lua_createtable (L, tmp.Count, 0);
 for n := 0 to tmp.Count - 1 do
     lua_setarr_s (L, n + 1, tmp[n]);

 fini.Free;
end;

function __fini_read_string (L: lua_State): Integer; cdecl;
var
   fini: TIniFile;
    scn: String;
    vln: String;
begin
 result := 1;
 fini := lua_objptr (L, 1);
 scn := LuaStrArg (L, 2); vln := LuaStrArg (L, 3);
 lua_pushwstr (L, fini.ReadString (scn, vln, LuaStrArg(L, 4)) );
end;


function _fini_section_exists(L: lua_State): Integer; cdecl;
var
   fini: TIniFile;
    scn: String;
begin
 result := 1;
 fini := lua_objptr (L, 1);
 scn := LuaStrArg (L, 2);
 lua_pushboolean (L, fini.SectionExists(scn));
end;

function __fini_write_bool(L: lua_State): Integer; cdecl;
var
   fini: TIniFile;
    scn: String;
    vln: String;
begin
 result := 0;
 fini := lua_objptr (L, 1);
 scn := LuaStrArg (L, 2); vln := LuaStrArg (L, 3);
 fini.WriteBool (scn, vln, lua_toboolean(L, 4));
end;

function __fini_write_int(L: lua_State): Integer; cdecl;
var
   fini: TIniFile;
    scn: String;
    vln: String;
begin
 result := 0;
 fini := lua_objptr (L, 1);
 scn := LuaStrArg (L, 2); vln := LuaStrArg (L, 3);
 fini.WriteInteger (scn, vln, lua_tointeger(L, 4));
end;

function __fini_write_number(L: lua_State): Integer; cdecl;
var
   fini: TIniFile;
    scn: String;
    vln: String;
begin
 result := 0;
 fini := lua_objptr (L, 1);
 scn := LuaStrArg (L, 2); vln := LuaStrArg (L, 3);
 fini.WriteFloat (scn, vln, lua_tonumber(L, 4));
end;

function __fini_write_string(L: lua_State): Integer; cdecl;
var
   fini: TIniFile;
    scn: String;
    vln: String;
begin
 result := 0;
 fini := lua_objptr (L, 1);
 scn := LuaStrArg (L, 2); vln := LuaStrArg (L, 3);
 fini.WriteString (scn, vln, LuaStrArg(L, 4));
end;

