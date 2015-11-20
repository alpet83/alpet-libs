unit LuaJSON;

interface
uses Windows, SysUtils, Classes, DBXJSON, JSONTools, LuaTypes, Lua5, LuaTools, StrClasses, Misc;

// JSON object export to Lua, by alpet
// splitted 15.10.13


function        _JA_Index (L: lua_State): Integer; cdecl;
procedure       _JA_Instance (L: lua_State; jo: TJSONArray; flags: DWORD = 0 );
function        _JO_Index (L: lua_State): Integer; cdecl;
procedure       _JO_Instance (L: lua_State; jo: TJSONObject; flags: DWORD = 0 );

function        _JV_Index (L: lua_State): Integer; cdecl;
procedure       _JV_Instance (L: lua_State; jv: TJSONValue; flags: DWORD = 0  );

function        _JSON_array (L: lua_State): Integer; cdecl;
function        _JSON_object (L: lua_State): Integer; cdecl;
function        _JSON_Parse (L: lua_State): Integer; cdecl;


implementation


procedure       _JA_Instance (L: lua_State; jo: TJSONArray; flags: DWORD );
begin
 AssignMetaIndex ( L, jo, _JA_Index, nil, 'gmt_JSONArray', MTF_OBJECT or flags );
end;


procedure _JO_Instance ( L: lua_State; jo: TJSONObject; flags: DWORD );
begin
 AssignMetaIndex ( L, jo, _JO_Index, nil, 'gmt_JSONObject', MTF_OBJECT or flags );
end;


procedure _JV_Instance (L: lua_State; jv: TJSONValue; flags: DWORD );
begin
 AssignMetaIndex ( L, jv, _JV_Index, nil, 'gmt_JSONValue', MTF_OBJECT or flags );
end;

function _JSON_array (L: lua_State): Integer; cdecl;
var
   ja: TJSONArray;
   ff: DWORD;
begin
 result := 1;
 ja := TJSONArray.Create;
 ff := 0;
 if ( lua_gettop(L) >= 1 ) and lua_toboolean (L, 1) then
      ff := MTF_ADD_GC;
 _JA_Instance ( L, ja, ff );
end;

function _JSON_object (L: lua_State): Integer; cdecl;
var
   jo: TJSONObject;
   ff: DWORD;
begin
 result := 1;
 jo := TJSONObject.Create;
 ff := 0;
 if ( lua_gettop(L) >= 1 ) and lua_toboolean (L, 1) then
      ff := MTF_ADD_GC;
 _JO_Instance ( L, jo, ff );
end;


function _JSON_Parse (L: lua_State): Integer; cdecl;
var
   jo: TJSONObject;
   ff: DWORD;
    s: String;
begin
 s := LuaStrArg (L);
 jo := JSONParse ( UTF8Encode (s) );
 ff := 0;

 if ( lua_gettop(L) >= 1 ) and lua_toboolean (L, 2) then
      ff := MTF_ADD_GC;

 if Assigned (jo) then
    _JO_Instance ( L, jo, ff )
 else
    lua_pushnil (L);
 result := 1;
end;

function _Auto_Value ( L: lua_State; jv: TJSONValue ): Integer;
begin
 // pushing instances or string value
 result := 1;
 if jv = nil then
    lua_pushnil (L)
 else
 if jv is TJSONObject then
    _JO_Instance ( L, TJSONObject(jv), 0 )
 else
 if jv is TJSONArray then
    _JA_Instance ( L, TJSONArray(jv), 0 )
 else
 if jv is TJSONValue then
    lua_pushwstr ( L, jv.Value );

end; // _Auto_Value


// lua procedures/wrappers

function _JA_AddItem (L: lua_State): Integer; cdecl;
var
   ja: TJSONArray;
   jv: TJSONValue;
   jo: TJSONObject;
   jp: TJSONPair;
    k: String;

begin
 ja := lua_objptr (L, 1);
 case lua_type(L, 2) of
   LUA_TBOOLEAN:
      ja.Add( lua_toboolean (L, 2) );
   LUA_TSTRING:
     begin
      k := LuaStrArg (L, 2);
      if lua_gettop(L) < 3 then
         ja.Add ( k )
      else
        begin  // pair variant
         jp := TJSONPair.Create ( k, LuaStrArg(L, 3) );
         jo := TJSONObject.Create ( jp );
         ja.Add( jo );
        end;

     end;
   LUA_TNUMBER:
      ja.Add ( lua_tonumber (L, 2) );
   LUA_TLUDATA:
     begin
      jv := lua_objptr (L, 2);
      if jv <> nil then ja.AddElement (jv);
     end;
 end;
 result := 0;
end;

function _JA_Index (L: lua_State): Integer; cdecl;
var
   ja: TJSONArray;
   jv: TJSONValue;
    k: String;
    i: Integer;
begin
 ja := lua_objptr (L, 1);
 k := LowerCase ( LuaStrArg (L, 2) );

 result := 1;
 if ( k = 'add_item' ) then
      lua_pushcfunction ( L, _JA_AddItem )
 else
 if ( k = 'size' ) or ( k = 'count' ) then
      lua_pushinteger ( L, ja.Size )
 else
 if ( k = 'min_index' ) then
      lua_pushinteger ( L, 0 )
 else
 if ( k = 'max_index' ) then
      lua_pushinteger ( L, ja.Size - 1 )
 else
  begin
   i := lua_tointeger (L, 2); // integer keys
   // Dec (i); // 1..count -> 0..count - 1 range conversion
   jv := nil;
   if i < ja.Size then jv := ja.Get(i);
   result := _Auto_Value ( L, jv );
  end;

end;

function _JV_Index (L: lua_State): Integer; cdecl;
var
   jv: TJSONValue;
    k: String;

begin
 result := 1;
 jv := lua_objptr (L, 1);
 k := LuaStrArg (L, 2);

 if k = 'str_value' then
    lua_pushwstr ( L, jv.Value )
 else
    result := 0;

end;

function _JO_AddPair(L: lua_State): Integer; cdecl; // TJSONObject.AddPair wrapper
var
   jo: TJSONObject;
   nm: String;

begin
 jo := lua_objptr (L, 1);
 nm := LuaStrArg (L, 2);

 if lua_gettop(L) > 2 then

   case lua_type(L, 3) of
    LUA_TBOOLEAN:
        jo.AddPair ( nm, IfV (lua_toboolean(L, 3), 'true', 'false') );
    LUA_TSTRING:
        jo.AddPair ( nm, LuaStrArg(L, 3) );
    LUA_TNUMBER:
        jo.AddPair ( nm, ftow ( lua_tonumber (L, 3) ) );
    LUA_TUSERDATA:
        jo.AddPair ( nm, lua_objptr (L, 3) ); // TJSONValue or inherited from
   end;

 result := 0;
end;

function _JO_Childs(L: lua_State): Integer; cdecl;
var
   jo: TJSONObject;
   jp: TJSONPair;
    i: Integer;
    k: String;
begin
 jo := lua_objptr (L, 1);
 lua_newtable (L);
 result := 1;

 for i := 0 to jo.Size - 1 do
  begin
   jp := jo.Get (i);
   k := jp.JsonString.Value;
   if ( k = '' ) or ( jp.JsonValue = nil ) then continue;

   _Auto_Value (L, jp.JsonValue);
   lua_sf (L, k);
  end;
end;



function _JO_Index (L: lua_State): Integer; cdecl;
var
   jo: TJSONObject;
   jv: TJSONValue;
   jp: TJSONPair;
    k: String;

begin
 result := _JV_Index (L);
 if result > 0 then exit;
 result := 1;
 jo := lua_objptr (L, 1);

 k := LuaStrArg (L, 2);

 jp := jo.Get(k);

 if jp = nil then
    k := LowerCase ( k );

 if k = 'add_pair' then
    lua_pushcfunction (L, _JO_AddPair)
 else
 if k = 'childs' then
    lua_pushcfunction (L, _JO_Childs)
 else
 if k = 'childs_count' then
    lua_pushinteger (L, jo.Size)
 else
 if k = 'free_object' then
    lua_pushcfunction (L, LuaFreeObject)
 else
 if k = 'to_string' then
    lua_pushwstr ( L,  jo.ToString )

 else
 if jp <> nil then
   begin
    jv := jp.JsonValue;
    result := _Auto_Value ( L, jv );
   end
 else
   lua_pushnil (L);


end;


end.
