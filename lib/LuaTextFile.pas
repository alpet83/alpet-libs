unit LuaTextFile;

interface
uses Windows, SysUtils, Classes, StrClasses, LuaTypes, LuaImports, LuaTools, Misc;


procedure  ExportAll (L: lua_State; bDisp: Boolean);

implementation

 {$I-}


function text_file_read(L: lua_State; line: Boolean): String;
var
   pt: ^Text;
begin
 pt := lua_objptr(L, 1);
 result := '#ERROR: wrong argument 1';
 if pt = nil then
    exit;

 result := '';

 if line then
    ReadLn (pt^, result)
 else
    Read (pt^, result);
end;

function text_file_write (L: lua_State; line: Boolean): Integer;
var
   pt: ^Text;
    s: String;
begin
 result := 1;
 pt := lua_objptr(L, 1);
 result := -1;
 if pt = nil then  exit;

 s := LuaStrArg (L, 2);

 result := Length (s);
 if line then
    WriteLn (pt^, s)
 else
    Write (pt^, s);
end;

function __tfile_close (L: lua_State): Integer; cdecl;
var
   pt: ^Text;
begin
 result := 0;
 pt := lua_objptr (L, 1);
 if pt = nil then exit;
 CloseFile (pt^);
 FreeMem (pt);
end;


function __tfile_read (L: lua_State): Integer; cdecl;
var
   pt: ^Text;
begin
 pt := lua_objptr (L, 1);
 result := 1;
 if (pt <> nil) and Eof(pt^) then
   lua_pushnil(L)
 else
   lua_pushwstr (L, text_file_read(L, FALSE));
end;

function __tfile_readln (L: lua_State): Integer; cdecl;
var
   pt: ^Text;
begin
 pt := lua_objptr (L, 1);
 result := 1;
 if (pt <> nil) and Eof(pt^) then
   lua_pushnil(L)
 else
   lua_pushwstr (L, text_file_read(L, TRUE));
end;

function __tfile_write (L: lua_State): Integer; cdecl;
begin
 result := 1;
 lua_pushinteger (L, text_file_write(L, FALSE));
end;

function __tfile_writeln (L: lua_State): Integer; cdecl;
begin
 result := 1;
 lua_pushinteger (L, text_file_write(L, FALSE));
end;



function __tfile_index (L: lua_State): Integer; cdecl;
var
  key: String;
   pt: ^Text;
begin
 result := 1;
 pt := lua_objptr (L, 1);
 key := LuaStrArg (L, 2);
 if key = 'read' then
    lua_pushcfunction (L, __tfile_read) else
 if key = 'readln' then
    lua_pushcfunction (L, __tfile_readln) else
 if key = 'write' then
    lua_pushcfunction (L, __tfile_write) else
 if key = 'writeln' then
    lua_pushcfunction (L, __tfile_writeln) else
 if key = 'close' then
    lua_pushcfunction (L, __tfile_close)
 else
   begin
    lua_pushwstr(L, 'wrong key ' + key);
   end;

end;



function text_file(L: lua_State): Integer; cdecl;
var
   pt: ^Text;
   fn: String;
   om: String;
   cp: WORD;
   ac: Integer;
begin
 result := 1;
 ac := lua_gettop (L);
 pt := AllocMem (sizeof(Text));
 fn := LuaStrArg (L);
 om := 'read';
 cp := 1251;

 if ac > 1 then om := LuaStrArg (L, 2);
 if ac > 2 then cp := lua_tointeger (L, 3);

 AssignFile (pt^, fn, cp);

 if om = 'append' then
    Append (pt^) else
 if om = 'read' then
    Reset (pt^) else
 if om = 'write' then
    ReWrite (pt^);

 if IOresult <> 0 then
  begin
   PrintError('Error open file ' + fn + ' with mode ' + om);
   FreeMem (pt);
   lua_pushnil (L);
   exit;
  end;


 AssignMetaIndex (L, pt, __tfile_index, nil, '_MT_TEXT_FILE');
end;



procedure  ExportAll (L: lua_State; bDisp: Boolean);
begin
 LuaRegFunc (L, 'text_file', text_file, '(file_name, [mode], [code_page]) // opening text file and returns object', bDisp);
end;


end.
