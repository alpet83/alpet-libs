(******************************************************************************
* Original copyright for the lua source and headers:
*  1994-2004 Tecgraf, PUC-Rio.
*  www.lua.org.
*
* Copyright for the Delphi adaption:
*  2005 Rolf Meyerhoff
*  www.matrix44.de
*
*  All rights reserved.
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************)
// Adapted by alpet for LUA 5.1.4
unit lua5;

interface
uses Windows, SysUtils, LuaTypes;

const
  LUA_VERSIONS  = 'Lua 5.1.4';
  LUA_COPYRIGHT = 'Copyright (C) 1994-2004 Tecgraf, PUC-Rio';
  LUA_AUTHORS   = 'R. Ierusalimschy, L. H. de Figueiredo & W. Celes';

  (* option for multiple returns in `lua_pcall' and `lua_call' *)
  LUA_MULTRET	  = -1;

  (*
  ** pseudo-indices
    WARNS: may be version depedence

  *)
  LUA_REGISTRYINDEX	= -10000;
  LUA_ENVIRONINDEX      = -10001;
  LUA_GLOBALSINDEX	= -10002;


  (* error codes for `lua_load' and `lua_pcall' *)
  LUA_ERRRUN    = 2;
  LUA_ERRSYNTAX = 3;
  LUA_ERRMEM    = 4;
  LUA_ERRERR    = 5;
  LUA_ERRFILE   = LUA_ERRERR + 1;

  LUA_ARG1 = $01;
  LUA_ARG2 = $02;
  LUA_ARG3 = $03;
  LUA_ARG4 = $04;
  LUA_ARG5 = $05;

  LUA_GCSTOP		= 0;
  LUA_GCRESTART		= 1;
  LUA_GCCOLLECT		= 2;
  LUA_GCCOUNT		= 3;
  LUA_GCCOUNTB		= 4;
  LUA_GCSTEP		= 5;
  LUA_GCSETPAUSE	= 6;
  LUA_GCSETSTEPMUL	= 7;
{$A4}




const

  (*
  ** basic types
  *)
  LUA_TNONE	     = -1;
  LUA_TNIL           =  0;
  LUA_TBOOLEAN       =  1;
  LUA_TLIGHTUSERDATA =  2; LUA_TLUDATA = LUA_TLIGHTUSERDATA;
  LUA_TNUMBER        =  3;
  LUA_TSTRING        =  4;
  LUA_TTABLE         =  5;
  LUA_TFUNCTION      =  6;
  LUA_TUSERDATA	     =  7;
  LUA_TTHREAD	     =  8;

  (* minimum Lua stack available to a C function *)
  LUA_MINSTACK = 20;


(* Macros *)
function lua_upvalueindex(i: Integer): Integer;

(*
** state manipulation
*)
function  lua_open: lua_State; cdecl;
procedure lua_close(L: lua_State); cdecl;
function  lua_newstate (f: lua_Alloc; ud: Pointer) : lua_State; cdecl;
function  lua_newthread(L: lua_State): lua_State; cdecl;


function lua_atpanic(L: lua_State; panicf: lua_CFunction): lua_CFunction; cdecl;

(*
** basic stack manipulation
*)
function lua_gettop(L: lua_State): Integer; cdecl;
procedure lua_settop(L: lua_State; idx: Integer); cdecl;
procedure lua_pushvalue(L: lua_State; idx: Integer); cdecl;
procedure lua_remove(L: lua_State; idx: Integer); cdecl;
procedure lua_insert(L: lua_State; idx: Integer); cdecl;
procedure lua_replace(L: lua_State; idx: Integer); cdecl;
function lua_checkstack(L: lua_State; sz: Integer): LongBool; cdecl;

procedure lua_xmove(from, dest: lua_State; n: Integer); cdecl;

(*
** access functions (stack -> C)
*)

function lua_isnumber(L: lua_State; idx: Integer): LongBool; cdecl;
function lua_isstring(L: lua_State; idx: Integer): LongBool; cdecl
function lua_iscfunction(L: lua_State; idx: Integer): LongBool; cdecl;
function lua_isuserdata(L: lua_State; idx: Integer): LongBool; cdecl;
function lua_type(L: lua_State; idx: Integer): Integer; cdecl;
function lua_typename(L: lua_State; tp: Integer): PAnsiChar; cdecl;
function lua_typeof(L: lua_State; idx: Integer): String;

function lua_equal(L: lua_State; idx1, idx2: Integer): LongBool; cdecl;
function lua_rawequal(L: lua_State; idx1, idx2: Integer): LongBool; cdecl;
function lua_lessthan(L: lua_State; idx1, idx2: Integer): LongBool; cdecl;

// ua_tointeger
function lua_tointeger(L: lua_State; idx: Integer): lua_Integer; cdecl;
function lua_tonumber(L: lua_State; idx: Integer): lua_Number; cdecl;

function lua_toboolean(L: lua_State; idx: Integer): LongBool; cdecl
function lua_tostring(L: lua_State; idx: Integer): PAnsiChar; cdecl; inline;
function lua_tolstring(L: lua_State; idx: Integer; var rlen: SIZE_T): PAnsiChar; cdecl;

function lua_strlen(L: lua_State; idx: Integer): Cardinal; cdecl;
function lua_tocfunction(L: lua_State; idx: Integer): lua_CFunction; cdecl;
function lua_touserdata(L: lua_State; idx: Integer): Pointer; cdecl;
function lua_tothread(L: lua_State; idx: Integer): lua_State; cdecl;
function lua_topointer(L: lua_State; idx: Integer): Pointer; cdecl;


(*
** push functions (C -> stack)
*)
procedure lua_pushnil(L: lua_State); cdecl;
procedure lua_pushnumber(L: lua_State; n: lua_Number); cdecl;
procedure lua_pushnumbers (L: lua_State; n1, n2: lua_Number); overload; inline;
procedure lua_pushnumbers (L: lua_State; n1, n2, n3: lua_Number); overload; inline;

procedure lua_pushinteger(L: lua_State; i: Integer); cdecl;
procedure lua_pushlstring(L: lua_State; s: PAnsiChar; ls: Cardinal); cdecl;
procedure lua_pushstring(L: lua_State; s: PAnsiChar); cdecl;
function  lua_pushvfstring(L: lua_State; fmt, argp: PAnsiChar): PAnsiChar; cdecl;
function  lua_pushfstring(L: lua_State; fmt: PAnsiChar): PAnsiChar; cdecl; varargs;
procedure lua_pushwstr (L: lua_State; const s: String);

procedure lua_pushcclosure(L: lua_State; fn: lua_CFunction; n: Integer); cdecl;
procedure lua_pushboolean(L: lua_State; b: Integer); cdecl; overload;
procedure lua_pushboolean(L: lua_State; b: LongBool); cdecl; overload;
procedure lua_pushlightuserdata(L: lua_State; p: Pointer); cdecl;
procedure lua_pushptr (L: lua_State; p: Pointer); inline;


(*
** get functions (Lua -> stack)
*)
procedure lua_gettable(L: lua_State; idx: Integer); cdecl;
procedure lua_rawget(L: lua_State; idx: Integer); cdecl;
procedure lua_rawgeti(L: lua_State; idx, n: Integer); cdecl;
procedure lua_createtable(L: lua_State; narr, nrec: Integer); cdecl;
function  lua_newtable(L: lua_State): Integer;
function  lua_newuserdata(L: lua_State; sz: Cardinal): Pointer; cdecl;
function  lua_getmetatable(L: lua_State; objindex: Integer): Integer; cdecl;
procedure lua_getfenv(L: lua_State; idx: Integer); cdecl;
procedure lua_getfield(L: lua_State; idx: Integer; const k: PAnsiChar); cdecl;



(*
** set functions (stack -> Lua)
*)
procedure lua_settable(L: lua_State; idx: Integer); cdecl;

procedure lua_rawset(L: lua_State; idx: Integer); cdecl;
procedure lua_rawseti(L: lua_State; idx, n: Integer); cdecl;
function  lua_setmetatable(L: lua_State; objindex: Integer): LongBool; cdecl;
function  lua_setfenv(L: lua_State; idx: Integer): LongBool; cdecl;
procedure lua_setfield(L: lua_state; idx: Integer; const k: PAnsiChar); cdecl;

(*
** `load' and `call' functions (load and run Lua code)
*)
procedure lua_call(L: lua_State; nargs, nresults: Integer); cdecl;
function  lua_pcall(L: lua_State; nargs, nresults, errfunc: Integer): Integer; cdecl;
function  lua_cpcall(L: lua_State; func: lua_CFunction; ud: Pointer): Integer; cdecl;
function  lua_load(L: lua_State; reader: lua_Chunkreader; dt: Pointer; chunkname: PAnsiChar): Integer; cdecl;

function  lua_dump(L: lua_State; writer: lua_Chunkwriter; data: Pointer): Integer; cdecl;


(*
** coroutine functions
*)
function lua_yield(L: lua_State; nresults: Integer): Integer; cdecl;
function lua_resume(L: lua_State; narg: Integer): Integer; cdecl;

(*
** garbage-collection functions
*)
function lua_getgcthreshold(L: lua_State): Integer; cdecl;
function lua_getgccount(L: lua_State): Integer; cdecl;
function lua_gc (L: lua_State; what, data: Integer): Integer; cdecl;
procedure lua_setgcthreshold(L: lua_State; newthreshold: Integer); cdecl;



(*
** miscellaneous functions
*)

function lua_version: PAnsiChar; cdecl;

function lua_error(L: lua_State): Integer; cdecl;

function lua_next(L: lua_State; idx: Integer): Integer; cdecl;
function lua_objlen(L: lua_State; idx: Integer): SIZE_T; cdecl;


procedure lua_concat(L: lua_State; n: Integer); cdecl;



(*
** ===============================================================
** some useful macros
** ===============================================================
*)

function lua_boxpointer(L: lua_State; u: Pointer): Pointer;

function lua_unboxpointer(L: lua_State; i: Integer): Pointer;

procedure lua_pop(L: lua_State; n: Integer);

procedure lua_pushliteral(L: lua_State; s: PAnsiChar);


(*
** compatibility macros and functions
*)


function lua_pushupvalues(L: lua_State): Integer; cdecl;

procedure lua_getregistry(L: lua_State);


// procedure lua_setglobal(L: lua_State; s: PAnsiChar);
// procedure lua_getglobal(L: lua_State; s: PAnsiChar);

procedure lua_setglobal(L: lua_State; const s: String);
procedure lua_getglobal(L: lua_State; const s: String);
procedure lua_setglobal_nil(L: lua_State; const s: String);

(* compatibility with ref system *)

const

  (* pre-defined references *)
  LUA_NOREF  = -2;
  LUA_REFNIL = -1;

function lua_ref(L: lua_State; lock: Boolean): Integer;

procedure lua_unref(L: lua_State; ref: Integer);

procedure lua_getref(L: lua_State; ref: Integer);



(*
** {======================================================================
** useful definitions for Lua kernel and libraries
** =======================================================================
*)

const
  (* formats for Lua numbers *)
  LUA_NUMBER_SCAN = '%lf';

  LUA_NUMBER_FMT  = '%.14g';

(* }====================================================================== *)


(*
** {======================================================================
** Debug API
** =======================================================================
*)


(*
** Event codes
*)
  LUA_HOOKCALL    = 0;
  LUA_HOOKRET	    = 1;
  LUA_HOOKLINE    = 2;
  LUA_HOOKCOUNT   = 3;
  LUA_HOOKTAILRET = 4;


(*
** Event masks
*)
  LUA_MASKCALL  = 1 shl LUA_HOOKCALL;
  LUA_MASKRET   = 1 shl LUA_HOOKRET;
  LUA_MASKLINE  = 1 shl LUA_HOOKLINE;
  LUA_MASKCOUNT = 1 shl LUA_HOOKCOUNT;




function lua_getstack(L: lua_State; level: Integer; var ar: lua_Debug): Integer; cdecl;
function lua_getinfo(L: lua_State; what: PAnsiChar; var ar: lua_Debug): Integer; cdecl;
function lua_getlocal(L: lua_State; var ar: lua_Debug; n: Integer): PAnsiChar; cdecl;
function lua_setlocal(L: lua_State; var ar: lua_Debug; n: Integer): PAnsiChar; cdecl;
function lua_getupvalue(L: lua_State; funcindex, n: Integer): PAnsiChar; cdecl;
function lua_setupvalue(L: lua_State; funcindex, n: Integer): PAnsiChar; cdecl;

function lua_sethook(L: lua_State; func: lua_Hook; mask, count: Integer): Integer; cdecl;
function lua_gethook(L: lua_State): lua_Hook; cdecl;
function lua_gethookmask(L: lua_State): Integer; cdecl;
function lua_gethookcount(L: lua_State): Integer; cdecl;

const
  LUA_COLIBNAME = 'coroutine';
function luaopen_base(L: lua_State): Integer; cdecl;

const
  LUA_TABLIBNAME = 'table';
function luaopen_table(L: lua_State): Integer; cdecl;

const
  LUA_IOLIBNAME = 'io';
  LUA_OSLIBNAME = 'os';
function luaopen_io(L: lua_State): Integer; cdecl;

const
  LUA_STRLIBNAME = 'string';
function luaopen_string(L: lua_State): Integer; cdecl;

const
  LUA_MATHLIBNAME = 'math';
function luaopen_math(L: lua_State): Integer; cdecl;

const
  LUA_DBLIBNAME = 'debug';
function luaopen_debug(L: lua_State): Integer; cdecl;


function luaopen_loadlib(L: lua_State): Integer; cdecl;
function luaopen_os(L: lua_State): Integer; cdecl;


(* to help testing the libraries *)
procedure lua_assert(c: Boolean);


(* compatibility code *)
function lua_baselibopen(L: lua_State): Integer;
function lua_tablibopen(L: lua_State): Integer;
function lua_iolibopen(L: lua_State): Integer;
function lua_strlibopen(L: lua_State): Integer;
function lua_mathlibopen(L: lua_State): Integer;
function lua_dblibopen(L: lua_State): Integer;

type
  luaL_reg = record
    name: PAnsiChar;
    func: lua_CFunction;
  end;

function luaL_newstate: Pointer; cdecl;
procedure luaL_openlib(L: lua_State; libname: PAnsiChar; lr: array of luaL_reg; nup: Integer); cdecl;
procedure luaL_openlibs(L: lua_State); cdecl;
function luaL_getmetafield(L: lua_State; obj: Integer; e: PAnsiChar): Integer; cdecl;
function luaL_callmeta(L: lua_State; obj: Integer; e: PAnsiChar): Integer; cdecl;
function luaL_typerror(L: lua_State; narg: Integer; tname: PAnsiChar): Integer; cdecl;
function luaL_argerror(L: lua_State; numarg: Integer; extramsg: PAnsiChar): Integer; cdecl;
function luaL_checklstring(L: lua_State; numArg: Integer; var ls: Cardinal): PAnsiChar; cdecl;
function luaL_optlstring(L: lua_State; numArg: Integer; def: PAnsiChar; var ls: Cardinal): PAnsiChar; cdecl;
function luaL_checknumber(L: lua_State; numArg: Integer): lua_Number; cdecl;
function luaL_optnumber(L: lua_State; nArg: Integer; def: lua_Number): lua_Number; cdecl;

procedure luaL_checkstack(L: lua_State; sz: Integer; msg: PAnsiChar); cdecl;
procedure luaL_checktype(L: lua_State; narg, t: Integer); cdecl;
procedure luaL_checkany(L: lua_State; narg: Integer); cdecl;

function luaL_newmetatable(L: lua_State; tname: PAnsiChar): Integer; cdecl;
procedure luaL_getmetatable(L: lua_State; tname: PAnsiChar); cdecl;
procedure luaL_checkudata(L: lua_State; ud: Integer; tname: PAnsiChar); cdecl;

procedure luaL_where(L: lua_State; lvl: Integer); cdecl;
function luaL_error(L: lua_State; fmt: PAnsiChar): Integer; cdecl; varargs;

function luaL_findstring(st: PAnsiChar; lst: array of PAnsiChar): Integer; cdecl;

function luaL_ref(L: lua_State; t: Integer): Integer; cdecl;
procedure luaL_unref(L: lua_State; t, ref: Integer); cdecl;

function luaL_getn(L: lua_State; t: Integer): Integer; cdecl;
procedure luaL_setn(L: lua_State; t, n: Integer); cdecl;

function luaL_loadfile(L: lua_State; filename: PAnsiChar): Integer; cdecl;
function luaL_loadbuffer(L: lua_State; buff: PAnsiChar; sz: Cardinal; name: PAnsiChar): Integer; cdecl;
function luaL_loadstring(L: lua_State; S: PAnsiChar): Integer; cdecl;



(*
** ===============================================================
** some useful macros
** ===============================================================
*)

function luaL_argcheck(L: lua_State; cond: Boolean; numarg: Integer; extramsg: PAnsiChar): Integer;
function luaL_checkstring(L: lua_State; n: Integer): PAnsiChar;
function luaL_optstring(L: lua_State; n: Integer; d: PAnsiChar): PAnsiChar;
function luaL_checkint(L: lua_State; n: Integer): Integer;
function luaL_checklong(L: lua_State; n: LongInt): LongInt;
function luaL_optint(L: lua_State; n, d: Integer): Integer;
function luaL_optlong(L: lua_State; n: Integer; d: LongInt): LongInt;


(*
** {======================================================
** Generic Buffer manipulation
** =======================================================
*)

const
  BUFSIZ = 512; (* From stdio.h *)
  LUAL_BUFFERSIZE = BUFSIZ;


type

  luaL_Buffer = record
    p: PAnsiChar; (* current position in buffer *)
    lvl: Integer;  (* number of strings in the stack (level) *)
    L: lua_State;
    buffer: array[0..LUAL_BUFFERSIZE - 1] of Char;
  end;

procedure luaL_putchar(var B: luaL_Buffer; c: AnsiChar);

procedure luaL_addsize(var B: luaL_Buffer; n: Integer);

procedure luaL_buffinit(L: lua_State; var B: luaL_Buffer); cdecl;
function luaL_prepbuffer(var B: luaL_Buffer): PAnsiChar; cdecl;
procedure luaL_addlstring(var B: luaL_Buffer; s: PAnsiChar; ls: Cardinal); cdecl;
procedure luaL_addstring(var B: luaL_Buffer; s: PAnsiChar); cdecl;
procedure luaL_addvalue(var B: luaL_Buffer); cdecl;
procedure luaL_pushresult(var B: luaL_Buffer); cdecl;


(* }====================================================== */



(*
** Compatibility macros and functions
*)

function lua_dofile(L: lua_State; filename: PAnsiChar): Integer; cdecl;
function lua_dostring(L: lua_State; str: PAnsiChar): Integer; cdecl;
function lua_dobuffer(L: lua_State; buff: PAnsiChar; sz: Cardinal; name: PAnsiChar): Integer; cdecl;

function luaL_check_lstr(L: lua_State; numArg: Integer; var ls: Cardinal): PAnsiChar;
function luaL_opt_lstr(L: lua_State; numArg: Integer; def: PAnsiChar; var ls: Cardinal): PAnsiChar;
function luaL_check_number(L: lua_State; numArg: Integer): lua_Number;
function luaL_opt_number(L: lua_State; nArg: Integer; def: lua_Number): lua_Number;
function luaL_arg_check(L: lua_State; cond: Boolean; numarg: Integer; extramsg: PAnsiChar): Integer;
function luaL_check_string(L: lua_State; n: Integer): PAnsiChar;
function luaL_opt_string(L: lua_State; n: Integer; d: PAnsiChar): PAnsiChar;
function luaL_check_int(L: lua_State; n: Integer): Integer;
function luaL_check_long(L: lua_State; n: LongInt): LongInt;
function luaL_opt_int(L: lua_State; n, d: Integer): Integer;
function luaL_opt_long(L: lua_State; n: Integer; d: LongInt): LongInt;


implementation // ====================================================================================================================================================================== //
uses Misc, StrUtils;

const
{$IFDEF CPUX64}
    LuaDllName = 'Lua5.1.x64.dll'; // 5.1.4
    LuaLibDllName = 'Lua5.1.x64.dll';  // отдельную либу не юзать
{$ELSE}
    LuaDllName = 'Lua5.1.dll'; // 5.1.4
    LuaLibDllName = 'Lua5.1.dll';  // отдельную либу не юзать
{$ENDIF}

function lua_upvalueindex(i: Integer): Integer;
begin
  Result := LUA_GLOBALSINDEX - i;
end;

function lua_open: lua_State; cdecl; external LuaDllName;
procedure lua_close(L: lua_State); cdecl; external LuaDllName;
function lua_newstate(f: lua_Alloc; ud: Pointer) : lua_State; cdecl; external LuaDllName;
function lua_newthread(L: lua_State): lua_State; cdecl; external LuaDllName;
function lua_atpanic(L: lua_State; panicf: lua_CFunction): lua_CFunction; cdecl; external LuaDllName;
function lua_gettop(L: lua_State): Integer; cdecl; external LuaDllName;
procedure lua_settop(L: lua_State; idx: Integer); cdecl; external LuaDllName;
procedure lua_pushvalue(L: lua_State; idx: Integer); cdecl; external LuaDllName;
procedure lua_remove(L: lua_State; idx: Integer); cdecl; external LuaDllName;
procedure lua_insert(L: lua_State; idx: Integer); cdecl; external LuaDllName;
procedure lua_replace(L: lua_State; idx: Integer); cdecl; external LuaDllName;
function lua_checkstack(L: lua_State; sz: Integer): LongBool; cdecl; external LuaDllName;
procedure lua_xmove(from, dest: lua_State; n: Integer); cdecl; external LuaDllName;
function lua_isnumber(L: lua_State; idx: Integer): LongBool; cdecl; external LuaDllName;
function lua_isstring(L: lua_State; idx: Integer): LongBool; cdecl external LuaDllName;
function lua_iscfunction(L: lua_State; idx: Integer): LongBool; cdecl; external LuaDllName;
function lua_isuserdata(L: lua_State; idx: Integer): LongBool; cdecl; external LuaDllName;
function lua_type(L: lua_State; idx: Integer): Integer; cdecl; external LuaDllName;
function lua_typename(L: lua_State; tp: Integer): PAnsiChar; cdecl; external LuaDllName;

function lua_typeof(L: lua_State; idx: Integer): String;
var ps: PAnsiChar;
    s: AnsiString;
begin
 idx := lua_type (L, idx);
 ps := lua_typename (L, idx);
 SetString (s, ps, StrLen (ps) );
 result := String (s);
end;

function lua_equal(L: lua_State; idx1, idx2: Integer): LongBool; cdecl; external LuaDllName;
function lua_rawequal(L: lua_State; idx1, idx2: Integer): LongBool; cdecl; external LuaDllName;
function lua_lessthan(L: lua_State; idx1, idx2: Integer): LongBool; cdecl; external LuaDllName;
function lua_tointeger(L: lua_State; idx: Integer): lua_Integer; cdecl; external LuaDllName;
function lua_tonumber(L: lua_State; idx: Integer): lua_Number; cdecl; external LuaDllName;
function lua_toboolean(L: lua_State; idx: Integer): LongBool; cdecl; external LuaDllName;
function lua_tolstring(L: lua_State; idx: Integer; var rlen: SIZE_T): PAnsiChar; cdecl; external LuaDllName;

function lua_tostring(L: lua_State; idx: Integer): PAnsiChar; cdecl;
var rlen: SIZE_T;
begin
 result := lua_tolstring(L, idx, rlen);
end;

function lua_strlen(L: lua_State; idx: Integer): Cardinal; cdecl; external LuaDllName;
function lua_tocfunction(L: lua_State; idx: Integer): lua_CFunction; cdecl; external LuaDllName;
function lua_touserdata(L: lua_State; idx: Integer): Pointer; cdecl; external LuaDllName;
function lua_tothread(L: lua_State; idx: Integer): lua_State; cdecl; external LuaDllName;
function lua_topointer(L: lua_State; idx: Integer): Pointer; cdecl; external LuaDllName;
procedure lua_pushnil(L: lua_State); cdecl; external LuaDllName;
procedure lua_pushnumber(L: lua_State; n: lua_Number); cdecl; external LuaDllName;
procedure lua_pushinteger(L: lua_State; i: Integer); cdecl; external LuaDllName;
procedure lua_pushlstring(L: lua_State; s: PAnsiChar; ls: Cardinal); cdecl; external LuaDllName;
procedure lua_pushstring(L: lua_State; s: PAnsiChar); cdecl; external LuaDllName;
function lua_pushvfstring(L: lua_State; fmt, argp: PAnsiChar): PAnsiChar; cdecl; external LuaDllName;
function lua_pushfstring(L: lua_State; fmt: PAnsiChar): PAnsiChar; cdecl; varargs; external LuaDllName;


procedure lua_pushnumbers (L: lua_State; n1, n2: lua_Number); overload; inline;
begin
 lua_pushnumber (L, n1); lua_pushnumber (L, n2);
end;
procedure lua_pushnumbers (L: lua_State; n1, n2, n3: lua_Number); overload; inline;
begin
 lua_pushnumbers (L, n1, n2);
 lua_pushnumber (L, n3);
end;




procedure lua_pushwstr (L: lua_State; const s: String);
var
   sa: AnsiString;
begin
 sa := AnsiString (s);
 lua_pushstring (L, PAnsiChar (sa));
end;



procedure lua_pushcclosure(L: lua_State; fn: lua_CFunction; n: Integer); cdecl; external LuaDllName;
procedure lua_pushboolean(L: lua_State; b: Integer); cdecl; external LuaDllName;
procedure lua_pushboolean(L: lua_State; b: LongBool); cdecl; external LuaDllName;
procedure lua_pushlightuserdata(L: lua_State; p: Pointer); cdecl; external LuaDllName;
procedure lua_pushptr (L: lua_State; p: Pointer); inline;
begin
 lua_pushlightuserdata(L, p);
end;

procedure lua_gettable(L: lua_State; idx: Integer); cdecl; external LuaDllName;
procedure lua_rawget(L: lua_State; idx: Integer); cdecl; external LuaDllName;
procedure lua_rawgeti(L: lua_State; idx, n: Integer); cdecl; external LuaDllName;
procedure lua_createtable(L: lua_State; narr, nrec: Integer); cdecl; external LuaDllName;

function lua_newtable(L: lua_State): Integer;
begin
 lua_createtable (L, 0, 0);
 result := lua_gettop (L);
end;

function lua_newuserdata(L: lua_State; sz: Cardinal): Pointer; cdecl; external LuaDllName;
function lua_getmetatable(L: lua_State; objindex: Integer): Integer; cdecl; external LuaDllName;
procedure lua_getfenv(L: lua_State; idx: Integer); cdecl; external LuaDllName;
procedure lua_getfield(L: lua_State; idx: Integer; const k: PAnsiChar); cdecl; external LuaDllName;
procedure lua_settable(L: lua_State; idx: Integer); cdecl; external LuaDllName;
procedure lua_rawset(L: lua_State; idx: Integer); cdecl; external LuaDllName;
procedure lua_rawseti(L: lua_State; idx, n: Integer); cdecl; external LuaDllName;
function lua_setmetatable(L: lua_State; objindex: Integer): LongBool; cdecl; external LuaDllName;
function lua_setfenv(L: lua_State; idx: Integer): LongBool; cdecl; external LuaDllName;
procedure lua_setfield(L: lua_state; idx: Integer; const k: PAnsiChar); cdecl; external LuaDllName;

procedure lua_call(L: lua_State; nargs, nresults: Integer); cdecl; external LuaDllName;
function lua_pcall(L: lua_State; nargs, nresults, errfunc: Integer): Integer; cdecl; external LuaDllName;
function lua_cpcall(L: lua_State; func: lua_CFunction; ud: Pointer): Integer; cdecl; external LuaDllName;
function lua_load(L: lua_State; reader: lua_Chunkreader; dt: Pointer; chunkname: PAnsiChar): Integer; cdecl; external LuaDllName;
function lua_dump(L: lua_State; writer: lua_Chunkwriter; data: Pointer): Integer; cdecl; external LuaDllName;
function lua_yield(L: lua_State; nresults: Integer): Integer; cdecl; external LuaDllName;
function lua_resume(L: lua_State; narg: Integer): Integer; cdecl; external LuaDllName;
function lua_getgcthreshold(L: lua_State): Integer; cdecl; external LuaDllName;
function lua_getgccount(L: lua_State): Integer; cdecl; external LuaDllName;
function lua_gc (L: lua_State; what, data: Integer): Integer; cdecl; external LuaDllName;
procedure lua_setgcthreshold(L: lua_State; newthreshold: Integer); cdecl; external LuaDllName;
function lua_version: PAnsiChar; cdecl; external LuaDllName;
function lua_error(L: lua_State): Integer; cdecl; external LuaDllName;
function lua_next(L: lua_State; idx: Integer): Integer; cdecl; external LuaDllName;
function lua_objlen(L: lua_State; idx: Integer): SIZE_T; cdecl; external LuaDllName;
procedure lua_concat(L: lua_State; n: Integer); cdecl; external LuaDllName;

function lua_boxpointer(L: lua_State; u: Pointer): Pointer;
begin
  Result := lua_newuserdata(L, SizeOf(Pointer));
end;

function lua_unboxpointer(L: lua_State; i: Integer): Pointer;
begin
  Result := lua_touserdata(L, i);
end;

procedure lua_pop(L: lua_State; n: Integer);
begin
  lua_settop(L, -(n) - 1);
end;

procedure lua_pushliteral(L: lua_State; s: PAnsiChar);
begin
  lua_pushlstring(L, s, StrLen(s) - 1);
end;

function lua_pushupvalues(L: lua_State): Integer; cdecl; external LuaDllName;

procedure lua_getregistry(L: lua_State);
begin
  lua_pushvalue(L, LUA_REGISTRYINDEX);
end;

procedure lua_setglobal(L: lua_State; const s: String);
begin
  lua_setfield(L, LUA_GLOBALSINDEX, PAnsiChar ( AnsiString (s) ));
end;

procedure lua_getglobal(L: lua_State; const s: String);
begin
  lua_getfield(L, LUA_GLOBALSINDEX, PAnsiChar ( AnsiString (s) ));
end;

procedure lua_setglobal_nil(L: lua_State; const s: String);
begin
 lua_pushnil (L);
 lua_setglobal (L, s);
end;

function lua_ref(L: lua_State; lock: Boolean): Integer;
begin
  if (lock) then
    Result := luaL_ref(L, LUA_REGISTRYINDEX)
  else
  begin
    lua_pushstring(L, 'unlocked references are obsolete');
    lua_error(L);
    Result := 0;
  end;
end;

procedure lua_unref(L: lua_State; ref: Integer);
begin
  luaL_unref(L, LUA_REGISTRYINDEX, ref);
end;

procedure lua_getref(L: lua_State; ref: Integer);
begin
  lua_rawgeti(L, LUA_REGISTRYINDEX, ref);
end;

function lua_getstack(L: lua_State; level: Integer; var ar: lua_Debug): Integer; cdecl; external LuaDllName;
function lua_getinfo(L: lua_State; what: PAnsiChar; var ar: lua_Debug): Integer; cdecl; external LuaDllName;
function lua_getlocal(L: lua_State; var ar: lua_Debug; n: Integer): PAnsiChar; cdecl; external LuaDllName;
function lua_setlocal(L: lua_State; var ar: lua_Debug; n: Integer): PAnsiChar; cdecl; external LuaDllName;
function lua_getupvalue(L: lua_State; funcindex, n: Integer): PAnsiChar; cdecl; external LuaDllName;
function lua_setupvalue(L: lua_State; funcindex, n: Integer): PAnsiChar; cdecl; external LuaDllName;

function lua_sethook(L: lua_State; func: lua_Hook; mask, count: Integer): Integer; cdecl; external LuaDllName;
function lua_gethook(L: lua_State): lua_Hook; cdecl; external LuaDllName;
function lua_gethookmask(L: lua_State): Integer; cdecl; external LuaDllName;
function lua_gethookcount(L: lua_State): Integer; cdecl; external LuaDllName;

function luaopen_base(L: lua_State): Integer; cdecl; external LuaLibDllName;
function luaopen_table(L: lua_State): Integer; cdecl; external LuaLibDllName;
function luaopen_io(L: lua_State): Integer; cdecl; external LuaLibDllName;
function luaopen_string(L: lua_State): Integer; cdecl; external LuaLibDllName;
function luaopen_math(L: lua_State): Integer; cdecl; external LuaLibDllName;
function luaopen_debug(L: lua_State): Integer; cdecl; external LuaLibDllName;
function luaopen_loadlib(L: lua_State): Integer; cdecl; external LuaLibDllName;
function luaopen_os (L: lua_State): Integer; cdecl; external LuaLibDllName;

procedure lua_assert(c: Boolean);
begin
end;

function lua_baselibopen(L: lua_State): Integer;
begin
  Result := luaopen_base(L);
end;

function lua_tablibopen(L: lua_State): Integer;
begin
  Result := luaopen_table(L);
end;

function lua_iolibopen(L: lua_State): Integer;
begin
  Result := luaopen_io(L);
end;

function lua_strlibopen(L: lua_State): Integer;
begin
  Result := luaopen_string(L);
end;

function lua_mathlibopen(L: lua_State): Integer;
begin
  Result := luaopen_math(L);
end;

function lua_dblibopen(L: lua_State): Integer;
begin
  Result := luaopen_debug(L);
end;

procedure luaL_openlib(L: lua_State; libname: PAnsiChar; lr: array of luaL_reg; nup: Integer); cdecl; external LuaLibDllName;
procedure luaL_openlibs(L: lua_State); cdecl; external LuaLibDllName;
function luaL_getmetafield(L: lua_State; obj: Integer; e: PAnsiChar): Integer; cdecl; external LuaLibDllName;
function luaL_callmeta(L: lua_State; obj: Integer; e: PAnsiChar): Integer; cdecl; external LuaLibDllName;
function luaL_typerror(L: lua_State; narg: Integer; tname: PAnsiChar): Integer; cdecl; external LuaLibDllName;
function luaL_argerror(L: lua_State; numarg: Integer; extramsg: PAnsiChar): Integer; cdecl; external LuaLibDllName;
function luaL_checklstring(L: lua_State; numArg: Integer; var ls: Cardinal): PAnsiChar; cdecl; external LuaLibDllName;
function luaL_optlstring(L: lua_State; numArg: Integer; def: PAnsiChar; var ls: Cardinal): PAnsiChar; cdecl; external LuaLibDllName;
function luaL_checknumber(L: lua_State; numArg: Integer): lua_Number; cdecl; external LuaLibDllName;
function luaL_optnumber(L: lua_State; nArg: Integer; def: lua_Number): lua_Number; cdecl; external LuaLibDllName;

procedure luaL_checkstack(L: lua_State; sz: Integer; msg: PAnsiChar); cdecl; external LuaLibDllName;
procedure luaL_checktype(L: lua_State; narg, t: Integer); cdecl; external LuaLibDllName;
procedure luaL_checkany(L: lua_State; narg: Integer); cdecl; external LuaLibDllName;

function luaL_newmetatable(L: lua_State; tname: PAnsiChar): Integer; cdecl; external LuaLibDllName;
procedure luaL_getmetatable(L: lua_State; tname: PAnsiChar); cdecl; external LuaLibDllName;
procedure luaL_checkudata(L: lua_State; ud: Integer; tname: PAnsiChar); cdecl; external LuaLibDllName;

procedure luaL_where(L: lua_State; lvl: Integer); cdecl; external LuaLibDllName;
function luaL_error(L: lua_State; fmt: PAnsiChar): Integer; cdecl; varargs; external LuaLibDllName;

function luaL_findstring(st: PAnsiChar; lst: array of PAnsiChar): Integer; cdecl; external LuaLibDllName;

function luaL_ref(L: lua_State; t: Integer): Integer; cdecl; external LuaLibDllName;
procedure luaL_unref(L: lua_State; t, ref: Integer); cdecl; external LuaLibDllName;

function luaL_getn(L: lua_State; t: Integer): Integer; cdecl; external LuaLibDllName;
procedure luaL_setn(L: lua_State; t, n: Integer); cdecl; external LuaLibDllName;

function luaL_loadfile(L: lua_State; filename: PAnsiChar): Integer; cdecl; external LuaLibDllName;
function luaL_loadbuffer(L: lua_State; buff: PAnsiChar; sz: Cardinal; name: PAnsiChar): Integer; cdecl; external LuaLibDllName;

function luaL_argcheck(L: lua_State; cond: Boolean; numarg: Integer; extramsg: PAnsiChar): Integer;
begin
  Result := 0;
  if not cond then
    Result := luaL_argerror(L, numarg, extramsg);
end;

function luaL_checkstring(L: lua_State; n: Integer): PAnsiChar;
var
  ls: Cardinal;
begin
  Result := luaL_checklstring(L, n, ls);
end;

function luaL_optstring(L: lua_State; n: Integer; d: PAnsiChar): PAnsiChar;
var
  ls: Cardinal;
begin
  Result := luaL_optlstring(L, n, d, ls);
end;

function luaL_checkint(L: lua_State; n: Integer): Integer;
begin
  Result := Trunc(luaL_checknumber(L, n));
end;

function luaL_checklong(L: lua_State; n: LongInt): LongInt;
begin
  Result := Trunc(luaL_checknumber(L, n));
end;

function luaL_optint(L: lua_State; n, d: Integer): Integer;
begin
  Result := Trunc(luaL_optnumber(L, n, d));
end;

function luaL_optlong(L: lua_State; n: Integer; d: LongInt): LongInt;
begin
  Result := Trunc(luaL_optnumber(L, n, d));
end;

procedure luaL_putchar(var B: luaL_Buffer; c: AnsiChar);
begin
  if Integer(B.p) < Integer((B.buffer) + LUAL_BUFFERSIZE) then
    luaL_prepbuffer(B);
  B.p^ := c;
  Inc(B.p);
end;

procedure luaL_addsize(var B: luaL_Buffer; n: Integer);
begin
  Inc(B.p, n);
end;

procedure luaL_buffinit(L: lua_State; var B: luaL_Buffer); cdecl; external LuaLibDllName;
function luaL_prepbuffer(var B: luaL_Buffer): PAnsiChar; cdecl; external LuaLibDllName;
procedure luaL_addlstring(var B: luaL_Buffer; s: PAnsiChar; ls: Cardinal); cdecl; external LuaLibDllName;
procedure luaL_addstring(var B: luaL_Buffer; s: PAnsiChar); cdecl; external LuaLibDllName;
procedure luaL_addvalue(var B: luaL_Buffer); cdecl;   external LuaLibDllName;
procedure luaL_pushresult(var B: luaL_Buffer); cdecl; external LuaDllName;

function luaL_newstate: Pointer; cdecl; external LuaLibDllName;
function luaL_loadstring(L: lua_State; S: PAnsiChar): Integer; cdecl; external LuaDllName;

function lua_dofile(L: lua_State; filename: PAnsiChar): Integer; cdecl;
begin
 luaL_loadfile(L, filename);
 result := lua_pcall(L, 0, LUA_MULTRET, 0);
end; // dofile

function lua_dostring(L: lua_State; str: PAnsiChar): Integer; cdecl;
begin
 result := luaL_loadstring (L, str);
 if result = 0 then
    result := lua_pcall(L, 0, LUA_MULTRET, 0);
end; // dostring

function lua_dobuffer(L: lua_State; buff: PAnsiChar; sz: Cardinal; name: PAnsiChar): Integer; cdecl;
begin
 luaL_loadbuffer (L, buff, sz, name);
 result := lua_pcall(L, 0, LUA_MULTRET, 0);
end; // dobuffer



function luaL_check_lstr(L: lua_State; numArg: Integer; var ls: Cardinal): PAnsiChar;
begin
  Result := luaL_checklstring(L, numArg, ls);
end;

function luaL_opt_lstr(L: lua_State; numArg: Integer; def: PAnsiChar; var ls: Cardinal): PAnsiChar;
begin
  Result := luaL_optlstring(L, numArg, def, ls);
end;

function luaL_check_number(L: lua_State; numArg: Integer): lua_Number;
begin
  Result := luaL_checknumber(L, numArg);
end;

function luaL_opt_number(L: lua_State; nArg: Integer; def: lua_Number): lua_Number;
begin
  Result := luaL_optnumber(L, nArg, def);
end;

function luaL_arg_check(L: lua_State; cond: Boolean; numarg: Integer; extramsg: PAnsiChar): Integer;
begin
  Result := luaL_argcheck(L, cond, numarg, extramsg);
end;

function luaL_check_string(L: lua_State; n: Integer): PAnsiChar;
begin
  Result := luaL_checkstring(L, n);
end;

function luaL_opt_string(L: lua_State; n: Integer; d: PAnsiChar): PAnsiChar;
begin
  Result := luaL_optstring(L, n, d);
end;

function luaL_check_int(L: lua_State; n: Integer): Integer;
begin
  Result := luaL_checkint(L, n);
end;

function luaL_check_long(L: lua_State; n: LongInt): LongInt;
begin
  Result := luaL_checklong(L, n);
end;

function luaL_opt_int(L: lua_State; n, d: Integer): Integer;
begin
  Result := luaL_optint(L, n, d);
end;

function luaL_opt_long(L: lua_State; n: Integer; d: LongInt): LongInt;
begin
  Result := luaL_optlong(L, n, d);
end;



function luaL_dostring(L: lua_state; s: PAnsiChar): lua_Result;

begin
 luaL_loadstring(L, s);
 result := lua_pcall(L, 0, LUA_MULTRET, 0);
end;




initialization
finalization
end.
