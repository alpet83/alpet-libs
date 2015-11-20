unit EasyECDSA;

interface
uses Windows, SysUtils, Classes, Math, Misc, LuaTypes, LuaImports, LuaTools;

{$I stkdef.inc}

type
    TBuffer1K = packed record
     cbLength: DWORD;
         data: array [0..1019] of BYTE;

     procedure  Reset;
    end;
    PBuffer1K = ^TBuffer1K;

procedure AddGlobals (L: lua_State; disp: Boolean);


{$IFDEF USING_CRYPTO}
function        create_keys       (priv_k, pub_k: PBuffer1K): Boolean; cdecl;
function        sign_data         (data, priv_k, signature: PBuffer1K): Boolean; cdecl;
function        verify_signature  (data, pub_k, signature: PBuffer1K): Boolean;  cdecl;
{$ENDIF}

implementation

// var


type
    TECDSAContext = class
    private
        priv_k: TBuffer1K;
    public
     signature: TBuffer1K;
         pub_k: TBuffer1K;

     function           field(tag: String): PBuffer1K;
    end;


procedure  TBuffer1K.Reset;
begin
 FillChar (self, sizeof(self), 0);
end;

{ TECDSAContext }

function TECDSAContext.field(tag: String): PBuffer1K;
begin
 result := nil;

 if tag = 'private' then
    result := @priv_k else
 if tag = 'public'  then
    result := @pub_k  else
 if tag = 'signature' then
    result := @signature;
end;



{$IFDEF USING_CRYPTO}

function        create_keys       (priv_k, pub_k: PBuffer1K): Boolean; cdecl; external 'EasyECDSA.dll' name '?create_keys@@YA_NPAU_BUFFER@@0@Z';
function        sign_data         (data, priv_k, signature: PBuffer1K): Boolean; cdecl; external 'EasyECDSA.dll' name '?sign_data@@YA_NPAU_BUFFER@@00@Z';
function        verify_signature  (data, pub_k, signature: PBuffer1K): Boolean;  cdecl; external 'EasyECDSA.dll' name '?verify_signature@@YA_NPAU_BUFFER@@00@Z';


// методы Lua-класса ECDSAContext
function        _create_keys(L: lua_State): Integer; cdecl;
var
   ctx: TECDSAContext;
    ok: Boolean;
begin
 ctx := lua_objptr (L, 1);
 ok := create_keys (@ctx.priv_k, @ctx.pub_k);
 lua_pushboolean (L, ok);
 result := 1;
end;



function       _init_ctx (L: lua_State): Integer; cdecl;
var
   ctx: TECDSAContext;
   tag: String;
   src: Pointer;
   dst: PBuffer1K;
begin
 ctx := lua_objptr (L, 1);
 Assert (ctx <> nil, 'ctx = nil for ECDSAContext method ');

 tag := LuaStrArg (L, 2);
 src := lua_topointer (L, 3);
 dst := ctx.field (tag);
 result := 1;
 if (dst = nil) or (src = nil) then
   begin
    PrintError('ECDSAContext:init invalid parameters for tag ' + tag);
    lua_pushboolean (L, FALSE);
    exit;
   end;

 dst.cbLength := lua_tointeger (L, 4);
 Assert (dst.cbLength <= High (dst.data), 'Buffer overrun in ECDSAContext:init');
 Move (src^, dst.data, dst.cbLength);
 lua_pushboolean (L, dst.cbLength > 0);
{$IFOPT D+}
 wprintf('[~T]. #DBG: ECDSAContext initialized field %s from $%p with %d bytes length',
                [tag, src, dst.cbLength]);
{$ENDIF}
end;

function       _store_field (L: lua_State): Integer; cdecl;
var
   ctx: TECDSAContext;
   tag: String;
   dst: Pointer;
   src: PBuffer1K;
   lim: Integer;
begin
 result := 1;
 ctx := lua_objptr (L, 1);
 Assert (ctx <> nil, 'ctx = nil for ECDSAContext method ');
 tag := LuaStrArg (L, 2);
 dst := lua_topointer (L, 3);
 src := ctx.field (tag);
 lim := lua_tointeger (L, 4);
 if (dst = nil) or (src = nil) then
   begin
    PrintError('ECDSAContext:store invalid parameters for tag ' + tag);
    lua_pushboolean (L, FALSE);
    exit;
   end;
 Move (src.data, dst^, Min (src.cbLength, lim));
 lua_pushinteger (L, src.cbLength);
end;


function        _sign_data(L: lua_State): Integer; cdecl;
var
   ctx: TECDSAContext;
   dat: TBuffer1K;
   src: Pointer;
    ok: Boolean;
begin
 result := 1;
 ctx := lua_objptr (L, 1);
 Assert (ctx <> nil, 'ctx = nil for ECDSAContext method ');
 src := lua_topointer (L, 2);
 if src = nil then
   begin
    PrintError('sign_data - invalid src pointer');
    exit;
   end;
 dat.cbLength := lua_tointeger (L, 3);
 Assert (dat.cbLength <= High (dat.data), 'Buffer overrun in ECDSAContext.sign_data');
 Move (src^, dat.data, dat.cbLength);
 ok := FALSE;
 try
   ok := sign_data (@dat, @ctx.priv_k, @ctx.signature);
 except
  on E: Exception do
   begin
    wprintf('#FATAL: cannot perform signature, private_key size = %d, signature size = %d',
                [ctx.priv_k.cbLength, ctx.signature.cbLength] );
    OnExceptLog('_sign_data', E, TRUE);
   end;

 end;
 lua_pushboolean (L, ok);
end;

function       _verify_signature (L: lua_State): Integer; cdecl;
var
   ctx: TECDSAContext;
   dat: TBuffer1K;
   src: Pointer;
    ok: Boolean;
begin
 ctx := lua_objptr (L, 1);
 Assert (ctx <> nil, 'ctx = nil for ECDSAContext method ');
 src := lua_topointer (L, 2);
 dat.cbLength := lua_tointeger (L, 3);
 Assert (dat.cbLength <= High (dat.data), 'Buffer overrun in ECDSAContext.verify_signature');
 Move (src^, dat.data, dat.cbLength);
 ok := verify_signature (@dat, @ctx.pub_k, @ctx.signature);
 lua_pushboolean (L, ok);
 result := 1;
end;


function _context_index (L: lua_State): Integer; cdecl;
var
   ctx: TECDSAContext;
   key: String;

begin
 ctx := lua_objptr (L, 1);
 key := LuaStrArg (L, 2);

 if key = 'create_keys' then
    lua_pushcfunction (L, _create_keys) else
 if key = 'init' then
    lua_pushcfunction (L, _init_ctx) else
 if key = 'store' then
    lua_pushcfunction (L, _store_field) else
 if key = 'sign_data' then
    lua_pushcfunction (L, _sign_data) else
 if key = 'verify_signature' then
    lua_pushcfunction (L, _verify_signature) else
 // properties
 if key = 'private_key' then
    lua_pushptr (L, @ctx.priv_k) else
 if key = 'public_key' then
    lua_pushptr (L, @ctx.pub_k) else
 if key = 'signature' then
    lua_pushptr (L, @ctx.signature)
 else
    lua_pushnil (L);


 if lua_islightuserdata (L, -1) then
    SetPointerMT (L, -1);

 result := 1;
end;

function _new_context (L: lua_State): Integer; cdecl;
var
   ctx: TECDSAContext;
begin
 ctx := TECDSAContext.Create;
 result := 1;
 AssignMetaIndex (L, ctx, _context_index, nil, 'GMT_ECDSA_CONTEXT', MTF_OBJECT or MTF_ADD_GC);
end;


{$ENDIF}

procedure AddGlobals (L: lua_State; disp: Boolean);
begin
 if not Assigned(L) then exit;
{$IFDEF USING_CRYPTO}
 LuaRegFunc (L, 'ECDSAContext',      _new_context,      '() // creates object for signature/verify data purpose',   disp);
{$ENDIF}
end;


initialization





end.
