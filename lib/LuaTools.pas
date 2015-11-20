unit LuaTools;

interface
{$I luadef.inc}
{$I stkdef.inc}

uses Windows, SysUtils, Classes, Misc, LuaTypes, Math, MD5, BaseLib, PSUtils, StrUtils, DateTimeTools, IniFiles,
     LuaImports,
     StrClasses, ArrayTypes;


const
    MTF_OBJECT = 1;
    MTF_ADD_GC = 2;


type
    PShort = ^SmallInt;
    THashVector = array [0..31] of BYTE;

    TRandomHash = packed record // 4xInt64 with slow "encryption"
    private
     raw_data:  THashVector;

     function                      ByteAddr (ofs: DWORD): PByte; // inline;
     procedure                     ReadBlock  (dst: PByteArray; ofs, size: DWORD); inline;
     procedure                     WriteBlock (src: PByteArray; ofs, size: DWORD); inline;

     function                      GetNonce: LARGE_INTEGER; inline;
     procedure                     SetNonce (const n: LARGE_INTEGER); inline;
     function                      GetNonce64: Int64; inline;
     procedure                     SetNonce64(const Value: Int64);
     function                      GetDigest: TMD5Digest;
     procedure                     SetDigest(const Value: TMD5Digest);
    procedure SetRotation(const Value: BYTE);

    public

     property     MD: TMD5Digest       read GetDigest   write SetDigest;
     property     Nonce: LARGE_INTEGER read GetNonce    write SetNonce;
     property     Nonce64: Int64       read GetNonce64  write SetNonce64;
     property     Rotation: BYTE       read raw_data[0] write SetRotation;



     procedure    Calc ( pBuff: Pointer; cbSize: DWORD ); overload;
     procedure    Calc ( buff: String ); overload;
     procedure    CheckPermut;
     function     Compare ( const ref: TRandomHash ): Boolean; inline;

     function     CRC: DWORD;
     function     Format: String;
     function     FormatRaw: String;

     procedure    RandInit;
     procedure    SetNonceHigh (v: DWORD); inline;
     procedure    SetNonceLow  (v: DWORD); inline;
    end;

    PRandomHash = ^TRandomHash;

const
    RHASH_SIZE = sizeof (TRandomHash);



type
    TRHashVector = packed array [0..RHASH_SIZE - 1] of BYTE;


    TDataWrapper = packed record
        obj: Pointer;
       sign: array [0..3] of AnsiChar; // signature pOBJ
      procedure    Init ( src: Pointer );
    end;

    PDataWrapper = ^TDataWrapper;

   PLuaEnv = ^TLuaEnv;

   TLuaEnv = record
      L: lua_State;
    env: Integer;
    top: Integer;

    procedure init (State: lua_State; env_idx: Integer = LUA_GLOBALSINDEX);

    function field_exists (idx: integer): Integer;

    function get_field (const idx: String): TLuaEnv;  overload;
    function get_field (const idx: Integer): TLuaEnv; overload;

    function is_table: Integer;


    function to_integer (rmv: Boolean): Integer;
    function to_number  (rmv: Boolean): lua_Number;
    function to_string  (rmv: Boolean): String;
    function to_pointer (rmv: Boolean): Pointer;
    function to_object  (rmv: Boolean): Pointer;

    function pop_field: Integer;
    function select (new_top: Integer): TLuaEnv;


   end;



procedure AssignMetaIndex ( L: lua_State; pData: Pointer; fIndex, fNewIndex: lua_CFunction; const mtname: String; flags: DWORD = 0 );
{$IFDEF CPUX86}
function  BinThisCall(L: lua_State): Integer; cdecl;
{$ENDIF}

function  LuaBitAND (L: lua_State): Integer; cdecl;
function  LuaBitOR (L: lua_State): Integer; cdecl;

{ memory functions }
function  LuaCalcCRC (L: lua_State): Integer; cdecl;
function  LuaFormatPtr(L: lua_State): Integer; cdecl;
function  LuaFindSubStr (L: lua_State): Integer; cdecl;
function  LuaSplit (L: lua_State): Integer; cdecl;
procedure LuaPushMemValue (L: lua_State; pAddr: Pointer; tag: String; cb: Integer);
function  LuaReadPtr (L: lua_State): Integer; cdecl;
function  LuaReadDMA (L: lua_State): Integer; cdecl;
function  LuaWriteDMA (L: lua_State): Integer; cdecl;
function  LuaUnlockDMA (L: lua_State): Integer; cdecl;
function  LuaRandomHash (L: lua_State): Integer; cdecl;

{ universal convertors }
function LuaAddr (L: lua_State; idx: Integer): NativeUInt; inline;
function LuaDWORD (L: lua_State; idx: Integer): UInt64;  inline;
function LuaQWORD (L: lua_State; idx: Integer): UInt64;

procedure LuaDumpStrings (L: lua_State; sl: TStrings; f: DWORD);


function  LuaFreeData (L: lua_State): Integer; cdecl;
function  LuaFreeObject (L: lua_State): Integer; cdecl;
function  LuaIniFile (L: lua_State): Integer; cdecl;

function  LuaStrArg (L: lua_State; n_arg: Integer = 1): String;
function  LuaTraceBack (L: lua_State; const sp: String = ''; depth: Integer = 2): String;
procedure LuaRegFunc (L: lua_State; const func: AnsiString; fptr: lua_CFunction; const params: String = ''; disp: Boolean = FALSE);

procedure SetPointerMT ( L: lua_State; idx: Integer );

function DumpValue (L: lua_State; idx: Integer): String;

function _LuaDumpVar (L: lua_State): Integer; cdecl;
function _StrToInt64 (L: lua_State): Integer; cdecl;
function _PtrIndex   (L: lua_State): Integer; cdecl;



procedure lua_assert (L: lua_State; condition: Boolean; const msg: String);
function lua_getobj ( L: lua_State; const name: String; pop_value: Boolean = FALSE ): Pointer;
function lua_objptr ( L: lua_State; idx: Integer; bClear: Boolean = FALSE ): Pointer;
function lua_objptr_req ( L: lua_State; idx: Integer; const msg: String ): Pointer;


procedure lua_pushcfunction (L: lua_State; f: lua_CFunction); inline;
procedure lua_pushint64 (L: lua_State; v: Int64);
procedure lua_pushobj (L: lua_State; pobj: Pointer);

function  lua_isfunction (L: lua_State; n: Integer): Boolean;
function  lua_istable (L: lua_State; n: Integer): Boolean;
function  lua_islightuserdata (L: lua_State; n: Integer): Boolean;
function  lua_isnil (L: lua_State; n: Integer): Boolean;
function  lua_isboolean (L: lua_State; n: Integer): Boolean;
function  lua_isnone (L: lua_State; n: Integer): Boolean;
function  lua_isnoneornil (L: lua_State; n: Integer): Boolean;


procedure lua_setlasterr (L: lua_State; err: String);
procedure lua_sf (L: lua_State; const k: String; idx: Integer = -2); inline;

procedure lua_export_array (L: lua_State; lpArray: PPointerArray; nCount: Integer; pExport: TLuaProcEx);

procedure lua_setarr_b (L: lua_State; i: Integer; v: Boolean; tidx: Integer = -3);
procedure lua_setarr_i (L: lua_State; i: Integer; v: Int64;   tidx: Integer = -3);
procedure lua_setarr_f (L: lua_State; i: Integer; v: Double;  tidx: Integer = -3);
procedure lua_setarr_s (L: lua_State; i: Integer; v: String;  tidx: Integer = -3);
procedure lua_setarr_p (L: lua_State; i: Integer; p: Pointer; tidx: Integer = -3);

procedure lua_setmap_b (L: lua_State; const k: String; v: Boolean; tidx: Integer = -2);
procedure lua_setmap_c (L: lua_State; const k: String; c: lua_CFunction; tidx: Integer = -2);
procedure lua_setmap_i (L: lua_State; const k: String; v: Int64;   tidx: Integer = -2);
procedure lua_setmap_f (L: lua_State; const k: String; v: Double;  tidx: Integer = -2);
procedure lua_setmap_s (L: lua_State; const k, v: String;          tidx: Integer = -2);
procedure lua_setmap_p (L: lua_State; const k: String; p: Pointer; tidx: Integer = -2);
procedure lua_setmap_lu (L: lua_State; const k: String; p: Pointer; tidx: Integer = -2);

procedure lua_get_field (L: lua_State; idx: Integer; const k: String); inline;
procedure lua_set_field (L: lua_State; idx: Integer; const k: String); inline;

procedure lua_register (L: lua_state; n: PAnsiChar;    f: lua_CFunction);
function  lua_table_type ( L: lua_State; idx: Integer; pSize: PInteger = nil ): Integer;
function  lua_toint64 (L: lua_State; idx: Integer): Int64;
function  lua_topstring(L: lua_State; idx: Integer): String;

// marco wrappers
function global_getboolean (L: lua_State; const s: String; pop_value: Boolean = TRUE): Boolean; inline;
function global_getinteger (L: lua_State; const s: String; pop_value: Boolean = TRUE): Int64; inline;
function global_getfloat   (L: lua_State; const s: String; pop_value: Boolean = TRUE): Double; inline;
function global_getpointer (L: lua_State; const s: String; pop_value: Boolean = TRUE): Pointer; inline;
function global_getstring  (L: lua_State; const s: String; pop_value: Boolean = TRUE): String; inline;
function global_gettable   (L: lua_State; const s: String): Boolean; inline;


function wtoa(const s: String): PAnsiChar; inline;


function _current_time (L: lua_State): Integer; cdecl;
function _precise_time (L: lua_State): Integer; cdecl;
function _sleep_ex (L: lua_State): Integer; cdecl;

procedure ExportAll (L: lua_State; disp: Boolean = FALSE);

function UpgradeStrings (L: lua_state; disp: Boolean = FALSE): Integer; cdecl;

function   f_random: Double; inline;
function u32_random: DWORD;  inline;

var
        mw_flags: DWORD = $0001;
     bc_disabled: Boolean = FALSE;
  func_blacklist: TList;             // !
   orig_str_indx: lua_CFunction;
    g_offset_map: TStrings = nil;
     hash_permut: THashVector;

implementation
uses Base64Tools, AnsiStrings;

const
   NONCE_BASE_OFFSET  = 2;
   DIGEST_BASE_OFFSET = 15;

var
   _traceback: lua_CFunction = nil;


function f_random: Double; inline;
begin
 result := Frac ( PreciseTime * Pi + Random() + 1 / GetCurrentProcessId );
end;

function u32_random: DWORD; inline;
begin
 result := Round ( f_random * 4294967296.0 );
end;


{ TRandomHash }

procedure TRandomHash.Calc(pBuff: Pointer; cbSize: DWORD);
var
   base: TMD5Digest;
    tmp: TMD5Digest;
     nv: LARGE_INTEGER;
begin
 nv := GetNonce;
 base := MD5Buffer ( nv, sizeof(nv) );
 tmp := MD5Buffer ( pBuff^, cbSize );
 tmp.L1 := tmp.L1 xor base.L2;
 tmp.L2 := tmp.L2 xor base.L1;
 MD := tmp;
end;

function TRandomHash.ByteAddr(ofs: DWORD): PByte;
begin
 ofs := hash_permut [1 + (ofs + Rotation) mod High(raw_data) ];
 Assert (ofs <= High(raw_data), 'ByteAddr - overrun!');
 result := @raw_data [ofs];
end;

procedure TRandomHash.Calc ( buff: String );
begin
 Calc (@buff[1], sizeof(CHAR) * Length(buff));
end;

procedure TRandomHash.CheckPermut;
var
   i: Integer;
begin
 for i := 1 to High (hash_permut) do
    Assert (hash_permut[i] <> hash_permut[0], 'hash_permut invalid. Raw data: ' + FormatRaw);
end;

function TRandomHash.Compare(const ref: TRandomHash): Boolean;
var
   pa, pb: PInt64Array;
   t1, t2: Boolean;
begin
 pa := @self.raw_data;
 pb := @ref.raw_data;
 t1 := ( pa[0] = pb[0] ) and ( pa[1] = pb[1] );
 t2 := ( pa[2] = pb[2] ) and ( pa[3] = pb[3] );
 result := t1 and t2;
 // result := MD5.MD5DigestCompare(md, ref.md) and ( nonce64 = ref.nonce64 );
end;

function TRandomHash.CRC: DWORD;
begin
 result := CalcCRC32(@raw_data, sizeof(raw_data));
end;

function TRandomHash.Format: String;
begin
 result := IntToHex (nonce64, 16);    // just prefix
 result := result + MD5.MD5DigestToStr ( md );
end;

function TRandomHash.FormatRaw: String;
var
   i: Integer;
begin
 result := '';
 for i := 0 to High (raw_data) do
     result := result + IntToHex (raw_data[i], 2) + ' ';
 result := result + 'P: ';
 for i := 0 to High (hash_permut) do
     result := result + IntToHex (hash_permut[i], 2) + ' ';
 result := Trim(result);
end;


function TRandomHash.GetDigest: TMD5Digest;
begin
 ReadBlock (@result, DIGEST_BASE_OFFSET, sizeof(TMD5Digest));
end;

function TRandomHash.GetNonce: LARGE_INTEGER;
begin
 result.QuadPart := Nonce64;
end;

function TRandomHash.GetNonce64: Int64;
begin
 ReadBlock (@result, NONCE_BASE_OFFSET, sizeof(result));
end;


procedure TRandomHash.RandInit;
var
   tmp: LARGE_INTEGER;
begin
 Repeat
  tmp.LowPart := u32_random;
  tmp.HighPart := u32_random;
 Until tmp.LowPart and $FF > 0;
 Nonce := tmp;
end;

procedure TRandomHash.ReadBlock(dst: PByteArray; ofs, size: DWORD);
var
      src: PByte;
        i: DWORD;
begin
 for i := 0 to size - 1 do
    begin
     src := ByteAddr (ofs + i);
     dst[i] := src^;
    end;
end;

procedure TRandomHash.SetDigest(const Value: TMD5Digest);
begin
 WriteBlock (@Value, DIGEST_BASE_OFFSET, sizeof(TMD5Digest));
end;

procedure TRandomHash.SetNonce(const n: LARGE_INTEGER);
begin
 Nonce64 := n.QuadPart;
end;

procedure TRandomHash.SetNonce64(const Value: Int64);
begin
 Rotation := BYTE (Value);
 // wprintf(' rotation = $%2X, filling with $%2X ', [Rotation, $1F xor Rotation]);
 WriteBlock (@Value, NONCE_BASE_OFFSET, sizeof(Value));
 Assert (Nonce64 = Value,
          SysUtils.Format('Invalid storage implementation, Nonce64 = %s, Value = %s,'#13#10' Raw data: %s ',
                [IntToHex(Nonce64, 8), IntToHex(Value, 8), FormatRaw] ));
end;

procedure TRandomHash.SetNonceHigh(v: DWORD);
var
   nv: LARGE_INTEGER;
begin
 nv := GetNonce;
 nv.HighPart := v;
 nonce := nv;
end;

procedure TRandomHash.SetNonceLow(v: DWORD);
var
   nv: LARGE_INTEGER;
begin
 nv := GetNonce;
 nv.LowPart := v;
 nonce := nv;
end;

procedure TRandomHash.SetRotation(const Value: BYTE);
begin
 raw_data[0] := Value;
 FillChar ( raw_data[1], sizeof(raw_data) - 1, $1F xor Rotation);
end;

procedure TRandomHash.WriteBlock(src: PByteArray; ofs, size: DWORD);
var
      dst: PByte;
        i: DWORD;
begin
 CheckPermut;
 for i := 0 to size - 1 do
   begin
    dst := ByteAddr (ofs + i);
    dst^ := src[i];
   end;
end;

function _current_time (L: lua_state): Integer; cdecl;
var
   st: TTimeStamp;
begin
 result := 2;
 st := CurrentTime;
 lua_pushinteger (L, st.Time);
 lua_pushinteger (L, st.Date);
end;

function _precise_time (L: lua_State): Integer; cdecl;
begin
 result := 1;
 lua_pushnumber (L, PreciseTime);
end;

function _sleep_ex (L: lua_state): Integer; cdecl;
begin
 result := 0;
 SleepEx ( lua_tointeger(L, 1), lua_toboolean (L, 2) );
end;


function wtoa(const s: String): PAnsiChar; inline;
begin
 result := PAnsiChar ( AnsiString(s) );
end;


procedure lua_pushcfunction(L: lua_State; f: lua_CFunction);
begin
  lua_pushcclosure(L, f, 0);
end;

procedure lua_pushint64;
begin
 if Abs (v) <= MAXINT then
    lua_pushinteger (L, Integer(v))
 else
    lua_pushwstr (L, IntToStr (v));
end;

procedure lua_register(L: lua_state; n: PAnsiChar; f: lua_CFunction);
begin
 lua_pushstring(L, n);
 lua_pushcfunction(L, f);
 lua_settable(L, LUA_GLOBALSINDEX);
end;


procedure lua_pushobj ( L: lua_State; pobj: Pointer );
var
   dw: PDataWrapper;
begin
 dw := lua_newuserdata ( L, sizeof(TDataWrapper) ); // auto pushed into stack
 dw.Init (pobj);
end;

function lua_isfunction(L: lua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TFUNCTION;
end;

function lua_istable(L: lua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TTABLE;
end;

function lua_islightuserdata(L: lua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TLIGHTUSERDATA;
end;

function lua_isnil(L: lua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TNIL;
end;

function lua_isboolean(L: lua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TBOOLEAN;
end;

function lua_isnone(L: lua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) = LUA_TNONE;
end;

function lua_isnoneornil(L: lua_State; n: Integer): Boolean;
begin
  Result := lua_type(L, n) <= 0;
end;

procedure lua_assert (L: lua_State; condition: Boolean; const msg: String);
begin
 if condition then exit;
 lua_pushwstr (L, msg);
 lua_error (L);
end;

procedure lua_export_array (L: lua_State; lpArray: PPointerArray; nCount: Integer; pExport: TLuaProcEx);
var
   n, t: Integer;
begin
 lua_createtable (L, nCount, 0);
 t := lua_gettop (L);
 for n := 0 to nCount - 1 do
  begin
    lua_pushinteger (L, n + 1);
    pExport (L, lpArray[n] );
    lua_settable (L, t);
  end;

end;

procedure lua_setlasterr (L: lua_State; err: String);
begin
 err := InfoFmt(err);
 err := AnsiReplaceStr ( err, '$trace_back', LuaTraceBack(L, ' ') );
 lua_pushwstr ( L, err );
 lua_setglobal ( L, 'last_error' );
end;


procedure lua_setarr_b (L: lua_State; i: Integer; v: Boolean; tidx: Integer);
begin
 lua_pushinteger (L, i);
 lua_pushboolean (L, v);
 if lua_istable (L, tidx) then
    lua_settable (L, tidx)
  else
    lua_pop (L, 2);
end;

procedure lua_setarr_i (L: lua_State; i: Integer; v: Int64; tidx: Integer);
begin
 lua_pushinteger (L, i);
 lua_pushnumber  (L, v);
 if lua_istable (L, tidx) then
    lua_settable (L, tidx)
  else
    lua_pop (L, 2);
end;

procedure lua_setarr_f (L: lua_State; i: Integer; v: Double; tidx: Integer);
begin
 lua_pushinteger (L, i);
 lua_pushnumber  (L, v);
 if lua_istable (L, tidx) then
    lua_settable (L, tidx)
 else
    lua_pop (L, 2);
end;

procedure lua_setarr_s (L: lua_State; i: Integer; v: String; tidx: Integer);
begin
 lua_pushinteger (L, i);
 lua_pushwstr (L, v);
 if lua_istable (L, tidx) then
    lua_settable (L, tidx)
 else
    lua_pop (L, 2);
end;

procedure lua_setarr_p (L: lua_State; i: Integer; p: Pointer; tidx: Integer);
begin
 lua_pushinteger (L, i);
 lua_pushptr  (L, P);
 if lua_istable (L, tidx) then
    lua_settable (L, tidx)
 else
    lua_pop (L, 2);
end;

procedure lua_sf (L: lua_State; const k: String; idx: Integer = -2); inline;
begin
 lua_setfield (L, idx, PAnsiChar( AnsiString(k) ));
end;

procedure lua_setmap_b (L: lua_State; const k: String; v: Boolean; tidx: Integer);
begin
 lua_pushboolean (L, v);
 lua_sf (L, k, tidx);
end;

procedure lua_setmap_c (L: lua_State; const k: String; c: lua_CFunction; tidx: Integer);
begin
 lua_pushcfunction (L, c);
 lua_sf (L, k, tidx);
end;

procedure lua_setmap_i (L: lua_State; const k: String; v: Int64; tidx: Integer);
begin
 lua_pushint64 (L, v);
 lua_sf (L, k, tidx);
end;

procedure lua_setmap_f (L: lua_State; const k: String; v: Double; tidx: Integer);
begin
 lua_pushnumber (L, v);
 lua_sf (L, k, tidx);
end;

procedure lua_setmap_s (L: lua_State; const k, v: String; tidx: Integer);
begin
 lua_pushwstr (L, v);
 lua_sf (L, k, tidx);
end;


procedure lua_setmap_lu (L: lua_State; const k: String; p: Pointer; tidx: Integer = -2);
begin
 lua_pushlightuserdata (L, p);
 lua_sf (L, k, tidx);
end;


procedure lua_setmap_p (L: lua_State; const k: String; p: Pointer; tidx: Integer);
begin
 lua_pushobj (L, p);
 lua_sf (L, k, tidx);
end;

procedure lua_get_field (L: lua_State; idx: Integer; const k: String);
var
   sa: AnsiString;
begin
 sa := AnsiString (k);
 lua_getfield (L, idx, PAnsiChar(sa));
end;

procedure lua_set_field (L: lua_State; idx: Integer; const k: String);
var
   sa: AnsiString;
begin
 sa := AnsiString (k);
 lua_setfield (L, idx, PAnsiChar(sa));
end;



function global_getboolean(L: lua_State; const s: String; pop_value: Boolean = TRUE): Boolean; inline;
begin
 result := FALSE;
 lua_getglobal(L, s);
 if lua_isboolean(L, -1) then
    result := lua_toboolean (L, -1)
 else
    pop_value := TRUE;

 if pop_value then
    lua_pop (L, 1);
end;

function global_getinteger(L: lua_State; const s: String; pop_value: Boolean = TRUE): Int64; inline;
begin
 result := 0;
 lua_getglobal(L, s);
 if lua_isnumber(L, -1) then
    result := Round ( lua_tonumber (L, -1) )
 else
    pop_value := TRUE;

 if pop_value then
    lua_pop (L, 1);
end;

function global_getfloat(L: lua_State; const s: String; pop_value: Boolean = TRUE): Double; inline;
begin
 result := 0.0;
 lua_getglobal(L, s);
 if lua_isnumber(L, -1) then
    result := lua_tonumber (L, -1)
 else
    pop_value := TRUE;

 if pop_value then
    lua_pop (L, 1);
end;

function global_getpointer(L: lua_State; const s: String; pop_value: Boolean = TRUE): Pointer; inline;
begin
 result := nil;
 lua_getglobal(L, s);
 if lua_islightuserdata(L, -1) or lua_isuserdata(L, -1) or lua_isfunction(L, -1) then
    result := lua_topointer (L, -1)
 else
    pop_value := TRUE;

 if pop_value then
    lua_pop (L, 1);
end;

function global_getstring(L: lua_State; const s: String; pop_value: Boolean = TRUE): String; inline;
begin
 lua_getglobal(L, s);
 if lua_isboolean(L, -1) then
   result := lua_topstring(L, -1)
 else
    pop_value := TRUE;

 if pop_value then
    lua_pop (L, 1);
end;

function global_gettable (L: lua_State; const s: String): Boolean;
begin
 lua_getglobal (L, s);
 result := lua_istable (L, -1) or lua_isuserdata (L, -1);
 if not result then
    lua_pop (L, 1);
end;


function lua_table_type ( L: lua_State; idx: Integer; pSize: PInteger ): Integer;
var
   num: Boolean; // all keys numeric
   cnt: Integer;
   key: Integer;
   mkv: Integer;
begin
 result := 0;
 if lua_type (L, idx) <> LUA_TTABLE then exit;

 lua_pushnil (L);

 num := TRUE;
 mkv := 0;
 cnt := 0;

 while ( lua_next (L, idx) <> 0 ) do
  begin
   key := 0;
   if lua_isnumber ( L, -2 ) then
      key := lua_tointeger ( L, -2 )
   else
      num := FALSE;

   mkv := Max (mkv, key);
   if key <= 0 then
      num := FALSE;

   lua_pop (L, 1);
   Inc (cnt); // pairs count
  end;

 if mkv > cnt then  num := FALSE;
 if pSize <> nil then
    pSize^ := cnt;


 result := IfV (num, 1, 2); // array or map
end;

function  lua_toint64 (L: lua_State; idx: Integer): Int64;
begin
 if lua_isnumber (L, idx) then
    result := Round ( lua_tonumber(L, idx) )
 else
    result := atoi ( LuaStrArg (L, idx ) );
end;

function lua_topstring(L: lua_State; idx: Integer): String;
var  ps: PAnsiChar;
      s: AnsiString;
   rlen: DWORD;
begin
 rlen := 0;
 try
  ps := lua_tolstring (L, idx, rlen);
  SetString (s, ps, rlen);
 except
  on E: Exception do
     OnExceptLog ( Format('lua_topstring L = $%p idx = %d, rlen = %d ', [L, idx, rlen]), E, TRUE );
 end;
 result := String (s);
end; // lua_getstring

function LuaStrArg (L: lua_State; n_arg: Integer = 1): String;
begin
 result := '';
 // and ( lua_type (L, n_arg) = LUA_TSTRING )
 if ( lua_gettop (L) >= n_arg ) then
      result := lua_topstring (L, n_arg);
end;


function LuaAddr (L: lua_State; idx: Integer): NativeUInt;
begin
 result := NativeUInt ( LuaQWORD (L, idx) );
end;

procedure LuaDumpStrings (L: lua_State; sl: TStrings; f: DWORD);
var
   n: Integer;
   s: String;
begin
  lua_createtable (L, sl.Count, 0);
  for n := 0 to sl.Count - 1 do
   begin
    s := sl [n];
    if f <> 0 then
       s := UnhideSP(s);
    lua_pushnumber(L, n + 1); // key
    lua_pushwstr (L, s);      // value
    lua_settable (L, -3);
   end;

end;

function LuaDWORD (L: lua_State; idx: Integer): UInt64;
begin
 result := DWORD ( LuaQWORD (L, idx) );
end; // LuaDWORD

function LuaQWORD (L: lua_State; idx: Integer): UInt64;

var
  cf: lua_CFunction;
   s: String;
   i: Integer;
begin
 result := 0;
 if lua_gettop (L) >= idx then
   case lua_type (L, idx) of
    LUA_TNUMBER:
      result := Round ( lua_tonumber (L, idx) );
    LUA_TSTRING:
      begin
       s := LuaStrArg(L, idx);

       if ( Pos('#', s) = 1 ) then
          begin
           Delete (s, 1, 1);
           result := 0;
           if Assigned(g_offset_map) then
              begin
               i := g_offset_map.IndexOf(s);
               if i >= 0 then
                  result := NativeInt (g_offset_map.Objects [i])
               else
                  PrintError('not found offset "' + s + '" in g_offset_map');
              end
           else
              PrintError('g_offset_map unassigned!');
          end
       else
       if Pos ('.', s) > 0  then
         result := GetModuleHandle ( PChar (s) )
       else
         result := atoi (s);
      end;
   LUA_TFUNCTION:
      begin
       cf := lua_tocfunction (L, idx);
       if Assigned (cf) then
          result := UInt64 ( Addr(cf) )
       else
          result := 0;
      end;

    else
       result := UInt64 ( lua_topointer (L, idx) );
   end;
end;




function  LuaFreeData (L: lua_State): Integer; cdecl;
var
   data: Pointer;
begin
 if lua_type(L, 1) = LUA_TUSERDATA then
  begin
   data := lua_objptr (L, 1, TRUE );
   FreeMem (data);
  end;
 result := 0;
end;



function  LuaFreeObject (L: lua_State): Integer; cdecl;
var
   obj: TObject;
    dw: PDataWrapper;
begin
 result := 0;
 if lua_type(L, 1) = LUA_TUSERDATA then
  begin
   dw := lua_touserdata (L, 1);
   if dw.sign <> 'pOBJ' then exit;
   obj := dw.obj;
   obj.Free;
   dw.obj := nil;
  end;

end;


function  LuaFreeObjectGC (L: lua_State): Integer; cdecl;
var
   obj: TObject;
    dw: PDataWrapper;

begin
 result := 0;
 dw := nil;
 obj := nil;
 try
   // copy-paste
   LuaFreeObject (L);
 except
  on E: Exception do
    begin
     if obj <> nil then
        wprintf ('[~T/~I].~C0C #FREE_OBJ_GC:~C07 obj at $%p, class = %s ~C07', [ dw.obj, obj.ClassName ]);
     Sleep(500);
     OnExceptLog ('LuaFreeObjectGC', E);
    end;
 end;
end;

function LuaMemCopy (L: lua_State): Integer; cdecl;
var
   src, dst: Pointer;
         cb: DWORD;
begin
 result := 0;
 if lua_gettop(L) < 3 then exit;
 dst := lua_objptr (L, 1);
 src := lua_objptr (L, 2);

 cb := lua_tointeger (L, 3);
 if Assigned (src) and Assigned (dst) then
    CopyMemory (dst, src, cb);
end;

function LuaRandomHash (L: lua_State): Integer; cdecl;
const
   RH_SIZE = sizeof(TRandomHash);

var
    argc: Integer;
     msg: String;
     raw: PRandomHash;
      rh: TRandomHash;

      bv: array [0..RH_SIZE - 1] of BYTE absolute rh;
      ml: Integer;
       n: Integer;

begin
 argc := lua_gettop(L);
 if argc < 3 then
    rh.RandInit
 else
    rh.nonce64 := atoi ( LuaStrArg (L, 3) );  // nonce

 ml := 128 * 1024;
 if argc > 1 then
    ml := lua_tointeger (L, 2); // length

 if lua_isuserdata (L, 1) or lua_islightuserdata (L, 1) then
    rh.Calc ( lua_objptr(L, 1), ml )
 else
  begin
   msg := LuaStrArg (L);
   if msg = '' then msg := 'void';
   ml := Min (ml, Length (msg));
   rh.Calc ( @AnsiString(msg)[1], ml );
  end;

 msg := rh.Format;
 lua_pushwstr (L, msg); // unmixed formad
 msg := '';
 // raw format for code inlining
 for n := 0 to RH_SIZE - 1 do
    begin
     if n > 0 then msg := msg + ', ';
     msg := msg + '$' + IntToHex( bv[n], 2 );
    end;
 lua_pushwstr (L, msg);

 raw := lua_newuserdata (L, sizeof(TRandomHash));
 raw^ := rh;
 result := 3;
end;


{$I lua_inifile.inc}

function  __fini_index (L: lua_State): Integer; cdecl;
var
   fini: TIniFile;
    key: String;
begin
 result := 1;
 fini := lua_objptr_req (L, 1, 'arg 0 missed');
 key := LuaStrArg (L, 2);

 if key = 'class_name' then
    lua_pushwstr (L, fini.QualifiedClassName) else
 if key = 'read_bool' then
    lua_pushcfunction (L, __fini_read_bool) else
 if key = 'read_int' then
    lua_pushcfunction (L, __fini_read_int) else
 if key = 'read_number' then
    lua_pushcfunction (L, __fini_read_number) else
 if key = 'read_section' then
    lua_pushcfunction (L, __fini_read_section) else
 if key = 'read_string' then
    lua_pushcfunction (L, __fini_read_string) else
 if key = 'section_exists' then
    lua_pushcfunction (L, _fini_section_exists) else
 if key = 'write_bool' then
    lua_pushcfunction (L, __fini_write_bool) else
 if key = 'write_int' then
    lua_pushcfunction (L, __fini_write_int) else
 if key = 'write_number' then
    lua_pushcfunction (L, __fini_write_number) else
 if key = 'write_string' then
    lua_pushcfunction (L, __fini_write_string) else
 if key = 'update_file' then
    begin
     fini.UpdateFile;
     lua_pushboolean (L, true);
    end else

    lua_pushnil (L);
end;



function  LuaIniFile (L: lua_State): Integer; cdecl;
var
   fini: TIniFile;
     fn: String;
begin
 result := 1;
 fn := LuaStrArg (L);
 fn := AnsiReplaceStr (fn, '$ExePath', ExePath);
 fn := AnsiReplaceStr (fn, '$DllPath', DllPath);

 if not FileExists(fn) then
    fn := FindConfigFile (fn);
 if Pos('\', fn) = 0 then
    fn := CorrectFilePath (DllPath + '..\conf\' + fn);


 fini := TIniFile.Create(fn);
 AssignMetaIndex (L, fini, __fini_index, nil, '__GMT_INI_FILE', MTF_OBJECT);
end;

function ObjToString(L: lua_State): Integer; cdecl;
var
   obj: TObject;
   res: String;
begin
 result := 1;
 res := 'nil';
 obj := nil;
 if lua_gettop (L) > 0 then
    obj := lua_objptr (L, 1);
 if obj <> nil then
    res := obj.ToString + '(' + FormatPtr(obj) + ')';
 lua_pushwstr (L, res);
end;


function VarToString(L: lua_State): Integer; cdecl;
const
   B2S: array [false..true] of String = ('false', 'true');

var
   obj: TObject;
    st: String;
begin
 result := 1;
 st := '?';

 if lua_gettop(L) > 0 then
  case lua_type (L, -1) of
       LUA_TNIL: st := 'nil';
   LUA_TBOOLEAN: st := 'bool=' + B2S[ lua_toboolean(L, -1) ];
    LUA_TLUDATA:
          begin
           obj := lua_topointer (L, -1);
           st := Format ( '%s@$%p', [obj.ClassName, Pointer(obj)] );
          end;

     LUA_TNUMBER: st := Format ( 'numeric=%f',  [lua_tonumber(L, - 1)] );
     LUA_TSTRING: st := 'string=' + LuaStrArg (L, -1);
      LUA_TTABLE: st := 'table';
   LUA_TUSERDATA:
             st := Format ( 'userdata@%p', [lua_topointer(L, -1)] );
  end
 else
  st := 'no_value';

 lua_pushwstr (L, st);
end;


function PtrOrString (L: lua_State; idx: Integer): String;
begin
 if lua_type (L, idx) in LUA_ANY_USERDATA then
    result := FormatPtr ( lua_objptr (L, idx) )
 else
   result := LuaStrArg ( L, idx );
end;

function _PtrAdd (L: lua_State): Integer; cdecl;
var
   a: Pointer;
   b: NativeUInt;
begin
 a := lua_objptr (L, 1);
 b := LuaAddr (L, 2);
 Inc ( NativeUInt(a), b );
 lua_pushptr ( L, a );
 result := 1;
end;

function _PtrSub (L: lua_State): Integer; cdecl;
var
   a: Pointer;
   b: NativeUInt;

begin
 a := lua_objptr (L, 1);
 b := LuaAddr (L, 2);
 Dec ( NativeUInt(a), NativeUInt(b) );
 lua_pushptr ( L, a );
 result := 1;
end;

function _PtrCmpEQ (L: lua_State): Integer; cdecl;
var
   a, b: Pointer;
begin
 a := lua_objptr (L, 1);
 b := lua_objptr (L, 2);
 lua_pushboolean ( L, a = b );
 result := 1;
end;

function _PtrCmpLT (L: lua_State): Integer; cdecl;
var
   a, b: Pointer;
begin
 a := lua_objptr (L, 1);
 b := lua_objptr (L, 2);
 lua_pushboolean ( L, NativeUInt (a) < NativeUInt (b) );
 result := 1;
end;

function _PtrConcat (L: lua_State): Integer; cdecl;
var
   a, b: String;
begin
 a := PtrOrString (L, 1);
 b := PtrOrString (L, 2);
 lua_pushwstr ( L, a + b );
 result := 1;
end;


function _PtrIndex (L: lua_State): Integer; cdecl;
var
    rh: TRandomHash;
     p: Pointer;
     k: String;
     r: Boolean;
begin
 p := lua_objptr (L, 1);
 k := LowerCase ( LuaStrArg (L, 2) );
 if k = 'valid' then
   begin
    r := ( DWORD (p) >= $10000 );
    if r then
       r := not IsBadReadPtr (p, 4);
    lua_pushboolean ( L, r );
   end else
 if k = 'read_ptr' then
    lua_pushcfunction ( L, LuaReadPtr ) else
 if k = 'read' then
    lua_pushcfunction ( L, LuaReadDMA ) else
 if k = 'write' then
    lua_pushcfunction ( L, LuaWriteDMA ) else
 if k = 'unlock' then
    lua_pushcfunction ( L, LuaUnlockDMA ) else
 if k = 'format' then
    lua_pushcfunction ( L, LuaFormatPtr ) else
 if k = 'crc32' then
    lua_pushcfunction ( L, LuaCalcCRC ) else
 if k = 'calc_hash' then
    lua_pushcfunction ( L, LuaRandomHash ) else
 if k = 'addr' then
    lua_pushnumber ( L, DWORD (p) ) else
 if k = 'rhash' then
    begin
     if lua_type (L, 1) in LUA_ANY_USERDATA then
        begin
         p := lua_objptr (L, 1);
         if DWORD(p) > $10000 then
           begin
            Move (p^, rh, sizeof (TRandomHash));
            lua_pushwstr (L, rh.Format);
           end
         else
            lua_pushwstr (L, 'Invalid pointer for .rhash = ' + FormatPtr(p));
        end
     else
         lua_pushwstr (L, '.rhash not supported for ' + lua_typeof(L, 1));
    end else
 if k = 'rhashr' then
    begin
     if lua_type (L, 1) in LUA_ANY_USERDATA then
        begin
         Move (lua_objptr (L, 1)^, rh, sizeof (TRandomHash));
         lua_pushwstr (L, rh.FormatRaw + ':' + IntToHex(rh.CRC, 8) + ' = ' + rh.Format + ' @ ' + IntToHex(rh.Nonce64, 16));
        end
     else
         lua_pushwstr (L, '.rhashr not supported for ' + lua_typeof(L, 1));
    end
 else
 if ( k = 'call' ) or ( k = 'call_func' ) then
      lua_pushcfunction ( L, BinThisCall ) else
 if ( k = 'copy' ) or ( k = 'copy_mem' ) then
      lua_pushcfunction (L, LuaMemCopy) else  // ptr:copy(src, len)
 if ( k = 'free' ) or ( k = 'release' ) then
      lua_pushcfunction (L, LuaFreeData)
 else
      lua_pushnil (L);
 result := 1;
end;


function _StrToInt64(L: lua_State): Integer; cdecl;
var
   s: String;
   v: Int64;
begin
 result := 1;
 s := LuaStrArg (L);
 v := atoi (s);
 lua_pushwstr (L, IntToStr(v));
end;


function BinThisCall(L: lua_State): Integer; cdecl;
{$IFDEF CPUX86}
var
   i, n, argc: Integer;
   st_argc: Integer;
   buff_ofs: Integer;
    argl: array [0..15] of DWORD;
   vbuff: array [0..1023] of Byte;
   arg_val, arg_eax, arg_ebx, arg_edx, arg_esi, arg_edi, arg_ebp: DWORD;
   ch: AnsiCHAR;
   cx: AnsiCHAR;
   fv: Single;
   s: String;

   push: Boolean;

   pfv: PSingle;
   pdv: PDouble;
   piv: PInt64;
   psa: PAnsiChar;
   psw: PWideChar;

   objp, func: NativeUInt;


begin
 result := 0;
 if bc_disabled then exit;



 // args: object ptr, method ptr, arg1, arg2...
 argc := lua_gettop(L);
 if argc < 2 then exit;

 objp := LuaAddr (L, 1);

 if objp < $10000 then
   begin
    PrintError(' BinThisCall: invalid parameter "this" = $' + IntToHex(objp, 4) );
    exit;
   end;


 func := LuaAddr (L, 2);

 if ( log_verbose < 5 ) and ( func_blacklist.IndexOf ( Ptr(func) ) >= 0 ) then exit;

 FillChar(vbuff, sizeof(vbuff), 0);

 Dec (argc, 2);

 arg_eax := 0;
 arg_ebx := 0;
 arg_edx := 0;
 arg_esi := 0;
 arg_edi := 0;
 asm
  mov arg_ebp, ebp
 end;

 st_argc := 0;
 buff_ofs := 0;

 for n := 0 to argc - 1 do
   case lua_type (L, n + 3) of
    LUA_TSTRING:
     begin
    s := LuaStrArg(L, n + 3);
    ch := #0;
    cx := #0;
    arg_val := 0;

    // EAX=0
    if ( Length (s) >= 5 ) and ( s [1] = 'E' ) and CharInSet( s [3], ['I', 'X', 'P'] ) and ( s [4] = '=' ) then
      begin
       ch := AnsiChar ( s [2] );
       cx := AnsiChar ( s [3] );
       s := Copy (s, 5, Length(s) - 4);
       arg_val := atoi (S);
      end;

    if  ( ch in ['A','B','D','S'] ) then
      case ch of
       'A': arg_eax := arg_val;
       'B': case cx of
             'P': arg_ebp := arg_val;
             'X': arg_ebx := arg_val;
            end; // in case
       'D': case cx of
             'I': arg_edi := arg_val;
             'X': arg_edx := arg_val;
            end;  // in case 2
       'S': case cx of
             'I': arg_esi := arg_val; // TODO: add ESP?
            end;
      end // case
    else
    // ivar=1 avar=help
    if Pos('var', s) = 2 then
     begin
      pfv := @vbuff[buff_ofs];
      pdv := @vbuff[buff_ofs];
      piv := @vbuff[buff_ofs];
      psa := @vbuff[buff_ofs];
      psw := @vbuff[buff_ofs];

      ch := AnsiChar ( s [1] );

      push := Pos('$', s) <> 5; // non aligner

      i := Pos('=', s);

      if i > 0 then
         Delete ( s, 1, i ); // remove any chars before '=', and '=' also

      //StrTok (s, ['=']); // remove param


      // skip var in buffer
      case ch of
       'i', 'f': Inc (buff_ofs, 4);
       'I', 'F': Inc (buff_ofs, 8);
       'a': Inc (buff_ofs, Length(s) + 1);
       'w': Inc (buff_ofs, sizeof(WideChar) * ( Length(s) + 1 ) );
      end;

      Assert ( buff_ofs < sizeof(vbuff), 'LuaThisCall: buffer overrun detected!');

      if i > 0 then
        case ch of
         'I',
         'i': piv^ := atoi (s);
         'f': pfv^ := atof (s);
         'F': pdv^ := atof (s);
         'a': AnsiStrings.StrPCopy(psa, AnsiString(s));
         'w': StrPCopy(psw, s);
        end;

      if ch = 'f' then
         ODS ('[~T]. #DBG: s = ' + s + ', fvar = ' + ftow(pfv^, '%.3f') + ' at ' + FormatPtr(pfv));


      if push then
        begin
         argl [st_argc] := DWORD (piv); // pointer to var
         Inc (st_argc);
        end;
     end // if variable
    else
    if Pos ('(f)', s) = 1 then  // indirect float value
     begin
      Delete(s, 1, 3);
      fv := atof (s);
      argl [st_argc] := PDWORD ( @fv )^;
      Inc (st_argc)
     end
    else
    if Pos ('(i)', s) = 1 then  // force int value
     begin
      Delete(s, 1, 3);
      argl [st_argc] := atoi (s);
      Inc (st_argc)
     end
    else
     begin // default variant
      argl [st_argc] := LuaAddr (L, n + 3);
      Inc (st_argc);
     end;

     end; // string arg

    LUA_TNUMBER:
      begin
       fv := lua_tonumber (L, n + 3);
       argl [st_argc] := PDWORD ( @fv )^;
       Inc (st_argc)
      end;

    LUA_TLIGHTUSERDATA:
      begin
       argl [st_argc] := LuaAddr (L, n + 3);
       Inc (st_argc);
      end;
   end; // for case

 if log_verbose >= 7 then
   begin
    s := IntToStr (st_argc) + ': ';
    for n := 0 to st_argc - 1 do
        s := s + '$' + IntToHex ( argl [n], 8 );

    ODS ( CFormat('[~T]. #DBG: BinThisCall this = $%x, func = $%x, { eax = $%x, ebx = $%x, edx = $%x, edi = $%x, esi = $%x, ebp = $%x }, pushed = %s', '~C07',
                                         [objp, func, arg_eax, arg_ebx, arg_edx, arg_edi, arg_esi, arg_ebp, s] ) );

    Sleep (100);
   end;

 // push argl [0]
 // push argl [1]

 try
   asm
    pushad
    mov   ecx, objp
    mov   edx, 0
  @putarg:
    cmp   edx, st_argc
    jae   @noargs         // if >= argc
    mov   ebx, DWORD PTR[argl + edx * 4]
    push  ebx
    inc   edx
    jmp   @putarg
  @noargs:
    mov   eax, arg_eax
    mov   ebx, arg_ebx
    mov   edx, arg_edx
    mov   edi, arg_edi
    mov   esi, arg_esi
    mov   ebp, arg_ebp
    call  DWORD PTR [func]
    mov   arg_eax, eax                  // typical result 1
    mov   arg_edx, edx                  // typical result 2
    // mov   edi, func
    // call  edi
    popad
   end;

   result := 2;
   lua_pushnumber (L, arg_eax);
   lua_pushnumber (L, arg_edx);

 except
  on E: EAccessViolation do
    begin
     func_blacklist.Add ( Ptr(func) );
     PrintError ('AV exception catched in BinThisCall: ' + E.Message);
    end;

  on E: Exception do
    begin
     func_blacklist.Add ( Ptr(func) );
     OnExceptLog ('BinThisCall failed at $' + IntToHex (func, 8), E);
    end;
 end;

end;
{$ELSE}
begin
 Assert(FALSE, 'Not adopted for x64-bit arch');
 result := 0;
end;
{$ENDIF CPUX86}

function LuaFormatMsg (L: lua_State): Integer; cdecl;
var
   s: String;
begin
 result := 1;
 s := LuaStrArg (L, 1);
 s := InfoFmt (s);
 lua_pushwstr (L, s);
end;

function LuaFormatPtr(L: lua_State): Integer; cdecl;
var
   s: String;
   p: Pointer;
   vt: Integer;
begin
 p := nil;
 vt := LUA_TNIL;

 if (lua_gettop (L) > 0) then
  begin
   vt := lua_type(L, 1);
   case vt of
    // , : //  lua_tolightuserdata (L, 1);
    LUA_TNUMBER:
       p := Ptr (lua_tointeger(L, 1));
    LUA_TUSERDATA,
    LUA_TLUDATA:
       p := lua_objptr (L, 1);
   else
       p := lua_topointer (L, 1);
   end; // case

  end;

 s := Format('$%P', [p]);
 // добавить тип переменной, при необходимости
 if (lua_gettop (L) > 1) and lua_toboolean(L, 2) then
     s := s + '@' + IntToStr ( vt );

 lua_pushstring (L, PAnsiChar (AnsiString(s)) );
 result := 1;
end;



function LuaFindSubStr (L: lua_State): Integer; cdecl;
var
   tmp: TStrMap;
   txt: String;
    dm: String;
     i: Integer;
begin
 txt := LuaStrArg (L);
 tmp := TStrMap.Create;
 dm := LuaStrArg (L, 3) + ';';

 if dm[1] = ';' then
    tmp.CommaText := LuaStrArg (L, 2)
 else
    tmp.Split ( dm[1], LuaStrArg (L, 2) );

 result := 2;

 for i := 0 to tmp.Count - 1 do
  begin
   dm := UnhideSP ( tmp [i] );
   if Pos( dm, txt ) = 0 then continue;
   lua_pushinteger ( L, i + 1 );
   lua_pushboolean ( L, dm = txt );
   tmp.Free;
   exit;
  end;

 lua_pushnil ( L );
 lua_pushboolean ( L, FALSE );
 tmp.Free;
end;

function  LuaHexToInt(L: lua_State): Integer; cdecl;
var
   i: Int64;
begin
 result := 1;
 i := atoi ( LuaStrArg(L) );
 lua_pushwstr (L, IntToStr(i));
end;

function  LuaMakePtr (L: lua_State): Integer; cdecl;
var
   p: Pointer;
begin
 result := 1;
 p := Ptr ( LuaAddr(L, 1) );
 lua_pushlightuserdata (L, p);
end;

function LuaSplit (L: lua_State): Integer; cdecl;
var
   s, sep: String;
   sl: TStrMap;
   f: DWORD;
begin
 s := LuaStrArg (L);
 sep := LuaStrArg (L, 2)+ ' ';
 sep := AnsiReplaceStr (sep, '\r', #13);
 sep := AnsiReplaceStr (sep, '\n', #10);
 sep := AnsiReplaceStr (sep, '\t', #8);
 sep := AnsiReplaceStr (sep, '\@', #7);

 sl := TStrMap.Create;
 try
  f := 0;
  if sep [1] <> ' ' then f := SPF_HIDESPACES;

  if sep [1] = #10 then
    sl.Text := s
  else
    sl.Split ( sep [1], s, f);

  result := 1;

  LuaDumpStrings (L, sl, f);
 finally
  sl.Free;
 end;

end; // LuaSplit

function LuaStrByte(L: lua_State): Integer; cdecl;
var
   ch: AnsiCHAR;
   ss: PAnsiChar;
   sl: DWORD;
    i: DWORD;
begin
 result := 1;
 ch := #00;
 sl := 0;

 ss := lua_tolstring (L, 1, sl);
 i := 0;
 if lua_gettop(L) > 1 then
    i := lua_tointeger (L, 2) - 1;

 if ( i < sl ) then
      ch := ss [i];

 lua_pushinteger (L, BYTE(ch));
end;


function LuaStrLower(L: lua_State): Integer; cdecl;
begin
 result := 1;
 lua_pushwstr (L, LowerCase( LuaStrArg(L, 1), loUserLocale) + 'l' );
end;

function LuaStrUpper(L: lua_State): Integer; cdecl;
begin
 result := 1;
 lua_pushwstr (L, UpperCase( LuaStrArg(L, 1), loUserLocale) + 'U' );
end;

function GetTacticalInfo: String;
var
   md: TMD5Digest;
   dt: TDateTime;
    s: String;
begin
 dt := GetDirCT ('ProgramFiles', 'Program Files');
 s := '';
 if dt > 0 then
    s := 'T1->' + FormatDateTime('yyyy.mm.dd hh:nn:ss', dt) + #10;
 dt := GetDirCT ('windir', 'Windows');
 if dt > 0 then
    s := s + 'T2->' + FormatDateTime('yyyy.mm.dd hh:nn:ss', dt) + #10;



 {$IFOPT D+}
  ODS('[~T]. #DBG: PCIdent info: '#10 + s);
 {$ENDIF}

 md := MD5StringW (s);
 result := '?' + MD5DigestToStr (md);
end;

function  LuaBitAND (L: lua_State): Integer; cdecl;
var
   v: UInt64;
begin
 result := 1;
 v := LuaQWORD (L, 1) and LuaQWORD (L, 2);
 lua_pushint64 (L, v);
end;

function  LuaBitOR (L: lua_State): Integer; cdecl;
var
   v: UInt64;
begin
 result := 1;
 v := LuaQWORD (L, 1) or LuaQWORD (L, 2);
 lua_pushint64 (L, v);
end;


function LuaCalcCRC (L: lua_State): Integer; cdecl;
var
   addr, ofs, cb: NativeUInt;
           start: DWORD;
           pdata: Pointer;
              sa: String;
begin
 addr := LuaAddr (L, 1);
 ofs :=  LuaAddr (L, 2);
 cb :=   LuaAddr (L, 3);
 start := $FFFFFFFF;
 sa := 'R';


 if lua_gettop(L) > 3 then
    start := LuaDWORD (L, 4);
 if lua_gettop(L) > 4 then
    sa := LuaStrArg (L, 5);

 result := 1;
 pdata := Ptr (addr + ofs);

 ofs := 0;
 try
  if not IsBadReadPtr ( pdata, cb ) then
    begin
     if sa = 'R' then
        ofs := CalcCRC32 ( pdata, cb, start );
     if sa = 'D' then
        ofs := CalcCRC32D ( pdata, cb, start );
    end;

 except
  on E: Exception do
     PrintError('Exception catched in LuaCalcCRC ' + E.Message);
 end;
 lua_pushinteger (L, ofs);
end;

function LuaCastBool (L: lua_State): Integer; cdecl;
var
   b: Boolean;
begin
 result := 1;
 b := FALSE;

 case lua_type(L, 1) of
  LUA_TBOOLEAN:
     b := lua_toboolean (L, 1);
  LUA_TNUMBER, LUA_TSTRING:
     b := lua_tonumber (L, 1) <> 0;
  LUA_TLUDATA, LUA_TUSERDATA, LUA_TFUNCTION:
     b := LuaAddr (L, 1) <> 0;
 end;

 lua_pushboolean (L, b);
end;


function LuaReadPtr (L: lua_State): Integer; cdecl;
var
  ofs: Integer;

   pp: PPointer;

begin
 result := 1;
 try
   pp := Ptr ( LuaAddr (L, 1) );
   if lua_gettop(L) > 1 then
      ofs := Integer ( LuaDWORD (L, 2) )
   else
      ofs := 0;
   if ofs > 0 then
      Inc (NativeUInt(pp), NativeUInt(+ofs)) // possible negative offset
   else
      Dec (NativeUInt(pp), NativeUInt(-ofs));

   if NativeUInt (pp) < $10000 then
      begin
       lua_pushnil (L);
       PrintError('ReadPtr: invalid source addr, below $10k = ' + FormatPtr(pp));
       exit;
      end;

   lua_pushptr (L, pp^);
   SetPointerMT (L, lua_gettop(L));
 except
   on E: Exception do
     OnExceptLog('LuaReadPtr', E, TRUE);
 end;

end;

function LuaReadDMA (L: lua_State): Integer; cdecl;
var
  addr, ofs: NativeUInt;
       argc: Integer;
     s, tag: String;
         cb: Integer;

begin
 argc := lua_gettop (L);
 result := 1;
 addr := LuaAddr (L, 1);
 ofs :=  LuaAddr (L, 2);
 tag := 'float';
 cb := 0;

 if ( addr < $10000 )  then
   begin
    PrintError( Format('ReadDMA($%x, $%x) - wrong initial pointers', [addr, ofs]) );
    wprintf('%s', [ LuaTraceback(L, '   ', 2) ]);
    lua_pushnil (L);
    exit;
   end;


 if argc > 2 then
    tag := LowerCase ( LuaStrArg (L, 3) );
 if argc > 3 then
    cb  := LuaAddr (L, 4);


 if (tag <> 'ansi') and (tag <> 'wide') then
     cb := Max (cb, 1);

 if tag = 'dpcid' then
  begin
   s := GetTacticalInfo;
   lua_pushwstr (L, s);
   exit;
  end;

 if log_verbose >= 7 then
    ODS( CFormat('[~T]. #DBG(ReadDMA): trying read %s at [$%x + $%x = $%x]', '~C07', [tag, addr, ofs, addr + ofs] ));

 LuaPushMemValue (L, Ptr (addr + ofs), tag, cb);
 result := 1;
end;


procedure LuaPushMemValue (L: lua_State; pAddr: Pointer; tag: String; cb: Integer);
var
  tmp: PAnsiChar;
   pr: Pointer;
   pa: Pointer;

   pb: PByteArray absolute pa;
   ps: PAnsiChar absolute pa;
   pu: PWideChar absolute pa;
   pp: PPointer absolute pa;
   pi: PInteger absolute pa;
   pf: PSingle absolute pa;
   pn: PDouble absolute pa;
   ph: PShort  absolute pa;
   pq: PInt64  absolute pa;
   pd: PDWORD  absolute pa;
   pw: PWORD   absolute pa;
   rb: NativeUInt;
    s: String;
    n: Integer;

begin
 pa := pAddr;

 if pa <> nil then
 try
  if tag = 'float' then
     lua_pushnumber (L, pf^) else
  if (tag = 'lpstr') or (tag = 'lpcstr') then
    begin
     ps := pp^;  // cb ignored
     lua_pushstring (L, ps)
    end else
  if tag = 'ansi' then
    begin
     if cb > 0 then
       begin
        tmp := AllocMem ( cb + 1 ); // with zero-char
        AnsiStrings.StrLCopy (tmp, ps, cb);
        lua_pushstring (L, tmp);
        FreeMem (tmp);
       end
     else
        lua_pushstring (L, ps)
    end
  else
  if (tag = 'wide') then
    begin
     if cb < 4 then cb := StrLen (pu);
     SetString (s, pu, cb);
     lua_pushwstr ( L, s );
    end
  else
  if (tag = 'byte') then
     lua_pushnumber (L, pb[0]) else
  if (tag = 'short') then
     lua_pushnumber (L, ph^) else
  if (tag = 'word') then
     lua_pushnumber (L, pw^) else
  if (tag = 'uint') or (tag = 'dword') then
     lua_pushnumber(L, pd^) else
  if (tag = 'int') or (tag = 'long') then
     lua_pushinteger(L, pi^) else // stricted range from -2^31 .. 2^31 - 1
  if (tag = 'int64') or (tag = 'qword') then
     lua_pushint64 (L, pq^) else
  if (tag = 'number') or (tag = 'double') then
     lua_pushnumber(L, pn^) else
  if (tag = 'pointer') or (tag = 'ptr') then
      lua_pushptr (L, pp^) else
  if (tag = 'safe_ptr') then
    begin
     if ReadProcessMemory (GetCurrentProcess, pa, @pr, sizeof (pr), rb) then
        lua_pushptr (L, pr)
     else
        lua_pushnil (L);
    end else
  if (tag = 'dump') then
    begin
     s := '';
     for n := 0 to cb - 1 do
         s := s + Byte2Hex  (pb [n]) + ' ';
     lua_pushwstr (L, Trim(s));
    end
  else
    begin
     PrintError ('PushMemValue, wrong tag ' + tag + ', returns nil');
     lua_pushnil(L);
    end;
 except
  on E: Exception do
     lua_pushnil (L);
 end;
end;

function LuaWriteDMA (L: lua_State): Integer; cdecl;
var
   n, argc, addr, ofs: NativeUInt;
   b: BYTE;
   tag, dump, hx: String;
   s: AnsiString;
   pf: PSingle;
   pb: PByteArray absolute pf;
   ps: PAnsiChar absolute pf;
   pp: PPointer absolute pf;
   pi: PInteger absolute pf;
   pn: PDouble absolute pf;
   ph: PShort  absolute pf;
   pd: PDWORD  absolute pf;
   pq: PInt64  absolute pf;
   pw: PWORD   absolute pf;


   res, rex: String;
begin
 result := 1;
 argc := lua_gettop (L);
 addr := LuaAddr (L, 1);
 ofs :=  LuaAddr (L, 2);
 {$IFDEF NEWEST_BUILD}
 // mw_flags := 0;
 {$ENDIF}

 if mw_flags and 1 = 0 then
  begin
   if log_verbose >= 2 then
      PrintError('Memory write protected. Try RunCommand("MW_ENABLE") before using WriteDMA. '#13#10 +
                        LuaTraceBack (L, #9#9) );

   lua_pushwstr (L, 'Memory write protected. Try RunCommand("MW_ENABLE") before using WriteDMA.');
   exit;
  end;

 // addr=1, ofs=2, value=3, tag=4
 tag := 'float';
 if argc > 3 then
    tag := LowerCase ( LuaStrArg (L, 4) )
 else
   case lua_type (L, 3) of
    LUA_TLIGHTUSERDATA,
    LUA_TUSERDATA: tag := 'pointer';
      LUA_TSTRING: tag := 'ansi';
      LUA_TNUMBER: tag := 'float';
     LUA_TBOOLEAN: tag := 'dword';
   else
     tag := 'float';
   end;

 if mw_flags and 2 <> 0 then
    wprintf ('[~T]. #WDMA: trying write at~C0D $%x~C0B +~C0D $%x~C07 value type~C0F %s~C07 ', [addr, ofs, tag] );

 pf := Ptr ( addr + ofs );


 if DWORD(pf) > $10000 then
 try
  res := '#SUCCESS:' + tag + '@' + Format('%x + %x = $%p', [addr, ofs, Pointer(pf)]);
  // write value by tag
  if (tag = 'float')   or (tag = 'single')   then pf^ := lua_tonumber(L, 3) else
  if (tag = 'double')  or (tag = 'number')   then pn^ := lua_tonumber(L, 3) else
  if tag = 'ansi'  then
      begin
       s := AnsiString ( LuaStrArg (L, 3) );
       AnsiStrings.StrPCopy ( ps, PAnsiChar (s) );
      end else

  if (tag = 'addr')                          then pp^ := Ptr ( LuaAddr(L, 3) ) else
  if (tag = 'byte')                          then pb[0] := BYTE    ( lua_tointeger(L, 3) ) else
  if (tag = 'word')                          then pw^   := WORD    ( lua_tointeger(L, 3) ) else
  if (tag = 'short')                         then ph^   := SmallInt( lua_tointeger(L, 3) ) else
  if (tag = 'uint')    or (tag = 'dword')    then
      pd^   := LuaDWORD (L, 3) else
  if (tag = 'int')     or (tag = 'long')     then
      pi^   := Integer ( lua_tointeger(L, 3) ) else
  if (tag = 'int64')   or (tag = 'qword') then
      pq^ := lua_toint64 (L, 3) else
  if (tag = 'pointer') or (tag = 'ptr') then
      pp^ := lua_topointer(L, 3) else
  if (tag = 'dump') then
     begin
      dump := LuaStrArg (L, 3);
      dump := AnsiReplaceStr (dump, ' ', ''); // remove spaces
      dump := AnsiReplaceStr (dump, '$', ''); // remove prefixes
      n := 0;
      res := '';
      rex := '';
      dump := Trim (dump);

      // цикл сгрызани€ текстовых токенов из строки дампа
      while (dump <> '') and (n < 256) do
       begin
        hx := Copy (dump, 1, 2);
        Delete (dump, 1, 2);
        b := atoi ('$' + hx);  // put byte
        pb [n] := b;                 // unsafe write
        res := res + IntToHex (b, 2) + ' ';
        rex := rex + IntToHex (pb [n], 2) + ' ';
        Inc (n);
       end;

      if res = rex then
        res := '#SUCCESS(WriteDMA): res = rex = ' + res
      else
        res := '#ERROR(WriteDMA): res = ' + res + ' vs rex = ' + rex;
     end
   else res := '#ERROR(WriteDMA): unknown tag ' + tag;

 except
  on E: Exception do
     res := 'Exception catched: ' + E.Message;
 end
 else
     res := '#FAILED: Unassigned ' + tag + '@' + Format('%x + %x = $%p', [addr, ofs, Pointer(pf)]);
 lua_pushwstr (L, res);
end;

function LuaUnlockDMA (L: lua_State): Integer; cdecl;
var
   addr, ofs: NativeUInt;
   p: Pointer;
begin
 result := 1;
 addr := LuaAddr (L, 1);
 ofs :=  LuaAddr (L, 2);
 p := Ptr ( addr + ofs );
 lua_pushwstr (L,  UnlockRegion (p));
end; // LuaUnlockDMA


function LuaTraceBack (L: lua_State; const sp: String; depth: Integer): String;

var
   r: DWORD;
   t: Integer;
   d: Integer;
begin
 result := 'L = nil';
 if L = nil then exit;

 t := lua_gettop(L);
 try
   if Assigned (_traceback) then
      lua_pushcfunction(L, _traceback)
   else
    begin
     lua_getfield (L, LUA_GLOBALSINDEX, 'debug');
     d := lua_gettop (L);
     lua_getfield (L, d, 'traceback');
     _traceback := lua_tocfunction (L, -1);
     lua_remove (L, d);
    end;
   lua_pushwstr (L, '');
   lua_pushinteger (L, depth);

   if lua_pcall (L, 2, LUA_MULTRET, 0) = 0 then
      result := String ( lua_tolstring (L, -1, r) );
   if sp <> '' then
      result := FormatRight (result, sp);
   lua_pop (L, 1);

 finally
  lua_settop (L, t);
  end;
end; // LuaTraceBack


procedure AssignMetaIndex (L: lua_State; pData: Pointer; fIndex, fNewIndex: lua_CFunction; const mtname: String; flags: DWORD );
var
   dw: PDataWrapper;
    t: Integer;
begin
 Assert ( pData <> nil, 'AssignMetaIndex: pData = nil' );

 dw := lua_newuserdata ( L, sizeof (TDataWrapper) );
 dw.Init (pData);

 // lua_pushptr (L, pData);                                // stack: pData
 t := lua_gettop (L);

 lua_getglobal (L, mtname);
 if lua_type (L, -1) <= LUA_TNIL then
   begin
    lua_pop (L, 1);
    lua_createtable (L, 0, 0);                                      // stack: pData, metatable
    lua_setglobal (L, mtname);
    lua_getglobal (L, mtname);
    if Assigned (fIndex) then
       lua_setmap_c (L, '__index', fIndex, t + 1);

    if Assigned (fNewIndex) then
       lua_setmap_c (L, '__newindex', fNewIndex, t + 1);


    if flags and MTF_OBJECT <> 0 then
      begin
       lua_setmap_c (L, '__tostring', ObjToString);
       if flags and MTF_ADD_GC <> 0 then
          lua_setmap_c (L, '__gc', LuaFreeObjectGC );
      end
    else
      begin
       lua_setmap_c (L, '__tostring', VarToString);
       if flags and MTF_ADD_GC <> 0 then
          lua_setmap_c (L, '__gc', LuaFreeData );
      end;
   end;

 if lua_type (L, -2) in LUA_ANY_USERDATA then
    lua_setmetatable ( L, -2 )        // _G[t].metatable = value at [-1]
 else
    PrintError('Prevented assign metatable to ' + lua_typeof(L, -2) );
end;


procedure SetPointerMT ( L: lua_State; idx: Integer );
const
   PTR_MT = 'GLOBAL_PTR_MT';

begin
 if idx < 0 then
    idx := lua_gettop(L) + idx + 1;

 lua_getglobal ( L, PTR_MT );
 if lua_isnil ( L, -1 ) then
   begin
    lua_pop (L, 1);
    lua_createtable ( L, 0, 0 );
    lua_setmap_c ( L, '__add',    _PtrAdd );
    lua_setmap_c ( L, '__sub',    _PtrSub );
    lua_setmap_c ( L, '__index',  _PtrIndex );
    lua_setmap_c ( L, '__concat', _PtrConcat );
    lua_setmap_c ( L, '__eq',     _PtrCmpEQ );
    lua_setmap_c ( L, '__lt',     _PtrCmpLT );
    lua_setmap_c ( L, '__pow',      LuaReadPtr );
    lua_setmap_c ( L, '__tostring', LuaFormatPtr );

    lua_setglobal ( L, PTR_MT ); // изъ€тие из стека, назначение глобальной
    lua_getglobal ( L, PTR_MT ); // возвращение в стек
   end;

 if lua_islightuserdata (L, idx) or lua_isuserdata(L, idx) then
    lua_setmetatable (L, idx)
 else
    lua_pop(L, 1);
end;


function FindGlobalFunc (L: lua_State; f: lua_CFunction): String;

var
   t: Integer;
begin
 result := 'CFunction(' + FormatPtr ( @f ) + ')';

 t := lua_gettop (L);
 lua_getglobal(L, '_G');
 lua_pushnil (L);

 while lua_next(L, -2) <> 0 do
  begin
   // -1 value -2 key
   if ( lua_type (L, -1) = LUA_TFUNCTION ) and ( @lua_tocfunction (L, -1) = @f ) then
      begin
       result := LuaStrArg (L, -2) + '(?)';
       break;
      end;

   lua_pop(L, 1);
  end;
 lua_settop (L, t);
end;


function DumpValue (L: lua_State; idx: Integer): String;
var
   nn: Integer;
begin
 result := '';
  case lua_type (L, idx) of
   LUA_TNUMBER:
     result := ftow ( lua_tonumber (L, idx) );
   LUA_TSTRING:
     result := LuaStrArg (L, idx);
    LUA_TTABLE:
      begin
       // only linear tables parsing!
       for nn := 1 to lua_objlen(L, idx) do
        begin
         lua_pushinteger (L, nn);
         lua_gettable (L, idx);  // trying get t[nn]

         if lua_type(L, -1) = LUA_TNIL then
           begin
            lua_pop (L, 1);
            break
           end
         else
           begin
            if nn > 1 then result := result + #13#10;
            result := result + DumpValue (L, lua_gettop(L));
            lua_pop (L, 1);
           end;
         if Length (result) > 50e6 then break; // extreme big string, prevent 'memory out'
        end; // for
      end;  // table
   LUA_TUSERDATA, LUA_TLIGHTUSERDATA:
     result := FormatPtr ( lua_topointer(L, idx) );
   LUA_TFUNCTION:
     result := FindGlobalFunc ( L, lua_tocfunction(L, idx) );
  end;     // case
end; // DumpValue


function lua_objptr ( L: lua_State; idx: Integer; bClear: Boolean ): Pointer;
var
   dw: PDataWrapper;

begin
 result := nil;

 case lua_type (L, idx) of
  LUA_TLUDATA:
      result := lua_topointer (L, idx);

  LUA_TUSERDATA:
      begin
       dw := lua_touserdata (L, idx);
       if ( lua_objlen(L, idx) = sizeof (TDataWrapper) ) and ( dw.sign = 'pOBJ' ) then
         begin
          result := dw.obj;
          if bClear then
             dw.obj := nil;
         end
       else
          result := dw;
      end;
 end; // case

end;

function lua_objptr_req ( L: lua_State; idx: Integer; const msg: String ): Pointer;
begin
 result := lua_objptr (L, idx);
 lua_assert (L, result <> nil, msg);
end;


function lua_getobj ( L: lua_State; const name: String; pop_value: Boolean = FALSE ): Pointer;
begin
 lua_getglobal ( L, name );
 result := lua_objptr (L, -1);
 if pop_value then
    lua_pop (L, 1);
end;


{ Lua exported functions }
function _LuaDumpVar (L: lua_State): Integer; cdecl;
var
   s: String;
begin
 result := 1;
 s := DumpValue (L, 1);
 lua_pushwstr (L, s);
end;




{ TObjWrapper }

procedure TDataWrapper.Init(src: Pointer);
begin
 sign := 'pOBJ';
 obj := src;
end;


{ TLuaEnv }

function TLuaEnv.get_field(const idx: String): TLuaEnv;
begin
 lua_getfield (L, env, wtoa(idx) );
 result := self;
 result.top := field_exists(-1);
 result.env := result.top;
end;

function TLuaEnv.get_field(const idx: Integer): TLuaEnv;
begin
 lua_pushinteger (L, idx);
 lua_gettable (L, env);
 result := self;
 result.top := field_exists(-1);
 result.env := result.top;
end;

function TLuaEnv.field_exists (idx: integer): Integer;
begin
 if lua_isnil (L, -1) then
    result := pop_field
 else
    result := lua_gettop (L);
end;

procedure TLuaEnv.init(State: lua_State; env_idx: Integer);
begin
 L := State;
 top := lua_gettop (L);
 env := env_idx;
end;

function TLuaEnv.is_table: Integer;
begin
 if lua_istable (L, top) or lua_isuserdata (L, top) then
    result := top
 else
    result := pop_field;
end;

function TLuaEnv.pop_field: Integer;
begin
 lua_pop (L, 1);
 result := 0;
end;

function TLuaEnv.to_integer (rmv: Boolean): Integer;
begin
 if top <> 0 then
   begin
    result := lua_tointeger (L, top);
    if rmv then lua_remove (L, top);
   end
 else
    result := 0;
end;

function TLuaEnv.to_number (rmv: Boolean): lua_Number;
begin
 if top <> 0 then
   begin
    result := lua_tonumber (L, top);
    if rmv then lua_remove (L, top);
   end
 else
    result := 0;
end;

function TLuaEnv.to_object(rmv: Boolean): Pointer;
begin
 if top <> 0 then
   begin
    result := lua_objptr (L, top);
    if rmv then lua_remove (L, top);
   end
 else
    result := nil;
end;

function TLuaEnv.to_string (rmv: Boolean): String;
begin
 if top <> 0 then
   begin
    result := LuaStrArg (L, top);
    if rmv then lua_remove (L, top);
   end
 else
    result := '';
end;

function TLuaEnv.to_pointer (rmv: Boolean): Pointer;
begin
 if top <> 0 then
   begin
    result := lua_topointer (L, top);
    if rmv then lua_remove (L, top);
   end
 else
    result := nil;
end;


function TLuaEnv.select (new_top: Integer): TLuaEnv;
begin
 result := self;
 result.top := new_top;
end;


function  LuaStringsIndex (L: lua_State): Integer; cdecl;
var
   v, k: String;
begin
 v := LuaStrArg(L, 1);
 k := LuaStrArg(L, 2);
 result := 1;
 if k = 'split' then
    lua_pushcfunction (L, LuaSplit) else
 if k = 'lower' then
    lua_pushcfunction (L, LuaStrLower) else
 if k = 'upper' then
    lua_pushcfunction (L, LuaStrUpper) else
 if k = 'find_sub' then
    lua_pushcfunction (L, LuaFindSubStr) else
     result := orig_str_indx (L);
end;

function UpgradeStrings (L: lua_state; disp: Boolean): Integer; cdecl;
var
     top: Integer;
     keys: TStrings;
       mt: Integer;
        k: String;
begin
 top := lua_gettop (L);
 result := 0;
 lua_pushwstr (L, 'test');
 if lua_getmetatable (L, -1) <> 0 then
   begin
    mt := lua_gettop(L);
    lua_getfield (L, mt, '__index');  // test, metatable, __index


    if disp then wprintf ('[~T]. #DBG: type of __index = %s ', [lua_typeof(L, -1)]);

    case lua_type (L, mt) of
     LUA_TFUNCTION:
       begin
        orig_str_indx := lua_tocfunction (L, mt);
        lua_pop(L, 1);
        lua_pushcfunction (L, LuaStringsIndex);
        lua_setfield(L, mt, '__index');
        // TODO: incomplete upgrade variant
       end;
      LUA_TTABLE:
          begin
           lua_pushnil (L);
           keys := TStringList.Create;
           while lua_next (L, mt) <> 0 do
             begin
              k := LuaStrArg(L, -2);
              // if disp then wprintf ('    %20s = %s', [k, DumpValue(L, -1)] );
              lua_pop (L, 1);
              keys.Add(k);
             end;

           // adding string stuff. Need only just patch __index table
           if keys.IndexOf('split') < 0 then
             begin
              lua_setmap_c (L, 'find_sub', LuaFindSubStr, mt );
              lua_setmap_c (L, 'split',    LuaSplit, mt );
              lua_setmap_c (L, 'upper',    LuaStrUpper, mt );
              lua_setmap_c (L, 'lower',    LuaStrLower, mt );
              // lua_setmap_c (L, 'get_byte', LuaStrByte, t ); // duplicate test of byte
             end;

           lua_pushnil (L);
           while lua_next (L, mt) <> 0 do
             begin
              k := LuaStrArg(L, -2);
              if disp then
                 wprintf ('    %20s = %s', [k, DumpValue(L, -1)] );
              lua_pop (L, 1);
             end;

           while lua_gettop(L) > mt do
                 lua_pop (L, 1);

           lua_setmetatable(L, top + 1);
           // lua_remove(L, mt);  // __index not need now
           keys.Free;
          end;
    end;


   end;

 lua_settop (L, top);
end;


 procedure LuaRegFunc (L: lua_State; const func: AnsiString; fptr: lua_CFunction; const params: String = ''; disp: Boolean = FALSE);
 var
     msg: String;
    i, p: Integer;
 begin
  {$IFOPT D+}
  if disp then
    begin
     msg := Format(#9'Registering function [$%p] ~C0A%s~C0F %s~C07', [Addr(fptr), String(func), params] );
     p := Pos('//', msg);
     if (p > 0) then
        begin
         // выравнивание комментари€
         Insert ('~C07', msg, p - 1);
         for i := p to 85 do
             Insert (' ', msg, i - 1);
        end;

     ODS( msg );
    end;
  {$ENDIF}
  try
   lua_pushcfunction (L, fptr);
   lua_setglobal (L, String(func) );
   // lua_setfield (L, gt, PAnsiChar (func));
   // lua_register (L, func, fptr);
  except
   on E: Exception do
      OnExceptLog ('LuaRegFunc', E);
  end;

 end;


procedure ExportAll (L: lua_State; disp: Boolean);
begin
 {$IFDEF CPUX86}
 LuaRegFunc (L, 'BinThisCall',  BinThisCall,    '(this, method_ptr, ...) // execute method of object', disp );
 {$ENDIF}
 LuaRegFunc (L, 'bit_and',      LuaBitAND,      '(a, b)',                                                  disp);
 LuaRegFunc (L, 'bit_or',       LuaBitOR,       '(a, b)',                                                  disp);
 LuaRegFunc (L, 'CurrentTime', _current_time,   '()',                                                      disp);
 LuaRegFunc (L, 'FormatMsg',    LuaFormatMsg,   '(msg) // replace templates ~~* in string ',         disp);
 LuaRegFunc (L, 'FormatPtr',    LuaFormatPtr,   '(value) // return hex-string with $',                 disp);
 LuaRegFunc (L, 'FreeData',     LuaFreeData,    '(ptr) // release luaicp data via FreeMem',            disp);
 LuaRegFunc (L, 'FreeObject',   LuaFreeObject,  '(obj) // release luaicp object via .Free',            disp);
 LuaRegFunc (L, 'IniFile',      LuaIniFile,     '(file_name) // return TIniFile object ',              disp);
 LuaRegFunc (L, 'HexToInt',     LuaHexToInt,    '("$"..hex_str)',                                          disp);
 LuaRegFunc (L, 'MakePtr',      LuaMakePtr,     '(addr)',                                                  disp);
 LuaRegFunc (L, 'PreciseTime',  _precise_time,  '()',                                                      disp);
 LuaRegFunc (L, 'SplitStr',     LuaSplit,       '(string, delimiter)',                                     disp);
 LuaRegFunc (L, 'toboolean',    LuaCastBool,    '(any_type)',                                              disp);
 LuaRegFunc (L, 'toint64',      _StrToInt64,    '(string)',                                                disp);


 UpgradeStrings (L, disp);

 lua_pushlightuserdata (L, Pointer(L));
 SetPointerMT (L, -1);
 lua_setglobal (L, 'vm_state');
end;


procedure InitHashing;
var
   i: Integer;
begin
 for i := 0 to High (hash_permut) do
     hash_permut[i] := i;

 hash_permut[0] := $FF;
end;


initialization
 InitHashing;
end.


