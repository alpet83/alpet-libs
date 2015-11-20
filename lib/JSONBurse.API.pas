unit JSONBurse.API;

interface
uses Windows, SysUtils, Classes, StrUtils, DBXJSON, MD5,
        Misc, Lua5, StrClasses, BTITypes, DateTimeTools, InstrDictionary, WThreads,
        LuaTypes, LuaTools, LuaEngine, LuaWebPacket, LuaJSON, UNIArray, JSONTools, JSONWeb.API, DepthCollector,
        sgcWebSocket_Classes, sgcWebSocket_Client, sgcWebSocket_CustomClient, sgcWebSocket_Client_SocketIO, sgcWebSocket,
        IdComponent, IdSSLOpenSSLHeaders, IdSSLOpenSSL, IdcTypes, IdException, IdHttp, IdIOHandler, IdStack, IdURI;


const
    TAG_DEPTH  = 3;
    TAG_FILLS  = 4;
    TAG_CONN   = 5;
    TAG_OI     = 7;
    POS_FIELDS: array [0..3] of String = ('in_position', 'position', 'var_margin', 'opt_margin');

 type
    TBurseConnection = class;

    TRequestFunc   = function ( const endpoint, id: String; params: TJSONObject = nil; bInfo: Boolean = FALSE ): String of object;


    TFillsCacheBase     = TMatrixArrayT<TFillInfo, PFillsArray>;

     TLuaThread = class ( TWorkerThread )
     protected

      wp_func: String;
       engine: TLuaEngine;
         conn: TBurseConnection;

      procedure          ProcessInit; override;
      function           ProcessRequest (const rqs: String; rqobj: TObject): Integer; override;

     public


      { methods }
      procedure          WorkProc; override;
     end; // TLuaThread



     // socketIO based connection
    TBurseConnection = class (TSessionConnection)
     private
           FScriptName: String;
           FOnDepthRow: TDataEventBTI;
            FInstrDict: TInstrDictionary;
        FLocalTimeBias: Integer;
           FOnFillInfo: TDataEventBTI;
           FOnCallFail: TMsgEventBTI;
          FOnOrderInfo: TDataEventBTI;
       FOnPositionInfo: TDataEventBTI;
           FOnOwnTrade: TDataEventBTI;
            FOnRequest: TRequestFunc;
          FOnFindOrder: TDataEventBTI;
            FOnCommand: TMsgEventBTI;


       procedure  SetScriptName (const Value: String);
       function   GetAuthString (const Key: String): String;
       procedure  InitScritEngine (LE: TLuaEngine);

     protected

         data_updf: DWORD;

         FAuthInfo: TStrMap;
           FEngine: TLuaEngine;
        FBurseName: String;
          FThreads: array [0..7] of TLuaThread;

      procedure    ProcessMessage ( jo: TJSONObject ); override; // обработка сообщений сервера
     public

      { props }

      property     Active: Boolean read FActive write FActive;
      property     AuthInfo [const Key: String]: String read GetAuthString;
      property     BurseName: String  read FBurseName write FBurseName;
      property     ScriptName: String read FScriptName write SetScriptName;
      property     Engine: TLuaEngine read FEngine;
      property     InstrDict: TInstrDictionary read FInstrDict write FInstrDict;
      property     LocalTimeBias: Integer read FLocalTimeBias;


      // event handlers
      property     OnCallFailed: TMsgEventBTI    read FOnCallFail     write FOnCallFail;
      property     OnCommand:  TMsgEventBTI      read FOnCommand      write FOnCommand;
      property     OnDepthRow: TDataEventBTI     read FOnDepthRow     write FOnDepthRow;
      property     OnFindOrder: TDataEventBTI    read FOnFindOrder    write FOnFindOrder;
      property     OnFillInfo:  TDataEventBTI    read FOnFillInfo     write FOnFillInfo;
      property     OnOrderInfo: TDataEventBTI    read FOnOrderInfo    write FOnOrderInfo;
      property     OnOwnTrade: TDataEventBTI     read FOnOwnTrade     write FOnOwnTrade;
      property     OnPositionInfo: TDataEventBTI read FOnPositionInfo write FOnPositionInfo;
      property     OnRequest: TRequestFunc       read FOnRequest      write FOnRequest;

      { C & D }
      constructor  Create (const AServerHost: String; AServerPort: WORD);
      destructor   Destroy; override;

      { methods }
      function     CommitRequest ( const rqs: String ): String; virtual;
      procedure    CheckScriptUpdated;
      procedure    LoadAuthInfo; virtual;
      procedure    PushAuthParams (L: lua_State); virtual;
      procedure    Subscribe (const channel, data, idKey: String); virtual;
     end;




procedure       _Ticker_Instance (L: lua_State; iinf: TInstrInfo);


function CalcNonce (m: Integer = 0): String;
function DigitChars(const s: String): Boolean;
function NotEqual (a, b: Single; tresh: Single = 0.001): Boolean;
function Hash63 (const s: AnsiString): Int64; overload;
function Hash63 (const s: WideString): Int64; overload;
function OrderNumber(const s: WideString): Int64;
function microtime: UInt64;
function microtime_str (): UTF8String;

implementation
uses Math, LuaHttp;


const
   MCS_COEF = 1000000;

var
    last_nonce_0: Double = 0;
    last_nonce_1: Double = 0;

function CalcDecimals (v: Double): Integer;
var
   n: Integer;
begin
 for n := 0 to 18 do
  begin
   result := n;
   if Frac(v) = 0 then break;
   v := v * 10;
  end
end;

function CalcNonce(m: Integer): String;
var
   dt: Double;
begin
 dt := 0;

 case m of
  0:
    begin
     repeat
      dt := Trunc ( ( 1027 + PreciseTime ) * 1e13 );
     until dt > last_nonce_0;
     last_nonce_0 := dt;
    end;
  1: begin
      dt := Trunc ( Now * 25 * 4000 ); // ~ seconds
      if dt <= last_nonce_1 + 0.1 then
         dt := last_nonce_1 + 1;

      last_nonce_1 := dt;
     end;

 end;
 result := ftow (dt, '%.0f');
end;


function DigitChars(const s: String): Boolean;
const
   DIGITS = ['0'..'9'];

var
   n: Integer;
begin
 result := Length(s) > 0;
 for n := 1 to Length (s) do
     if not CharInSet ( s[n], DIGITS ) then result := FALSE;
end;

function NotEqual (a, b: Single; tresh: Single = 0.001): Boolean;
begin
 a := ( a - b ) / b;
 result := ( Abs (a) > tresh );
end;

function Hash63 (const s: AnsiString): Int64; overload;
var
   d: TMD5Digest;
begin
 d := MD5.MD5StringA (s);
 result := d.L1 xor d.L2;
 result := result and $7FFFFFFFFFFFFFFF; // 63 bit hash original
end;

function Hash63 (const s: WideString): Int64; overload;
var
   d: TMD5Digest;
begin
 d := MD5.MD5StringA ( AnsiString(s) );
 result := d.L1 xor d.L2;
 result := result and $7FFFFFFFFFFFFFFF; // 63 bit hash original
end;

function OrderNumber(const s: WideString): Int64;
begin
 if ( Length(s) <= 18 ) and ( DigitChars(s) ) then
    result := atoi (s)
 else
    result := Hash63 (s);
end;

function microtime: UInt64;
var
   bias: Double;
    pdt: TDateTime;
    ldt: TDateTime;
     st: TSystemTime;


begin
 bias := GetLocalTimeBias();
 GetSystemTime (st);
 ldt := SystemTimeToDateTime (st);
 pdt := PreciseTime + bias / (24 * 60); // to local time
 if Abs ( pdt - ldt ) < 1 / 50 then
    ldt := pdt; // compatible calc


 result := DateTimeToUnixTime64 ( ldt );  // время в формате GMT microseconds
end;

function microtime_str (): UTF8String;
begin
 result := UTF8String ( IntToStr ( microtime ) );
end;

function microtime2dt ( mt: Int64; tb: Integer ): TDateTime;
var
   v: Double;
begin
 v := ( mt mod MCS_COEF );
 result := UNIXTimeToDateTime ( Trunc ( mt / MCS_COEF ), tb );
 result := result + v * DT_ONE_MCS;
end;



function FieldCoefsInt ( var k: String; iinf: TInstrInfo; var pc, vc: Single ): Boolean;
var
   p: Integer;
begin
 p := Pos( '_int', k );
 pc := 1;
 vc := 1;
 result := ( p > 1 ) and ( Length(k) - p = 3 );
 if result then
  begin
   pc := iinf.price_coef;
   vc := iinf.vol_coef;
   k := AnsiReplaceStr ( k, '_int', '' );
  end;
end;



function _Format_Float (L: lua_State): Integer; cdecl;
var
   r, fmt: String;
begin
 fmt := LuaStrArg (L);
 r := FormatFloat ( fmt, lua_tonumber (L, 2) );
 r := ReplaceChars ( r, [','], '.' );
 lua_pushwstr (L, r);
 result := 1;
end;


function _lua_ODS (L: lua_State): Integer; cdecl;
var
    argc, flags: NativeUInt;
           conn: TBurseConnection;
              s: String;
begin
 result := 0;
 flags := 255;
 argc := lua_gettop(L);
 if argc = 0 then exit;
 conn := lua_getobj (L, 'connection', TRUE);
 if Assigned ( conn ) and ( conn is TBurseConnection ) and Assigned ( conn.Context ) then
    try
     flags := conn.Context.LogFlags;
    except
     on E: Exception do
        OnExceptLog( '_lua_ODS', E );
    end;

 if lua_isstring(L, LUA_ARG1 ) then
   begin
    s := lua_topstring (L, LUA_ARG1);
    if (argc >= 2) and ( lua_isnumber(L, LUA_ARG2) ) then
        flags := Trunc ( lua_tonumber(L, LUA_ARG2) );
    ODS ( s, flags );
   end;
end; // lua_ODS


function _make_nonce (L: lua_State): Integer; cdecl;
var
   n: Integer;
begin
 n := lua_tointeger (L, 1);
 lua_pushwstr (L, CalcNonce (n) );
 result := 1;
end;

function _URL_Encode (L: lua_State): Integer; cdecl;
var
    rs: String;
begin
 // enc := TUTF8Encoding.Create;
 rs := TIdURI.ParamsEncode ( LuaStrArg(L) );
 lua_pushwstr (L, rs);
 // enc.Free;
 result := 1;
end;



function _StoreDepth (L: lua_State): Integer; cdecl;
var
    conn: TBurseConnection;
    argc: Integer;
   instr: String;
     row: TDepthRow;


begin
 // this = connection, instr, price_int, bid_ask, vol_add_int, vol_total_int, unix_time_now
 result := 0;
 FillChar (row, sizeof(row), 0);

 argc := lua_gettop (L);

 conn := lua_objptr (L, 1);
 if (conn = nil) or not Assigned (conn.OnDepthRow) then exit;
 instr := LuaStrArg (L, 2);                                        // 1
 row.pszInstr := @instr[1];
 row.i_price  := lua_toint64 (L, 3);                               // 2
 row.is_bid   := ( LuaStrArg(L, 4) = 'bid' );                      // 3
 row.i_addvol := lua_toint64 (L, 5);                               // 4
 if argc > 5 then row.i_totvol := lua_toint64 (L, 6);              // 5
 if argc = 7 then row.i_now    := lua_toint64 (L, 7);              // 6
 conn.OnDepthRow ( @row, nil );
end;


function _Trade_UpdField (L: lua_State; conn: TBurseConnection; pf: PFillInfo; idx_key, idx_val: Integer): String;


var
   pc, vc: Single;
     iinf: TInstrInfo;
       dt: TDateTime;
       iv: Int64;
        k: String;
        v: Double;
begin
 result := '';
 if lua_isnil (L, idx_val) then exit;

 k := LuaStrArg (L, idx_key);
 v := lua_tonumber (L, idx_val);
 iv := lua_toint64 (L, idx_val);
 iinf := TInstrInfo ( pf.iinf );

 Assert ( iinf <> nil, '_Trade_UpdField: iinf = nil' );

 FieldCoefsInt ( k, iinf, pc, vc );

 if k = 'id' then
    pf.TradeNo := lua_toint64 (L, idx_val)
 else
 if k = 'price' then
    pf.e_price := v / pc
 else
 if k = 'qty' then
    pf.qty := v / vc
 else
 if k = 'account_id' then     // for MtGox this value by default = 0, if user trade
    pf.l_account := Round (v)
 else
 if k = 'buy' then
    pf.buy := lua_toboolean (L, idx_val)
 else
 if k = 'unix_date' then
   begin
    dt := UNIXTimeToDateTime ( Round(v), conn.LocalTimeBias );    // TODO: time bias must be local
    pf.bstime := DateTimeToTimeStamp (dt);
   end
 else
 if k = 'position' then
    pf.reached_pos := v / vc
 else
 if k = 'unix_time' then
   begin
    dt := microtime2dt (iv, conn.LocalTimeBias);
    pf.bstime := DateTimeToTimeStamp (dt);
   end
 else
 if k = 'order' then // ignore-not-store
   begin
    result := LuaStrArg (L, idx_val);
    pf.OrderNo := atoi (result);
   end;
end;

function _OwnTrade (L: lua_state): Integer; cdecl;
var
   conn: TBurseConnection;
   tnum: UInt64;

begin
 result := 0;
 conn := lua_objptr (L, 1);
 tnum := lua_toint64 (L, 2);
 if ( conn = nil ) or not Assigned ( conn.OnOwnTrade ) then exit;
 conn.OnOwnTrade ( @tnum, nil );
end;


function _StoreTrade (L: lua_state): Integer; cdecl;
var
  conn: TBurseConnection;
  iinf: TInstrInfo;
   oid: String;
   res: String;
    fi: TFillInfo;
    td: Integer;
begin
 result := 0;
 // getting fields from array
 conn := lua_objptr (L, 1);
 iinf := lua_objptr (L, 2); // ticker param

 if ( conn = nil ) or ( iinf = nil ) or not Assigned ( conn.OnFillInfo ) then exit;

 if lua_type (L, 3) <> LUA_TTABLE then exit;
 FillChar ( fi, sizeof(fi), 0 );
 // anonymous fill default
 fi.n_account := -1;
 fi.l_account := -1;
 fi.reached_pos := MAXINT;

 fi.iinf := iinf;
 fi.iUID := iinf.UID;
 oid := '';

 lua_pushnil (L); // traversing table from first item
 while lua_next (L, 3) <> 0 do
  begin
   res := _Trade_UpdField ( L, conn, @fi, -2, -1 );
   if res <> '' then
      oid := res;
   lua_pop (L, 1); // remove value, but rest key
  end;

 td := DateTimeToTimeStamp ( PreciseTime ).Time - fi.bstime.Time;

 if ( log_verbose > 8 ) then
   conn.Context.LogMsg ( '[~T]. #FILL_INFO: instr = %10s, price = %9s, qty = %9s, side = %9s,' +
                               ' trade_no = %10d, order_no = %10d, time = %s (delayed = %5d)',
                        [iinf.InstrAlias,
                          '~C0D' + iinf.FormatPrice(fi.e_price),
                          '~C0D' + iinf.FormatVolume(fi.qty),
                          IfV(fi.buy, '~C0Abuy', '~C0Csell'), fi.TradeNo, fi.OrderNo,
                          '~C0F' + FormatTime('hh:mm:ss.zzz', fi.bstime.Time), td] );   //

 fi.iinf := iinf;
 conn.OnFillInfo ( @fi, PChar (oid) );
end;


function _Order_UpdField (L: lua_State; conn: TBurseConnection; pord: POrder; idx_key, idx_val: Integer): String;

const
   MCS_COEF = 1000000;

var
   pc, vc: Single;
     iinf: TInstrInfo;
       dt: TDateTime;
       ts: TTimeStamp;
       iv: Int64;
        k: String;
        v: Double;
begin
 result := '';
 if lua_isnil (L, idx_val) then exit;

 k := LuaStrArg (L, idx_key);
 v := lua_tonumber (L, idx_val);
 iv := lua_toint64 (L, idx_val);

 iinf := TInstrInfo (pord.pnext);
 pc := 1; vc := 1;
 if iinf <> nil then
    FieldCoefsInt ( k, iinf, pc, vc );


 if k = 'price' then
    pord.price   := v / pc
 else
 if k = 'qty' then
    pord.qty     := v / vc
 else
 if k = 'balance' then
    pord.balance := v / vc
 else
 if ( k = 'buy' ) or ( k = 'to_buy' ) then
    pord.buy := lua_toboolean (L, idx_val)
 else
 if k = 'order_no' then
    pord.OrderNo := iv
 else
 if k = 'oid' then
    result := LuaStrArg (L, idx_val)
 else
 if k = 'int_id' then
    pord.InternalID := iv
 else
 if k = 'unix_date' then
    begin
     dt := UnixTimeToDateTime ( iv, GetLocalTimeBias );
     ts := DateTimeToTimeStamp ( dt );
     pord.date_a := ts.Date;
     pord.atime  := ts.Time;
    end
 else
 if k = 'status' then
    pord.status := DWORD(iv)


end;


function _StoreOrder (L: lua_state): Integer; cdecl;
var
  conn: TBurseConnection;
  iinf: TInstrInfo;
   ord: TOrder;
   oid: String;
    sv: String;
begin
 result := 0;
 // getting fields from array
 conn := lua_objptr (L, 1); // connection this
 iinf := lua_objptr (L, 2); // ticker param
 if ( conn = nil ) or not Assigned ( conn.OnOrderInfo ) then exit;

 if lua_type (L, 3) <> LUA_TTABLE then exit;

 oid := 'noname';

 FillChar ( ord, sizeof(ord), 0 );
 if Assigned(iinf) then // possible nil
  begin
   ord.instrID := iinf.UID;
   ord.pnext := Pointer(iinf);
  end;


 lua_pushnil (L); // traversing table from first item
 while lua_next (L, 3) <> 0 do
  begin
   sv := _Order_UpdField ( L, conn, @ord, -2, -1 );
   if sv <> '' then oid := sv;
   lua_pop (L, 1); // remove value, but rest key
  end;

 ord.pnext := nil;

 conn.OnOrderInfo ( @ord, PChar(oid) );
end;

function _Ticker_LockQuotes (L: lua_State): Integer; cdecl;
var
   iinf: TInstrInfo;
   lock: Boolean;
     dc: TDepthCollector;
begin
 result := 0;

 iinf := lua_objptr (L, 1);
 lock := lua_toboolean (L, 2);


 if lock then
    iinf.lock_flags := iinf.lock_flags or  $00000001
 else
    iinf.lock_flags := iinf.lock_flags and $FFFFFFFE;

 dc := TDepthCollector ( iinf.data [TAG_DEPTH] );
 if dc = nil then exit;

 if lock then
    dc.Lock ('script-LockQuotes')
 else
    dc.Unlock;
end;


function _Ticker_UpdField (L: lua_State; iinf: TInstrInfo; idx_key, idx_val: Integer): Boolean;


var
   pc, vc: Single;
   lp, lq: Single;
     conn: TBurseConnection;
     apos: TRTMAccPos;
       pf: Integer;
       iv: Int64;
       dt: TDateTime;
        i: Integer;
        k: String;
        v: Double;
        p: Integer;

begin
 k := LuaStrArg ( L, idx_key );
 v := lua_tonumber ( L, idx_val );
 iv := lua_toint64 ( L, idx_val );
 p := Pos( '_int', k );
 pc := 1;
 vc := 1;
 if ( p > 1 ) and ( Length(k) - p = 3 ) then
  begin
   pc := iinf.price_coef;
   vc := iinf.vol_coef;
   k := AnsiReplaceStr ( k, '_int', '' );
  end;

 result := TRUE;
 lp := iinf.last_price;
 lq := iinf.last_qty;

 conn := TBurseConnection (iinf.data [TAG_CONN]);

 FillChar (apos, sizeof(apos), 0); // TODO: account used as default value
 apos.InstrID := iinf.UID;

 pf := -1;
 k := AnsiReplaceStr(k, 'balance', 'position');

 // обновление позиций
 for i := 0 to High(POS_FIELDS) do
   if Pos ( POS_FIELDS[i], k ) > 0 then
      begin
       pf := i;
       break;
      end;


 if k = 'bid' then
    iinf.bid := v / pc
 else
 if ( k = 'ask' ) or ( k = 'offer' ) then
    iinf.offer := v / pc
 else
 if k = 'last_price' then
    lp := v / pc
 else
 if k = 'last_qty' then
    lq := v / vc
 else
 if Pos ( 'last_time', k ) > 0 then
   begin
    if Pos('_sec', k) > 0 then
       dt := UNIXTimeToDateTime ( iv, conn.LocalTimeBias )
    else
    if Pos('_msec', k) > 0 then
       dt := microtime2dt (iv * 1000, conn.LocalTimeBias)
    else
       dt := microtime2dt (iv, conn.LocalTimeBias);
    iinf.last_time := DateTimeToTimeStamp ( dt );
    conn.Context.LogMsg ('[~T].~C0F #TICKER:~C07 last_time assigned~C0A %s~C07 for~C0F %s~C07', [ FormatDateTime('hh:nn:ss.zzz', dt), iinf.InstrCode ] );
   end
 else
 if k = 'day_vol' then
   begin
    if iinf.Info.voltoday > v then
       iinf.last_qty := iinf.Info.voltoday - v;
    iinf.Info.voltoday := v;
   end
 else
 if k = 'high' then
    iinf.max_price := v / pc
 else
 if k = 'low' then
    iinf.min_price := v / pc
 else
 if ( k = 'avg' ) or ( k = 'open' ) then
    iinf.open_price := v / pc
 else
 if ( k = 'oi' ) or ( k = 'openint' ) then
    iinf.oi_curr := Round (v)
 else
 if ( k = 'min_step' ) then
 begin
    iinf.decimals := CalcDecimals (v);
    iinf.min_fstep := v;
    pc := Power (10, iinf.decimals );
    iinf.min_step := Round ( v * pc );
 end
 else
 if ( k = 'decimals' ) then
    iinf.decimals := Round (v)
 else
 if ( k = 'price_coef' ) then
    iinf.price_coef := v / pc
 else
 if ( k = 'min_price' ) then
    iinf.min_price := v / pc
 else
 if ( k = 'max_price' ) then
    iinf.max_price := v / pc
 else
 if ( pf >= 0 ) and Assigned (conn) and Assigned (conn.OnPositionInfo)  then
   begin
    case pf of
     0: apos.in_pos     := v / vc;
     1: apos.cur_pos    := v / vc;
     2: apos.margin     := v / vc;
     3: apos.opt_margin := v / vc;
    end;

    apos.tags [0] := pf;
    conn.OnPositionInfo ( iinf, @apos );
   end
 else
    result := FALSE;

 if ( lp <> iinf.last_price ) or ( lq <> iinf.last_qty ) then
   begin
    iinf.last_price := lp;
    iinf.last_qty := lq;
   end;

 if result then
    InterlockedIncrement ( iinf.tags [TAG_DEPTH] ); // some field updated, so can be send
end;

function _Ticker_NewIndex (L: lua_State): Integer; cdecl; // update ticker fields
var
   iinf: TInstrInfo;
begin
 iinf := lua_objptr (L, 1);
 if iinf <> nil then
    _Ticker_UpdField ( L, iinf, 2, 3 );
 result := 0;
end;

function _Ticker_UpdValues (L: lua_State): Integer; cdecl; // update fields from array (map)
var
   iinf: TInstrInfo;

begin
 result := 0;
 if lua_gettop(L) < 2 then exit;
 iinf := lua_objptr (L, 1);
 if  (iinf = nil) then exit;
 if lua_type (L, 2) <> LUA_TTABLE then exit;

 lua_pushnil (L); // traversing table from first item
 while lua_next (L, 2) <> 0 do
  begin
   _Ticker_UpdField ( L, iinf, -2, -1 );
   lua_pop (L, 1); // remove value, but rest key
  end;
end;


function _Ticker_Index (L: lua_State): Integer; cdecl; // return ticker fields/methods
var
   iinf: TInstrInfo;
      k: String;
begin
 result := 1;
 iinf := lua_objptr (L, 1);

 if iinf = nil then
   begin
    lua_pushnil (L);
    exit;
   end;

 k := LuaStrArg (L, 2);
 if ( k = 'update_values' ) or ( k = 'update_info' ) then
    lua_pushcfunction (L, _Ticker_UpdValues)
 else
 if k = 'bid' then
    lua_pushnumber (L, iinf.bid )
 else
 if ( k = 'ask' ) or ( k = 'offer' ) then
    lua_pushnumber (L, iinf.offer )
 else
 if k = 'last_price' then
    lua_pushnumber (L, iinf.last_price )
 else
 if k = 'last_qty' then
    lua_pushnumber (L, iinf.last_qty)
 else
 if ( k = 'avg' ) or ( k = 'open' ) then
    lua_pushnumber (L, iinf.open_price )
 else
 if k = 'instr_alias' then
    lua_pushwstr(L, iinf.InstrAlias)
 else
 if ( k = 'instr_board' ) or ( k = 'board' ) then
    lua_pushwstr(L, iinf.Board)
 else
 if k = 'instr_code' then
    lua_pushwstr(L, iinf.InstrCode)
 else
 if k = 'short_name' then
    lua_pushwstr(L, iinf.ShortName)
 else
 if k = 'price_coef' then
    lua_pushnumber (L, iinf.price_coef )
 else
 if k = 'vol_coef' then
    lua_pushnumber (L, iinf.vol_coef )
 else
 if k = 'decimals' then
    lua_pushinteger (L, iinf.decimals )
 else
 if k = 'quoted_instr' then
    _Ticker_Instance (L, iinf.PairQuoted)
 else
 if k = 'lock_quotes' then
    lua_pushcfunction (L, _Ticker_LockQuotes)
 else
 if k = 'vol_decimals' then
    lua_pushinteger (L, iinf.vol_decimals )
 else
    lua_pushnil (L);

 // WARN: _crc32 auto-updated from DataHash method
 iinf.Info._updt := CurrentTime;

 if iinf.Info.TestUpdated then
    iinf.Info.chg_time := iinf.Info._updt;
end;

procedure _Ticker_Instance (L: lua_State; iinf: TInstrInfo);
begin
 if iinf = nil then
    lua_pushnil (L)
 else
    AssignMetaIndex ( L, iinf, _Ticker_Index, _Ticker_NewIndex, 'gmt_Ticker' );
end;

function _LuaThread_AddRqs (L: lua_State): Integer; cdecl;
var
   lth: TLuaThread;
   rqs: String;
begin
 result := 0;
 lth := lua_objptr (L, 1);
 rqs := LuaStrArg (L, 2);
 lth.AddRequest (rqs);
end;

function _LuaThread_Index (L: lua_State): Integer; cdecl;
var
   lth: TLuaThread;
     k: String;
begin
 result := 1;
 lth := lua_objptr (L, 1);
 k   := LuaStrArg (L, 2);
 if k = 'add_request' then
    lua_pushcfunction ( L, _LuaThread_AddRqs)
 else
    lua_pushnil (L);
end;

function _Conn_AccountID (L: lua_State): Integer; cdecl;
var
   conn: TBurseConnection;
    acc: String;

begin
 conn := lua_objptr (L, 1);
 acc := LuaStrArg (L, 2);
 if Assigned (conn) and Assigned (conn.Context) then
    result := Conn.Context.Accounts.IndexOf (acc)
 else
    result := 0;
 lua_pushinteger (L, result);
 result := 1;

end;

function _Conn_BeginThread (L: lua_State): Integer; cdecl;
var
    func: String;
    conn: TBurseConnection;
    indx: Integer;
     lth: TLuaThread;

begin
 result := 1;
 conn := lua_objptr (L, 1);
 indx := lua_tointeger (L, 2) and 7;
 func := LuaStrArg (L, 3);

 lth := conn.FThreads [indx];
 if lth = nil then
   begin
    lth := TLuaThread.Create (FALSE, 'LuaThread#' + IntToStr(indx));
    lth.conn := conn;
    lth.WaitStart ();
    conn.FThreads [indx] := lth;
   end;

 lth.AddRequest ('INIT_ENGINE');
 lth.wp_func := func;

 AssignMetaIndex ( L, lth, _LuaThread_Index, nil, 'gmt_LuaThread' );
end;


function _Conn_Command (L: lua_state): Integer; cdecl;
var
   conn: TBurseConnection;
begin
 result := 0;
 conn := lua_objptr (L, 1);
 if (conn = nil) or not Assigned (conn.OnCommand) then exit;
 conn.OnCommand ( LuaStrArg(L, 2), LuaStrArg(L, 3) );
end;


function _Conn_FindOrder (L: lua_State): Integer; cdecl;
var
    conn: TBurseConnection;
    iinf: TInstrInfo;
     oid: String;
     ord: TOrder;
begin
 result := 1;
 conn := lua_objptr (L, 1);
 if (conn = nil) or not Assigned (conn.OnFindOrder) then exit;
 oid := LuaStrArg (L, 2);
 FillChar (ord, sizeof(ord), 0);

 if not conn.OnFindOrder ( PChar(oid), @ord ) then
    begin
     lua_pushnil (L);
     exit;
    end;

 lua_newtable (L);
 lua_setmap_f ( L, 'price',     ord.price );
 lua_setmap_f ( L, 'qty',       ord.qty );
 lua_setmap_b ( L, 'buy',       ord.buy );
 lua_setmap_f ( L, 'order_no',  ord.OrderNo );
 lua_setmap_f ( L, 'int_id',    ord.InternalID );

 if ord.instrID <= 0 then exit;
 iinf := conn.InstrDict.FindInstr ( ord.instrID );
 if iinf = nil then exit;
 lua_setmap_s ( L, 'instr_board', iinf.Board );
 lua_setmap_s ( L, 'instr_alias', iinf.InstrAlias );
 lua_setmap_s ( L, 'instr_code', iinf.InstrCode );
 lua_setmap_s ( L, 'instr_name', iinf.ShortName );
 lua_setmap_f ( L, 'price_coef', iinf.price_coef );
 lua_setmap_f ( L, 'vol_coef',   iinf.vol_coef );
end;

function _Conn_GetTicker (L: lua_State): Integer; cdecl;
var
    conn: TBurseConnection;
   instr: String;
    iinf: TInstrInfo;
begin
 result := 1;
 conn := lua_objptr (L, 1);
 if (conn = nil) then exit;
 instr := LuaStrArg (L, 2);

 iinf := conn.InstrDict.GetInstrInfo ( instr, 3 );
 if iinf = nil then
    lua_pushnil (L)
 else
   begin
    iinf.data [TAG_CONN] := conn;
    _Ticker_Instance ( L, iinf );
   end;
end;

function _Conn_HttpRqs ( L: lua_state): Integer; cdecl;
var
   conn: TBurseConnection;
     ih: TIdHttp;
     ac: Integer;
     ff: DWORD;
begin
 ac := lua_gettop (L);
 conn := lua_objptr (L, 1);
 result := 1;
 if conn = nil then;

 ih := TIdHttp.Create (nil);

 ih.ConnectTimeout := 0;
 ih.ReadTimeout := 10000;
 ih.HandleRedirects := TRUE;
 ih.AllowCookies := TRUE;
 ih.HTTPOptions := ih.HTTPOptions - [hoForceEncodeParams];


 if ac > 1 then
   begin
    ih.URL.Host := LuaStrArg (L, 2);
    ih.Request.Host := ih.URL.Host;
   end;

 ih.Request.Accept := '*/*';
 ih.Request.AcceptCharSet := 'utf-8';
 ih.Request.CharSet := 'utf-8';
 ih.Request.Connection := 'close';
 ih.Request.ContentLanguage := 'en,ru';
 ih.Request.Pragma := 'no-cache';
 ih.Request.CacheControl := 'no-cache';
 ih.Request.ContentType := 'application/x-www-form-urlencoded; charset=utf-8';
 ih.Request.UserAgent := 'Mozilla/4.0 (compatible; Indy HTTP based program)';

 ff := lua_tointeger (L, 3);
 // 'Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; WOW64; Trident/5.0; MAAU)';
 AssignMetaIndex ( L, ih, _IdHttp_Index, _IdHttp_NewIndex, 'IdHttp_object', ff );

end;

function _Conn_Request ( L: lua_state): Integer; cdecl;
var
    argc: Integer;
    conn: TBurseConnection;
  params: TJSONObject;
  id, ep: String;
    info: Boolean;
     res: String;


begin
 result := 1;
 argc := lua_gettop (L);
 conn := lua_objptr (L, 1);
 ep := LuaStrArg (L, 2);
 id := LuaStrArg (L, 3);
 params := nil;
 info := FALSE;

 if argc > 3 then params := lua_objptr (L, 4);
 if argc > 4 then info := lua_toboolean (L, 5);

 res := '#FAIL: conn = nil or callback not set';

 if Assigned (conn) and Assigned (conn.OnRequest) then
    res := conn.OnRequest ( ep, id, params, info );
 lua_pushwstr (L, res);
end;

function _Conn_Subscribe ( L: lua_state): Integer; cdecl;
var
    argc: Integer;
    conn: TBurseConnection;
    data: String;
     chn: String;
     idk: String;

begin
 argc := lua_gettop (L);
 conn := lua_objptr (L, 1);
 result := 0;
 if conn = nil then exit;
 data := '';
 chn := '';
 idk := '';
 chn := LuaStrArg (L, 2);
 if argc >= 4 then
   begin
    data := LuaStrArg (L, 3);
    idk := LuaStrArg (L, 4);
   end;

 conn.Subscribe ( chn, data, idk );
end;


function _Conn_OnCallFailed ( L: lua_state): Integer; cdecl;
var
   conn: TBurseConnection;
    msg: String;
     id: String;
begin
 result := 0;
 conn := lua_objptr (L, 1);
 id := LuaStrArg (L, 2);
 msg := LuaStrArg (L, 3);
 if Assigned ( conn.OnCallFailed ) then
    conn.OnCallFailed ( id, msg );
end;


function _Conn_OnDataUpdated ( L: lua_state): Integer; cdecl;
var
   conn: TBurseConnection;
   data: String;

begin
 result := 0;
 conn := lua_objptr (L, 1);
 data := LuaStrArg (L, 2);

 if ( data = 'fills' ) then
    conn.data_updf := conn.data_updf or UPDF_FILLS
 else
 if data = 'ticker' then
    conn.data_updf := conn.data_updf or UPDF_INSTR_INFO
 else
 if ( data = 'trades' ) or ( data = 'ticks' ) then
    conn.data_updf := conn.data_updf or UPDF_TICKS
 else
 if data = 'orders' then
    conn.data_updf := conn.data_updf or UPDF_ORDERS
 else
 if data = 'positions' then
    conn.data_updf := conn.data_updf or UPDF_POSITIONS
 else
 if data = 'quotes' then
    conn.data_updf := conn.data_updf or UPDF_INSTR_QTS;
end;

function _Conn_Index (L: lua_state): Integer; cdecl;
var
   conn: TBurseConnection;
      k: String;
begin
 conn := lua_objptr (L, 1);
 k := LowerCase ( LuaStrArg (L, 2) );
 result := 1;
 if k = 'account_id' then
    lua_pushcfunction (L, _Conn_AccountID )
 else
 if k = 'active' then
    lua_pushboolean ( L, conn.Active )
 else
 if k = 'begin_thread' then
    lua_pushcfunction ( L, _Conn_BeginThread )
 else
 if k = 'command' then
    lua_pushcfunction ( L, _Conn_Command )
 else
 if k = 'data_updf' then
    lua_pushinteger ( L, conn.data_updf )
 else
 if k = 'find_order' then
    lua_pushcfunction ( L, _Conn_FindOrder )    // find order object by oid/order-no
 else
 if k = 'get_ticker' then
    lua_pushcfunction ( L, _Conn_GetTicker )
 else
 if k = 'http_request' then
    lua_pushcfunction ( L, _Conn_HttpRqs )
 else
 if k = 'own_trade' then
    lua_pushcfunction ( L, _OwnTrade )
 else
 if k = 'request' then
    lua_pushcfunction ( L, _Conn_Request )
 else
 if k = 'store_depth' then
    lua_pushcfunction ( L, _StoreDepth )
 else
 if k = 'store_trade' then
    lua_pushcfunction ( L, _StoreTrade )
 else
 if k = 'store_order' then
    lua_pushcfunction ( L, _StoreOrder )
 else
 if k = 'subscribe' then
    lua_pushcfunction ( L, _Conn_Subscribe )
 else
 if k = 'on_data_upd' then
    lua_pushcfunction ( L, _Conn_OnDataUpdated )
 else
 if ( k = 'on_failed_call' ) or ( k = 'on_invalid_call' ) then
    lua_pushcfunction ( L, _Conn_OnCallFailed )
 else
   begin
    PrintError('connection has no field ' + k );
    lua_pushnil (L);
   end;
end;

function _Conn_NewIndex (L: lua_state): Integer; cdecl;
var
   conn: TBurseConnection;
      k: String;
begin
 conn := lua_objptr (L, 1);
 k := LowerCase ( LuaStrArg (L, 2) );
 result := 0;
 if k ='data_updf' then
    conn.data_updf := atoi ( LuaStrArg (L, 3) )
 else;


end;


procedure _Conn_Instance (L: lua_State; conn: TBurseConnection);
begin
 AssignMetaIndex ( L, conn, _Conn_Index, _Conn_NewIndex, 'gmt_BurseConnection' );
end;


{ TBurseConnection }

procedure TBurseConnection.CheckScriptUpdated;
begin
 if not FileExists ( ScriptName ) then exit;

 InitScritEngine (Engine);

end;

procedure TBurseConnection.InitScritEngine ( LE: TLuaEngine );
begin
 if LE.LoadScript ( ScriptName, TRUE ) then
 with LE do
   begin
    // updating global variables and functions
    VarSet ( 'connection', self );
    _Conn_Instance ( State, self );
    lua_setglobal  ( State, 'connection' );
    lua_pushnil ( State );
    lua_setglobal ( State, 'ODS' );

    RegFunc ( 'ODS',           @_lua_ODS );
    RegFunc ( 'base64_decode', @_base64_decode );
    RegFunc ( 'current_time',  @_current_time );
    RegFunc ( 'data_buffer',   @_data_buffer );
    RegFunc ( 'format_float',  @_Format_Float );
    RegFunc ( 'make_nonce',    @_make_nonce );
    RegFunc ( 'sleep_ex',      @_sleep_ex );
    RegFunc ( 'utf8_buffer',   @_utf8_buffer );
    RegFunc ( 'web_packet',    @_web_packet );
    RegFunc ( 'JSON_object',   @_JSON_object );
    RegFunc ( 'JSON_parse',    @_JSON_parse );
    RegFunc ( 'URL_encode',    @_URL_encode );

    // constant values
    VarSet ( 'ORDS_ACTIVE',    ORDS_ACTIVE,    TRUE );
    VarSet ( 'ORDS_CANCELLED', ORDS_CANCELLED, TRUE );
    VarSet ( 'ORDS_EXPENSIVE', ORDS_EXPENSIVE, TRUE );
    VarSet ( 'ORDS_FORGOTTEN', ORDS_FORGOTTEN, TRUE );
    VarSet ( 'ORDS_MAPPED',    ORDS_MAPPED,    TRUE );

    VarSet ( 'ORDS_MATCHED',   ORDS_MATCHED,   TRUE );
    VarSet ( 'ORDS_REFUSED',   ORDS_REFUSED,   TRUE );
    VarSet ( 'ORDS_REJECTED',  ORDS_REFUSED,   TRUE );
    VarSet ( 'ORDS_PENDING',   ORDS_PENDING,   TRUE );
    VarSet ( 'ORDS_PS_ERROR',  ORDS_PS_ERROR,  TRUE );
    VarSet ( 'ORDS_UPDATED',   ORDS_UPDATED,   TRUE );

    Execute;
   end;
end;

function TBurseConnection.CommitRequest(const rqs: String): String;
begin
 Client.WriteData ( rqs );
 result := 'OK';
end;

constructor TBurseConnection.Create(const AServerHost: String; AServerPort: WORD);
var
   ext: String;
begin
 inherited Create (AServerHost, AServerPort);
 FAuthInfo := TStrMap.Create (self);
 FEngine := TLuaEngine.Create;

 ext := ExtractFileExt ( misc.ModuleName );
 ScriptName := AddSlash ( misc.DllPath ) + AnsiReplaceStr ( misc.ModuleName, ext, '.lua' );
 FLocalTimeBias := GetLocalTimeBias();

end;

destructor TBurseConnection.Destroy;
begin
 OpThreadList ( OP_THREAD_STOP,  0, @FThreads, 8 );
 OpThreadList ( OP_WAIT_STOP, 5000, @FThreads, 8 );
 OpThreadList ( OP_FREE_NIL,     0, @FThreads, 8 );

 inherited;
 FAuthInfo.Free;
 FEngine.Free;
end;

function TBurseConnection.GetAuthString(const Key: String): String;
begin
 result := FAuthInfo.Values [Key];
end;

procedure TBurseConnection.LoadAuthInfo;
begin
 //
end;

procedure TBurseConnection.ProcessMessage (jo: TJSONObject);
var
   t, tt: Integer;
begin
 Engine.Lock('ProcessMessage');
 try
   t := lua_gettop(Engine.State);
   // passing message to Lua
   Engine.VarSet ( 'message', jo );

   _JO_Instance ( Engine.State, jo );
   lua_setglobal ( Engine.State, 'message' );
   Engine.CallFunc ( 'on_message' ); // script must interpret message, push-back data


   tt := lua_gettop(Engine.State);
   if tt > t then
     begin
      PrintError( Format('after call on_message lua stack was changed from %d to %d', [t, tt] ) );
      lua_settop (Engine.State, t);
     end;


 finally
   Engine.Unlock;
 end;

 inherited;
end;

procedure TBurseConnection.PushAuthParams(L: lua_State);
begin
 //
end;

procedure TBurseConnection.SetScriptName(const Value: String);
begin
 FScriptName := Value;
 CheckScriptUpdated;
end;


procedure TBurseConnection.Subscribe(const channel, data, idKey: String);
begin
 // nope
end;



{ TLuaThread }

procedure TLuaThread.ProcessInit;
begin
 inherited;
 engine := TLuaEngine.Create;
 Garbage.Add (engine);
end;

function TLuaThread.ProcessRequest(const rqs: String; rqobj: TObject): Integer;
var
   s: String;
begin
 result := inherited ProcessRequest (rqs, rqobj);

 if Pos( 'SET_SCRIPT=', rqs ) = 1 then
    begin
     s := rqs;
     Delete (s, 1, Length('SET_SCRIPT='));
     Engine.LoadScript ( s, TRUE );
    end;

 if ( rqs = 'INIT_ENGINE' ) and Assigned (conn) then
     conn.InitScritEngine ( engine );
end;

procedure TLuaThread.WorkProc;
begin
 inherited;
 if ( engine <> nil ) and ( wp_func <> '' ) and
      Assigned (conn) and conn.Active then
                engine.CallFunc ( wp_func );
end;


end.
