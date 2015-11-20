unit LuaTypes;

interface
uses Windows, SysUtils, WinHeap;



{$A4}
{$I lua_config.inc}


const
  LUA_IDSIZE = 60;

  LUA_VERSIONS  = 'Lua 5.1.0';
  LUA_COPYRIGHT = 'Copyright (C) 1994-2004 Tecgraf, PUC-Rio';
  LUA_AUTHORS   = 'R. Ierusalimschy, L. H. de Figueiredo & W. Celes';

  (* option for multiple returns in `lua_pcall' and `lua_call' *)
  LUA_MULTRET	  = -1;
  LUAC_SIGNATURE  = #27'Lua';

  (*
  ** pseudo-indices
   *)
  LUA_REGISTRYINDEX	= -10000;
  LUA_ENVIRONINDEX	= -10001;
  LUA_GLOBALSINDEX	= -10002;

  (* error codes for `lua_load' and `lua_pcall' *)
  LUA_ERRRUN    = $001;
  LUA_ERRFILE   = $002;
  LUA_ERRSYNTAX = $003;
  LUA_ERRMEM    = $004;
  LUA_ERRERR    = $005;
  LUA_ERRBAD_CC = $201;

  LUA_TNONE	       = -1;
  LUA_TNIL                 = 0;
  LUA_TBOOLEAN             = 1;
  LUA_TLIGHTUSERDATA       = 2; LUA_TLUDATA = LUA_TLIGHTUSERDATA;
  LUA_TNUMBER              = 3;
  LUA_TSTRING              = 4;
  LUA_TTABLE               = 5;
  LUA_TFUNCTION            = 6;
  LUA_TUSERDATA	           = 7;
  LUA_TTHREAD              = 8;

  LUA_ANY_USERDATA: set of BYTE = [LUA_TLIGHTUSERDATA, LUA_TUSERDATA];
       LUA_TOBJECT: set of BYTE = [LUA_TNIL, LUA_TLIGHTUSERDATA, LUA_TUSERDATA];

  LUA_HOOKCALL	       = 0;
  LUA_HOOKRET	         = 1;
  LUA_HOOKLINE         = 2;
  LUA_HOOKCOUNT	       = 3;
  LUA_HOOKTAILRET      = 4;

  LUA_MASKCALL	       = (1 shl LUA_HOOKCALL);   // 1
  LUA_MASKRET	       = (1 shl LUA_HOOKRET);    // 2
  LUA_MASKLINE	       = (1 shl LUA_HOOKLINE);   // 4
  LUA_MASKCOUNT	       = (1 shl LUA_HOOKCOUNT);  // 8


type

  SIZE_T = DWORD;

  lua_State = type Pointer;
  lua_Result = Integer;
  lua_Integer = Integer;
  lua_Number = Double;


  lua_Alloc = function (ud, p: Pointer; osize, nsize: SIZE_T): Pointer;  cdecl;
  lua_CFunction = function (L: lua_State): Integer; cdecl;

  TAllocRec = record
   tid: DWORD;
   fma: TFastMemAllocator;
  end;


  lua_Object = type Pointer;

  TLUAInterceptFunc = lua_CFunction; // function ( L: lua_State ): Integer; cdecl;


  (*
  ** functions that read/write blocks when loading/dumping Lua chunks
  *)
  lua_Chunkreader = function(L: lua_State; ud: Pointer; var sz: Cardinal): PAnsiChar; cdecl;
  lua_Chunkwriter = function(L: lua_State; p: Pointer; sz: Integer; ud: Pointer): Integer; cdecl;

  // lua_GCObject = pointer;
  lu_byte = BYTE;
  lu_int32 = DWORD;

  // lua_StateRec = record;

  TLuaProc    = procedure (L: lua_State);
  TLuaProcEx  = procedure (L: lua_State; p: Pointer);
  TLuaMethod  = procedure (L: lua_State) of object;


  lua_CommonHeader = packed record //
   next: Pointer;
   tt, marked: lu_byte;
  end;

  lua_GCHeader = lua_CommonHeader;



  lua_Value = packed record
   case BYTE of
    0: ( p, h: Pointer );
    1: ( n: lua_Number );
    2: ( b: Int64 );
    3: ( gc: pointer );
  end;    // union for any

  lua_TValue = packed record
   value: lua_Value;       // 64-bit value
   tt, rsv: LongInt;
  end;

  lua_ClosureHeader = packed record
   ch: lua_CommonHeader;
   isC: lu_byte;
   nupvalues: lu_byte;
   gclist: Pointer; // ^lua_GCObject
   env: Pointer;
  end;

  lua_CClosure = record
   h: lua_ClosureHeader;
   f: lua_CFunction;
   upvalue: array [0..0] of lua_TValue;
  end;

  lua_LClosure = record
   h: lua_ClosureHeader;
   p: Pointer;
   upvalue: Pointer;
  end;

  lua_Closure = record
   case BYTE of
    0: (c: lua_CClosure);
    1: (l: lua_LClosure);
  end;



  lua_GCObject = record
  case BYTE of
   0: ( gch: lua_GCHeader );
   1: ( cl: lua_Closure );
  end;

  PGCObject = ^lua_GCObject;


  Instruction = DWORD;


  stkId = ^lua_TValue;

  lua_TStack = packed array [0..63] of lua_TValue;
  lua_PStack = ^lua_TStack;


  lua_Debug = record
    event: Integer;
    name: PAnsiChar; (* (n) *)
    namewhat: PAnsiChar; (* (n) `global', `local', `field', `method' *)
    what: PAnsiChar; (* (S) `Lua', `C', `main', `tail' *)
    source: PAnsiChar; (* (S) *)
    currentline: Integer; (* (l) *)
    nups: Integer;  (* (u) number of upvalues *)
    linedefined: Integer; (* (S) *)
    short_src: array[0..LUA_IDSIZE-1] of AnsiChar; (* (S) *)
    (* private part *)
    i_ci: Integer; (* active function *)
   end;

  PLua_debug = ^lua_Debug;
  lua_PDebug = PLua_Debug;

  lua_Hook = procedure(L: lua_State; var ar: lua_Debug); cdecl;


  lua_CallInfo = record
   base, func, top: StkId;
   savedpc: ^Instruction;
   nresults: Integer;
   tailcalls: Integer;
  end;

  stringtable = record
   hash: Pointer;
   nuse: lu_int32;
   size: Integer;
  end;

  global_State = record
   strt: stringtable;
   frealloc: lua_Alloc;
   ud: Pointer;
   currentwhite: lu_byte;
   gc_state: lu_byte;
   // TODO: incomplete...
  end;

  PLuaGlobalState = ^global_State;

  {$A1}
  // stateRec for xrayLua 1.0004, after optimizator
  {$IFDEF LUAJIT}

  // LuaJIT structure for lua_State
  lua_StateRec = packed record      // $00
   // CommonHeader
            next: PGCObject;
      tt, marked: lu_byte;          // $04, $05
   //
          status: lu_byte;          // $06
          _align: BYTE;             // $07
             top: StkId;
            base: StkId;

               G: PLuaGlobalState;  // $10
              ci: ^lua_CallInfo;
         savedpc: PDWORD;
      stack_last: StkId;
           stack: StkId;
          end_ci: ^lua_CallInfo;
         base_ci: ^lua_CallInfo;

      stacksize: Integer;
        size_ci: Integer;
        nCcalls: WORD;
        _align2: WORD;

       hookmask: lu_byte;
        _align3: BYTE;
      allowhook: lu_byte;
        _align4: BYTE;


  basehookcount: Integer;
      hookcount: Integer;
           hook: lua_Hook;

           l_gt: lua_TValue;
            env: lua_TValue;
       openupval: Pointer;
          gclist: Pointer;
        errorJmp: Pointer;
         errfunc: Integer;
//         buff: array [0..1023] of AnsiChar;
  end;

  {$ELSE}

  // xrLua structure for lua_State
  lua_StateRec = packed record
    // CommonHeader
            next: PGCObject;
      tt, marked: lu_byte;     // $04, $05
    //
          status: lu_byte;
        hookmask: lu_byte;     // $06, $07
       stacksize: Integer;     // $08
             top: StkId;       // $0C
            base: StkId;       // $10
               G: PLuaGlobalState;         // $14
              ci: ^lua_CallInfo;          // $18
         savedpc: PDWORD;            // $1C
      stack_last: StkId;          // $20
           stack: StkId;               // $24
          end_ci: ^lua_CallInfo;     // $28
         base_ci: ^lua_CallInfo;     // $2C
         size_ci: Integer;           // $30
   // nested C calls
         nCcalls: WORD;           // $34
      baseCcalls: WORD;           // $36
   basehookcount: Integer;     // $38
       hookcount: Integer;     // $3C
            hook: lua_Hook;    // $40
            foo0: DWORD;       // $44
            l_gt: lua_TValue;  // $48
             env: lua_TValue;  // $58
       openupval: Pointer;     // $68
          gclist: Pointer;     // $6C
        errorJmp: Pointer;     // $70  to struct lua_longjmp
         errfunc: Integer;     // $74
  //   last_flds: array [0..7] of Integer;     // $78
  end; // lua_StateRec

  {$ENDIF}

  lua_PStateRec = ^lua_StateRec;
{



  #define CommonHeader	GCObject *next; lu_byte tt; lu_byte marked

  // lua_State ------------------------------------

  CommonHeader;
  lu_byte status;
  StkId top;  /* first free slot in the stack */
  StkId base;  /* base of current function */
  global_State *l_G;
  CallInfo *ci;  /* call info for current function */
  const Instruction *savedpc;  /* `savedpc' of current function */
  StkId stack_last;  /* last free slot in the stack */
  StkId stack;  /* stack base */
  CallInfo *end_ci;  /* points after end of ci array*/
  CallInfo *base_ci;  /* array of CallInfo's */
  int stacksize;
  int size_ci;  /* size of array `base_ci' */
  unsigned short nCcalls;  /* number of nested C calls */
  unsigned short baseCcalls;  /* nested C calls when resuming coroutine */
  lu_byte hookmask;
  lu_byte allowhook;
  int basehookcount;
  int hookcount;
  lua_Hook hook;
  TValue l_gt;  /* table of globals */
  TValue env;  /* temporary place for environments */
  GCObject *openupval;  /* list of open upvalues in this stack */
  GCObject *gclist;
  struct lua_longjmp *errorJmp;  /* current error recover point */
  ptrdiff_t errfunc;  /* current error handling function (stack index) */
}


// void luaU_header (char* h)
{
 int x=1;
 memcpy(h,LUA_SIGNATURE,sizeof(LUA_SIGNATURE)-1);
 h+=sizeof(LUA_SIGNATURE)-1;
 *h++=(char)LUAC_VERSION;
 *h++=(char)LUAC_FORMAT;
 *h++=(char)*(char*)&x;
 *h++=(char)sizeof(int);
 *h++=(char)sizeof(size_t);
 *h++=(char)sizeof(Instruction);
 *h++=(char)sizeof(lua_Number);
 *h++=(char)(((lua_Number)0.5)==0);
}

  TLUAC_HEADER = packed record
   signature: array [0..3] of AnsiChar;  // #27Lua
    luac_ver: BYTE;  // $51
    luac_fmt: BYTE;  // $00
     dummy_1: BYTE;  // $01
      sz_int: BYTE;
      sz_szt: BYTE;
    sz_instr: BYTE;
   sz_number: BYTE;
    no_float: BYTE;
  end;

  PLUAC_HEADER = ^TLUAC_HEADER;

var
   g_panic: lua_CFunction = nil;


implementation




end.
