unit LuaEngine;
interface
{$I luadef.inc}
uses Windows, SysUtils, StrUtils, Classes, StrClasses, Scripting, Misc,
     {$IFDEF XRAY_LUA}
      XrayLua,
     {$ELSE}
      Lua5,
     {$ENDIF}
     WinHeap, LuaTypes, LuaTools;

{ Общая поддержка движка Луа, для выполнения сценариев. }

type
       TFuncParamCB = function ( Engine: TScriptEngine ): Integer of object; // must return count of params pushed

       TStrEvent = procedure (const s: String) of object;
// -------------------------------------------------
       TLuaEngine = class (TScriptEngine)
       private
        prv_script_file: String;
        prv_script_time: TDateTime;
        FOnScriptError: TStrEvent;
        FLastError: String;

          FJitCode: Boolean;
        FPopResult: Boolean;


        procedure       CheckRegVar (const sName: String; vt: DWORD; push: Boolean = FALSE);
        procedure       OnPCallResult (const fname: String; err_idx, svtop: Integer);


       protected
                FState: lua_State;
                FAlloc: TFastMemAllocator;
              FJitBuff: array of AnsiChar;
        FScriptUpdated: Boolean;

        procedure        DefErrorHandler(const s: String);
       public

        property         JITCode: Boolean read FJITCode;
        property         LastError: String read FLastError write FLastError;
        property         ScriptUpdated: Boolean read FScriptUpdated;
        property         State: lua_State read FState;
        property         OnScriptError: TStrEvent read FOnScriptError write FOnScriptError;
        property         PopResult: Boolean read FPopResult write FPopResult;


        { C & D }
        constructor      Create;
        destructor       Destroy; override;

        { methods }
        function          AddPanic: Integer; virtual;  // Push panic callback
        procedure         Assign ( src: TPersistent ); override;
        procedure         CallFunc (const fname: String); override;
        procedure         CallFuncArg (const fname: String; argcb: TFuncParamCB );

        procedure         CallFuncEx (const fname, in_params, out_params: String); override;
        procedure         CloseState;
        procedure         CheckStack (const ctx: String; norm: Integer = 10);

        procedure         Init; override;

        procedure         LoadBuffer ( src: PAnsiChar; cb: Integer );
        procedure         LoadFromFile(const FileName: string); override;
        function          LoadScript (const sFileName: String; check_time: Boolean = FALSE): Boolean; virtual;

        procedure         NewTable (const sName: String);

        function          RegFunc (const fname: String; pfunc: Pointer): Boolean; override;
        function          ReadArray (const sName: String; idx: Integer = 0): TScriptVarArray; override;
        procedure         WriteArray (const sName: String; varr: TScriptVarArray; idx: Integer = 0); override;
        procedure         WriteTable (const sName: String; varr: TScriptVarTable; idx: Integer = 0); override;
        function          ReadTable (const sName: String; idx: Integer = 0): TScriptVarTable; override;

        procedure         ReadVar (sv: PScriptVar); override;
        procedure         WriteVar (sv: PScriptVar); override;

        procedure         RegVar(sv: PScriptVar; bRegister: Boolean = TRUE); override;
        procedure         RemovePanic ( idx: Integer );
        procedure         SetGlobal (const sName: String); overload;
        procedure         SetGlobal (const sName: String;  exp_proc: TLuaProc); overload;
        procedure         SetGlobal (const sName: String; exp_method: TLuaMethod); overload;


        procedure         Execute; override;  // выполнить сценарий

        function          VarType (const sName: String ): DWORD; override;

       end; // TLuaEngine


var
   gScriptsPath: String = '';

function  lua_varType (L: lua_State; index: Integer): DWORD; // конвертировать константы
procedure lua_readVar(L: lua_State; index: Integer; psv: PScriptVar);

procedure lua_readTable (L: lua_State; index: Integer; aobj: TScriptVarTable);

function DMAlloc(ud, p: Pointer; osize, nsize: DWORD): Pointer; cdecl;

function  LogMsg(L: lua_State): Integer; cdecl;
function  SplitString(L: lua_State): Integer; cdecl;

implementation
uses Math;

function ReadBytesDump (const sFileName: String; cb: Integer): String;
var
   buff: array of BYTE;
     rb: Integer;
      f: File;
begin
 result := '';
 SetLength (buff, cb);
 AssignFile (f, sFileName);
 {$I-}
 Reset (f, 1);
 BlockRead (f, buff[0], cb, rb);
 CloseFile (f);

 if rb = cb then
    for rb := 0 to cb - 1 do
        result := result + ' ' + IntToHex( buff[rb], 2 );
 if Length (result) > 0 then
    Delete (result, 1, 1); // remove 1st space
end;

function  DumpByteCode (L: lua_State; pSrc: Pointer; sz: Integer; ud: Pointer): Integer; cdecl;
var
  le: TLuaEngine;
  pb: PByteArray;
   s: String;
   n: Integer;
begin
 le := ud;
 if (le <> nil) and ( pSrc <> nil ) and ( sz > 0) then
  begin
   result := 0;
   pb := pSrc;
   s := '';
   for n := 0 to sz - 1 do
      begin
       s := s + ' ' + IntToHex ( pb [n], 2 );
       if n and 15 = 15 then s := s + #13#10;
      end;
   ODS('[~T]. #DUMP: ByteCode from ' + le.Name + ':~C0F'#13#10 + s + '~C07');
  end
 else
   result := -1;
end;

// functions

function panic_cb(L: lua_State): Integer; cdecl;
var
   err: String;
    sv: String;
    le: TLuaEngine;

begin
 try
  err := LuaStrArg (L, 1);
  sv  := LuaTraceback (L, ' ', 1);
  err := err + #13#10 + sv;
 except
  on E: Exception do
     OnExceptLog('panic_cb', E);

 end;

 lua_getglobal (L, 'last_error');
 sv := LuaStrArg(L, -1);
 if sv <> '' then err := err + #13#10'last_error: ' + sv;
 lua_pop(L, 1);

 lua_getglobal (L, 'EngineObject');
 le := lua_objptr (L, -1);
 lua_pop(L, 1);

 if Assigned (le) then
    le.LastError := err
 else
    PrintError('panic_cb captured error: '#13#10 + err);


 result := 0;
end;

function  LogMsg(L: lua_State): Integer; cdecl; // lua_ODS equivalent
var
   s, f: String;
   flags: DWORD;
begin
 result := 0;


 s := '';
 f := '';
 if lua_gettop(L) > 0 then s := LuaStrArg (L);
 if lua_gettop(L) > 1 then f := LuaStrArg (L, 2);

 flags :=  IfV ( f = '', 255, 0 );

 if (f <> '') and CharInSet ( f[1], ['$', '0'..'9'] ) then flags := atoi (f);

 if Pos ('f', f) > 0 then flags := flags or PMF_OUTFILE;
 if Pos ('c', f) > 0 then flags := flags or PMF_OUTCON;
 if Pos ('-', f) > 0 then flags := flags or PMF_NO_ODS;

 ODS (s, flags);
end;

function  SplitString(L: lua_State): Integer; cdecl;
var
   tmp: TStrings;
   src,
   sep: String;
   n: Integer;
begin
 tmp := TStringList.Create;
 try

  src := LuaStrArg (L, 1);
  sep := IfV ( lua_gettop(L) > 1, LuaStrArg(L, 2), ',' ) + ' ';

  tmp.Delimiter := sep [1];
  tmp.DelimitedText := src;


  result := 1;

  lua_newtable (L);

  for n := 0 to tmp.Count - 1 do
   begin
    lua_pushnumber (L, n + 1);
    lua_pushwstr (L, tmp [n]);
    lua_settable (L, -3);
   end;


 finally
  tmp.Free;
 end;

end; // StringSplit



function DMAlloc(ud, p: Pointer; osize, nsize: DWORD): Pointer; cdecl;
var
   fma: TFastMemAllocator;
begin
 result := nil;

 if ud <> nil then
  begin
   fma := ud;
   if nsize > 0 then
     begin
      result := fma.AllocMem (nsize);
      if (osize > 0) and (p <> nil) then
          SafeMove(p^, result^, Min (nsize, osize) );
     end
   else
     fma.FreeMem (p, osize);
   exit;
  end;


 if nsize = 0 then
    FreeMem (p)
 else
  begin
   if p = nil then
    begin
     p := AllocMem (nsize);
     // FillChar (p^, nsize, $00);
    end
   else
     ReAllocMem (p, nsize);
   result := p;
  end;
end; // DMAlloc


{ TLuaEngine }

procedure TLuaEngine.Assign(src: TPersistent);
begin
  inherited;
 prv_script_file := '';
end;

function TLuaEngine.AddPanic: Integer;
begin
 result := 0;
 lua_getglobal(State, 'panic_cb');
 if lua_iscfunction(State, -1) then
    result := lua_gettop(State)
 else
    lua_pop(State, 1);
end;


procedure TLuaEngine.OnPCallResult  ( const fname: String; err_idx, svtop: Integer );
var
   errc, s: String;
   err_var: TScriptVar;
    n, top: Integer;

begin
 if err_idx <> 0 then
  try
     errc := '#' + IntToStr(err_idx);
     case err_idx of
         LUA_ERRRUN: errc := 'Runtime Error';
        LUA_ERRFILE: errc := 'Error in file operation';
      LUA_ERRSYNTAX: errc := 'Syntax error';
         LUA_ERRMEM: errc := 'Memory allocation error';
         LUA_ERRERR: errc := 'Error in error-handler';
      LUA_ERRBAD_CC: errc := 'Closure not declared ' + fname;
     end;

     if err_idx < 10 then
      begin
       FillChar (err_var, sizeof(err_var), 0);
       err_var.vtype := lua_varType (FState, -1);
       lua_readVar( FState, -1, @err_var);
       lua_pop(FState, 1);

       s := '';
       case  err_var.vtype of
         SVT_STRING, SVT_USTRING:
             s := err_var.wstr;
         SVT_NUMBER:
             s := FormatFloat ('0.####', err_var.fval);
       end;

       s := s + FLastError;

       if s = '' then
          s := 'Unknown error, type = ' + IntToStr (err_var.vtype);

       LastError := '[' +ClassName + '.CallFunc[Ex]("' + fname + '")] ' +
                         'lua_pcall returned:~C0F '#13#10 + errc + ': '#13#10#9 + s;
      end
     else
      LastError := '[' +ClassName + '.CallFunc("' + fname + '")] detected error: ' + errc;

     if Assigned (OnScriptError) then
        OnScriptError (LastError);
 except
  on E: Exception do
     OnExceptLog ( ClassName + '.OnPCallResult(' + fname + ')', E);
 end;
 top := lua_gettop(State);
 if top <> svtop then
   begin
    s := '';
    for n := svtop + 1 to top do
        s := s + DumpValue ( State, n ) + '~C0B;~C0A';
    ODS( CFormat( '[~T].~C0C #WARN:~C07 lua_pcall for function %s changes stack from %d to %d, forgotten: %s', '~C07', [fname, svtop, top, s] ) );
    lua_settop (State, svtop);
   end;
end;

procedure TLuaEngine.CallFunc(const fname: String);
begin
 CallFuncArg (fname, nil);
end;

procedure TLuaEngine.CallFuncArg(const fname: String; argcb: TFuncParamCB);
var
   err_idx: Integer;
    pf_idx: Integer;
     svtop: Integer;
      pcnt: Integer;



begin
 FLastError := '';
 svtop := lua_gettop(State);
 VarSet('last_error', '');

 pf_idx := AddPanic;
 try
   lua_getglobal(State, fname);
   err_idx := 0;
   pcnt := 0;

   if lua_isnil (State, -1) then
     begin
      err_idx := LUA_ERRBAD_CC;
      lua_pop (State, 1);
     end
   else
     try
      if Assigned (argcb) then
           pcnt := argcb (self);

      err_idx := lua_pcall (State, pcnt, LUA_MULTRET, pf_idx);
     except
      on E: Exception do
         OnExceptLog ('lua_pcall at ' + LuaTraceBack(FState), E);
     end;
 finally
  RemovePanic ( pf_idx );
 end;

 OnPCallResult ( fname, err_idx, svtop );
end;

procedure TLuaEngine.CallFuncEx(const fname, in_params, out_params: String);
var
   n, err, cnti, cnto: Integer;
               pf_idx: Integer;
                svtop: Integer;
                 s, t: String;
begin
 cnti := 0;
 svtop := lua_gettop(State);
 VarSet('last_error', '');
 pf_idx := AddPanic;
 FLastError := '';
 try
   lua_getglobal(State, fname);
   s := in_params;
   Repeat
     t := Trim( StrTok(s, [',']) );
     if t = '' then continue;
     // если имеется глобальная переменная - её сохранить
     if VarList.FindVar(t) >= 0 then
        lua_getglobal (FState, t)
     else
        begin
         // сохранение в стек констант
         if IsQuotedStr(t) then
            lua_pushstring(State, PAnsiChar ( AnsiString(t) ) )
         else
         if IsNumber(t) then
            lua_pushnumber(State, atof(t));

        end;
     Inc (cnti);
   Until (s = '');

   sltmp.Split (',', out_params);  // куда запомнить ответы
   cnto := sltmp.Count;

   // protected-call
   err := lua_pcall (State, cnti, cnto, pf_idx);


   if err = 0 then
     for n := sltmp.Count - 1 downto 0 do
      begin
       s := Trim ( sltmp[n] );
       lua_setglobal ( FState, s );
      end;
 finally
  RemovePanic (pf_idx);
 end;
 OnPCallResult ( fname + '(' + in_params + '): ' + out_params, err, svtop );
end; // CalcFuncEx


procedure TLuaEngine.CloseState;
begin
 if State <> nil then
 try
  lua_gc (State, LUA_GCCOLLECT, 0);
  lua_close ( State );
 except
  on E: Exception do
     OnExceptLog( ClassName + '.CloseState ' + FormatPtr(FState), E );
 end;
 FState := nil;
end;



constructor TLuaEngine.Create;
begin
 FSupCodes := LUA_VERSIONS;
 inherited Create;
 FOnScriptError := DefErrorHandler;
 // varList.Engine := self;
 varList.AutoRegVars := TRUE;
 FAlloc := TFastMemAllocator.Create ( ClassName + '.FAlloc', FALSE, FALSE );
 FAlloc.dbg_flag := 1;
 PopResult := TRUE;
 Init;
end;

procedure TLuaEngine.DefErrorHandler(const s: String);
begin
 PrintError (s);
end;

destructor TLuaEngine.Destroy;
begin
 try
  if FState <> nil then lua_close(FState);
  SetLength (FJitBuff, 0);
  FAlloc.Free;
  inherited;
 except
  on E: Exception do
     OnExceptLog ('TLuaEngine.Destroy', E);
 end;
end;

procedure TLuaEngine.Execute;
var
    _script: AnsiString;
    ecb_idx: Integer;
    err_idx: Integer;
    err_var: TScriptVar;
    s, errc: String;
        why: String;
       lres: Integer;
         sa: AnsiString;

begin
 // загрузка и выполнение сценария
 Lock('Execute');
 try
   try
     LastError := '';
     _script := AnsiString (Text);
     VarSet('last_error', '');
     VarSet('EngineObject', self);

     FScriptUpdated := FALSE;

     lua_atpanic (FState, panic_cb);
     lua_pushcclosure (FState, panic_cb, 0);

     ecb_idx := lua_gettop (FState);
     why := 'luaL_loadstring';

     if JitCode then
       begin
        sa := AnsiString ( 'JIT:' + self.Name );
        why := 'luaL_loadbuffer';
        lres := luaL_loadbuffer (FState, PAnsiChar ( FJitBuff ), Length ( FJitBuff ),  PAnsiChar (sa) );
       end
     else
       begin
        sa := AnsiString ( prv_script_file );
        if sa = '' then
           sa := AnsiString ( Name + '@' + ClassName );
        lres := luaL_loadbuffer (FState, PAnsiChar ( _script ), Length (_script), PAnsiChar (sa) );
       end;

     if lres = 0 then
       begin
        // lua_dump (State, DumpByteCode, self) ;

        why := 'lua_pcall';
        err_idx := lua_pcall (FState, 0, LUA_MULTRET, ecb_idx);
       end
     else
        err_idx := lres;

     if ( err_idx = 0 ) then exit;

     errc := '#' + IntToStr(err_idx);
     case err_idx of
         LUA_ERRRUN: errc := 'Runtime Error';
        LUA_ERRFILE: errc := 'Error in file operation';
      LUA_ERRSYNTAX: errc := 'Syntax error';
         LUA_ERRMEM: errc := 'Memory allocation error';
         LUA_ERRERR: errc := 'Error in error-handler';
     end;

     s := '?in stack value type = ' + lua_typeof (State, -1);

     FillChar (err_var, sizeof(err_var), 0);
     err_var.vtype := lua_varType (FState, -1);

     lua_readVar (FState, -1, @err_var);


     lua_pop(FState, 1);

     case  err_var.vtype of
       SVT_STRING, SVT_USTRING:
           s := err_var.wstr;
       SVT_NUMBER:
           s := FormatFloat ('0.####', err_var.fval);
     end;


     LastError := LastError + #13#10 + '[' +ClassName + '.Execute()] ' + why +
                       ' returned error ' + errc + ': '#13#10#9 + s;

     if Pos('luaL_loadstring', why) > 0 then
        LastError := LastError + #13#10'Script dump: '#13#10 + Text;

     if Assigned (OnScriptError) then
        OnScriptError (LastError);

   except
    on E: Exception do
       PrintError (ClassName + '.Execute Exception catched, message: ' + E.Message);
   end;

  finally
   Unlock;
  end;

end;

function lua_ODS(L: lua_State): Integer; cdecl;
var
    argc, flags: NativeUInt;
    s: String;
begin
 result := 0;
 flags := 255;
 argc := lua_gettop(L);
 if argc = 0 then exit;

 if lua_isstring(L, LUA_ARG1 ) then
   begin
    s := lua_topstring (L, LUA_ARG1);
    if (argc >= 2) and ( lua_isnumber(L, LUA_ARG2) ) then
        flags := Trunc ( lua_tonumber(L, LUA_ARG2) );
    ODS ( s, flags );
   end;
end; // lua_ODS

procedure TLuaEngine.Init;
var
   code: AnsiString;
      n: Integer;
begin
 if FState <> nil then
    CloseState;

 inherited Init;
 ODS('[~T].~C09 #REINIT_ENGINE:~C07 ' + Name + '@' + QualifiedClassName );

 FAlloc.Clear;

 FState := lua_newstate (DMAlloc, nil);
 Assert ( Assigned (FState), ClassName + '.Init self.FState is unassigned ');
 luaL_openlibs (FState);

 // luaopen_os (FState);
 // luaopen_loadlib (FState);

 code := AnsiString ( 'package.path="' + AnsiReplaceStr ( gScriptsPath, '\', '\\' ) + '?.lua"' );
 if lua_dostring ( State, PAnsiChar (code) ) <> 0 then
   begin
    PrintError ( 'error executing init code ' + LuaStrArg( State, -1 ) );
    lua_pop ( State, 1 );
   end;


 // lua_settop (FState, 0);


 VarSet('lua_e', self);
 RegFunc ('ODS', @lua_ODS);

 LuaTools.ExportAll (State);

 RegFunc ('panic_cb', @panic_cb);

 for n := 0 to VarList.Count - 1 do
     WriteVar ( @VarList.Items [n] );
end;

procedure TLuaEngine.LoadBuffer(src: PAnsiChar; cb: Integer);

begin
 FJitCode := ( cb > 4 ) and ( PDWORD ( src )^ = $014A4C1B );
 if FJitCode then
  begin
   SetLength ( FJitBuff, cb );
   SafeMove ( src^, FJitBuff[0], cb );
  end
 else
  begin
   SetLength ( FJitBuff, 0 );
   Text := AnsiTrim2W ( src );
  end;

 FScriptUpdated := TRUE;
end;

procedure TLuaEngine.LoadFromFile(const FileName: string);
var
  dump: String;
    cb: Integer;
    rb: Integer;
    fs: TFileStream;
     f: File;
begin
 dump := ReadBytesDump (FileName, 4);
 FJitCode :=  ( dump = '1B 4C 4A 01' );
 if FJitCode then
   begin

    AssignFile (f, FileName);
    {$I-}
    Reset (f, 1);
    Assert ( IOresult = 0, 'Error opening file for read ' + FileName );

    cb := FileSize (f);
    if cb > 0 then
     begin
      SetLength ( FJitBuff, cb );
      BlockRead ( f, FJitBuff[0], cb, rb );
      Assert ( cb = rb, Format ('From file %s readed %d bytes from %d', [FileName, rb, cb] ) );
     end;
    CloseFile (f);
   end
 else
   try
    fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    LoadFromStream(fs);
    fs.Free;
   except
    on E: Exception do
       PrintError('TFileStream.Create ("' + FileName + '") failed with exception:' + E.Message);
   end;



 FScriptUpdated := TRUE;
end;

function TLuaEngine.LoadScript(const sFileName: String; check_time: Boolean): Boolean;
var
   ftime: TDateTime;
begin
 result := FALSE; // not loaded
 if not FileExists ( sFileName ) then exit;

 if ( sFileName = prv_script_file ) and ( not check_time ) then exit;

 ftime := FileWriteTime ( sFileName );

 if ( sFileName = prv_script_file ) and ( check_time ) then
   begin
    if ftime = prv_script_time then exit;
   end;


 prv_script_file := sFileName;
 prv_script_time := ftime;

 LoadFromFile (sFileName);
 // Insert (0, 'collectgarbage(256)');
 result := Count > 0;
end;

procedure TLuaEngine.NewTable(const sName: String);
var
   psv: PScriptVar;
begin
 psv := VarList.AddVar (sName, SVT_TABLE );
 RegVar (psv, TRUE);
end;

function TLuaEngine.RegFunc(const fname: String; pfunc: Pointer): Boolean;
begin
 lua_register (FState, PAnsiChar (AnsiString(fname)), lua_CFunction (pfunc));
 if log_verbose > 9 then
    ODS( CFormat('[~T]. #DBG: for lua_State $%p registered function %-20s = $%p ', '~C07', [FState, fname, pfunc] ) );

 result := TRUE;
end;

procedure TLuaEngine.RegVar(sv: PScriptVar; bRegister: Boolean);
begin
 // typical set default
 if bRegister then
   begin
    if sv.vtype and SVT_COMPLEX <> 0 then
      begin
       lua_newtable(FState);
       lua_setglobal(FState, sv.vname);
       exit;
      end
    else
      exit;
    WriteVar (sv);
   end;
end;

procedure TLuaEngine.RemovePanic(idx: Integer);
begin
 if ( idx > 0 ) and ( lua_type ( State, idx ) = LUA_TFUNCTION ) then lua_remove ( State, idx );
end;

procedure TLuaEngine.SetGlobal(const sName: String);
begin
 lua_setglobal (State, sName);
end;

procedure TLuaEngine.SetGlobal(const sName: String; exp_method: TLuaMethod);
begin
 exp_method (State);
 SetGlobal (sName);
end;

procedure TLuaEngine.SetGlobal(const sName: String; exp_proc: TLuaProc);
begin
 exp_proc (State);
 SetGlobal (sName);
end;

function TLuaEngine.VarType(const sName: String): DWORD;
begin
 lua_getglobal (State, sName);
 result := lua_varType (State, -1);
 lua_pop (State, 1);
end;

procedure lua_readVar(L: lua_State; index: Integer; psv: PScriptVar);
var pastr: PAnsiChar;
       rl: Cardinal;
begin
 case psv.vtype and $FF of
  SVT_INTEGER: psv.ival := lua_tointeger (L, index);
    SVT_FLOAT: psv.fval := lua_tonumber (L, index);
  SVT_POINTER: psv.pval := lua_topointer(L, index);
   SVT_STRING, SVT_USTRING:
      begin
       pastr := lua_tolstring (L, index, rl);
       if pastr <> nil then
         begin
          SetString (psv.astr, pastr, rl);
          psv.wstr := WideString (psv.astr);
         end;
      end;
  SVT_BOOLEAN: psv.bval := lua_toboolean (L, index);
 end;

end; // lua_readVar

function lua_varType (L: lua_State; index: Integer): Cardinal;
var lt: Integer;
begin
 result := 0;
 lt := lua_type (L, index);
 if lt = LUA_TNONE then exit;
 case lt of
      LUA_TNIL: result := SVT_NIL;
   LUA_TNUMBER: result := SVT_NUMBER;
  LUA_TBOOLEAN: result := SVT_BOOLEAN;
   LUA_TSTRING: result := SVT_STRING;
    LUA_TTABLE: result := SVT_TABLE;

  LUA_TFUNCTION,
  LUA_TUSERDATA,
  LUA_TTHREAD,
  LUA_TLIGHTUSERDATA: result := SVT_POINTER;

 end;
end; // lua_varType

procedure lua_getComplex(L: lua_State; index: Integer; root: TScriptVarComplex; psv: PScriptVar);
var sva: TScriptVarArray;
begin
 psv.vtab := TScriptVarTable.Create;
 root.Collector.Add (psv.vtab);
 lua_readTable (L, index, psv.vtab);
 if psv.vtab.IsArray(1) then
  begin
   sva := psv.vtab.ToArray;
   root.Collector.Add(sva);
   root.Collector.Remove (psv.vtab);
   psv.varr := sva;
   psv.vtype := SVT_ARRAY;
  end;


end; // lua_getComplex

procedure lua_readArray (L: lua_State; index: Integer; aobj: TScriptVarArray);
var n, cnt: Integer;
    psva: PSVarsArray;
    psv: PScriptVar;
begin
 aobj.Clear;    // Включает уничтожение объектов, ссылки устаревают!!!
 cnt := lua_objlen (L, index);
 if cnt = 0 then exit;
 psva := aobj.AddRows (cnt);
 FillChar (psva^, sizeof(TScriptVar) * cnt, 0);

 for n := 1 to cnt do
  begin
   psv := @psva [n - 1];
   lua_rawgeti(L, index, n);            // извлечение значения в стек
   psv.vtype := lua_varType (L, -1);
   case psv.vtype of
    SVT_TABLE:
       lua_getComplex (L, lua_gettop(L), aobj.Root, psv);
    else
      lua_readVar (L, -1, psv);
   end;
   lua_pop (L, 1);

  end;

end; // lua_readArray

procedure lua_readTable (L: lua_State; index: Integer; aobj: TScriptVarTable);
const
   LUA_KEY = -2;
   LUA_VAL = -1;
var pcnt, prvtop: Integer;
    psp: PScriptVarPair;
begin
 aobj.Clear;    // Включает уничтожение объектов, ссылки устаревают!!!


 pcnt := 0;
 prvtop := lua_gettop(L);
 lua_pushnil(L); // указать ключ, который индексирует нулевую пару массива

 //  for n := 1 to cnt do
 while true do
  begin
   if lua_next (L, index) = 0 then break;
   Inc (pcnt);
   psp := aobj.AddRows(1);
   FillChar(psp^, sizeof(TScriptVarPair), 0);
   // psp := @psva [n - 1];
   // считывание одной пары
   with psp^ do
    begin
     key.vtype := lua_varType (L, LUA_KEY);
     if key.vtype <> 0 then
        lua_readVar (L, LUA_KEY, @key);
     value.vtype := lua_varType (L, LUA_VAL);
     if value.vtype = SVT_TABLE then
        lua_getComplex (L, lua_gettop(L), aobj.Root, @value)
     else
      if value.vtype <> 0 then
         lua_readVar (L, LUA_VAL, @value);
      lua_pop (L, 1); // убрать только значение, но оставить ключ!
    end; // whith psp^
   if psp = nil then exit;// for watches
   // сборка мусора
   //   while (prvtop + 1 < lua_gettop(L)) do lua_pop(L, 1);
  end; // for n


  aobj.CorrectCount (pcnt);
  aobj.UpdateMap;

  // подчистить стек - убрать ключи и т.п.
  while (prvtop < lua_gettop(L)) do lua_pop(L, 1);

end; // lua_readArray


function lua_pushVar (L: lua_State; sv: PScriptVar): Boolean;
begin
 result := TRUE;

 case sv.vtype and $FF of
  SVT_INTEGER: lua_pushinteger (L, sv.ival); // integer
    SVT_FLOAT: lua_pushnumber  (L, sv.fval);
  SVT_BOOLEAN: lua_pushboolean (L, Integer(sv.bval) );
  SVT_POINTER: lua_pushobj     (L, sv.pval);
   SVT_STRING, SVT_USTRING:
     begin
      if ( sv.vtype and $0F = SVT_USTRING ) then  sv.astr := AnsiString (sv.wstr);
      lua_pushstring (L, PAnsiChar(sv.astr) );
     end;
  else result := FALSE;
 end;
end; // lua_pushVar


function TLuaEngine.ReadArray(const sName: String; idx: Integer = 0): TScriptVarArray;
var
   psv: PScriptVar;

begin
 if sName <> '' then
  begin
   psv := VarList.AddVar (sName, SVT_ARRAY);
   lua_getglobal (FState, sName);
   idx := lua_gettop (FState);
   result := psv.varr;
  end
 else
   result := TScriptVarArray.Create(TRUE); // вызывающий отвечает за удаление этого объекта!

 if lua_istable (FState, idx) then
    lua_readArray (FState, idx, result);

end;// ReadArray

function TLuaEngine.ReadTable(const sName: String; idx: Integer): TScriptVarTable;
var
   psv: PScriptVar;
begin
 if sName <> '' then
  begin
   psv := VarList.AddVar (sName, SVT_TABLE);
   lua_getglobal (FState, sName);
   idx := lua_gettop (FState);
   result := psv.vtab;
  end
 else
   result := TScriptVarTable.Create(TRUE); // вызывающий отвечает за удаление этого объекта!

 if lua_istable (FState, idx) then
    lua_readTable (FState, idx, result);
end;// ReadTable



procedure TLuaEngine.ReadVar(sv: PScriptVar);
begin
 // составные типы не читаются напрямую
 if (sv.vtype and SVT_COMPLEX <> 0) then exit;
 if sv.vtype = SVT_AUTO then
    sv.vtype := VarType ( sv.vname );

 lua_getglobal (FState, sv.vname );
 lua_readVar (FState, -1, sv);
 if PopResult then
    lua_pop(FState, 1);
end; // ReadVar


procedure TLuaEngine.CheckRegVar;
var
   psv: PScriptVar;
begin
 psv := VarList.AddVar (sName, vt);

 lua_getglobal (State, sName);

 if lua_isnil (State, -1) then
   begin
    lua_pop (State, 1);
    RegVar (psv);
    lua_getglobal (State, sName);
   end;

 Assert (lua_istable (State, -1), 'for global "' + sName + '" returned non-table value, type = ' + IntToStr ( lua_type (FState, -1) ));


 if not push then
    lua_pop (State, 1);

end;

procedure TLuaEngine.CheckStack (const ctx: String; norm: Integer);
var
   top: Integer;
begin
 top := lua_gettop (State);
 if top > norm then
     PrintError('lua stack is used, top = ' + IntToStr(top) + ', ctx =~C0F ' + ctx );
end;

procedure TLuaEngine.WriteArray(const sName: String; varr: TScriptVarArray; idx: Integer);
var
   top: Integer;
     n: Integer;
begin
 top := lua_gettop (State);

 if sName <> '' then
  begin
   CheckRegVar ( sName, SVT_ARRAY, TRUE );
   idx := lua_gettop (State);
  end;

 // setting items
 if (idx > 0) and (lua_istable(FState, idx)) then
     for n := 0 to varr.Count - 1 do
     if varr.Items[n].vtype and SVT_COMPLEX = 0 then
        begin
         lua_pushnumber (State, n + 1);

         if lua_pushVar (State, @varr.Items [n]) then
            lua_settable (State, idx)
         else
            lua_pop(State, 1); // remove index
        end;


 lua_settop (State, top);
end; // WriteArray

procedure TLuaEngine.WriteTable(const sName: String; varr: TScriptVarTable; idx: Integer);
var
   top: Integer;
     n: Integer;
begin
 top := lua_gettop (State);

 if sName <> '' then
  begin
   CheckRegVar ( sName, SVT_TABLE, TRUE );
   idx := lua_gettop (State);
  end;

 // setting items
 if (idx > 0) and (lua_istable(FState, idx)) then
     for n := 0 to varr.Count - 1 do
     with varr.Pairs[n] do
     if ( key.vtype or value.vtype ) and SVT_COMPLEX = 0 then
        begin
         lua_pushVar (State, @key);

         if lua_pushVar (State, @value) then
            lua_settable (State, idx)
         else
            lua_pop(State, 1); // remove index
        end;


 lua_settop (State, top);
end; // WriteTable



procedure TLuaEngine.WriteVar(sv: PScriptVar);
begin
 if (sv.vtype and SVT_COMPLEX <> 0) then exit;
 // lua_checkstack (FState, 2048);
 // with lua_PStateRec (FState)^ do stack := base;

 if lua_pushVar (FState, sv) then
    lua_setglobal(FState, sv.vname )
 else
    PrintError ('lua_pushVar unsuccessful for ' + sv.vname);
end; // WriteVar

initialization
 gScriptsPath := CorrectFilePath ( ExePath + '..\scripts\');
end.
