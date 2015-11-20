unit SegMath;

interface
uses Windows, SysUtils, Classes, Misc, StrClasses, ChartData, UNIArray, Math, WThreads, DateTimeTools, UTITypes, ArrayTypes,
     UTIConst, InstrDictionary, PackedStorage, FinMath, SMTypes, Scripting, Lua5, LuaEngine, LuaTypes, LuaTools;

{ ������ ���������� ���������������� ����������� ����������.
  ������������ ������������ ������ �������, ��� ������� ����������
  ������������� ��� �������� ���������������� ����������.

  ����������� ����� �������� �������, ����� ����������� ��������
  ������� ����������, � ��������� � ����������� �� ���������� ������
  �� ������������ ��� ��������������� ���������.


}



     (* ���������� ������ LUA-script ��� ������������ ������������ ���������, �� ������� DLL � ������������.
      �������������: ������������ ������ ���������� � ��������� ����������, �� ��������
       ��������� ��������. � ����������� ������������� ���� ������ ����������� ����������.


      �������� Lua �������� ��������� - ��� ������� ����� ����������� ��� ������� �������� � ��������� ����� �����:



       vRSI = MakeData ("RSI");
       vSMA = MakeData ("SMA");
       vSIG = MakeData ("SIG");
       vSIGPOS = MakeData ("SIGPOS");
       nPeriods = Params [0];   // ������ ��������� ���������� �������������� ��� ������� �������, ������ � Movings, MovAlgs
       nSmooth = Params [1];    // � ������������� ����� ��������� ���������

       SetCallback ("calcJob", "calcJob");


       function calcJob ()
       {

           // ������� ����������, � ��������� �������/�������� ������ � ������ ����������
           CalcInd ( "RSI", { CLOSE }, { vRSI }, { nPeriods } );
           CalcInd ( "SMA", { vRSI }, { vSMA }, { nSmooth } );
           CalcInd ( "CROSS", { vRSI, vSMA }, { vSIG } );
           CalcInd ( "DSP", { vSIG }, { vSIGPOS }, { 1 } );  // ������ ���������������

        }

     *)


const
   DEF_SINGLE_PARAMS = 'Account,Active,AntiSystem,Comission,CurrentPos,GroupId,Instrument,' +
                       'Money,OptFlags,PositionLimit,Slippage,TradeVolume,UnitSize';


    // -------------------------------------------------------------------------------------------------------------------------------------- //
type

    TSMFuncDesc = class
    private
     function CreateParams: PSMFuncParams;    // �������� �������, ���������� ��������� ��� ���������� ���������.
    protected
     FOwner: TStrMap;

    public

     call_num: Integer;
     lib_name: String;
     the_func: TSMFunction;
     the_parlist: array [0..63] of PSMFuncParams; // ��� ���������������� ������� ������ �������


     { C & D }
     constructor        Create (AFunc: TSMFunction; AOwner: TStrMap; aflags: DWORD = 0);
     destructor         Destroy; override;

     { methods }
     procedure          LoadParams (the_params: PSMFuncParams; lua_e: TLuaEngine; idx: Integer);
     procedure          ReleaseMemory;

    end;

    // ����������� ������� ���������������� ����������
    // ����� ��������� ��� ����� ������ �������, ��� � ������������ (��� ���������) ������ � �����������/�����������.

    TSMCalctimeData = record
     calc_range: TIndexRange;
     n_segment: Integer;
    end; // TSMCalctimeData

    TSMFuncList = class (TStrMap)
    private
     FLocal: Boolean;

     function GetDesc(index: Integer): TSMFuncDesc;  // ����������� ���������

    public
     { props }
     property           Local: Boolean read FLocal;
     property           DescList[index: Integer]: TSMFuncDesc read GetDesc;

     { C & D }
     constructor        Create (bLocal: Boolean);

     { methods }
     function           AddDesc (const sFuncName, sModule: String; afunc: TSMFunction): TSMFuncDesc;
     function           FindDesc (const sFuncName: String): TSMFuncDesc;
    end; // TSMFuncList

    // ����� ����������� ���������� ����������� ��������
    TSMDataCalcJob = class (TBaseCalcJob)
    private
     function  MapDataList(dmap: TDataMap; resv_count, n_last, calc_bars: Integer): Integer;
     procedure UpdateRanges(dmap: TDataMap);

    protected
     _lua_e: TLuaEngine;
     _funcList: TSMFuncList;  // ������ �������, ���������� � ���������� ������� ����������
     FScriptFile: String;
     FSegmentSize: DWORD;

     s_data: TDataMap;             // ������ ������������������ ��������
     l_data: array of TSMDataRec;  // ��� ������������ ���������� data-���������� ��������
     l_dcnt: Integer;
     l_dsize: Integer;

     i_data: TInstrDataList; // ������ �� ������������

     min_items: Integer;
     work_range: Integer;
     stg_params: TStrMap;
     data_origin: Integer;
     init_func: String;
     FOwner: TObject;


     function           GetDataRec(const sName: String): PSMDataRec;
    public

     CTMData: TSMCalctimeData;

     { props }
     property   lua_e: TLuaEngine read _lua_e;
     property   funcList: TSMFuncList read _funcList;
     property   InitFunc: String read init_func write init_func;
     property   ScriptFile: String read FScriptFile write FScriptFile;
     property   SegmentSize: DWORD read FSegmentSize write FSegmentSize;
     property   StgParams: TStrMap read stg_params;


     { C & D }
     constructor        Create (AOwner: TObject);
     destructor         Destroy; override;

     { Methods }


     function                  Calculate (InData, OutData: TDataMap; resv_count, work_count, nFirst, nLast: Integer): Integer; override;


     procedure                 InitJob;
     procedure                 PushParams (varname, parlist: String; vtype: DWORD);
     procedure                 RegWorkGlobals;


    end; // TSMDataCalcJob


function GlobalSMFuncList: TSMFuncList;


implementation
uses FinCharts, Frames, Indicators, TradeBase;


var
   gSMFuncList: TSMFuncList;


function ctxAlloc ( dwBytes: DWORD; pp: Pointer ): Pointer; stdcall;
begin
 if pp <> nil then
  begin
   ReallocMem (pp, dwBytes);
   result := pp;
  end
 else
  GetMem (result, dwBytes);
end; // ctxAlloc

function RefArrayPtr (p: Pointer; ofs: Integer; item_size: Integer = 4): Pointer; inline;
begin
 result := Ptr ( Integer(p) + ofs * item_size );
end;


procedure ApplyOffsets (the_params: PSMFuncParams);
var n, o: Integer;
    pr: PSMDataRec;
begin
 // TODO: ���������� �����������, ��� �������� �������� �� ��������� ������, �.�. �������� ��������� �� ������� ������� ������

 // ���������� ��������
 for n := 0 to High (the_params.in_data) do
  begin
   { ������������� �������� ������� ������, �������������� ��������� �������.
     |-------ORG-^--------------------|
     ����� ������� ������� i, ���������� ��� i + ofs, � ���������� ��������� ����� �� ������� src.iLast
     ==================================================================================================
     ������������� �������� ������� ������, �������������� ��������� �������.
     |-----^-ORG----------------------|
     ��� ���� ������� i ���������� ��� i - ofs.
   }
   pr := the_params.in_data [n];
   if (pr <> nil) and (pr.lporig <> nil) then
     begin
      o := the_params.offsets._in [n];
      pr.fdata := RefArrayPtr ( pr.lporig, o );
      if o > 0 then
         Dec (pr.ValidRange.iLast, o); // ���������� ������ �� ������ �������

      the_params.applied._in [n] := o;
     end;

   // ==================================================================================================
   // �������� �������� (��� �����������)
   pr := the_params.out_data [n];

   if (pr <> nil) and (pr.lporig <> nil) then
     begin
      o := the_params.offsets._out [n];
      if o <> 0 then
         pr.fdata := RefArrayPtr ( pr.lporig, o );
      the_params.applied._out [n] := o;
     end;

  end;


end;

procedure ResetOffsets (the_params: PSMFuncParams);
var n, o: Integer;
    pr: PSMDataRec;

begin
 for n := 0 to High (the_params.in_data) do
  begin
   pr := the_params.in_data [n];
   if pr <> nil then
    begin
     pr.fdata := pr.lporig;
     o := the_params.offsets._in [n];
     if o > 0 then
        Inc (pr.ValidRange.iLast, o); // ������������ �������� ��� ����������� ��������
    end;

   pr := the_params.out_data [n];
   if pr <> nil then
    begin
     pr.fdata := pr.lporig;
     the_params.applied._in [n] := 0;
     the_params.applied._out [n] := 0;
    end;

  end; // for

end;

// ������� ������� - �������� ������������������ �������.
function lua_CalcInd (L: lua_State): Integer; cdecl;
var lua_e: TLuaEngine;
    fclist: TSMFuncList;
    dcj: TSMDataCalcJob;
    argc: Integer; //
    s: String;
    fdesc: TSMFuncDesc;
    the_params: PSMFuncParams;

begin
 {
    1. ��������� ���������� �� LUA
    2. ��������� �������, � � ��������������� ���������� c ����������
    3. ��� �������� �������� - ������������� ��������� � ����������

 }

 result := 0; // no results
 // ��������� ������� ������
 lua_e := global_getpointer (L, 'lua_e');
 dcj := global_getpointer (L, 'g_calc_job');
 if Assigned (lua_e) and Assigned (dcj) then else exit;

 fclist := dcj.funcList;

 argc := lua_gettop (L);
 if argc <= 0 then exit;                // ����� ��� ������� �������� �������
 s := lua_topstring(L, LUA_ARG1);
 fdesc := fclist.FindDesc (s);
 if not Assigned (fdesc) then exit;

 with dcj.CTMData, fdesc do
  begin
    the_params := the_parlist [call_num];
    if the_params = nil then
      begin
       the_params := CreateParams;
       the_parlist [call_num] := the_params;
      end;

    the_params.iFirst := calc_range.iFirst;
    the_params.iLast :=  calc_range.iLast;
    the_params.n_segment := n_segment;
    the_params.stg_flags := ( dcj.stg_params.IntValues ['OptFlags'] );

    // --------------------------------------------------------------

    if ( the_params.n_segment = 0 ) and ( argc > 1 ) then
       begin
        LoadParams (the_params, lua_e, 2); // ��������� ��������� �� ���������� �������.
        the_params.instr_data := dcj.i_data;

       end;

    Assert (Assigned (the_func), 'fdesc.the_func unassigned!');

    ApplyOffsets ( the_params );

    the_func ( the_params);

    ResetOffsets ( the_params );

    Inc (call_num);         // ����� ������� ��������
  end; // with

end;  // lua_CalcInd

function lua_RegData (L: lua_State): Integer; cdecl;
// ����������� ��������� ������-��������
var
   dcj: TSMDataCalcJob;
   argc: Integer;
   sltmp: TStrMap;
   in_data, out_data: TDataMap;
   s: String;
   n: Integer;
begin
 result := 0;
 dcj := global_getpointer (L, 'g_calc_job');
 argc := lua_gettop (L);  // conceptual not supported nested calls?
 if Assigned (dcj) and (argc > 0) then else exit;
 in_data :=  global_getpointer (L, 'g_in_data');
 out_data := global_getpointer (L, 'g_out_data');
 if (in_data = nil) or (out_data = nil) then exit;

 sltmp := TStrMap.Create;
 try
  s := lua_topstring (L, LUA_ARG1);
  sltmp.Split (',', s);
  {ODS('[~T]. #SDBG: Registering data from script: ~C0A' + s + '~C07 argc = ' + IntToStr(argc));
  for n := 1 to argc do
     ODS ('arg #' + IntToStr(n) + ' = ' + lua_topstring(L, n));}
  for n := 0 to sltmp.Count - 1 do
     begin
      s := Trim ( sltmp[n] );
      if (in_data.IndexOf(s) < 0) and (out_data.IndexOf(s) < 0) then
          dcj.s_data.AddArray32 ( s );
     end;
 finally
  sltmp.Free;
 end;
end; // lua_RegData

function GetArrayValuePtr(pdtr: PSMDataRec; idx: Integer): PSingle;
begin
 result := nil;
 if idx < 0 then idx := pdtr.ValidRange.iLast + idx + 1;
 if (pdtr.ValidRange.iFirst <= idx) and (idx <= pdtr.ValidRange.iLast) then
          result := @pdtr.fdata [idx];
end; // GetArrayValuePtr

function lua_GetArrayValue (L: lua_State): Integer; cdecl;
var
   argc: Integer;
   pdtr: PSMDataRec;
   pr: PSingle;
   fResult: lua_Number;
   idx: Integer;
begin
 result := 1;
 fResult := 0.0;
 argc := lua_gettop (L);
 idx := -1;
 if argc >= 1 then

  repeat
   pdtr := lua_topointer (L, LUA_ARG1);
   if pdtr = nil then
    begin
     PrintError('GetArrayValue: array-ptr not assigned = nil!');
     break;
    end;
   if argc > 1 then
      idx := Round ( lua_tonumber(L, LUA_ARG2) );

   if (pdtr.wType = SDR_FLOAT_ARRAY) then
    begin
     pr := GetArrayValuePtr (pdtr, idx);
     if pr <> nil then fResult := pr^;
    end;

  until TRUE;

 lua_pushnumber(L, fResult);
end;



{ TSMFuncDesc }

constructor TSMFuncDesc.Create(AFunc: TSMFunction; AOwner: TStrMap; aflags: DWORD);
begin
 FOwner := Aowner;
 the_func := AFunc;
end;


destructor TSMFuncDesc.Destroy;
begin
 ReleaseMemory;
 inherited;
end;

function  TSMFuncDesc.CreateParams: PSMFuncParams;
begin
 GetMem (result, sizeof (TSMFuncParams));
 FillChar (result^, sizeof(TSMFuncParams), 0);
 result.dwVersion := SMLIB_VERSION;
 result.dwSize :=    sizeof (TSMFuncParams);
 result.pfcAlloc :=  ctxAlloc;
 result.AOwner := self;
end; // CreateParams

function sv_array(psv: PScriptVar): TScriptVarArray;
begin
 result := nil;
 if (psv <> nil) and (psv.vtype = SVT_ARRAY) then result := psv.varr;
end; // sv_get_array

procedure TSMFuncDesc.LoadParams(the_params: PSMFuncParams; lua_e: TLuaEngine; idx: Integer);
var
   vtab: TScriptVarTable;
   in_data, out_data, params: TScriptVarArray;
   n: Integer;

begin
 // 1. �������� ������� �������� ����������. =======================

 vtab := lua_e.ReadTable ('', idx);
 try
  if vtab.Count = 0 then exit;
  // ���������� ������� ����������
  in_data := sv_array ( vtab.Values['in_data'] );
  out_data := sv_array ( vtab.Values['out_data'] );
  if (in_data = nil) then exit;
  FillChar (the_params.in_data, sizeof (the_params.in_data), 0);
  FillChar (the_params.out_data, sizeof (the_params.out_data), 0);
  FillChar (the_params.i_params, sizeof (the_params.i_params), 0);
  FillChar (the_params.f_params, sizeof (the_params.f_params), 0);
  if Assigned (the_params.pContext) then FreeMem (the_params.pContext);
  the_params.pContext := nil;

  // TODO: � ���� ����� ������ ���������� �������� � ����������
  // ����������� ������ �� �������
  for n := 0 to in_data.Count - 1 do
         the_params.in_data[n] :=  in_data.Items[n].pval; // link to first row
  if Assigned(out_data) then
     for n := 0 to out_data.Count - 1 do
         the_params.out_data[n] := out_data.Items[n].pval;
  // ��������� const-���������� ����������
  params := sv_array ( vtab.values['i_params'] );
  if Assigned(params) then
     for n := 0 to params.Count - 1 do
         the_params.i_params[n] := Round ( params.Items[n].fval );

  params := sv_array ( vtab.values['f_params'] );
  if Assigned(params) then
     for n := 0 to params.Count - 1 do
         the_params.f_params[n] := params.Items[n].fval;

  // ��������� �������� �������� ������
  params := sv_array ( vtab.values['in_offsets'] );
  if Assigned(params) then
     for n := 0 to params.Count - 1 do
         the_params.offsets._in [n] := Round ( params.Items[n].fval );

  // ��������� �������� ������ ������
  params := sv_array ( vtab.values['out_offsets'] );
  if Assigned(params) then
     for n := 0 to params.Count - 1 do
         the_params.offsets._out [n] := Round ( params.Items[n].fval );

  // permanent call = the_func (the_params);

 finally
  vtab.Free;
 end;
end; // LoadParams

procedure TSMFuncDesc.ReleaseMemory;
var n: Integer;
begin
 for n := 0 to High (the_parlist) do
 if Assigned ( the_parlist [n] ) then
 with the_parlist [n]^ do
   begin
    if Assigned(pContext) then FreeMem (pContext, ctx_size );
    FreeMem ( the_parlist [n] );
    the_parlist [n] := nil;
   end;

end;



{ TSMFuncList }

constructor TSMFuncList.Create(bLocal: Boolean);
var n: Integer;
    fd: TSMFuncDesc;
begin
 FLocal := bLocal;
 inherited Create();
 Sorted := TRUE;
 CaseSensitive := FALSE;
 OwnsObjects := TRUE;
 // ����������� ���� ��������� �������
 if bLocal and Assigned(gSMFuncList) then
   for n := 0 to gSMFuncList.Count - 1 do
   begin
    fd := gSMFuncList.Objects [n] as TSMFuncDesc;
    self.AddDesc (gSMFuncList[n], 'TradeSystem', fd.the_func);
   end;

end; // Create

function TSMFuncList.FindDesc(const sFuncName: String): TSMFuncDesc;
var
   n: Integer;
   fd: TSMFuncDesc;
   fn, lib: String;
begin
 result := nil;
 fn := sFuncName;
 lib := '';
 if Pos('.', fn) > 0 then
    lib := StrTok (fn, ['.']);
 for n := 0 to Count - 1 do
  begin
   fd := TSMFuncDesc (Objects [n]);
   if (sFuncName = Strings[n]) and ( (lib = fd.lib_name ) or (lib = '') ) then else continue;
   result := fd;
   exit;
  end;
end;  // FindDesc

function TSMFuncList.GetDesc(index: Integer): TSMFuncDesc;
begin
 result := TSMFuncDesc ( Objects [index] );
end;

function TSMFuncList.AddDesc(const sFuncName, sModule: String; afunc: TSMFunction): TSMFuncDesc;
var
   flags: DWORD;
   s: String;

begin
 result := FindDesc (sFuncName);
 if result <> nil then exit;
 flags := 0;
 if Local then flags := flags or SMF_HASPARAMS;
 result := TSMFuncDesc.Create(AFunc, self, flags);
 s := sModule;
 result.lib_name := s;
 if Pos('.', s) > 0 then
    result.lib_name := StrTok (s, ['.']); // remove extension
 AddObject (sFuncName, result);
end;

{ TSMDataCalcJob }
procedure TSMDataCalcJob.UpdateRanges ( dmap: TDataMap );
var
   data: TMatrixArray;
   a32: TArray32;
   s: String;
   n, i: Integer;
   pdtr: PSMDataRec;

begin
 for n := 0 to dmap.Count - 1 do
   begin
    data := dmap.ArrayByIndex (n);
    s := dmap.Strings [n];
    if data is TDateMap then continue;
    pdtr := GetDataRec (s);
    data.iFirstValid := pdtr.ValidRange.iFirst + pdtr.nOrigin;
    data.iLastValid :=  pdtr.ValidRange.iLast + pdtr.nOrigin;
    {$IFOPT D+}
    if not (data is TArray32) then continue;
    a32 := TArray32 (data);
    for i := 0 to a32.iFirstValid - 1 do a32.FloatItems [i] := 65536;
    for i := a32.iLastValid + 1 to a32.Count - 1 do a32.FloatItems [i] := 65536;
    {$ENDIF}
   end;

end; // UpdateRanges


function TSMDataCalcJob.MapDataList ( dmap: TDataMap; resv_count, n_last, calc_bars: Integer ): Integer;
var n, i: Integer;
    need_data, view_bars: Integer;

    pdtr: PSMDataRec;
    pdta: Pointer;
    data: TMatrixArray;
    right_bar: Integer;
    s: String;

begin
 result := 1;

 view_bars := calc_bars - resv_count;
 // shift := resv_count - resv_half; // if half = 255, shift = 256!


 for n := 0 to dmap.Count - 1 do
  begin
   s := dmap.Strings [n];
   if dmap.Objects [n] = nil then continue;
   data := dmap.ArrayByIndex (n);
   // ��� ����������� ������ �� ��������������?
   // if data.indexList[5] > 0 then continue;
   // Inc (data.indexList [5]);

   pdtr := GetDataRec (s);
   data.iFirstValid := data_origin;

   if data is TDateMap then // ������ ����� ���� ������ ���������� ���������
    begin
     pdtr.lporig := data;
     pdtr.nCount := data.Count;
     pdtr.wType := SDR_DATRNG_ARRAY;
    end
   else
    begin
      // ��������� ��������������� ����� �� ��������
      // CHECKIT: ��� ������� �� ������ �������� ���������� ������ ���������� �������!
      need_data := calc_bars + data_origin;

      if need_data > data.Count then
        begin
         i := need_data - data.Count;
         pdta := data.AddRows (i);
         data.iLastValid := data.Count - 1;
         FillChar (pdta^, i * data.RowSize, 0); // for debug
         // ODS ('[~T]. #DBG: Vector ~C0A' + data.Name + '~C07 extended to ~C0D' + IntToStr(data.Count) + '~C07 items');
        end;

     // TODO: ��������� ������� ��������� �������� �������� (�������)
     // ��������� ������� ������� = � ������ ���� �������� ���������, ������������ ���������� ������
     // data.Count > work_range * 2
     if (data.Flags and DSF_BARS_DATA <> 0) then
        right_bar := 1 + n_last
     else
        right_bar := need_data;

     { ����� ��������� origin, ����� ��� ����� ���������������� shift ����
       ��� ��������� �������, ��� �������� ���������
       calc_bars = need_bars + resv_count
     }
     i := Max (data_origin, right_bar - calc_bars);

     // ������� ORG ����� ���������� �������� ������� shift
     pdtr.nOrigin := i; // must-be n_first + shift - 1 ( items[256]? )

     pdtr.lporig := data.Rows [i]; // in-moment link!
     // ��������� ����. ���������� ��������� �� ������ lporig
     pdtr.nCount := right_bar - i; // �� nOrigin �� iLast

     // ������������� �������� ��������, � ������ �������� � ������ �������
     pdtr.ValidRange.iFirst := 0; // always it !
     pdtr.ValidRange.iLast := pdtr.nCount - 1;
     pdtr.nMask := MAXINT;
     // ��������� �������� ����������� ��������
     data.iFirstValid := pdtr.nOrigin;
     data.iLastValid :=  pdtr.nOrigin + pdtr.ValidRange.iLast;
     //
     data.iRight := data.iLastValid;
     data.iLeft :=  data.iLastValid - view_bars + 1;
     data.iOrigin := pdtr.nOrigin;


     if data.IsFloat then
        pdtr.wType := SDR_FLOAT_ARRAY
     else
        pdtr.wType := SDR_INT_ARRAY;

     if right_bar = 0 then break;
    end;

   pdtr.fdata := pdtr.lporig;
   pdtr.wItemSize := data.RowSize
  end;
end; // RegDataList

procedure TSMDataCalcJob.RegWorkGlobals;
var n: Integer;
    s: String;
    ownr: TTradeSystem;
begin
 lua_e.VarSet('g_calc_job', self);
 lua_e.VarSet('g_func_list', funcList);

 lua_e.RegFunc ('CalcInd', @lua_CalcInd);
 lua_e.RegFunc ('RegData', @lua_regData);
 lua_e.RegFunc ('GetArrayValue', @lua_GetArrayValue);
 s := 'noname';
 if Assigned (FOwner) and (FOwner is TTradeSystem) then
  begin
   ownr := TTradeSystem ( FOwner );
   if Assigned (ownr.StgConfig) then s := ownr.StgConfig.Name;
  end;

 lua_e.VarSet('g_stg_name', s);

 with stg_params do
 for n := 0 to Count - 1 do
     PushParams (Names[n], ValueFromIndex [n], DWORD ( Objects [n] ) );
end;



function TSMDataCalcJob.Calculate(InData, OutData: TDataMap; resv_count, work_count, nFirst, nLast: Integer): Integer;
var
   desc: TSMFuncDesc;
   pdtr: PSMDataRec;
   pdtl: PInstrDataPtl;
   n, loop_range: Integer;
   s, sfx: String;
   ma: TMatrixArray;
   is_chart: Boolean;
   need_bars, calc_bars, rel_last: Integer;
   nFirstCheck, nLastCheck: Integer;
begin
 result := 0;
 if not FileExists (ScriptFile) then
   begin
    PrintError('Not found script file ' + ScriptFile);
    Exit;
   end;

 data_origin := TTradeSystem(FOwner).DataOrigin;
 // TODO: ���������� ���� �����������, �������� �������� LUA
 // ����� ������� ������ ���� ������������ ��� ��������� ���������, ����� Movings, MovAlgs, TakeProfit � �� ��������
 { ���������� �������� �������������� src-������ �� ������� ��������� ��������. ���������� �������������
   ���������� �� ����� ����� ��������������� ��������������.

   ����� ����: data_origin - ��������� ������� ��������������, ����� �� ����������� � resv_count
 }
 // 1. ������������� ���������� ����������
 l_dcnt := 0;
 lua_e.Init;           // ����� ��������� VM �� ����������� �������
 RegWorkGlobals;

 is_chart := FALSE;

 // ���������� �� ���������� ���������� ������������, ����� ��������� ���� ��������� ���������� ������
 // ���������������� ����������� ���������� ������������ � �������?
 for n := 0 to OutData.Count - 1 do
   begin
    ma := OutData.ArrayByIndex(n);
    if ma.Flags and DSF_DISPLAYED <> 0 then is_chart := TRUE;
   end;

 if not is_chart then
   asm
    nop
   end;

 // InData ������ ��������� ��� ������ �� �����/�����
 s_data.Clear;
 // ���������� �������������� ��� ���������� ������������
 resv_count := Min (nFirst, resv_count);     // can-be not reserving
 if nFirst < data_origin then
  begin
   PrintError(Format('������ ������������ ������������ �������� ��� ' + ClassName +
              '.CalcJob %d..%d - ����� ��� data_origin ���������. ', [nFirst, nLast]) );
   data_origin := nFirst;   // can
  end;




 need_bars := (nLast - nFirst + 1);   // ������� ������ ��������� ����������� �����

 min_items := Max(0, nLast + 1 - data_origin); // base minimum orienting

 // ����� ������� � ���������� ����������� ������
 for n := 0 to inData.Count - 1 do
   begin
    ma := inData.ArrayByIndex (n);
    if (not Assigned (ma)) or (ma is TDateMap) then continue;

    if (ma.RowSize = 4) and (ma.Flags and DSF_BARS_DATA <> 0) then
      begin
       ma.iFirstValid := Max (ma.iFirstValid, data_origin);
       ma.iLastValid := ma.Count - 1;
       min_items := Min (min_items, ma.ValidCount);
      end;
   end;

 if ( need_bars >= min_items ) then
  begin
   // ��������� �������������� �� �����������, MAX_ORG ��������
   // ODS('[~T]. #DBG: reserving only origin = ' + IntToStr(data_origin));
   resv_count := 0;
  end;

 // ����������� ������� �� ���������
 resv_count := Min (resv_count, min_items - need_bars);
 resv_count := Max (resv_count, 0);
 calc_bars := need_bars + resv_count;    // ������� ����� ����� ���������� ������ �� ������ � �������� � �������� Valid
 work_range := Min (calc_bars, min_items);


 // � ��� ������� ���������� ���������� ������������ ������!?
 s_data.preCreateItems :=  work_range;
 OutData.preCreateItems := work_range;

 MapDataList (InData,  resv_count, nLast, calc_bars);
 MapDataList (OutData, resv_count, nLast, calc_bars);

 FillChar (i_data[0], sizeof(i_data[0]) * MAX_INSTR_DATA, 0);

 // ������� ������ ������������, ��� �������������� ���� �����������
 for n := 0 to MAX_INSTR_DATA - 1 do
  begin
   s := 'CLOSE';
   sfx := '';
   if n > 0 then sfx := IntToStr(n + 1);
   s := s + sfx;
   if InData.IndexOf(s) < 0 then continue;
   GetMem (pdtl, SizeOf (TInstrDataPtl));
   FillChar (pdtl^, sizeof (TInstrDataPtl), 0);
   i_data [n] := pdtl;
   pdtl.cnt_items := InData.Arrays [s].Count; // ��� �������������� ��������
   pdtl.price_close := GetDataRec (s);
   pdtl.price_open  :=  GetDataRec ('OPEN' + sfx);
   pdtl.price_high  :=  GetDataRec ('HIGH' + sfx);
   pdtl.price_low   :=  GetDataRec ('LOW' + sfx);
   pdtl.time_open   := GetDataRec ('TIME_OPEN' + sfx);
   pdtl.time_close  := GetDataRec ('TIME_CLOSE' + sfx);
   pdtl.volume := GetDataRec ('VOLUME' + sfx);
   s := InData.Values ['INSTR_ALIAS' + sfx];
   StrLCopy (pdtl.instr_alias, PAnsiChar( AnsiString(s) ) , 24);
   ma := InData.Arrays ['DATE'];
   if not Assigned (ma) then continue;

   pdtl.cnt_days := ma.Count;
   pdtl.date_map := ma.FirstRow;
  end;


 // ��� ���������� ��������
 lua_e.VarSet('g_in_data', InData);
 lua_e.VarSet('g_out_data', OutData);
 lua_e.VarSet('is_chart', is_chart );

 // 2. ���������� ���������� (�����������������) ������� ��������

 lua_e.LoadScript (ScriptFile, TRUE);

 try
  lua_e.Execute;
  lua_e.CallFunc (InitFunc); // ����-����������� ������

 except
   on E: Exception do
      PrintError (' in ' + ClassName + '.Calculate catched exception on lua_e.execute: ' + E.Message);
 end; // try

 // �������� ������� ��������� ������, ���� ��� ���������� ��� �������� ��� ��������
 for n := s_data.Count - 1 downto 0 do
  begin
   s := s_data.Strings [n];
   if ( InData.IndexOf(s) >= 0 ) or ( OutData.IndexOf(s) >= 0 ) then
        s_data.Delete(n);
  end;

 // ���������� ���������� ������ ��������
 if s_data.Count > 0 then
    MapDataList (s_data, resv_count, nLast, calc_bars);

 // TODO: ���������� ���������. ���� ������� �� ������� �������������������� ������, ���� ������� �����!

 for n := 0 to outData.Count - 1 do
     outData.ArrayByIndex(n).FillData(0);
 for n := 0 to s_data.Count - 1 do
     s_data.ArrayByIndex(n).FillData(0);


 { �� ������ ����� ������������������ ���������� � ������� ������ �������� � LUA }
 for n := 0 to l_dcnt - 1 do
    lua_e.VarSet ( String (l_data[n].name), @l_data[n] );




 // TODO: ��������� ����������� ��������� [0..Resv_count] + [nFirst .. nLast]
 // nFirst-nLast ��������������� �������� �� �������� ������, ����� CLOSE/TIME_CLOSE
 // 5. ���������� �������� �� ��������� ������.


 CTMData.n_segment := 0;

 nFirstCheck := nFirst;
 rel_last := 0;

 with CTMData,  CTMData.calc_range do
 repeat

  if n_segment = 0 then
   begin
    // ������������� ���������
    iFirst := 0;           // at left = 256
    rel_last  := calc_bars - 1;  // at left = 256 + 256 + 70 - 1.
   end;

  loop_range := Integer(SegmentSize);


  iLast := Min (rel_last, iFirst + loop_range - 1);
  if (rel_last - iLast < 200) then
    begin
     iLast := rel_last;
     loop_range := iLast - iFirst + 1;
    end;

  if iFirst > iLast then exit;

  {$IFOPT D+}

  nLastCheck := Min (nLast,  nFirstCheck + loop_range - 1);

  lua_e.VarSet('iFirst', iFirst);
  lua_e.VarSet('iLast', iLast);
  lua_e.VarSet('nFirst', nFirstCheck);
  lua_e.VarSet('nLast', nLastCheck);
  if iLast - iFirst < 00  then
    begin
     ODS('[~T] #DBG: Calculating segment #' + IntToStr(n_segment));
    end;
  nFirstCheck := nLastCheck + 1;

  {$ENDIF}


  // ���������� ��������� ��������
  for n := 0 to funcList.Count - 1 do
   begin
    desc := funcList.DescList [n];
    desc.call_num := 0;                        // ����� ������� �������
    //desc.the_params.iFirst := iFirst;
    //desc.the_params.iLast := iLast;
   end;


  try
   lua_e.CallFunc ('CalcJob');
   if lua_e.LastError <> '' then
      with TTradeSystem (FOwner)do CalcErrors := CalcErrors + 1;
  except
   on E: Exception do
     PrintError (' in ' + ClassName + '.Calculate catched exception on calcJob: ' + E.Message);
  end;
  iFirst := iLast + 1;
  Inc (n_segment);
 until iLast >= rel_last;
 // TODO: fill everything iFirstValid, iLastValid
 UpdateRanges (OutData);
 for n := 0 to funcList.Count - 1 do
     funcList.DescList [n].ReleaseMemory;

 // ������������ ��� ������!
 l_dcnt := 0;
 FillChar (l_data[0], sizeof (TSMDataRec) * l_dsize, 0);

 for n := 0 to High (i_data) do
   if i_data [n] <> nil then
      FreeMem (i_data [n]);

 lua_e.VarList.Clear;
 lua_e.CloseState;
end; // Calculate

constructor TSMDataCalcJob.Create;
begin
 FOwner := AOwner;
 _lua_e := TLuaEngine.Create;
 _funcList := TSMFuncList.Create (TRUE);
 SegmentSize := 2048; // ������� ����� �����, ��� data-��� L1 ������� 32��
 stg_params := TStrMap.Create(self);
 s_data := TDataMap.Create(self);
 l_dsize := 256;
 SetLength (l_data, l_dsize);
 init_func := 'InitJob';
 data_origin := TTradeSystem(FOwner).DataOrigin;
end;

destructor TSMDataCalcJob.Destroy;
begin
 _lua_e.Free;
 _funcList.Free;
 SetLength (l_data, 0);
 s_data.Free;
 stg_params.Free;
 inherited;
end;

function TSMDataCalcJob.GetDataRec(const sName: String): PSMDataRec;
var n: Integer;
begin
 for n := 0 to l_dcnt - 1 do
  if l_data[n].name = AnsiString (sName) then
     begin
      result := @l_data[n];
      exit;
     end;

 if l_dcnt >= l_dsize then
   begin
    Inc (l_dsize, 256);
    SetLength (l_data, l_dsize);
   end;
 n := l_dcnt;
 Inc (l_dcnt);
 result := @l_data[n];
 StrLCopy (result.name, PAnsiChar ( AnsiString(sName) ), 15);
end;

procedure TSMDataCalcJob.InitJob;
var
   sfile: String;
begin
 lua_e.Init;
 try
  RegWorkGlobals;
  sfile := ScriptFile;
  if Pos ('\', sfile) = 0 then
     sfile := CorrectFilePath ( ExePath + sfile );

  lua_e.LoadScript (sfile, TRUE);
  lua_e.Execute;
  lua_e.CallFunc ( InitFunc );
 except
  on E: Exception do
    PrintError ('Exception catched in ' + ClassName + '.InitJob#1: ' + E.Message);
 end;

 lua_e.CloseState;

end;

var single_params: TStrMap;

procedure TSMDataCalcJob.PushParams(varname, parlist: String; vtype: DWORD);
var sltmp: TStrMap;
    vtt, s: String;
    psv, psi: PScriptVar;
    n, err: Integer;
    sparam,
    bi: Boolean;
    ival: Integer;
    fval: Double;
begin
 // TODO: ���������� ����� �������� ���������� � ���������� ���������� LUA
 sltmp := TStrMap.Create;
 try
  sltmp.Split (',', parlist);
  vtt := 't';   // ��� ���������� ���������� ����� ���������
  if Pos (':', varname) > 0 then
    begin
     vtt := varname;
     varname := Strtok (vtt, [':']);
     if Length (vtt) > 1 then
        SetLength (vtt, 1);
     vtt := LowerCase (vtt);
    end;

  sparam := ( single_params.IndexOf ( varName ) >= 0 ) or ( vtt = 'u' );
  if ( sltmp.Count = 1 ) and  sparam  then
   begin
    // ival := 0;
    // fval := 0;
    if IsNumber(parlist) then
      begin
       bi := ( Pos('$', parlist) = 1 );
       if bi then
        begin
         Val(parlist, ival, err);
         fval := ival;
        end
       else
        begin
         Val (parlist, fval, err);
         bi := Trunc (fval) = fval;
        end;

       lua_e.VarSet (varname, fval, bi); // set var
      end
    else
      lua_e.VarSet (varname, parlist);

    exit;
   end;

  psv := lua_e.VarList.AddVar (varname, SVT_ARRAY);

  for n := 0 to sltmp.Count - 1 do
    begin
     s := sltmp[n];
     psi := psv.varr.AddRows(1);
     FillChar(psi^, sizeof(TScriptVar), 0);
     psi.vtype := SVT_STRING;
     psi.wstr := s;
     psi.astr := AnsiString (s);
     if IsNumber (s) then
      begin
       psi.vtype := SVT_NUMBER;
       psi.fval := atof(psi.wstr);
      end;
    end;

  lua_e.WriteArray (varname, psv.varr);


  //  lua_e.WriteVar (psv);
 finally
  sltmp.Free;
 end;
end; // PushParams


function GlobalSMFuncList: TSMFuncList;
begin
 if gSMFuncList = nil then
    gSMFuncList := TSMFuncList.Create (FALSE);
 result := gSMFuncList;
end;


procedure InitModule;
var sz: Integer;
begin
 sz := sizeof(TSMFuncParams);
 Assert ( sz = 4096, 'sizeof (TSMFuncParams) must be 4096, but = ' + IntToStr (sz));
 single_params := TStrMap.Create;
 single_params.CaseSensitive := FALSE;
 single_params.Sorted := TRUE;
 single_params.Split (',', DEF_SINGLE_PARAMS);
end;

procedure FinalizeModule;
begin
 single_params.Free;
 gSMFuncList.Free;
end;

initialization
 InitModule;
finalization
 FinalizeModule;
end.
