unit SMAlgs;

interface
uses Windows, SysUtils, StrUtils, StrClasses, Classes, Misc, SegMath, SMTypes, UTITypes, ArrayTypes, Math, UTIConst;


procedure LoadIndLib (const lib: String);

implementation



function SMInitArray (fp: PSMFuncParams): Integer; stdcall;
var
   dst: PSMDataRec;
   fval: Single;
   n, n_first: Integer;
begin
 result := 0;
 if CheckDataParams (fp.out_data, 'out_params', 1) then else exit;

 dst := fp.out_data [0];
 fval := fp.f_params [0];

 n_first := fp.StrictSrcFirst;

 for n := n_first to fp.iLast do
          dst.fdata [n] := fval;


 fp.LimitResults (0, fp.iLast);
 result := fp.iLast + 1;
end;  // SMInitArray

function SMCross (fp: PSMFuncParams): Integer; stdcall;
var
   mvaf, mvas: PSMDataRec;
   pmvaf, pmvas: PSingleArray;
   dst: PSMDataRec;
   pdst: PSingleArray;
   vcross: Single;
   flags: DWORD;
   n, p, n_first, n_last: Integer;
begin
 result := 0;
 if CheckDataParams (fp.in_data, 'in_params', 2) and
    CheckDataParams (fp.out_data, 'out_params', 1) then else exit;

 mvaf := fp.in_data [0];
 mvas := fp.in_data [1];
 dst := fp.out_data [0];
 flags := fp.i_params [0];



 pmvaf := mvaf.fdata;
 pmvas := mvas.fdata;
 pdst := dst.fdata;

 n_first := fp.StrictSrcFirst;
 n_last  := fp.StrictSrcLast;


 for n := n_first to n_last do
 if n > 0 then
  begin
   p := n - 1;
   vcross := 0;
   // cross over
   if ( pmvaf[p] <= pmvas[p] ) and ( pmvaf[n] > pmvas[n] ) then vcross := +1;
   if ( pmvaf[p] >= pmvas[p] ) and ( pmvaf[n] < pmvas[n] ) then vcross := -1;
   // проверка на разрешение оверлап-записей
   if ( flags and 1 <> 0 ) and ( vcross = 0 ) then
    else
        pdst [n] := vcross;
  end;

 fp.LimitResults (n_first, n_last);

 result := fp.iLast + 1;
end;  // SMInitArray

function SMBuySell (fp: PSMFuncParams): Integer; stdcall;
// простая функция покупки-продажи
{
   typical  inputs: sig
   typical outputs: sigpos

 Флажки контроля управления:
  * стратегия реального-времени (смена позиции до закрытия бара)
  * шорты разрешены/запрещены

 Обрабатываются сигналы открытия-переворота стратегии,
   лимитные и закрывающие не воспринимаются правильно.

}

var
   bRealTime, bAllowShort: Boolean;
   sig, sigpos: PSMDataRec;
   psig, psigpos: PSingleArray;
   i, n, r_ofs, n_first, n_last : Integer;

   sigv, targetpos, curpos, trade_vol, pos_limit: Single;


begin
 result := 0;
 bRealTime :=    (fp.stg_flags and STGF_REALTIME <> 0);
 bAllowShort :=  (fp.stg_flags and OPTF_BLOCK_SHORT = 0);
 r_ofs := Integer (not bRealTime) and 1;  // фактически место может иметь вытаскивание curpos из прошлого

 if CheckDataParams (fp.in_data,  'in_params', 1) and
    CheckDataParams (fp.out_data, 'out_params', 1) then else exit;

 sig := fp.in_data [NPARAM1];
 sigpos := fp.out_data [NPARAM1];

 trade_vol := fp.f_params [NPARAM1];
 pos_limit := Min (trade_vol, fp.f_params [NPARAM2]);

 curpos := 0;
 if fp.n_segment > 0 then
    curpos := PSingle ( @fp.small_ctx[0] )^;

 psig := sig.fdata;
 psigpos := sigpos.fdata;

 n_first := fp.StrictSrcFirst;
 n_last :=  fp.StrictSrcLast - r_ofs;

 for n := n_first to n_last do
  begin
   sigv := psig [n];
   if sigv > 0 then
      begin
       if curpos < 0 then targetpos := trade_vol else targetpos := curpos + trade_vol;
       if targetpos <= pos_limit then curpos := targetpos;
      end;
   if sigv < 0 then
      if bAllowShort then
      begin
       if curpos > 0 then targetpos := -trade_vol else targetpos := curpos - trade_vol;
       if targetpos >= -pos_limit then curpos := targetpos;
      end
      else curpos := 0;
   i := n + r_ofs;
   psigpos [i] := curpos;
  end;
 PSingle ( @fp.small_ctx[0] )^ := curpos;

 // last saved item = n_last + r_ofs

 // поскольку функция критическая, надо распространить последний результат на все оставшиеся бары
 for n := n_last + 1 + r_ofs to fp.iLast do
     psigpos [n] := curpos;


 fp.LimitResults (n_first, fp.iLast)

end; // SMBuySell


function SMCalcSMA (fp: PSMFuncParams): Integer; stdcall;
type
    TSMAContext = record
     summ: Single;
     fcnt: Single;
    end;

    PSMAContext = ^TSMAContext;

var
   pctx: PSMAContext;
   src, dst: PSMDataRec;
   psrc, pdst: PSingleArray;
   np: Single;
   n, n_periods, n_back, n_first, n_last: Integer;

begin
 pctx := @fp.small_ctx;
 if fp.n_segment = 0 then
   begin
    pctx.summ := 0;
    pctx.fcnt := 0;
   end;
 // назначение параметров и структур с данными
 n_periods := Max (1, fp.i_params [0]);
 src := fp.in_data [0];
 dst := fp.out_data [0];
 result := 0;

 if CheckDataParams (fp.in_data, 'in_params', 1) and
    CheckDataParams (fp.out_data, 'out_params', 1) and (n_periods > 0) then else exit;

 np := 1 / n_periods;
 psrc := src.fdata;
 pdst := dst.fdata;



 n_first := fp.StrictSrcFirst;
 n_last := fp.StrictSrcLast;
 n_back := n_periods - 1;

 Assert (n_first >= 0);

 if UpCase(ReadKey) = 'D' then
    ODS( CFormat ('[~T]. #DBG(CalcSMA): n_first = %d, n_last = %d ', '~C07', [n_first, n_last]) );


 // рассчет простой скользящей средней оптимизированным алгоритмом
 with pctx^ do
 for n := n_first to n_last do
  begin
   if (n > src.ValidRange.iLast) then
     begin
      PrintError (Format('Index outbound n = %d > iLast = %d, fp.iLast = %d',
                   [n, src.ValidRange.iLast, fp.iLast]));
      exit;
     end;

   fcnt := fcnt + 1.0;
   if psrc [n] <= 0 then
    asm
     nop
    end;
   summ := summ + psrc [n];
   if n > fp.iLast - 10 then
    asm
     nop
    end;

   if (fcnt >= n_periods) then
     begin
      pdst [n] := summ * np;         // summ / nPeriods
      if n < n_back then
         Assert (false, Format ('n < n_back. fcnt = %.0f, n_seg = %d', [fcnt, fp.n_segment]));
      summ := summ - psrc [n - n_back];
      dst.ValidRange.iLast := n;
     end
    else
     // плохо - если недостаточно элементов для резерва, забивается фейк
     if n >= src.ValidRange.iFirst then
      begin
       pdst [n] := summ / fcnt;
       dst.ValidRange.iFirst := n + 1;
      end;
  end;

 fp.LimitResults(n_first, n_last);
 result := dst.ValidRange.iLast + 1; // last filled bar
end; // SMCalcSMA

// ----------------------------- -----------------------------  -----------------------------


function SMCalcEquity ( fp: PSMFuncParams ): Integer; stdcall;
{ Входящие данные для CalcEquity:
    SIGPOS - позиция по инструменту
    PRICE - ожидаемая цена исполнения, без проскальзывания, если отсутствует = BARs.CLOSE
    UNIT_SIZE  - размер юнита, если отсутствует  = 1
    POINT_COST - стоимость шага в деньгах (экспериментально для RI* фучей), если отсутствует = 1

  * данные по торгуемому инструменту (TradeInstr);

  Выходящие данные:
    EQUITY_CLOSE  - по оценке закрытия бара (в процентах/деньгах)
    EQUITY_OPEN   - по оценке открытия бара

  Параметры integer:
    1. флажки характера рассчетов (оптимизации) / проскальзывания / комиссии

  Параметры float:
    1. Проскальзывание
    2. Комиссия

}

type
    TFContext = record
     // in-data
     sig_pos, price, unit_size, point_cost: TSMDataRec;
     // pprice, punit_size, ppoint_cost: PSingleArray;
     // out-data
     eqty_close, eqty_open, price_out: TSMDataRec;

     fixed: array [0..15] of Single;

     spval: Single;                       // sig pos value
     prvsp: Single;
     start_money: Single;
     comission: Single;
     slippage: Single;
     comis_rel: Single;
     slip_rel: Single;
     cur_free: Single;                    // сколько свободных денег на виртуальном счете

     market_pos: TMarketUnit;             // рыночная позиция в контрактах по инструменту
     trade_pass, price_out_bar: Boolean;

     // параметрические характеристики вывода.
     cfg_flags: DWORD;
     lot_size: Single;
     lim_vol: Integer;
     rtm_ofs: Integer;
     anti_system, allow_short, allow_opt, bDump, last: Boolean;
     sig_cnt: Integer;   // количество сигналов/переворотов

    end; // TFContext


var
   pctx: ^TFContext;

  procedure InitParam(nIndex: Integer; src: Boolean; var vdtr: TSMDataRec);
  var
     dtl: ^TIndDataList;
     pdtr: PSMDataRec;
  begin
   if src then dtl := @fp.in_data else dtl := @fp.out_data;
   pdtr := dtl [nIndex];
   if src then else Inc (nIndex, 8);
   vdtr.fdata := @pctx.fixed [nIndex];
   vdtr.nMask := 0;
   if pdtr = nil then exit;
   vdtr := pdtr^;
   vdtr.nMask := MAXINT;
  end;

  procedure InitContext;
  begin
   with pctx^ do
    begin
     FillChar (pctx^, sizeof (TFContext), 0);

     // параметры управления
     start_money :=    fp.i_params [NPARAM1];
     cfg_flags :=      fp.i_params [NPARAM2];

     // параметры расходов
     slippage  :=  fp.f_params [NPARAM1]; // may be zero
     comission :=  fp.f_params [NPARAM2];
     lot_size :=   fp.f_params [NPARAM3]; // размер лота для фучей


     sig_cnt := 0;
     // входящие данные
     sig_pos :=    fp.in_data [NPARAM1]^;
     if fp.in_data[NPARAM2] <> nil then
        InitParam(NPARAM2, TRUE, price)
     else
       begin
        if (cfg_flags and OPTF_EXEC_TBC <> 0) or (cfg_flags and STGF_REALTIME <> 0) then
            price := fp.instr_data[0].price_close^
        else
            price := fp.instr_data[0].price_open^; // traded instrument
       end;

     InitParam(NPARAM3, TRUE, unit_size);
     InitParam(NPARAM4, TRUE, point_cost);

     // выходные данные
     InitParam(NPARAM1, FALSE, eqty_close);
     InitParam(NPARAM2, FALSE, eqty_open);
     InitParam(NPARAM3, FALSE, price_out);


     if lot_size = 0 then lot_size := 1;

     cur_free := start_money;

     slip_rel := 0;
     comis_rel := 0;

     rtm_ofs := 1;
     if (cfg_flags and STGF_REALTIME <> 0) or (cfg_flags and OPTF_EXEC_TBC = 0) then
         rtm_ofs := 0;

     if (cfg_flags and OPTF_FIXED_SLIP = 0) then
         slip_rel := slippage * 0.01;
     if (cfg_flags and OPTF_FIXED_COMIS = 0) then
         comis_rel := comission * 0.01;

     allow_short := ( cfg_flags and OPTF_BLOCK_SHORT = 0 );
     anti_system := ( cfg_flags and OPTF_ANTI_SYSTEM <> 0 );


    end;

  end; // InitContext


var
   i_data: PInstrDataPtl;
   usize, dpval: Single;
   trade_vol: Integer;
   ps_high, ps_low, ps_open, ps_close, rest_open, rest_close: Single;
   psv, pst, total_comission: Single;
   dir_sign: Integer;
   n, n_first, n_last: Integer;
begin
 result := 0;
 if fp.instr_data [0] = nil then exit;

 if CheckDataParams (fp.in_data, 'in_params', 1) and
    CheckDataParams (fp.out_data, 'out_params', 1)  then else exit;

 pctx := @fp.small_ctx;

 n_first := fp.in_data[NPARAM1].ValidRange.iFirst;

 // селекция начала диапазона доступных данных
 for n := NPARAM2 to NPARAM3 do
  if fp.in_data[n] <> nil then
     n_first := Max (n_first, fp.in_data[n].ValidRange.iFirst);


 n_first := fp.StrictSrcFirst;
 n_last := fp.StrictSrcLast;

 if fp.n_segment = 0 then
    InitContext;

 i_data := fp.instr_data [0];
 pctx.prvsp := 0;
 trade_vol := 0;


 with fp^, pctx^ do
 if i_data <> nil then
 for n := n_first to n_last do
  begin
   ps_open :=  i_data.price_open.fdata [n];
   ps_close := i_data.price_close.fdata [n];

   rest_open := ps_open * market_pos + cur_free;

   spval := sig_pos.fdata [n]; // уже текущая позиция по факту исполнения. Флажок real-time не действует здесь

   if anti_system then spval := -spval;

   trade_pass := (spval <> prvsp);

   if trade_pass then
     begin
      usize := unit_size.fdata [n and unit_size.nMask];
      if usize <= 0 then usize := 1;
      // рассчет ожидаемой рыночной позиции
      dpval := spval * usize * lot_size;
      // дельта изменения позиции
      trade_vol := Round ( dpval - market_pos );

      prvsp := spval;
      // для избежания дальнейших вычислений...
      if trade_vol = 0 then trade_pass := FALSE;
     end;

   // ----------------- рассчет исполнения заявок --------------
   if trade_pass then
     begin
        dir_sign := Sign (trade_vol);
        psv :=  price.fdata [ (n - rtm_ofs) and price.nMask];   // цена для совершения виртуальной сделки

        ps_high  := i_data.price_high.fdata [n];
        ps_low   := i_data.price_low.fdata [n];

        Inc (sig_cnt);
        pst := psv;  // ожидаемая цена исполнения (не окончательный вариант)
        // пересчет проскальзывание из процентов в пунктов
        if (cfg_flags and OPTF_FIXED_SLIP = 0) then
          begin
           slippage := pst * slip_rel;
           // if slippage > 20 then slippage := Round(slip);
          end;
        // ---------------------------------------------
        if dir_sign > 0 then
          begin
           // покупка = если попытка осуществить сделку ниже бара, она проводится с опозданием по цене близкой к CLOSE
           if pst < ps_low then
                 begin
                  pst := ps_high + slippage;
                  price_out_bar := TRUE;     // в этом баре заявка не исполнилась
                 end;
          end
        else
          begin
           // продажа = если попытка осуществить сделку выше бара, она проводится с опозданием по цене близкой к CLOSE
           if pst > ps_high then
                 begin
                  pst := ps_low - slippage;
                  price_out_bar := TRUE;     // в этом баре заявка не исполнилась
                 end;
          end;
       // ------------------------ -----------------------
       // добавление проскальзывания
       pst := pst + slippage * dir_sign;

       // раундинг цены сделки
       if (pst > 30000) then pst := Round(pst / 5) * 5
           else
             if (pst > 5000) then pst := Round(pst);

       total_comission := Abs(trade_vol) * comission; // базовая комиссия (брокер+бирж. сбор) для фучей
       if (cfg_flags and OPTF_FIXED_COMIS = 0) then
           total_comission := total_comission * pst; // комиссия ММВБ за денежный объём сделки

       cur_free := cur_free - (trade_vol * pst) - total_comission; // корректировка счета-баланса
       market_pos := market_pos + trade_vol;
       pst := trade_vol;
       while ( Abs(pst) > 1) do pst := pst * 0.1; // minimize
       price_out.fdata [n and price_out.nMask] := pst; // 1 / 1000

       price_out_bar := FALSE;
     end;

   rest_close := ps_close * market_pos + cur_free;
   // ============= WRITING RESULT ================= //
   if cfg_flags and OPTF_EQTY_PRCNT <> 0 then
      begin
       eqty_open.fdata  [n and eqty_open.nMask]  := 100 * rest_open / start_money;
       eqty_close.fdata [n and eqty_close.nMask] := 100 * rest_close / start_money;
      end
   else
      begin
       eqty_open.fdata  [n and eqty_open.nMask]  := rest_open;
       eqty_close.fdata [n and eqty_close.nMask] := rest_close;
      end;

  end;

 fp.LimitResults(n_first, n_last);
end; // SMCalcEquity

{ Обработка регистрации функций индикаторов из DLL.
 ------------------
 Программа сканирует папку plugins на предмет нахождения в ней файлов с расширениями indlib.
 Файлы последовательно загружаются как DLL, и в них выполняется функция Initialize.

 Прототип функции Delphi:
   function Initialize (callbacks: PCallbackList): PExportList;



}

procedure LogODS(pmsg: PWideChar); stdcall;
begin
 ODS ( String (pmsg) );
end;


type
    TInitFunction = function (callbacks: PCallbackList): PExportList; stdcall;


procedure LoadIndLib (const lib: String);
var
   fcl: TSMFuncList;
   finit: TInitFunction;
   explst: PExportList;
   cblist: PCallbackList;
   hLib: THandle;
   i: Integer;
   lib_name: String;

begin
 ODS ('[~T].~C0F #MSG: Loading ind-library ~C0A' + lib + '~C07...');
 lib_name := PluginsPath + lib;
 hLib := LoadLibrary( PChar(lib_name) );
 if hLib = 0 then
    begin
     i := GetLastError;
     PrintError ('LoadLibrary fails, error = ' + err2str (i) );
     exit;
    end;
 fcl := GlobalSMFuncList;


 GetMem (cblist, sizeof (TCallbackList));

 try
   cblist.items_count := 1;
    with cblist.func_list [0] do
     begin
      func_ver := $0001;
      StrPCopy (func_name, 'LogODS');
      StrPCopy (call_type, 'stdcall');
      func_ptr := @LogODS;
     end;


   try
     finit := GetProcAddress (hlib, 'Initialize');
     if not Assigned (finit) then
        begin
         PrintError ('function Initialize is not defined in ' + lib_name);
         FreeLibrary (hlib);
         exit;
        end;

     explst := finit (cblist);
     {$R-}
     if Assigned (explst) then
       for i := 0 to explst.items_count - 1 do
          with explst.func_list [i] do
             fcl.AddDesc( String(func_name), lib, func_ptr);


     GlobalFree ( DWORD(explst) );

   except
     on E: Exception do
       PrintError('Exception while initializing library ' + lib_name + ':' + E.Message);
   end; // try
 finally
  FreeMem (cblist);
 end; // try 1
end; // LoadIndLib


procedure InitModule;
var
   fcl: TSMFuncList;
   flibs: TFileList;

   lib_name: String;
   n: Integer;
begin
 fcl := GlobalSMFuncList;
 fcl.AddDesc('SMA', 'TradeSys', SMCalcSMA);
 fcl.AddDesc('InitArray', 'TradeSys', SMInitArray);
 fcl.AddDesc('Cross',   'TradeSys', SMCross);
 fcl.AddDesc('BuySell', 'TradeSys',SMBuySell);
 fcl.AddDesc('Equity',  'TradeSys', SMCalcEquity);

 flibs := TFileList.Create();

 Sleep(200);


 try
  flibs.FindFiles(PluginsPath + '*.ilb');
  lib_name := 'nonsens!';


  if flibs.Count > 0 then
   for n := flibs.Count - 1 downto 0 do
      try
       lib_name := flibs [n];
       LoadIndLib (lib_name);
      except
       on E: Exception do
          OnExceptLog ('SMAlgs.InitModule(lib_name = ' + lib_name + ') ', E);
      end;



  Sleep(200); // wait for text-console-output
  ODS(' #DBG: sizeof(TInstrDataPtl) = ' + IntToStr (sizeof(TInstrDataPtl)) );
  ODS(' #DBG: sizeof(TSMDataRec) = ' + IntToStr (sizeof(TSMDataRec)) );
 finally
  flibs.Free;
 end;


end; // InitModule

procedure FinalizeModule;
begin

end;


initialization
 InitModule;
finalization
 FinalizeModule;
end.
