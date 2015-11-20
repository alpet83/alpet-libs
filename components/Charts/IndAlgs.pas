unit IndAlgs;

interface
uses Windows, SysUtils, Classes, Misc, StrClasses, ChartData, UNIArray, Math, WThreads, DateTimeTools, ArrayTypes, UTIConst, UTITypes, UTIFunc, FinMath, TradeBase;
{$DEFINE OPTALGS}

const
    DEFAULT_SLIPPAGE = 0.00025;
    TMF_IMM_CLOSE = $0020;
    TMF_CLOSE_ANY = $0040;
    TMF_FAST_REST = $0100;
    TMF_PROF_REST = $0200;



procedure       InitBasicEquity (ts: TTradeSystem; flags: DWORD);


procedure       InitModule;
procedure       FinalizeModule;


implementation
uses PackedStorage, InstrDictionary;


procedure InitBasicEquity (ts: TTradeSystem; flags: DWORD);

var iinf: TInstrInfo;
    lot_size, slip, tmon, tcom: Single;
    backRef, plim: Integer;
    optf: DWORD;
    stg: TStrategyConfig;
    src, dst: String;

begin
 stg := ts.StgConfig;
 with ts do
 begin
  src := 'SIGPOS,PRICE,OPEN,HIGH,LOW,CLOSE,UNIT_SIZE';
  dst := 'EQTY_OPEN,EQTY,PRICE_OUT,TRADES';

  plim := 1;
  backRef := 1;
  tcom := 0.00;
  lot_size := 1;

  AddData(dst + ',PRICE,EQTY_RSI', FALSE);

  tmon := 16000;
  optf := OptFlags or flags;
  slip := 0.03; // меньше 1/30 процента, с расчетом на хорошую систему оптимизации.

  if (stg <> nil) and (stg.DataConfig[0] <> nil) then
    begin
     if stg.RealTime then backRef := 0;

     plim := stg.PositionLimit;
     slip := stg.Slippage;

     if stg.AntiSystem < 0 then
        optf := optf or OPTF_ANTI_SYSTEM;

     iinf := stg.DataConfig[0].InstrInfo;
     if iinf <> nil then
       begin
        lot_size := iinf.lot_size;
        if lot_size <= 0 then lot_size := 1;
        if iinf.Board = 'FUT' then tcom := 1.5 / Max(1, lot_size); // за всё сдирать по черному!
        tmon := iinf.trade_money;
       end;
     if stg.Comission > 0 then
        tcom := stg.Comission;
    end;
  if stg.Money > 0 then tmon := stg.Money;


  if slip > 1 then optf := optf or OPTF_FIXED_SLIP;

  // торговля по одному лоту, с опозданием сигнала в один бар. Начальный объем = ??к, проскальзывание = 0.1%
  with AddCalcJob('CALC_' + dst, 'CALC') do
       AddFuncParams ('EQUITY', src, dst, Format('0,%d,%d,%d', [backRef, optf, plim]),
                                          Format ('%.0f,%.5f,%.5f,%.2f', [ tmon, slip, tcom, lot_size]) );
 end;
end; // InitAC_Equity



function CalcRSI (jp: TCalcJobParams): Integer;
// функция рассчета работает в режиме добавления!
var
   src, dst: TArray32;
   nPeriods: Integer;
   i, ns, cnt: Integer;
   dta, dtaprv, dnchg, upchg, rs, prv: Single;
   pdst: PSingleArray;
   vtemp: array [0..127] of Single;
begin
 src := jp.src32 [0];           // source vals
 dst := jp.dst32 [0];
 nPeriods := jp.iParams[0];
 cnt := src.ValidCount;  // сколько всего можно (и нужно) просчитать элеметов
 result := 0;
 if (nPeriods <= 0) or (cnt <= 0) then exit;
 // по возможности функция рассчитает данные для всего cnt
 dst.iFirstValid := dst.Count;       // самое оптимистичное значение
 pdst := PSingleArray ( dst.AddRows (cnt) );
 // последние значимые элементы
 dst.iLastValid := dst.Count - 1;
 //dst.iLast := dst.iLastValid;

 pdst[0] := 50;

 FillChar(vtemp[0], sizeof(Single) * 128 , 0);
 prv := src.FloatItems[0];

 upchg := 0;
 dnchg := 0;
 i := 0;
 for ns := 1 to src.iLastValid do
  begin
   pdst[ns] := 50;
   dta := src.FloatItems[ns] - prv;
   if dta >= 0 then
      upchg := upchg + dta
    else
      dnchg := dnchg - dta;

   vtemp [i] := dta;
   Inc(i);
   if i >= nPeriods then i := 0;
   dtaprv := vtemp[i];

   if ns >= nPeriods - 1 then
     begin
      rs := 100;
      if dnchg > 0 then rs := Abs( upchg / dnchg );
      rs := 100 - ( 100 / ( 1+rs ));

      pdst [ns] := rs;
      if dtaprv >= 0 then
         upchg := upchg - dtaprv
      else
         dnchg := dnchg + dtaprv;
     end;

   prv := src.FloatItems[ns];
  end;
end;

function CalcSMA (jp: TCalcJobParams): Integer;
// функция рассчета работает в режиме добавления!
var
   src, dst: TArray32;
   nPeriods: Integer;
   ns, cnt: Integer;
   src_first: Integer;
   fcnt, summ, np: Single;
   pdst: PSingleArray;

begin
 src := jp.src32 [0];           // source vals
 dst := jp.dst32 [0];
 nPeriods := jp.iParams[0];
 cnt := src.ValidCount;  // сколько всего можно (и нужно) просчитать элеметов
 result := 0;
 if (nPeriods <= 0) or (cnt <= 0) then exit;
 // по возможности функция рассчитает данные для всего cnt
 dst.iFirstValid := dst.Count;       // самое оптимистичное значение
 pdst := PSingleArray ( dst.AddRows (cnt) );
 // последние значимые элементы
 dst.iLastValid := dst.Count - 1;
 //dst.iLast := dst.iLastValid;
 np := 1 / nPeriods;
 // проверка на возможность резервирования в источнике
 if src.iFirstValid >= nPeriods then
    src_first := src.iFirstValid - nPeriods + 1
 else
    src_first := 0;

 summ := 0;
 fcnt := 0;
 ASSERT (src.FloatItems <> nil, 'CalcSMA: src.FloatItems = nil');

 for ns := src_first to src.iLastValid do
  begin
   fcnt := fcnt + 1.0;

   summ := summ + src.FloatItems [ns];
   if (fcnt >= nPeriods) then
     begin
      pdst [result] := summ * np;         // summ / nPeriods
      summ := summ - src.FloatItems [ns + 1 - nPeriods];
      Inc (result);
     end
    else
     // плохо - если недостаточно элементов для резерва, забивается фейк
     if ns >= src.iFirstValid then
      begin
       pdst [result] := summ / fcnt;
       dst.iFirstValid := dst.iFirstValid + 1;
       Inc (result);
      end;
  end;

end;  // CalcSMA


function  CalcEMA (jp: TCalcJobParams): Integer;
// функция рассчета работает в режиме добавления!
var
   nPeriods: Integer;
   dst, src: TArray32;
   ns, cnt: Integer;
   src_first: Integer;
   ema, pval, cval: Double;
   pdst: PSingleArray;
begin
 src := jp.src32 [0];           // source vals
 dst := jp.dst32 [0];
 nPeriods := jp.iParams[0];
 cnt := jp.srcd.ItemsCount;
 result := 0;
 if (nPeriods <= 0) or (cnt <= 0) then exit;
 pval := 2 / (nPeriods + 1);
 cval := 1 - pval;
 pdst := jp.srcd.InitDest ( dst );
 // dst.iFirst := dst.iFirstValid;      // место начала добавления
 nPeriods := Min (200, nPeriods * 4);   // для большей точности - рассчитать дополнительное кол-во элементов

 if src.iFirstValid >= nPeriods then
    src_first := src.iFirstValid - nPeriods + 1       // забалагуритьнах бары
 else
    src_first := 0;

 cnt := 0;
 ema := src.FloatItems [src_first];
 // if (src_first > 0) then ema := src.FloatItems [src_first - 1];

 for ns := src_first to src.iLastValid do
  begin
   Inc (cnt);
   ema := src.FloatItems [ns] * pval + ema * cval;
   pdst [result] := ema;
   Inc (result);

   if (cnt >= nPeriods) then
   else
      dst.iFirstValid := dst.iFirstValid + 1;
  end;

end;  // CalcEMA {}


function CalcHistogram (jp: TCalcJobParams ): Integer;
var n: Integer;
    red, green, dst: TArray32;
    diff, prvd: Double;
    pred, pgreen, pdst,  s0, s1: PSingleArray;
    cnt: Integer;
    pt: TProfileTimer;
    e: Double;
begin
 pt := TProfileTimer.Create;
 result := 0;
 red := jp.dst32 [0];
 green := jp.dst32 [1];
 dst := jp.dst32 [2];
 cnt := jp.srcd.ItemsCount;
 if cnt <= 0 then exit;
 pred := jp.srcd.InitDest (red);
 pgreen := jp.srcd.InitDest (green);
 pdst := jp.srcd.InitDest (dst);

 s0 := jp.srcd.GetValues(0);
 s1 := jp.srcd.GetValues(1);

 e := pt.Elapsed;
 if (e > 100) then ODS ('[~T]. #OPT: CalcHistogram init time = ' + ftow(e, '%.2f') );
 pt.Start;

 prvd := 0;
 with jp.srcd do
 for n := 0 to cnt - 1 do
  begin
   diff := s0 [n] - s1 [n];
   pdst [n] := diff;
   if diff < prvd then
     begin
      pred [n] := diff;
      pgreen [n] := 0;
     end
   else
     begin
      pred [n] := 0;
      pgreen [n] := diff;
     end;
   prvd := diff;
   Inc (result);
  end;
 e := pt.Elapsed;
 if (e > 100) then ODS ('[~T]. #OPT: CalcHistogram calc time = ' + ftow(e, '%.2f') );
 pt.Free;
end; // CalcOSC

function CalcHistogram3 (jp: TCalcJobParams ): Integer;
// рассчет трехзональной гистограммы
var n: Integer;
    red, green, yellow: TArray32;
    diff, prvd: Double;
    diffv: array of Double;
    pred, pgreen, pyellow: PSingleArray;
    dta_min: Single;
    sum: Double;
    cnt: Integer;

begin
 result := 0;
 red := jp.dst32 [0];
 green := jp.dst32 [1];
 yellow := jp.dst32 [2];
 cnt := jp.srcd.ItemsCount;
 if cnt <= 0 then exit;
 pred := jp.srcd.InitDest (red);
 pgreen := jp.srcd.InitDest (green);
 pyellow := jp.srcd.InitDest (yellow);
 // рассчет значений гистограммы
 SetLength(diffv, cnt);
 sum := 0;
 with jp.srcd do
 for n := 0 to cnt - 1 do
  begin
   Read (n);
   diff := Values [0] - Values [1];
   diffv[n] := diff;
   sum := sum + Abs(diff);
  end;
 sum := sum / (1.0 * cnt); // среднее значение гистограммы

 dta_min := jp.fParams[0] * sum;   // минимальное значение изменения, для не желтого результат               

 prvd := 0;

 for n := 0 to cnt - 1 do
  begin
   diff := diffv[n];
   if Abs(diff - prvd) <= dta_min then
    begin
     pred [n] := 0;
     pgreen [n] := 0;
     pyellow [n] := diff;
    end
   else
     begin
       if diff < prvd then
         begin
          pred [n] := diff;
          pgreen [n] := 0;
          pyellow [n] := 0;
         end
       else
         begin
          pred [n] := 0;
          pgreen [n] := diff;
          pyellow [n] := 0;
         end;
     end; // ------------------- red & green    
   prvd := diff;
   Inc (result);
  end;
 SetLength(diffv, 0);
end; // CalcOSC


function  CalcMedian (jp: TCalcJobParams): Integer;
var
    dst: TArray32;
    n, i, rcnt, cnt: Integer;
    pdst, s0, s1: PSingleArray;
begin
 dst := jp.dst32 [0];
 result := 0;
 cnt := jp.srcd.ItemsCount;
 if cnt <= 0 then exit;
 pdst := jp.srcd.InitDest (dst);
 s0 := jp.srcd.GetValues(0);
 s1 := jp.srcd.GetValues(1);
 i := 0;
 {$IFDEF OPTALGS}
  rcnt := (cnt shr 3) shl 3; // = div 8 * 8, sample from 67 = 64

  while (i < rcnt) do
   begin
    pdst[i + 0] := ( s0 [i + 0]  + s1 [i + 0] ) * 0.5;
    pdst[i + 1] := ( s0 [i + 1]  + s1 [i + 1] ) * 0.5;
    pdst[i + 2] := ( s0 [i + 2]  + s1 [i + 2] ) * 0.5;
    pdst[i + 3] := ( s0 [i + 3]  + s1 [i + 3] ) * 0.5;
    pdst[i + 4] := ( s0 [i + 4]  + s1 [i + 4] ) * 0.5;
    pdst[i + 5] := ( s0 [i + 5]  + s1 [i + 5] ) * 0.5;
    pdst[i + 6] := ( s0 [i + 6]  + s1 [i + 6] ) * 0.5;
    pdst[i + 7] := ( s0 [i + 7]  + s1 [i + 7] ) * 0.5;
    Inc (i, 8);
   end;
 {$ENDIF}
 for n := i to cnt - 1 do
       pdst[n] := ( s0 [n]  + s1 [n] ) * 0.5 ;

 result := cnt;
end; // CalcMedian

function  CalcSum (jp: TCalcJobParams): Integer;
var
    dst: TArray32;
    n, i, rcnt, cnt: Integer;
    s0, s1, pdst: PSingleArray;
begin
 dst := jp.dst32 [0];
 result := 0;
 cnt := jp.srcd.ItemsCount;
 if cnt <= 0 then exit;
 pdst := jp.srcd.InitDest (dst);
 s0 := jp.srcd.GetValues(0);
 s1 := jp.srcd.GetValues(1);
 i := 0;

 {$IFDEF OPTALGS}
  rcnt := (cnt shr 3) shl 3; // = div 8 * 8, sample from 67 = 64
  while (i < rcnt) do
   begin
    pdst[i + 0] := ( s0 [i + 0]  + s1 [i + 0] );
    pdst[i + 1] := ( s0 [i + 1]  + s1 [i + 1] );
    pdst[i + 2] := ( s0 [i + 2]  + s1 [i + 2] );
    pdst[i + 3] := ( s0 [i + 3]  + s1 [i + 3] );
    pdst[i + 4] := ( s0 [i + 4]  + s1 [i + 4] );
    pdst[i + 5] := ( s0 [i + 5]  + s1 [i + 5] );
    pdst[i + 6] := ( s0 [i + 6]  + s1 [i + 6] );
    pdst[i + 7] := ( s0 [i + 7]  + s1 [i + 7] );
    Inc (i, 8);
   end;
 {$ENDIF}

 with jp.srcd do
 for n := i to cnt - 1 do
      pdst[n] := ( s0 [n] + s1 [n] );
 result := cnt;
end; // CalcSum

function  Substract (jp: TCalcJobParams): Integer;
var
    dst: TArray32;
    n, cnt: Integer;
    flags: DWORD;
    q: Single;
    s0, s1, pdst: PSingleArray;
begin
 dst := jp.dst32 [0];
 result := 0;
 cnt := jp.srcd.ItemsCount;
 if cnt <= 0 then exit;
 flags := jp.iParams [0];
 pdst := jp.srcd.InitDest (dst);
 s0 := jp.srcd.GetValues(0);
 s1 := jp.srcd.GetValues(1);
 q := 1;
 if (flags and $02 <> 0) then q := -1;

 with jp.srcd do
 if flags and $01 <> 0 then
    for n := 0 to cnt - 1 do
         pdst[n] := Abs ( s0 [n] - s1 [n] ) * q
 else
    for n := 0 to cnt - 1 do
         pdst[n] :=     ( s0 [n] - s1 [n] ) * q;
 result := cnt;
end; // Substract

function  Divide (jp: TCalcJobParams): Integer;
var
    dst: TArray32;
    n, i, rcnt, cnt: Integer;
    q: Single;

    s0, s1, pdst: PSingleArray;
begin

 result := 0;
 cnt := jp.srcd.ItemsCount;
 if cnt <= 0 then exit;
 s0 := jp.srcd.GetValues(0);
 s1 := jp.srcd.GetValues(1);

 if jp.dst_count = 0 then
   pdst := s0
 else
  begin
   dst := jp.dst32 [0];
   pdst := jp.srcd.InitDest (dst);
  end;

 q := jp.fParams [0];
 if q = 0 then q := 1;
 i := 0;
 rcnt := (cnt shr 3) shl 3; // = div 8 * 8, sample from 67 = 64
 if rcnt >= 0 then

 try
  if q = 1 then
   begin
    {$IFDEF OPTALGS}
      while (i < rcnt) do
       begin
        pdst[i + 0] := ( s0 [i + 0] / s1 [i + 0] );
        pdst[i + 1] := ( s0 [i + 1] / s1 [i + 1] );
        pdst[i + 2] := ( s0 [i + 2] / s1 [i + 2] );
        pdst[i + 3] := ( s0 [i + 3] / s1 [i + 3] );
        pdst[i + 4] := ( s0 [i + 4] / s1 [i + 4] );
        pdst[i + 5] := ( s0 [i + 5] / s1 [i + 5] );
        pdst[i + 6] := ( s0 [i + 6] / s1 [i + 6] );
        pdst[i + 7] := ( s0 [i + 7] / s1 [i + 7] );
        Inc (i, 8);
       end;
     {$ENDIF}
    for n := i to cnt - 1 do
        pdst[n] := ( s0 [n] / s1 [n] )
   end
  else
   begin
    {$IFDEF OPTALGS}
      while (i < rcnt) do
       begin
        pdst[i + 0] := ( q * s0 [i + 0] / s1 [i + 0] );
        pdst[i + 1] := ( q * s0 [i + 1] / s1 [i + 1] );
        pdst[i + 2] := ( q * s0 [i + 2] / s1 [i + 2] );
        pdst[i + 3] := ( q * s0 [i + 3] / s1 [i + 3] );
        pdst[i + 4] := ( q * s0 [i + 4] / s1 [i + 4] );
        pdst[i + 5] := ( q * s0 [i + 5] / s1 [i + 5] );
        pdst[i + 6] := ( q * s0 [i + 6] / s1 [i + 6] );
        pdst[i + 7] := ( q * s0 [i + 7] / s1 [i + 7] );
        Inc (i, 8);
       end;
     {$ENDIF}

    for n := i to cnt - 1 do
        pdst[n] := ( q * s0 [n] / s1 [n] );
   end;


  except
   on E: EZeroDivide do
     PrintError('FinMath:Divide - ошибка деления на ноль');
  end;
 result := cnt;
end; // Divide


function TPSLFilter (jp: TCalcJobParams): Integer;
const
   SIG_TP = 5;
   SIG_SL = 6;
// TODO: переделать под POST_PROCESS
// фильтр обработки cигналов по алгоритмам TakeProfit/StopLoss
var
   srcd: TSyncData;
   mig_type: Integer;

   abs_levels, allow_short, ap_zone, close_pos, float_stop, imm_exec, sig_pass: Boolean;
   i, n, nrng, cnt, optype, stgf, flags: Integer;
   s0, s1, s2, s3, s4, s5: PSingleArray;
   tp_long, tp_short, sl_long, sl_short, tp_drop: Single;
   cv, hv, lv, lot_size, target, target_q: Single;
   target_sl, target_tp: Single;

   active_sig, asig_price, trade_price, src_sig: Single;
   in_sigpos, sigpos, prv_sigpos: Single;

   sigv, trade_vol, limit_vol: Integer;
   rsig, cur_pos, src_pos, chg_pos, curr_free: Single;

                procedure       UpdateTargets;
                begin
                 if cur_pos = 0 then exit;
                 target_q := 1;
                 if abs_levels then
                    target_q := 1 / lot_size
                 else
                    target_q := target_q * cv;
                 if (cur_pos > 0) then
                  begin
                   target_tp := trade_price + target_q * tp_long;
                   target_sl := trade_price - target_q * sl_long;
                  end
                 else
                  begin
                   target_tp := trade_price - target_q * tp_short;
                   target_sl := trade_price + target_q * sl_short;
                  end;
                end; // UpdateTargets


begin
  result := 0;
  srcd := jp.srcd;
  cnt := srcd.ItemsCount;
  if (cnt <= 0) then exit;
  // -------------------- INIT DATA ------------------------ //
  s0 := jp.srcd.GetValues(0);     // SIG                - сигнал для перезахода (восстановления позиции)
  s1 := jp.srcd.GetValues(1);     // SIGPOS             - текущая позиция (обрываемая фильтром)
  s2 := jp.srcd.GetValues(2);     // SIGPRICE           - цена исполнения заявок
  s3 := jp.srcd.GetValues(3);     // high
  s4 := jp.srcd.GetValues(4);     // low
  s5 := jp.srcd.GetValues(5);     // close


  stgf := jp.iParams [0];
  flags := jp.iParams [1];

  trade_vol := jp.iParams [2];
  limit_vol :=   jp.iParams [3];


  allow_short := (stgf and OPTF_BLOCK_SHORT = 0);

  close_pos := (flags and TPSL_CLOSE_POS <> 0);
  imm_exec :=  (flags and TPSL_IMM_EXEC <> 0);
  float_stop := (flags and TPSL_FLOAT_STOP <> 0);

  mig_type := 0; // тупой перенос сигнала
  if (flags and TPSL_SMART_MIGRATE) <> 0 then
                mig_type := 1; // улучшающая миграция сигнала

  abs_levels := (flags and TPSL_ABS_LEVELS <> 0);

  if abs_levels then
    begin
     sl_short := jp.fParams [0];
     sl_long :=  jp.fParams [1];
     tp_short := jp.fParams [2];
     tp_long :=  jp.fParams [3];
     tp_drop :=  jp.fParams [4];

     if tp_long <= 0 then  tp_long :=  1e20;
     if tp_short <= 0 then tp_short := 1e20;
     if sl_long <= 0 then  sl_long :=  1e20;
     if sl_short <= 0 then sl_short := 1e20;
    end
  else
    begin
     sl_short := jp.fParams [0] * 0.01;
     sl_long :=  jp.fParams [1] * 0.01;
     tp_short := jp.fParams [2] * 0.01;
     tp_long :=  jp.fParams [3] * 0.01;
     tp_drop :=  jp.fParams [4] * 0.01;

     if tp_long <= 0  then tp_long := 0.2;
     if tp_short <= 0 then tp_short := 0.2;
     if sl_long <= 0  then sl_long := 1;
     if sl_short <= 0 then sl_short := 1;
    end;

 lot_size := jp.fParams [5];
 if lot_size <= 0 then lot_size := 1;

 trade_price := 0;
 cur_pos := 0;
 curr_free := 0;
 src_pos := 0;
 active_sig := 0;
 asig_price := 0;
 prv_sigpos := 0;
 // режим обработки фильтра - замещение существующих сигналов и корректировка цены исполнения
 for n := 0 to cnt - 1 do
  begin
   curr_free := 0;
   src_sig := s0 [n];
   in_sigpos := s1 [n];
   if (n > 0) then
     begin
      if (in_sigpos = 0)  and (prv_sigpos <> 0) then src_sig := $C * Sign(-prv_sigpos);
      if (in_sigpos <> 0) and (prv_sigpos <> in_sigpos) then src_sig := Sign (in_sigpos - prv_sigpos); // простой обезличенный сигнал
     end;

   prv_sigpos := in_sigpos;

   hv := s3 [n];
   lv := s4 [n];
   cv := s5 [n];

   rsig := src_sig;

   // регистрация текущего сигнала
   if Abs(src_sig) > 0.5 then
      begin
       trade_price := s2 [n];
       ProcessTrade (@src_pos, @curr_free, trade_price, trade_vol, limit_vol, src_sig, allow_short);
       rsig := Sign (src_pos - cur_pos) * Abs(src_sig);
       cur_pos := src_pos;      // -------------------------- синхронизация позиции -------------------- //
       active_sig := 0;
       asig_price := trade_price;
       UpdateTargets;
      end;

   sig_pass := FALSE;

   if active_sig <> 0 then
      begin
       // активный сигнал - переворотный. Уровни пробоя перемещаются, против него в случае включения соотв. оптимизации
       sig_pass := ( (active_sig > 0) and (cv > asig_price) ) or  ( (active_sig < 0) and (cv < asig_price) );
      end;


   if sig_pass then
         begin
          trade_price := cv + cv * Sign(active_sig) * 0.00025; // ухудшенная цена (купить дороже, шортить дешевле)
          chg_pos := cur_pos;
          ProcessTrade(@cur_pos, @curr_free, trade_price, trade_vol, limit_vol, active_sig, allow_short);
          rsig := Sign(cur_pos - chg_pos) * 5;
          active_sig := 0;
          asig_price := 0;
         end;

   // реверсирование позиции по достижению цели
   if (cur_pos <> 0) and (trade_price > 0) then
     begin
      sigv := 0;
      // переворот позиции в шорт, по профиту или убытку
      if (cur_pos > 0) then
       begin
        if (hv > target_tp) and (src_sig <= 0) then sigv := -SIG_TP;
        if (lv < target_sl) then sigv := -SIG_SL;
        if float_stop then target_sl := Max(target_sl, cv - target_q * sl_long); // обновление цели
       end
      else
      // переворот позиции в лонг по профиту или убытку
       begin
        if (lv < target_tp) and (src_sig >= 0) then sigv := +SIG_TP;
        if (hv > target_sl) then sigv := +SIG_SL;
        if float_stop then target_sl := Min (target_sl, cv + target_q * sl_short); // обновление цели
       end; // rpos < 0

      if Abs(sigv) = 5 then
         begin
          active_sig := sigv;
          if (tp_drop > 0) then
             asig_price := IfV( active_sig > 0, cv + target_q * tp_drop, cv - target_q * tp_drop ) // по преодолению уровня!
          else
             asig_price := IfV( active_sig > 0, hv, lv );
          if imm_exec then
            begin
             rsig := sigv;
             trade_price := cv + cv * Sign(active_sig) * 0.00025;
            end
          else
             active_sig := sigv;
         end;
       // стоп-лосевые сигналы проходят вне очереди
       if Abs(sigv) = 6 then
         begin
          rsig := sigv;
          trade_price := cv + cv * Sign(active_sig) * 0.00025;
         end;

      // ------------------------------- SIGNAL MIGRATION ------------------------------
      if Abs (active_sig) = SIG_TP then
        // несколько вариантов миграции сигнала переворота по профиту | лосю
        begin
         if (tp_drop > 0) then
          // оптимизация включает отступ
          asig_price := IfV( active_sig > 0, Min(asig_price, cv + target_q * tp_drop), Max(asig_price, cv - target_q * tp_drop) )
         else
          case mig_type of
           0: asig_price := IfV( active_sig > 0, hv, lv);
           1: asig_price := IfV( active_sig > 0, Min(asig_price, hv), Max(asig_price, lv));
          end;
        end;

     end;


   if Abs (rsig) > 0.5 then
     begin
      if  ( Abs(rsig) = SIG_SL ) or ( ( Abs(rsig) = SIG_TP )  and ( close_pos ) ) then
                rsig := $0C * Sign (rsig); // модификация сигнала на закрывающий
      ProcessTrade (@cur_pos, @curr_free, trade_price, trade_vol, limit_vol, rsig, allow_short);
      UpdateTargets;
     end;

   s0 [n] := rsig;              // для наблюдения
   s1 [n] := cur_pos;
   s2 [n] := trade_price;
  end;

end;  // TPSLFilter


function InitValues (jp: TCalcJobParams): Integer;
// простая инициализирующая заполнялка
var
   ref: TArray32;
   dst: TArray32;
   pdata: PSingleArray;
   fval: Single;
   i: Integer;
begin
 result := 0;
 ref := jp.src32[0];
 dst := jp.dst32[0];
 if (ref = nil) or (dst = nil) or (jp.srcd.ItemsCount = 0) then exit;

 fval := jp.fParams [0];
 pdata := jp.srcd.InitDest(dst);
 if pdata <> nil then
    for i := dst.iFirstValid to dst.iLastValid do pdata [i] := fval;
 result := dst.ValidCount;
end; // InitValues

function CalcEquity (jp: TCalcJobParams): Integer;
// TODO: переделать под SIGPOS
var
    srcd: TSyncData;
    dst_open, dst_close, psout, trades: TArray32;
     backRef, flags: Integer;
    start_money, comission, mis_percent, total_comission, slip: Single;
    // -----------------------------
    n, cnt: Integer;
    pdst_open, pdst_close, pprice_out, ptrades: PSingleArray;
    ps_open, ps_high, ps_low, ps_close: Single;

    psv, pst, dir_sign: Single;

    sigpos, dstpos, prvpos: Single;

    lot_size, unit_size, rest_open, rest_close: Single;          // объем всех средств в денежном эквиваленте
    cur_free: Single;             // сколько свободных денег
    market_pos: TMarketUnit;
    trade_vol, qty, lim_vol: Integer;
    trade_pass, price_out_bar: Boolean;
    antisys, allow_short, allow_opt, bDump, last: Boolean;
    sig_cnt: Integer;   // количество сигналов/переворотов

    s0, s1, s2, s3, s4, s5, usz: PSingleArray;
begin
   // алгоритм требует поставки данных PRICE,OPEN,CLOSE по инструменту
   // рассчет условной доходности в процентном значении
   srcd := jp.srcd;
   dst_open := jp.dst32[0];
   dst_close := jp.dst32[1];
   psout := jp.dst32[2];
   trades := jp.dst32[3];

   // jp.iParams[0]; // ignored?
   backRef := jp.iParams [1];
   flags :=   jp.iParams [2];
   lim_vol := Max(1, jp.iParams [3]);

   start_money := jp.fParams [0];
   mis_percent := jp.fParams [1];
   comission := jp.fParams [2];
   lot_size := jp.fParams [3]; // лот может быть неполным (для S&P 500).
   if lot_size <= 0 then lot_size := 1;
   // ------------ data init
   result := 0;
   cnt := srcd.ItemsCount;
   if (cnt <= 0) or (psout = nil) then exit;

   pdst_open := srcd.InitDest(dst_open);
   pdst_close := srcd.InitDest(dst_close);
   pprice_out := srcd.InitDest(psout);
   ptrades := srcd.InitDest(trades);
   // ------------ sys init
   if (start_money <= 0) then start_money := 20000;
   cur_free := start_money;
   // ------------ processing

   antisys :=     (flags and OPTF_ANTI_SYSTEM <> 0);
   allow_short := (flags and OPTF_BLOCK_SHORT = 0);
   allow_opt :=   (flags and OPTF_BLOCK_OPT = 0);


   // источник исполненных сигналов берется из прошлого, для не RTM-исполнения
   Dec (srcd.Offsets[0], backRef);
   Dec (srcd.Offsets[1], backRef);

   s0 := jp.srcd.GetValues(0);     // sigpos
   s1 := jp.srcd.GetValues(1);     // trade price
   s2 := jp.srcd.GetValues(2);     // open
   s3 := jp.srcd.GetValues(3);     // high
   s4 := jp.srcd.GetValues(4);     // low
   s5 := jp.srcd.GetValues(5);     // close
   usz := jp.srcd.GetValues(6);     // unit-size

   // инициация первого бара
   pdst_open [0] := 100;
   pdst_close [0] := 100;
   pprice_out [0] := s1[0];

   pst := s2[0]; // at open!

   slip := 0;
   prvpos := 0;

   if (flags and OPTF_FIXED_SLIP = 0) then
       mis_percent := mis_percent * 0.01
   else
       slip := mis_percent;


   sig_cnt := 0;

   trade_vol := 0;
   market_pos := 0;


   for n := backRef to cnt - 1 do
    begin
    // цены для рассчета сделки
     ps_open  := s2 [n];
     ps_close := s5 [n];


     rest_open := ps_open * market_pos + cur_free;  // что на открытии бара


     sigpos := s0 [n];       // исполненный сигнал

     if antisys then sigpos := -sigpos;


     ptrades[n] := 0;

     // проверка на ограничение объема
     if Abs(sigpos) > lim_vol then sigpos := lim_vol * Sign(sigpos);

     trade_pass := (  sigpos <> prvpos );

     if trade_pass then
      begin
       unit_size := usz [n];
       if unit_size <= 0 then unit_size := 1;
       dstpos := sigpos * unit_size * lot_size;             // ожидаемая рыночная позиция

       trade_vol := Round ( dstpos - market_pos );
       prvpos := sigpos;
       if trade_vol = 0 then trade_pass := false;
      end;

     if trade_pass then
       begin
        dir_sign := Sign (trade_vol);
        qty :=  Abs( trade_vol );
        psv :=  s1 [n];        // цена для совершения виртуальной сделки

        ps_high  := s3 [n];
        ps_low   := s4 [n];


        Inc (sig_cnt);
        pst := psv;

        if (flags and OPTF_FIXED_SLIP = 0) then
          begin
           slip := pst * mis_percent;
           if slip > 20 then slip := Round(slip);
          end;

        price_out_bar := FALSE;
        // Покупка по несколько большей цене или продажа по несколько меньшей цене
        // ++ Проверка, если цена не вписывается в тело бара, то совершить сделку в районе CLOSE+-SLIPPAGE
        // ++ 6.12.08: сильное проскальзывание для опоздавших сделок
        if dir_sign > 0 then
          begin
           // покупка = если попытка осуществить сделку ниже бара, она проводится с опозданием по цене близкой к CLOSE
           if pst < ps_low then
                 begin
                  pst := ps_close + slip;
                  price_out_bar := TRUE;     // в этом баре заявка не исполнилась
                 end;

           pst := pst + slip;

           if backRef > 0 then // только для стратегий отложенного исполнения
             begin
              if allow_opt and (not price_out_bar) then pst := Min(pst, ps_open + 2 * slip); // оптимизация цены
              pst := Min(pst, ps_high);  // не слишком ухудшать цену
             end;
          end
        else
          begin
           // продажа = если попытка осуществить сделку выше бара, она проводится с опозданием по цене близкой к CLOSE
           if pst > ps_high then
                 begin
                  pst := ps_close - slip;
                  price_out_bar := TRUE;     // в этом баре заявка не исполнилась
                 end;

           pst := pst - slip;

           if backRef > 0 then // только для стратегий отложенного исполнения
             begin
              if allow_opt and (not price_out_bar) then pst := Max(pst, ps_open - 2 * slip); // оптимизация цены
              pst := Max(pst, ps_low);  // не слишком ухудшать цену
             end;
          end;

        // раундинг цены сделки
        if (pst > 30000) then pst := Round(pst / 5) * 5
           else
             if (pst > 5000) then pst := Round(pst);

        total_comission := Abs(trade_vol) * comission; // базовая комиссия (брокер+бирж. сбор) для фучей
        if (total_comission > 0) and (comission < 0.5) then
           total_comission := total_comission * pst; // комиссия ММВБ за денежный объём сделки

        cur_free := cur_free - (trade_vol * pst) - total_comission; // корректировка счета-баланса
        market_pos := market_pos + trade_vol;
        pst := trade_vol;
        while ( Abs(pst) > 1) do pst := pst * 0.1; // minimize
        ptrades[n] := pst; // 1 / 1000
       end; // on-process the trade

     rest_close := ps_close * market_pos + cur_free;

     if flags and OPTF_EQTY_PRCNT <> 0 then
      begin
       pdst_open [n]  := 100 * rest_open / start_money;
       pdst_close [n] := 100 * rest_close / start_money;
      end
     else
      begin
       pdst_open [n]  := rest_open;
       pdst_close [n] := rest_close; 
      end;

     pprice_out[n] := pst;
    end;

end; // CalcEquity


function ZigZagLevels (jp: TCalcJobParams): Integer;
{ Формирование уровней относительно зиг-зага, считаемого по абсолютным/процентным
  величинам.

  Алгоритм: При обнаружении текущего загиба, на требуемую величину, производится
   перенос экстремума, с "прорисовкой" уровней предыдущего движения. Для конечных
   значений рисуются уровни относительно последнего экстремума.

  Участнеги:
    * плавающий (текущий) экстремум - часто обновляется, является точкой вероятного
      излома, но не является фактическим изломом - от него не строятся уровни
    * предыдущий экстремум - используется для определения текущего направления
}
var
   srcd: TSyncData;
   break_lim, level_lim: Single; // ограчения излома и уровня отступа
   zvals, lvals: TArray32;
   pzv, plv: PSingleArray;
   i, n, cnt, lstart, n_last: Integer;
   ex_pos: Integer;      // позиция экстремума
   firstv: Single;
   zv, prv_ex, cur_ex, curv: Single;
   cur_dir, prv_dir, lvdir: Integer;
begin
 srcd := jp.srcd;
 zvals := jp.dst32[0];
 lvals := jp.dst32[1];
 break_lim := jp.fParams[0];
 level_lim := jp.fParams[1];
 result := 0;
 cnt := srcd.ItemsCount;
 if cnt <= 0 then exit;
 pzv := srcd.InitDest(zvals);
 plv := srcd.InitDest(lvals);
 srcd.Read(0);
 firstv := srcd.Values[0];
 cur_ex := firstv;
 prv_ex := firstv;
 zv := firstv;
 lstart := 0;
 ex_pos := 0;
 // ------------------------------------------
 n_last := cnt - 1;
 for n := 1 to n_last do
 begin
  srcd.Read(n);
  curv := srcd.Values[0];
  if (Round (curv * 100) = 15255) then
    asm
     nop
    end;
  lvdir := 0;
  // направление текущего движения
  cur_dir := Sign (curv - cur_ex);
  prv_dir := Sign (cur_ex - prv_ex);
  if (cur_dir = 0) then continue;
  // усиление предыдущего движения (расширение)
  if ((prv_dir > 0) and (curv > cur_ex)) or
     ((prv_dir < 0) and (curv < cur_ex)) then
    begin
     cur_ex := curv;
     ex_pos := n;
    end;
  // слом вверх, или вниз
  if ((curv > cur_ex + break_lim) and (prv_dir <= 0 )) or
     ((curv < cur_ex - break_lim) and (prv_dir >= 0 )) then
    begin
     zv := prv_ex;
     prv_ex := cur_ex;
     lvdir := prv_dir;
     cur_ex := curv;
    end;
  if (n = n_last) then
   begin
    lvdir := prv_dir;
    zv := prv_ex;
    ex_pos := cnt;
   end;
  if (lvdir = 0) then continue;
  for i := lstart to ex_pos - 1 do
   begin
    pzv[i] := zv;
    plv[i] := zv + lvdir * level_lim;
   end;
  lstart := ex_pos;
  ex_pos := n;
 end; // for
end;

function CalcCross(jp: TCalcJobParams): Integer;
var
   srcd: TSyncData;
   sigc: TArray32;
   psig: PSingleArray;
   ofst: Single;
   cnt, n: Integer;
   a, b, sigv, sigl: Single;
begin
 srcd := jp.srcd;
 sigc := jp.dst32[0];
 ofst := jp.fParams[0];
 sigl := Max(1, jp.fParams[1]);

 result := 0;
 cnt := srcd.ItemsCount;
 if cnt <= 0 then exit;
 psig := srcd.InitDest(sigc);
 sigv := 0;
 for n := 0 to cnt - 1 do
  begin
   srcd.Read(n);
   a := srcd.Values[0];
   b := srcd.Values[1];
   // если сейчас состояние А пересекла Б + ofst
   if (sigv >= 0) and (a < b - ofst) then sigv := -sigl;
   if (sigv <= 0) and (a > b + ofst) then sigv := +sigl;
   if sigv <> 0 then
    asm
     nop
    end;
   psig [n] := sigv;
  end;
 result := cnt;
end;

function VolByATR (jp: TCalcJobParams): Integer;
var
   vUnitSize: TArray32;
   flags, n, cnt: Integer;
   hv, lv, cv, pcv: Single;
   money, pcost, mvolat: Single;
   usz, np, atr, prv_atr, tr: Single;
   hdta, ldta: Single;
   pdst, s0, s1, s2: PSingleArray;


begin

 cnt := jp.srcd.ItemsCount;
 result := cnt;
 vUnitSize := jp.dst32 [0];
 if (cnt <= 0) or (vUnitSize = nil) then exit;
 // -------------------- INIT DATA ------------------------ //
 s0 := jp.srcd.GetValues(0);     // high
 s1 := jp.srcd.GetValues(1);     // low
 s2 := jp.srcd.GetValues(2);     // close

 flags := jp.iParams[0];
 if flags = 0 then;

 np := jp.iParams[1];
 money := Max ( jp.fParams[0] / 100, 0 ); // к процентному виду
 pcost := jp.fParams[1];                  // point cost

 pdst := jp.srcd.InitDest (vUnitSize);

 if pcost <= 0 then
   begin
    pcost := 10;
{    if s0[0] < 100000 then pcost := 5;
     if s0[0] < 50000 then pcost := 1;
     if s0[0] < 10000 then pcost := 0.1;
     if s0[0] < 2000 then pcost := 0.01;
     if s0[0] < 100 then pcost := 0.001; }
   end;

 prv_atr := 0;
 pcv := 0;
 // TODO: сделать фиксинг по дневкам в зависимости от флажков
 usz := 1;

 if np <= 2 then np := 20;

 for n := 0 to cnt - 1 do
  begin
   hv := s0 [n];
   lv := s1 [n];
   cv := s2 [n];
   hdta := hv - pcv;
   ldta := pcv - lv;

   pdst [n] := Max(1, usz);     // сохранять с опозданием в один бар, для стратегий реального-времени


   {if flags and 1 <> 0 then
    begin
     hdta := Abs(hdta);
     ldta := Abs(ldta);
    end; }

   tr := Max(hv - lv, hdta);
   tr := Max(tr, ldta);
   atr := ( (np - 1)  * prv_atr + tr ) / np;
   mvolat := (atr * pcost);
   if (mvolat > 0) and (n >= np) then
      usz := Floor ( money / mvolat )
   else
      usz := 1;


   prv_atr := atr;
   pcv := cv;
  end;

end; // VolByATR

function VolByPrice (jp: TCalcJobParams): Integer;
var
   vUnitSize: TArray32;
   n, cnt: Integer;
   cv: Single;
   money: Single;
   usz: Single;
   pdst, s0: PSingleArray;
begin
 cnt := jp.srcd.ItemsCount;
 result := cnt;
 vUnitSize := jp.dst32 [0];
 if (cnt <= 0) or (vUnitSize = nil) then exit;
 // -------------------- INIT DATA ------------------------ //
 s0 := jp.srcd.GetValues(0);     // close or high
 money := Max (0, jp.fParams[0]); //
 pdst := jp.srcd.InitDest (vUnitSize);
 usz := 1;
 for n := 0 to cnt - 1 do
  begin
   cv := s0 [n];
   pdst [n] := Max(1, usz);     // сохранять с опозданием в один бар, для стратегий реального-времени
   usz := Floor ( Money / cv );
  end;
end; // VolByATR

procedure       InitModule;
begin
 if gFuncStorage = nil then
    FinMath.InitModule;
 if gFuncStorage = nil then
   begin
    PrintError('Не создано общее хранилище функций модуля FinMath.');
    exit;
   end;
 with gFuncStorage do
  begin
   RegFunc ('MEDIAN',   CalcMedian, 2, 1);
  {
   RegFunc ('RSI',      CalcRSI, 1, 1);
   RegFunc ('SMA',      CalcSMA, 1, 1);
   RegFunc ('EMA',      CalcEMA, 1, 1);
   RegFunc ('ZIGL',     ZigZagLevels, 1, 2);
   // ----------- 2 in data -----------
   RegFunc ('CROSS',    CalcCross, 2, 1);

   RegFunc ('DIV',      Divide, 2, 1);
   RegFunc ('SUB',      Substract, 2, 1);
   RegFunc ('SUM',      CalcSUM, 2, 1);
   RegFunc ('HTG',      CalcHistogram, 2, 2);
   RegFunc ('HTG3',     CalcHistogram3, 2, 3);
   RegFunc ('INIT_VAL',  InitValues, 1, 1);
   RegFunc ('VolByATR',  VolByATR, 3, 1);
   RegFunc ('VolByPrice',  VolByPrice, 1, 1);

   // ----------- 5 in data ------------
   // RegFunc ('EQFILTER', EQFilter, 5, 0);
   // RegFunc ('SCFILTER', SCFilter, 5, 0);
   RegFunc ('TPSLFILTER', TPSLFilter, 6, 0);
   // ----------- 6 in data ------------
   RegFunc ('EQUITY',   CalcEquity, 6, 4);
   // RegFunc ('OSCFILTER', OSCFilter, 6, 2);
   // ----------------------------------
   // RegFunc ('TMFILTER', TMFilter, 7, 2);
   }
  end;
end; // InitModule




procedure       FinalizeModule;
begin
 // no actions...
end;

initialization
 InitModule;
finalization
 FinalizeModule;
end.
