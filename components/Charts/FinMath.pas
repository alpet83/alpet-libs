unit FinMath;

interface
uses Windows, SysUtils, Classes, Misc, StrClasses, ChartData, UNIArray, ArrayTypes, Math, WThreads, DateTimeTools, UTITypes, UTIConst,
     InstrDictionary, PackedStorage, SMTypes;

{
   Логика рассчета элементов для отрисовки графика.
    В каждом результирующем массиве получаются (требуемое + резервное) количество элементов.
    После рассчета используются только требуемые - для отображения графика.

}
const
    SIG_SELL = -1;
    SIG_BUY  = +1;
    CJ_PARAM_COUNT = 16;


type
    TDataList32 = array [0..CJ_PARAM_COUNT - 1] of TArray32;
    PDataList32 = ^TDataList32;

    // Класс синхронизированных исходных данных
    TSyncData = class
    private

     FDataCount: Integer;
     FData: TDataList32;
     FItemsCount: Integer;
    public

     Offsets: array [0..CJ_PARAM_COUNT - 1] of Integer;
      Values: array [0..CJ_PARAM_COUNT - 1] of Single;

     property                   ItemsCount: Integer read FItemsCount;
     { Methods }
     procedure                  Init (srcd: TDataList32; cnt: Integer);
     function                   InitDest (dst: TArray32): PSingleArray;
     procedure                  Read (nIndex: Integer);
     function                   GetValues(nData: Integer): PSingleArray;

    end; // TSyncData

   TDataRefItem = class
   public
    pData: PSingleArray;
    nSize: Integer;
    iFirstValid, iLastValid: Integer;
    procedure           Load (psrc: Pointer);
    procedure           Store (pdst: Pointer);
   end; // TDataCacheItem

   TCalcDataCache = class (TStrMap)
   protected
    FCacheData: Pointer;
    FCacheUsed: Integer;
    FCacheSize: Integer;
    FCacheHits: Integer;
    function            GetRef(const sIndex: String): TDataRefItem;
    procedure           ResizeCache (newSize: Integer);
   public


    { Props }
    property            CacheHits: Integer read FCacheHits;

    property            Map [const sIndex: String]: TDataRefItem read GetRef;

    { C & D}
    constructor         Create;
    destructor          Destroy; override;
    { Methods }
    procedure           Clear; override;
    procedure           InsertData(const sIndex: String; a32: TArray32);
   end; // TCalcDataCache


    // комплекс параметров передаваемых функции рассчета
   TCalcJobParams = class
   private
     function                   UpdateDataRefs (slParams: TStrMap; const suffix: String; pData: PDataList32): Integer;

   public
     src32, dst32: TDataList32;
     src_count, dst_count: Integer;     // количества параметров данных
     srcd: TSyncData;
     InData, OutData: TDataMap;
     rtm_exec: Boolean;                 // исполнение сигнала в реальном времени
     nFirst, nLast: Integer;            // индексы в массиве WorkData используемой стратегии
     stat_result: TStrMap;              // общая статистика по рассчетам
     iinf: TInstrInfo;
     data_ref: TTicksHistory;      // тики для скальперских систем


     res_ofst: array [0..CJ_PARAM_COUNT - 1] of Integer; // смещение выходных данных
     iParams: array  [0..CJ_PARAM_COUNT - 1] of Integer;
     fParams: array  [0..CJ_PARAM_COUNT - 1] of Single;
     rParams: array  [0..CJ_PARAM_COUNT - 1] of TSecondsRange;

     { C & D }
     constructor                Create;
     destructor                 Destroy; override;
     { Methods }

     procedure                  InitData (slParams: TStrMap);
     procedure                  InitParams (slParams: TStrMap);

     procedure                  SetFParams ( f0, f1, f2, f3, f4, f5, f6, f7: Single );  // init first 8 params
     procedure                  SetIParams ( i0, i1, i2, i3, i4, i5, i6, i7: Integer );

     procedure                  OffsetData (pdl: PDataList32; n_index: Integer);

   end; // TCalcJobParams

    // класс осуществляющий рассчет данных
    TDataCalcJob = class (TBaseCalcJob)
    private
      procedure SplitParams(const sParams, suffix: String);
    protected
     row: TStrMap;
     FOwner: TObject;           // ссылка на торговую систему, владельца
     cpt: TProfileTimer;
     src_params: String;
    public
     Params: TStrMap;

     calc_cache: TCalcDataCache;        // неблокируемый кэш рассчитанных прежде данных


     JParams: TCalcJobParams;   // обработанные параметры

     property   Owner: TObject read  FOwner;
     { C & D }

     constructor               Create (AOwner: TObject);
     destructor                Destroy; override;

     { Methods }
     procedure                 AddParam (const sParam: String; obj: TObject = nil);
     procedure                 AddParams (const sParList: String);
     procedure                 AddFuncParams (const sFunc, src, dst: String;
                                              const sIntParams: String = '';
                                              const sFloatParams: String = '');

     function                  Calculate (InData, OutData: TDataMap; resv_count, work_count, nFirst, nLast: Integer): Integer; override;
    end; // TDataCalcJob


    TCalcJobFunc = function (jp: TCalcJobParams): Integer;

    TFunctionDesc = class
    public
     in_data_cnt: Integer;
     out_data_cnt: Integer;
     i_params_cnt, f_params_cnt, r_params_cnt: Integer;
     the_func: TCalcJobFunc;
    end; // TCalcJobFunc

    TFunctionStorage = class (TStrMap)
    private
      function GetItems(const sIndex: String): TFunctionDesc;
    protected
    public

     property                   Items[const sIndex: String]: TFunctionDesc read GetItems; default;
     { C  & D }
     constructor                Create;
     { methods }
     function                   RegFunc (const sName: String; AFunc: TCalcJobFunc; in_dc, out_dc: Integer): TFunctionDesc;
    end;  // TFunctionStorage

var
  PSPool: TWorkThreadPool = nil;
  gFuncStorage: TFunctionStorage = nil;

procedure       InitModule;
procedure       FinalizeModule;



implementation
uses TradeBase, BaseLib;

{ TDataCalcJob }

procedure TDataCalcJob.SplitParams (const sParams, suffix: String);
var n: Integer;
begin
 row.Split(',', sParams);
 for n := 0 to row.Count - 1 do
     if row[n] <> '' then AddParam (suffix + Byte2Hex (n) + '=' + row [n] ) else break;
end;

procedure TDataCalcJob.AddFuncParams;
begin
 AddParam ('FUNC=' + sFunc);
 if src_params <> '' then src_params := src_params + '&';
 src_params := src_params + src;
 if sIntParams <> '' then src_params := src_params + '_' + sIntParams;
 if sFloatParams <> '' then src_params := src_params + '_' + sFloatParams;
 SplitParams (src, 'SRC_');
 SplitParams (dst, 'DST_');
 SplitParams (sIntParams, 'INTPARAM_');
 SplitParams (sFloatParams, 'FLOATPARAM_');
 JParams.InitParams (Params);
end;

procedure TDataCalcJob.AddParam(const sParam: String; obj: TObject);
begin 
 Params.AddObject (sParam, obj);
end;

procedure TDataCalcJob.AddParams(const sParList: String);
begin
 with Params do
  begin
   Delimiter := ';';
   DelimitedText := DelimitedText + ';' + sParList;
  end;
end;

function TDataCalcJob.Calculate;

var
   srq, sn, func: String;
   range_size, n: Integer;
   src: TMatrixArray;
   dst, closev: TArray32;
   last_close: Integer;
   elapsed, ef: Single;
   pst: TProfStat;
   ref: TDataRefItem;
   fdesc: TFunctionDesc;
   sfx: String;

begin
 // данная функция позволяет вычислить необходимое количество данных для заполнения фрейма [VisibleBars]
 // или в режиме боевого рассчета - обновлять или добавлять данные в массивы

 result := 0;
 // need_count := nLast - nFirst + 1;

 range_size := (nLast - nFirst);

 resv_count := Min (resv_count, nFirst);
 nFirst := nFirst - resv_count;


 JParams.nFirst := nFirst;
 JParams.nLast := nLast;



 JParams.InData := InData;
 JParams.OutData := OutData;
 JParams.InitData (Params);

 srq := Params.Values ['RQS'];
 closev := InData['CLOSE'];
 func := Params.Values ['FUNC'];

 // TODO: add fast request 'SYNC'
 if ( (srq = 'COPY') or (srq = 'INT2FLOAT') ) and (InData <> OutData) then
   begin
    for n := 0 to JParams.src_count - 1 do
     begin
      src := JParams.src32[n];
      if (src.Count <= 0) or (n >= JParams.dst_count) then continue;
      dst := JParams.dst32[n];

      dst.iRight := dst.iLastValid;

      if src <> dst then
       begin
        dst.Clear; // overwrite-mode
        if (srq = 'COPY') then
          begin
           if (func = 'DIRECT') then
             begin
              dst.CopyItems (src, 0, src.Count - 1);
              dst.iFirstValid := src.iFirstValid;
              dst.iLastValid := src.iLastValid;
             end
           else
             begin
              dst.CopyItems (src, nFirst, nLast);
              dst.iFirstValid := TTradeSystem (FOwner).DataOrigin;
              dst.iLastValid := dst.Count - 1;
             end;
           dst.iRight := dst.iLastValid;
          end;

        dst.iLeft := dst.iRight - range_size;

        if (srq = 'INT2FLOAT') then
            dst.ImportIntItems (src, nFirst, nLast);

       end;
     end;
   end;


 if (srq = 'CALC') and (func <> '') then // рассчет мувинга
   begin
    ef := 0;
    cpt.Start;
    ref := nil;
    result := 0;
    sfx := '';
    sn := '';
    if ( calc_cache <> nil ) and ( JParams.dst_count = 1 ) then
      begin
       sn := func + ':' + src_params + '@' + IntToStr ( JParams.res_ofst [0] );
       ref := calc_cache.Map[sn];
      end;

    if ref <> nil then
     begin
      ef := cpt.Elapsed;
      // копирование расчитанных данных из кэша
      dst := JParams.dst32[0];
      dst.AddRows (ref.nSize div SizeOf(Single));
      dst.iFirstValid := ref.iFirstValid;
      dst.iLastValid := ref.iLastValid;
      ref.Store ( dst.FloatItems );
      sfx := '_CH';
     end
    else
     begin
      // вызов функции для рассчета данных
      fdesc := gFuncStorage [func];
      if Assigned (fdesc) and Assigned (fdesc.the_func) then
        begin
         result := fdesc.the_func ( jParams );
         ef := 20000 + cpt.Elapsed;
        end
      else
        PrintError ('Function ~C0A<' + func + '>~C0C is not registered');
     end;

    // сохранение в кэше данных рассчитанных отдельными функциями

    if (result > 0) and (sn <> '')  then
       begin
        dst := JParams.dst32[0];
        calc_cache.InsertData( sn, dst );
       end;   {}


    elapsed := cpt.Elapsed;
    pst := pstList.Items['FUNC_' + func + sfx];
    pst.Update(cpt);


    if (Elapsed > 200) then
     ODS( CFormat('[~T/~I]. #OPT_WARN: function %s calculated with %d bars.' +
          ' Elapsed = %.2f msec, ef = %.2f msec, CPU-time = %.2f msec', '~C07',
         [func, result, Elapsed, ef, cpt.CPUElapsed]));
    // настройка индексного пространства
    last_close := 0;
    if closev <> nil then last_close := closev.Count - 1;

    with JParams do
    if JParams.dst_count > 0 then
      begin
       // обработка смещения данных в прошлое/будущее после выполнения одного индикатора
       if last_close > 0 then
          for n := 0 to dst_count - 1 do
            begin
             dst := dst32 [n];
             dst.iRight := dst.iLastValid;
             dst.iLeft := Max (0, dst.iLastValid - range_size + 1 ) ;

             OffsetData(@dst32, n);
             // dst32 [n].iLastValid := dst32 [n].Count - 1;
             // положительное смещение, для совмещения индексов входных данных, с выходными
             dst.iRefOffset := (last_close - dst.iLastValid);
             Inc ( dst.indexList [5] );


            end;

       if (result = 0) then
           result := JParams.dst32[0].Count;
      end;

    exit;
   end; // action-calc
end;

constructor TDataCalcJob.Create;
begin
 FOwner := AOwner;
 Params := TStrMap.Create (self);
 Params.Sorted := TRUE;
 Params.CaseSensitive := FALSE;
 Params.OwnsObjects := TRUE;
 JParams := TCalcJobParams.Create;
 cpt := TProfileTimer.Create;
 inherited Create;
 Row := TStrMap.Create;
 pstList := TProfStatList.Create(self);
end;

destructor TDataCalcJob.Destroy;
begin
 Params.Clear;
 Params.Free;
 Row.Free;
 JParams.Free;
 cpt.Free;
 pstList.Free;
 inherited;
end;

{ TSyncData }

function TSyncData.GetValues(nData: Integer): PSingleArray;
var n_offset: Integer;
begin
 n_offset := Offsets [nData];
 result := nil;
 ASSERT( n_offset < MAXINT div 16, 'wrong array offset!');
 if FData[nData] = nil then
   begin
    PrintError('Accessing to data-stream #' + IntToStr(nData) + ' is outbound');
    exit;
   end;

 try
  result := FData[nData].FloatItems;
  if n_offset > 0 then
     result := @result [n_offset];
  // получение указателя на недействительную область памяти!!! с ним нужно работать крайне осторожно!!!
  while (n_offset < 0) do
   begin
    Inc(n_offset);
    Dec( DWORD(result), sizeof(result[0]));
   end;
 except
  on E: ERangeError do
   PrintError('TSyncData.GetValues cause exception ERangeError. nData = ' + IntToStr(nData));
 end;
 ASSERT ( result <> nil, 'TSyncData.GetValues: result = nil, nData = ' + IntToStr (nData) +
                ', array-name = '  + FData[nData].Name);
end;

procedure TSyncData.Init(srcd: TDataList32; cnt: Integer);
var n: Integer;
begin
 ASSERT (cnt > 0, 'Needs one or more data arrays!');
 Move(srcd, FData, sizeof (FData));
 { судя по логике, идет поиск вектора с минимальным количеством данных.
   Его количество юзается как прото для выходных данных.
   Вопрос связан с привязкой баров к правой границе карты (обратный порядок в массивах).
 }
 FDataCount := cnt;
 FItemsCount := FData[0].ValidCount;

 for n := 1 to cnt - 1 do
     FItemsCount := Min (FItemsCount, FData[n].ValidCount);

 // смещения начала действительной области?
 for n := 0 to cnt - 1 do
     Offsets [n] := FData[n].iLastValid - FItemsCount + 1; // TODO: забыл что это за хрень
end;

function TSyncData.InitDest(dst: TArray32): PSingleArray;
begin
 dst.iFirstValid := dst.Count;
 result :=  dst.AddRows(FItemsCount) ;
 dst.iLastValid := dst.Count - 1;
end;

procedure TSyncData.Read(nIndex: Integer);
var n, i, dcnt: Integer;
begin
 for n := 0 to FDataCount - 1 do
  begin
   ASSERT (n < 8, 'Outbound value position #' + IntToStr(n));
   i := Offsets[n] + nIndex;
   dcnt := FData[n].Count;
   Values[n] := 0;
   if (i < 0) or (dcnt <= 0) then continue;
   ASSERT ( (i >= 0) and (i < dcnt), 'Outbound index = ' + IntToStr(i) + ', FData[n].Count = ' + IntToStr(dcnt));
   Values[n] := FData[n].FloatItems[i];
  end;
end;

{ TCalcJobParams }

function TCalcJobParams.UpdateDataRefs (slParams: TStrMap; const suffix: String; pData: PDataList32): Integer;
var n: Integer;
   k, sn: String;
   a32: TArray32;
   issrc: Boolean;
begin
 result := 0;
 issrc := (suffix = 'SRC_');

 for n := 0 to CJ_PARAM_COUNT - 1 do
  begin
   k := suffix + Byte2Hex (n);
   sn := slParams.Values [k];
   if sn = '' then break;
   a32 := nil;
   if issrc then a32 := InData [sn];
   if (a32 = nil) then
      begin
       a32 := OutData [sn];
       if a32 = nil then
        begin
         // сюда обычно выносит, если пропущены параметры функции TDataCalcJob.AddFuncParams
         PrintError('TCalcJobParams.UpdateDataRefs: Data array ' + sn + ' not exist in InData/OutData. Created by system.');
         a32 := OutData.AddArray32(sn, TRUE);
        end;
       if issrc then a32.flags := a32.flags or 2;
      end
   else a32.SetBounds(nFirst, nLast, 3);           // настройка диапазонов для ИСХОДНЫХ даннов OHLC/V;


   
   if (not issrc) then
    begin
     a32.Clear;
     a32.iFirstValid := MAXINT;
    end;

   pData [result] := a32;
   Inc (result);
 end; // for
end; // InitData

procedure TCalcJobParams.InitData(slParams: TStrMap);

begin
 src_count := 0;
 dst_count := 0;
 // обновление ссылок на данные
 if not Assigned (InData) and Assigned (OutData) then exit;

 FillChar (src32, sizeof(src32[0]) * CJ_PARAM_COUNT, 0);
 FillChar (dst32, sizeof(dst32[0]) * CJ_PARAM_COUNT, 0);

 src_count := UpdateDataRefs (slParams, 'SRC_', @src32);
 dst_count := UpdateDataRefs (slParams, 'DST_', @dst32);
 if src_count > 0 then
     srcd.Init (src32, src_count);
end; // InitData

procedure TCalcJobParams.InitParams (slParams: TStrMap);
var
   n: Integer;
   sn: String;
begin
 // получения списка целочисленных параметров
 FillChar(iParams, sizeof(iParams[0]) * CJ_PARAM_COUNT, 0);
 for n := 0 to CJ_PARAM_COUNT - 1 do
     begin
      sn := 'INTPARAM_' + Byte2Hex (n);
      if slParams.IndexOfName(sn) < 0 then break;
      iParams [n] := slParams.IntValues [sn];
     end;

 FillChar(fParams, sizeof(fParams[0]) * CJ_PARAM_COUNT, 0);
 for n := 0 to CJ_PARAM_COUNT - 1 do
      begin
       // получения списка "вещественных" параметров
       sn := 'FLOATPARAM_' + Byte2Hex (n);
       if slParams.IndexOfName(sn) < 0 then break;
       fParams [n] := slParams.FloatValues [sn];
      end;

 FillChar(rParams, sizeof(rParams[0]) * CJ_PARAM_COUNT, 0);
 FillChar(res_ofst, sizeof(res_ofst [0]) * CJ_PARAM_COUNT, 0);
       
end;


procedure TCalcJobParams.OffsetData(pdl: PDataList32; n_index: Integer);
var
   data: TArray32;
   v: Single;
   pdata: PSingleArray;
   n, ofst, iFirst, iLast: Integer;
begin
 ofst := res_ofst [n_index];
 data := pdl[n_index];
 if ( ofst = 0 ) then exit;
 if  ( data.Count <= 0 ) then exit;
 pdata := data.FloatItems;
 data.iLastValid := data.Count - 1;
 // смещение данных из прошлого в будущее (metastock mode)
 if ofst < 0 then
   begin
    ofst := -ofst;
    iLast := data.Count - 1 - ofst;
    iFirst := 0;
    for n := iLast downto iFirst do
        pdata[n + ofst] := pdata[n];
    for n := 0 to ofst - 1 do pdata[n] := 0; // для отладки
    data.iFirstValid := data.iFirstValid + ofst;
   end
 else
   begin // смещение данных из будущего в прошлое
    // перемещение назад, копированием -  смещение влево на графике
    Move (pdata^[ofst], pdata^, sizeof(pdata[0]) * (data.Count - ofst) );
    data.iLastValid := data.iLastValid - ofst;
    v := pdata[data.iLastValid];
    // заполнение неотражаемого участка, для отладки
    for n := data.iLastValid + 1 to data.Count - 1 do
        pdata [n] := v;
   end;
end;

procedure TCalcJobParams.SetFParams(f0, f1, f2, f3, f4, f5, f6, f7: Single);
begin
 fParams[0] := f0;
 fParams[1] := f1;
 fParams[2] := f2;
 fParams[3] := f3;
 fParams[4] := f4;
 fParams[5] := f5;
 fParams[6] := f6;
 fParams[7] := f7;
end;

procedure TCalcJobParams.SetIParams(i0, i1, i2, i3, i4, i5, i6, i7: Integer);
begin
 iParams[0] := i0;
 iParams[1] := i1;
 iParams[2] := i2;
 iParams[3] := i3;
 iParams[4] := i4;
 iParams[5] := i5;
 iParams[6] := i6;
 iParams[7] := i7;
end;

// TCalcJobParams.Init

constructor TCalcJobParams.Create;
begin
 srcd := TSyncData.Create;
 stat_result := TStrMap.Create (self);
end;

destructor TCalcJobParams.Destroy;
begin
 srcd.Free;
 stat_result.Free;
 inherited;
end;

{ TCalcDataCache }

procedure TCalcDataCache.Clear;
begin
 inherited;
 FCacheUsed := 0;
 FCacheHits := 0;
end;

constructor TCalcDataCache.Create;
begin
 inherited Create(nil);
 ResizeCache(512 * 1024);
 Sorted := TRUE;
 OwnsObjects := TRUE;
end;

destructor TCalcDataCache.Destroy;
begin
 Clear;
 FreeMem (FCacheData);
 inherited;
end;

function TCalcDataCache.GetRef(const sIndex: String): TDataRefItem;
begin
 result := TDataRefItem ( FindObject (sIndex) );
 if result <> nil then
    Inc (FCacheHits);
end;

procedure TCalcDataCache.InsertData(const sIndex: String; a32: TArray32);
var ref: TDataRefItem;
    dsize: Integer;
begin
 dsize := a32.Count * a32.RowSize;
 while (dsize + FCacheUsed > FCacheSize) do
        ResizeCache (FCacheSize + 4096 * 1024);
 ref := GetRef(sIndex);
 if ref = nil then
    begin
     ref := TDataRefItem.Create;
     AddObject (sIndex, ref);
     ref.pData := Ptr ( Integer(FCacheData) + FCacheUsed );
     ref.nSize := dsize;
     ref.iFirstValid := a32.iFirstValid;
     ref.iLastValid := a32.iLastValid;
     Inc(FCacheUsed, ref.nSize);
     ODS('[~T/~I]. To cache added data ~C0A' + sIndex + '~C07');
     while (FCacheUsed and $1F = 0) do Inc(FCacheUsed);
     ref.Load(a32.FloatItems);
    end;

end; // InsertData

procedure TCalcDataCache.ResizeCache(newSize: Integer);
var old_ptr: Pointer;
    n: Integer;
    i_ptr: Int64;
begin
 old_ptr := FCacheData;
 ODS(Format('[~T/~I]. #OPT: Reallocating calc_cache to~C0D %d~C07 KiB', [newSize div 1024]));
 ReAllocMem (FCacheData, newSize);
 if (old_ptr <> nil) and (FCacheData <> old_ptr) then
  for n := 0 to Count - 1 do
    with TDataRefItem (Objects[n]) do
     begin
      i_Ptr := Int64 ( pData ) + Int64 (FCacheData) - Int64 (old_ptr);
      pData := Ptr ( i_Ptr );
     end;
 FCacheSize := newSize;
end; // ResizeCache

{ TDataRefItem }

procedure TDataRefItem.Load(psrc: Pointer);
begin
 MoveMemory(pData, psrc, nSize);
end;

procedure TDataRefItem.Store(pdst: Pointer);
begin
 MoveMemory(pdst, pData, nSize);
end;

{ TFunctionStorage }

constructor TFunctionStorage.Create;
begin
 inherited Create (nil);
 Sorted := TRUE;
 CaseSensitive := FALSE;
 OwnsObjects := TRUE;
end;

function TFunctionStorage.GetItems(const sIndex: String): TFunctionDesc;
var i: Integer;
begin
 result := nil;
 i := IndexOf (sIndex);
 if i >= 0 then
    result := TFunctionDesc ( Objects [i] );
end; // GetItems

function TFunctionStorage.RegFunc(const sName: String; AFunc: TCalcJobFunc; in_dc, out_dc: Integer): TFunctionDesc;
begin
 result := TFunctionDesc.Create;
 AddObject (sName, result);
 result.the_func := AFunc;
 result.in_data_cnt := in_dc;
 result.out_data_cnt := out_dc;
 Assert ( in_dc <= CJ_PARAM_COUNT,  'RegFunc (' + sName +') to many in_data params' );
 Assert ( out_dc <= CJ_PARAM_COUNT, 'RegFunc (' + sName + ') to many out_data params' );
end; // RegFunc

procedure       InitModule;
begin
 if Assigned (PSPool) then exit;
 PSPool := TWorkThreadPool.Create (nil);
 gFuncStorage := TFunctionStorage.Create;
end;

procedure       FinalizeModule;
begin
 PSPool.StopAll;
 PSPool.Free;
 gFuncStorage.Free;
end;

initialization
 InitModule;
finalization
 FinalizeModule;
end.
