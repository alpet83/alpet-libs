unit DepthCollector;

interface
uses Windows, SysUtils, Classes, StrUtils, StrClasses, Math, Misc, FastSync,
        DateTimeTools, InstrDictionary, BTITypes, ContNrs, UNIArray;

type
       TDepthRow = packed record // 48 bytes
        i_now: Int64;
      i_price: Int64;
     i_totvol: Int64;
     i_addvol: Int64;     // volume change
       dt_add: TDateTime; // for latency measure
       is_bid: Boolean;   // bid or ask
     pszInstr: PChar;     // instrument code
    end;

    PDepthRow = ^TDepthRow;
    TDepthRows256 = packed array [0..256] of TDepthRow;
    PDepthRows256 = ^TDepthRows256;
    TDepthRows16K = packed array [0..16383] of TDepthRow;
    PDepthRows16K = ^TDepthRows16K;

    TDepthRowsArrayBase = TMatrixArrayT<TDepthRow, PDepthRows16K>;

    TDepthRowsArray = class (TDepthRowsArrayBase)
    private
     // asks, bids: TList;
      upd_stamp: Integer;
       sc_share: TCritSection;
           iinf: TInstrInfo;
    public

     bind_price: Int64;   // Цена привязки набора строк, например $50
       buy_side: Boolean;
     round_coef: Double;
          limit: Integer;
     { C & D }
     constructor        Create ( AOwner: TInstrInfo ); virtual;
     destructor         Destroy; override;
     { methods }
     procedure          ApplyRow ( row: TDepthRow );
     function           FindAddRow ( price: Int64 ): Integer;
     procedure          Dump ( ls: TStrings );
     procedure          Enum ( cbf: TDataEventBTI; const params: Pointer );
    end; // TDepthRowsArray

    TDepthCollector = class
    private
     FBids, FAsks: TObjectList;
      FPriceRound: Int64;
         sc_share: TCritSection;
        dump_path: String;
             iinf: TInstrInfo;

     function GetAsksCache (index: Integer): TDepthRowsArray;
     function GetBidsCache (index: Integer): TDepthRowsArray;
     function DumpOne (pRow, params: Pointer): Boolean;
    protected
     upd_stamp: Integer;
     // collected bids/asks
     col_bids, col_asks: TDepthRows256;
     cnt_bids, cnt_asks: Integer;
     function AggregateOne ( pRow, params: Pointer): Boolean;
    public
     // vars
     updated: Boolean;
     { props }
     property           Asks [index: Integer]: TDepthRowsArray read GetAsksCache;
     property           Bids [index: Integer]: TDepthRowsArray read GetBidsCache;
     property           PriceRound: Int64 read FPriceRound write FPriceRound;

     { C & D }
     constructor        Create ( AOwner: TInstrInfo );
     destructor         Destroy; override;
     { methods }
     procedure          Aggregate;
     procedure          Clear;
     procedure          Dump ( side: CHAR );
     procedure          Enum ( cbf: TDataEventBTI; const params: Pointer; side: CHAR );
     function           FindAddCache ( price: Int64; bid: Boolean ): TDepthRowsArray;

     procedure          Lock(const ctx: String);
     procedure          Unlock;
    end;



implementation

function testDepthRowPriceAbove ( a, b: Pointer ): Integer;
begin
 result := PDepthRow (a).i_price - PDepthRow (b).i_price;
end;

function testDepthRowPriceBelow ( a, b: Pointer ): Integer;
begin
 result := PDepthRow (b).i_price - PDepthRow (a).i_price;
end;

function testArrayPriceAbove ( a, b: Pointer ): Integer;
var ac, bc: TDepthRowsArray;
begin
 ac := a; bc := b;
 result := ac.bind_price - bc.bind_price;
end;

function testArrayPriceBelow ( a, b: Pointer ): Integer;
var ac, bc: TDepthRowsArray;
begin
 ac := a; bc := b;
 result := bc.bind_price - ac.bind_price;
end;


{ TDepthRowsArray }



procedure TDepthRowsArray.ApplyRow ( row: TDepthRow );
var
   dst: PDepthRow;
    fp: Double;
    vu: Int64;
     i: Integer;
begin
 sc_share.Lock ('ApplyRow');
 try
   fp := Round ( row.i_price / iinf.price_coef * round_coef );  // value 315******** -> 31.5 -> 3150
   vu := Round ( fp / round_coef * iinf.price_coef );

   {
   if IsKeyPressed (VK_MENU) then
      ODS( CFormat('[~T/~I]. #DEPTH: price = %10.5f (rounded %10.5f), volume_add = %10.5f, volume_total = %10.5f ', '~C07',
                              [row.i_price / iinf.price_coef,
                                        vu / iinf.price_coef,
                               row.i_addvol / iinf.vol_coef,
                               row.i_totvol / iinf.vol_coef] ) ); // }

   // row.i_price := vu;
   i := FindAddRow (row.i_price);
   if i < 0 then exit;  // cannot add more
   // updated := TRUE;
   dst := @Items[i];


   if ( row.i_addvol <> 0 ) and ( row.i_totvol <= 0 ) then  // I-frame
     begin
      vu := dst.i_totvol + row.i_addvol;
      {
      if IsKeyPressed (VK_MENU) then
         ODS( CFormat('[~T/~I]. #RELATIVE_DEPTH: price = %10.5f, volume_add = %10.5f, volume_total = %10.5f (prev %10.5f) ', '~C07',
                                [row.i_price / iinf.price_coef, row.i_addvol / iinf.vol_coef, vu / iinf.vol_coef, dst.i_totvol / iinf.vol_coef]) ); // }
      dst.i_totvol := vu;
     end
   else
     dst^ := row;                                           // key-frame
   // Inc ( dst.i_volume, row.i_volume );

   // remove very tiny lots
   if dst.i_totvol / iinf.vol_coef <= 0.00001 then
      self.DeleteItems (i, 1);

   if i < self.Count then
     begin
      dst := @Items[i];
      Assert ( dst.i_totvol > 0, 'strange overwrite' );
     end;

 finally
  sc_share.Unlock;
 end;
end;

constructor TDepthRowsArray.Create (AOwner: TInstrInfo);
begin
 inherited Create();

 iinf := AOwner;
 sc_share := TCritSection.Create ( ClassName + '.sc_share' );

 round_coef := Power (10, iinf.decimals);
 limit := 200;
end;

destructor TDepthRowsArray.Destroy;
begin
 sc_share.Free;
 inherited;
end;

procedure TDepthRowsArray.Dump;
var
  pr: PDepthRow;
   s: String;
   n: Integer;
begin
 if Count = 0 then exit;

 if buy_side then
    Sort ( testDepthRowPriceBelow ) // price order must be downscending
 else
    Sort ( testDepthRowPriceAbove ); // price order must be ascending

 for n := 0 to self.Count - 1 do
  begin
   pr := self.Rows [n];
   s := Format('PRICE = %20.5f; AMOUNT = %20.9f ', [ pr.i_price / iinf.price_coef, pr.i_totvol / iinf.vol_coef ] );
   ls.Add (s);
  end;
 ls.Add('________________________________________________');
end;

procedure TDepthRowsArray.Enum(cbf: TDataEventBTI; const params: Pointer);
var
   pr: PDepthRow;
    n: Integer;
begin
 if buy_side then
    Sort ( testDepthRowPriceBelow ) // price order must be downscending
 else
    Sort ( testDepthRowPriceAbove ); // price order must be ascending

 for n := 0 to Count - 1 do
  begin
   pr := self.Rows [n];
   if not cbf (pr, params) then break;
  end;

 cbf ( nil, self ); // loop end detection
end; // Enum

function TDepthRowsArray.FindAddRow(price: Int64): Integer;
var
  row: PDepthRow;
    n: Integer;
begin
 // sc_share.Lock('FindAddRow');
 // TODO: use sorting scheme if possible with binary-search

 for n := Count -1 downto 0 do
  begin
   row := @Items [n];
   if row.i_price = price then
    begin
     result := n;
     exit;
    end;
  end; // for

 result := Count;
 row := AddRow;
 FillChar ( row^, sizeof(TDepthRow), 0 );
 row.i_price := price;
end; // FindAddRow

{ TDepthCollector }

procedure TDepthCollector.Aggregate;
const
   QLIM = MAX_DOM_DEEP div 2;
var
   qr: TRTMQuoteRow;
   ad: PDepthRow;
   bd: PDepthRow;

   cc: Integer;
   ba: Integer;
   bb: Integer;
    n: Integer;

begin
 // load rounded best bids and asks
 {
 Dump('B');
 Dump('S'); //    }
 updated := FALSE;

 if iinf.lock_flags and 1 > 0 then exit;

 sc_share.Lock('Aggregate');
 try
   // up to 128 of each can be collected
   cnt_asks := 0;
   cnt_bids := 0;
   FillChar ( col_asks, sizeof (col_asks), 0 );
   FillChar ( col_bids, sizeof (col_bids), 0 );
   Enum ( AggregateOne, @col_asks, 'A' );
   Enum ( AggregateOne, @col_bids, 'B' );

   iinf.QCount := 0;
   Inc ( upd_stamp );
   iinf.CheckResetQuotes ( upd_stamp );

   // TODO: resolve best bid / offer conflict due round prices
   bb := cnt_bids - 1; // best bid
   ba := 0;

   while ( bb >= 0 ) and ( ba < cnt_asks ) do
    begin
     ad := @col_asks [ba];
     bd := @col_bids [bb];
     if ( bd.i_price < ad.i_price ) then break; // normalized

     ODS( CFormat('[~T].~C0C #WARN:~C07 detected quotes overlab bid %f >= ask %f ', '~C07', [bd.i_price, ad.i_price] ) );
     // cropping bids or asks
     if bd.dt_add < ad.dt_add then  // bid added before ask
        Inc (ba)
     else
        Dec (bb);
    end; // while

   ba := Min ( ba, cnt_asks - 1 );
   bb := Min ( QLIM - 1, bb );
   bb := Max ( bb, 0 );
   FillChar ( qr, sizeof(qr), 0 );
   qr.utime := CurrentTime.Time;
   // first puts bids (reverse order, price must be ascending)
   // best bid = first bid in col_bids (?)
   cc := 0;
   for n := bb downto 0 do
     begin
      qr.price := col_bids [n].i_price / iinf.price_coef;
      qr.qty   := col_bids [n].i_totvol / iinf.vol_coef;
      qr.flags := QRF_BID;
      iinf.InsertQuote ( @qr );
      Inc (cc);
      if cc >= QLIM then break;
     end;

   // next puts asks (normal order)
   cc := 0;
   for n := ba to cnt_asks - 1 do
     begin
      qr.price := col_asks [n].i_price / iinf.price_coef;
      qr.qty   := col_asks [n].i_totvol / iinf.vol_coef;
      qr.flags := 0;
      iinf.InsertQuote ( @qr );
      Inc (cc);
      if cc >= QLIM then break;
     end;

   iinf.OnQuotesUpdate;
 finally
  sc_share.Unlock;
 end;

end;

function TDepthCollector.AggregateOne;
var
   rscale: Double;
   rprice: Int64;
     step: Int64;
     diff: Int64;
       bs: Boolean;
       cl: PDepthRows256;
       lr: PDepthRow;
       pc: PInteger;
       pr: PDepthRow;
       vp: Double;
        i: BYTE;
begin
 result := TRUE;
 pr := pRow;
 if pr = nil then exit;

 bs := ( params = @col_bids );
 pc := IfV ( bs, @cnt_bids, @cnt_asks );
 cl := IfV ( bs, @col_bids, @col_asks );

 // если идет перебор бидов, каждая следующая строка стакана должна быть с меньшей ценой
 // если идет перебор асков, каждая следующая строка стакана должна быть с большей ценой

 rscale := Power ( 10, iinf.decimals );  // 2 -> 100

 step := Round ( iinf.price_coef / rscale ); // integer step between rounded rows

 vp := pr.i_price * rscale / iinf.price_coef; // 55.15 ->> 5515 int

 if not bs and ( rscale < iinf.price_coef ) then
    asm
     nop
    end;


 if bs then
    rprice := Trunc ( vp )
 else
    rprice := UpTrunc ( vp );

 rprice := Round ( rprice * iinf.price_coef / rscale );     // back to large int

 //  fp := Round ( row.i_price / iinf.price_coef * round_coef );  // value 315******** -> 31.5 -> 31500
 //  vu := Round ( fp / round_coef * iinf.price_coef );



 i := pc^ - 1;

 if pc^ > High (col_bids) then
   asm
    nop
   end;


 if pc^ = 0 then
    lr := @cl^[0]
 else
    lr := @cl^[i];

 diff := Abs (lr.i_price - pr.i_price);

 if diff >= step then
  begin
   if pc^ < High (col_bids) then Inc ( pc^ );
   i := pc^ - 1;
   lr := @cl^[i]; // new selected
  end;

 // initialization new
 if lr.i_price = 0 then
  begin
   lr.i_price := rprice;
   lr.i_totvol := pr.i_totvol;
   lr.is_bid := bs;
  end
 else
   Inc ( lr.i_totvol, pr.i_totvol );  // aggregation

 result := ( pc^ < High (col_bids) ); // when must stop
end;

procedure TDepthCollector.Clear;

begin
 sc_share.Lock('Clear');
 try
  cnt_bids := 0;
  cnt_asks := 0;
  FAsks.Clear;
  FBids.Clear;
 finally
  sc_share.Unlock;
 end;
end;

constructor TDepthCollector.Create;
begin
 iinf := AOwner;

 FAsks := TObjectList.Create (TRUE);
 FBids := TObjectList.Create (TRUE);

 // вычисление агрегатора цены
 // 0.01 -> 0.1
 PriceRound := 100;

 sc_share := TCritSection.Create ( ClassName + '.sc_share' );
 dump_path := CorrectFilePath ( ExePath + '\..\logs\' + iinf.InstrCode );
 CheckMakeDir (dump_path);
 dump_path := dump_path + '\';
end;

destructor TDepthCollector.Destroy;
begin
 FAsks.Free;
 FBids.Free;
 sc_share.Free;
 inherited;
end;

function TDepthCollector.DumpOne;
var
  dra: TDepthRowsArray;
   ls: TStrings;
   pr: PDepthRow;
    s: String;
begin
 result := TRUE;
 pr := pRow;
 ls := params;
 if pr <> nil then
    s := Format('PRICE = %20.5f; AMOUNT = %20.9f ', [ pr.i_price / iinf.price_coef, pr.i_totvol / iinf.vol_coef ] )
 else
   begin
    dra := params;
    s := '_________________________________________________________! ' + iinf.FormatPrice ( dra.bind_price / iinf.price_coef);
   end;
 ls.Add (s);
end;


procedure TDepthCollector.Dump(side: CHAR);
var
   bs: Boolean;
   ls: TStrings;
   fn: String;

begin
 ls := TStringList.Create;
 bs := ( side = 'B' );

 try
  if bs then
     fn := 'bids.dump'
  else
     fn := 'asks.dump';
  Enum ( DumpOne, ls, side );
  ls.SaveToFile ( dump_path + fn )
 finally
  ls.Free;
 end;
end; // Dump


procedure TDepthCollector.Enum;
var
   bs: Boolean;
   cl: TObjectList;
    n: Integer;

begin
 // universal enumeration for all or one side rows
 repeat
  bs := ( side = 'B' );

  if bs then
    begin
     cl := FBids;
     cl.Sort ( testArrayPriceBelow );
    end
  else
    begin
     cl := FAsks;
     cl.Sort ( testArrayPriceAbove );
    end;

  if cl.Count > 0 then
    for n := 0 to cl.Count - 1 do
        TDepthRowsArray ( cl [n] ).Enum ( cbf, params );

  if side = '*' then
     side := 'B'
  else
     break; // swap side from all to bid

 until FALSE;

end; // Enum

function TDepthCollector.FindAddCache (price: Int64; bid: Boolean): TDepthRowsArray;
var
  dra: TDepthRowsArray;
   cl: TObjectList;
    n: Integer;
begin
 result := nil;

 if ( iinf.InstrAlias = 'LTC-X' ) and bid then
   asm
    nop
   end;

 // получение цены привязки
 if bid then
   begin
    price := Trunc ( price / PriceRound );
    price := Max (price, 1);
   end
  else
    price := UpTrunc ( price / PriceRound );

 if price <= 0 then exit;

 if bid then
    cl := FBids
 else
    cl := FAsks;


 sc_share.Lock('FindAdd');
 try
   // TODO: use binary search for speed improvement
   for n := 0 to cl.Count - 1 do
    begin
     dra := TDepthRowsArray ( cl [n] );
     if dra.bind_price <> price then continue;
     result := dra;
     exit;
    end;

   result := TDepthRowsArray.Create ( iinf );
   result.bind_price := price;
   result.buy_side := bid;
   cl.Add (result);
  finally
   sc_share.Unlock;
  end;

end;

function TDepthCollector.GetAsksCache(index: Integer): TDepthRowsArray;
begin
 result := TDepthRowsArray ( FAsks[index] );
end;

function TDepthCollector.GetBidsCache(index: Integer): TDepthRowsArray;
begin
 result := TDepthRowsArray ( FBids[index] );
end;


procedure TDepthCollector.Lock(const ctx: String);
begin
 sc_share.Lock (ctx)
end;

procedure TDepthCollector.Unlock;
begin
 sc_share.Unlock;
end;

end.
