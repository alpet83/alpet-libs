unit xlTable;

interface
uses Windows, StrUtils, SysUtils, Classes, DDEml, Misc;

const
    TDT_FLOAT           = $0001;
    TDT_STRING          = $0002;
    TDT_BOOL            = $0003;
    TDT_ERROR           = $0004;
    TDT_BLANK           = $0005;
    TDT_INT             = $0006;
    TDT_SKIP            = $0007;
    TDT_TABLE           = $0010;



type
    TByteArray2G = packed array [0..MAXINT - 2] of Byte;
    PByteArray2G = ^TByteArray2G;

    TXlTableHdr = packed record
     tdt, cb: WORD;
     rows, cols: WORD;
    end; // TTableHdr

    PXlTableHdr = ^TXlTableHdr;

    TXlDataBlock = packed record
     tdt, cb: WORD;
     data: packed array [0..$FFFE] of byte;     // в реальности байт конечно меньше
    end; // TXlDataBlock

    PXlDataBlock = ^TXlDataBlock;

    TXlCell = record
     tdt: WORD;
     FirstCol, FirstRow: Integer;
     sval: AnsiString;
     case byte of
      0: (fval: Double);
      1: (ival: ShortInt); // также err-val
      2: (bval: Bool);
    end; // TXlCell


    TXlTableRaw = class (TMemoryStream)         // объект дл€ сохранени€ сырых данных, и считывани€ эффективных
    private
    protected
     cur_block, block_ofs: Integer;
     FBlocks: TList;
     FRow, FCol: Integer;
     FRowCount, FColCount: Integer;
     FFirstRow, FFirstCol: Integer;
     procedure           MoveCaret;
    public

     lstart_time: TDateTime;
     { props }
     property            Blocks: TList read FBlocks;
     property            ColCount: Integer read FColCount;
     property            RowCount: Integer read FRowCount;
     property            Col: Integer read FCol write FCol;
     property            Row: Integer read FRow write FRow;

     // origin
     property            FirstRow: Integer read FFirstRow;
     property            FirstCol: Integer read FFirstCol;


     { C & D }
     constructor         Create;
     destructor          Destroy; override;
     { Methods }
     function            BlockType(nIndex: Integer = -1): Integer;
     procedure           Parse;


     procedure           LoadFromData (pData: Pointer; cbSize: DWORD);
     procedure           LoadFromDDE (data: HDDEData);


     function            ReadCell(var cell: TXlCell): Boolean;
     procedure           SetOrigin(const sOrigin: String);

    end; // TXlTableRaw





implementation

{ TXlTableRaw }

function TXlTableRaw.BlockType(nIndex: Integer): Integer;

begin
 result := 0;
 if nIndex < 0 then nIndex := cur_block;
 if ( blocks.Count <= nIndex ) then exit;
 result := PXlDataBlock (blocks[nIndex]).tdt;
end;

constructor TXlTableRaw.Create;
begin
 inherited Create;
 FBlocks := TList.Create;
end;

destructor TXlTableRaw.Destroy;
begin
 FBlocks.Free;
 inherited;
end;

procedure TXlTableRaw.LoadFromData(pData: Pointer; cbSize: DWORD);
begin
 Size := cbSize;
 if Size > 0 then
  begin
   Move(pData^, Memory^, Size);
   Parse;
  end;
end;

procedure TXlTableRaw.LoadFromDDE(data: HDDEData);
begin
 lstart_time := Now;
 blocks.Clear;
 Size := DdeGetData(data, nil, 0, 0);
 if Size = 0 then exit;
 DdeGetData(data, Memory, Size, 0);
 Parse;
end; // LoadFromDDE

procedure TXlTableRaw.Parse;
var
   ofs: DWORD;
   src: PByteArray2G;
   pb: PXlDataBlock;
   ph: PXlTableHdr;
begin
 Position := 0;
 cur_block := 0;
 block_ofs := 0;
 ofs := 0;
 src := Memory;
 FRow := FirstRow;
 FCol := FirstCol;
 blocks.Clear;
 if (size < 4) or (src = nil) then exit;

 while (ofs + 4 < Size) do
  begin
   pb := @src[ofs];
   if (pb.tdt = TDT_TABLE) and (pb.cb >= 4) then
     begin
      ph := Pointer(pb);
      FRowCount := ph.rows;
      FColCount := ph.cols;
      if FRowCount > 1000 then
         ODS( Format('[~T]. #DBG: Parsing data of table with size~C0D %d x %d ~C07', [FColCount, FRowCount]) );
     end;
   blocks.Add(pb);
   Inc(ofs, pb.cb + 4);
  end;
end; // Parse

procedure TXlTableRaw.MoveCaret;
begin
 Inc (FCol);
 if FCol > FColCount then
  begin
   FCol := FirstCol;
   Inc(FRow);
  end;
end;

var out_evts: Integer = 0;

function TXlTableRaw.ReadCell(var cell: TXlCell): Boolean;
var
   pb: PXlDataBlock;
   i, c: Integer;

begin
 result := FALSE;
 cell.sval := '';
 FillChar(cell, sizeof(cell), 0);
 if (cur_block >= blocks.Count) then exit;
 pb := blocks[cur_block];

 cell.tdt := pb.tdt;
 // с чего начинаетс€ €чейка, ручеек и проча€ хрень
 cell.FirstCol := Col; // сдвиг вправо
 cell.FirstRow := Row;

 if out_evts < 100 then
   begin
    // ODS ( Format('Reading cell %d x %d, FirstCol = %d ', [Col, Row, cell.FirstCol]) );
    Inc (out_evts);
   end;


 if ( pb.tdt = TDT_BLANK ) or ( pb.tdt = TDT_SKIP ) then
  begin
   c := PWORD(@pb.data)^;
   cell.ival := c;
   for i := 1 to c do MoveCaret;
   Inc(cur_block);
  end
 else
   if pb.tdt <> TDT_TABLE then MoveCaret; // once




 case pb.tdt of
  TDT_TABLE:
     begin
      cell.sval := AnsiString ( Format('%d,%d', [FColCount, FRowCount]) );
      // что так
      FCol := FirstCol;
      FRow := FirstRow;
      cell.FirstCol := Col; // сдвиг вправо
      cell.FirstRow := Row;
      Inc(block_ofs, 4);
     end;
  TDT_FLOAT:
     begin
      cell.fval := PDouble( @pb.data[block_ofs] )^;
      Inc(block_ofs, 8);
     end;
  TDT_INT, TDT_BOOL, TDT_ERROR:
     begin
      cell.ival := PWORD( @pb.data[block_ofs] )^;
      Inc (block_ofs, 2);
     end;
   TDT_STRING:
     begin
      c := pb.data[block_ofs];
      if c > 0 then
        begin
         SetLength(cell.sval, c);
         Move(pb.data[block_ofs + 1], cell.sval[1], c);
        end;
      Inc(block_ofs, c + 1);
     end;
 end; // case


 if (pb.cb <= block_ofs) then
   begin
    block_ofs := 0;
    Inc(cur_block);
   end;

  result := cell.tdt <> 0;
end; // ReadCell


procedure TXlTableRaw.SetOrigin(const sOrigin: String);
var rtmp: TStrings;
    err: Integer;
    s: String;
begin
 FFirstRow := 1;
 FFirstCol := 1;

 rtmp := TStringList.Create;
 try
  rtmp.Delimiter := ':';
  rtmp.DelimitedText := sOrigin;
  if rtmp.Count < 2 then exit;
  s := rtmp[0];
  s := AnsiReplaceStr(s, 'R', #32);
  s := AnsiReplaceStr(s, 'C', #32);
  rtmp.Delimiter := #32;
  rtmp.DelimitedText := Trim(s);
  if rtmp.Count > 1 then
   begin
    Val(rtmp[0], FFirstRow, err);
    Val(rtmp[1], FFirstCol, err);
    //  ODS (Format ('[~T]. #DBG: xlTableRaw.SetOrigin fixed origin at %d x %d ', [FirstCol, FirstRow] ));
   end;
 finally
  rtmp.Free;
 end;

end; // SetOrigin




end.
