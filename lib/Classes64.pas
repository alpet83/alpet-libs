unit Classes64;

interface
uses Windows, SysUtils, Classes;

type
  TMemoryStream64 = class(TCustomMemoryStream)
  private
    // FSize: Int64;
    FCapacity: Int64;

    procedure SetCapacity(NewCapacity: Int64);
  protected
    function Realloc(var NewCapacity: Int64): Pointer; virtual;
    property Capacity: Int64 read FCapacity write SetCapacity;

  public


    { C & D }
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure SetSize (const NewSize: Int64); overload; override;
    function  Write (const Buffer; Count: Int64): Int64;
  end;



implementation
const
  MemoryDelta = $2000; { Must be a power of 2 }


{ TMemoryStream64 }

procedure TMemoryStream64.Clear;
begin
  SetCapacity(0);
  SetPointer (Memory, 0);
  Seek (0, soBeginning);
end;

destructor TMemoryStream64.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TMemoryStream64.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;


procedure TMemoryStream64.LoadFromStream(Stream: TStream);
var
  Count: Int64;
begin
  Stream.Position := 0;
  Count := Stream.Size;
  SetSize(Count);
  if Count <> 0 then
     Stream.ReadBuffer(Memory^, Count);
end;

function TMemoryStream64.Realloc(var NewCapacity: Int64): Pointer;
begin
  if (NewCapacity > 0) and (NewCapacity <> Size) then
    NewCapacity := (NewCapacity + (MemoryDelta - 1)) and not (MemoryDelta - 1);
  Result := Memory;
  if NewCapacity <> FCapacity then
  begin
    if NewCapacity = 0 then
    begin
      FreeMem(Memory);
      Result := nil;
    end else
    begin
      if Capacity = 0 then
        GetMem(Result, NewCapacity)
      else
        ReallocMem(Result, NewCapacity);
      if Result = nil then raise Exception.Create('Cannot realloc to size ' + IntToStr(newCapacity));
    end;
  end;
end;

procedure TMemoryStream64.SetCapacity(NewCapacity: Int64);
begin
  SetPointer(Realloc(NewCapacity), Size);
  FCapacity := NewCapacity;
end;

procedure TMemoryStream64.SetSize(const NewSize: Int64);
var
  OldPosition: Int64;
begin
  OldPosition := Position;
  SetCapacity(NewSize);
  // FSize := NewSize;
  SetPointer (Memory, NewSize);

  if OldPosition > NewSize then Seek(0, soFromEnd);
end;

function TMemoryStream64.Write(const Buffer; Count: Int64): Int64;
var
  Pos: Int64;
begin
  if (Position >= 0) and (Count >= 0) then
  begin
    Pos := Position + Count;
    if Pos > 0 then
    begin
      if Pos > Size then
      begin
        if Pos > FCapacity then
           SetCapacity(Pos);
        SetPointer (Memory, Pos);
      end;
      System.Move(Buffer, (PByte(Memory) + Position)^, Count);
      Seek (Pos, soBeginning);
      Result := Count;
      Exit;
    end;
  end;
  Result := 0;
end;

end.
