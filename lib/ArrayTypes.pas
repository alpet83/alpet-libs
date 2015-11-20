unit ArrayTypes;

interface
uses Windows, SysUtils, Classes;

const
   MAX_ARRAY_INDEX = MAXINT div 32 - 1;




type

    TIndexRange = packed record
     iFirst, iLast: Integer;
     procedure          Init (a, b: Integer); inline;
    end;

    PIndexRange = ^TIndexRange;

    TIntRange = packed record
     iMin, iMax: Integer;
     procedure          Init (a, b: Integer); inline;
    end;

    TDWORDRange = packed record
     nMin, nMax: DWORD;
    end;


    TDataLockStates = ( DLS_READ, DLS_WRITE );

    TLargeByteArray = packed array [0..MAX_ARRAY_INDEX - 1] of BYTE;
    PLargeByteArray = ^TLargeByteArray;

    TSingleArray = packed array [0..MAX_ARRAY_INDEX - 1] of Single;
    PSingleArray = ^TSingleArray;

    TDWORDArray = packed array [0..MAX_ARRAY_INDEX - 1] of DWORD;
    PDWORDArray = ^TDWORDArray;

    TQWORDArray = packed array [0..MAX_ARRAY_INDEX - 1] of UInt64;
    PQWORDArray = ^TQWORDArray;

    TIntArray = packed array [0..MAX_ARRAY_INDEX - 1] of Integer;
    PIntArray = ^TIntArray;

    TPtrArray = packed array [0..MAX_ARRAY_INDEX - 1] of Pointer;
    PPtrArray = ^TPtrArray;
    // --------------- 64-bits arrays ----------- //

    TReal48Array = packed array [0..MAX_ARRAY_INDEX div 2 - 1] of Real48;
    PReal48Array = ^TReal48Array;


    TDoubleArray = packed array [0..MAX_ARRAY_INDEX div 2 - 1] of Double;
    PDoubleArray = ^TDoubleArray;

    TExtendedArray = packed array [0..MAX_ARRAY_INDEX div 2 - 1] of Extended;
    PExtendedArray = ^TExtendedArray;


    TInt64Array = packed array [0..MAX_ARRAY_INDEX div 2 - 1] of Int64;
    PInt64Array = ^TInt64Array;

    TIntradayRange = packed record
     n_date: Integer;
     iFirst, iLast: Integer;
     reserv: Integer;
    end; // TDayRange, 16 bytes

    PIntradayRange = ^TIntradayRange;

    TIntradayRangeArray = packed array [0..MAX_ARRAY_INDEX - 1] of TIntradayRange;
    PIntradayRangeArray = ^TIntradayRangeArray;

implementation

{ TIndexRange }

procedure TIndexRange.Init(a, b: Integer);
begin
 iFirst := a;
 iLast := b;
end;

{ TIntRange }

procedure TIntRange.Init(a, b: Integer);
begin
 iMin := a;
 iMax := b;
end;

end.
