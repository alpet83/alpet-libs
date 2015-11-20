unit Algs;
{ модуль модных алгоритмов.
  начатао 3.01.2008, собственно мной
 }
interface
uses Windows, SysUtils, Classes, ContNrs, Math;


type
    { compares pData[iTest] and pValue
       equiv to  cmp_func (a, b)
        when (pData[iTest] > pValue) must return positive value
        when (pData[iTest] < pValue) must return negative value
           }
    TBSCompareFunc = function (iTest: Integer; pData, pValue: Pointer): Integer;

function     BinarySearch (pData, pValue: Pointer; Count: Integer; compFunc: TBSCompareFunc;
                            pLowBound: PInteger = nil; pHighBound: PInteger = nil): Integer;

implementation


function    BinarySearch (pData, pValue: Pointer; Count: Integer; compFunc: TBSCompareFunc;
                            pLowBound: PInteger; pHighBound: PInteger): Integer;

var
  L, H, I, C: Integer;
begin
  Result := -1;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := compFunc (I, pData, pValue); // a ? b = when (>) +1, when (=) 0, when (<) -1
    if C < 0 then L := I + 1    // семпл ожидаемо находится правее
      else
    begin
      H := I - 1;               // семпл ожидаемо находится левеее
      if C = 0 then
      begin
        Result := I;
        break;
      end;
    end;
  end; // while (L <= H)
  if l > h then
    begin
     c := l;
     l := h;
     h := c;
    end;

  if pLowBound <> nil then
     pLowBound^ := Max(0, L);
  if pHighBound <> nil then
     pHighBound^ := Min(Count - 1, H);
end;


end.
