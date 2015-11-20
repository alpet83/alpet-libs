unit BaseLib;

interface
uses Windows;


type

    TByteArray16M = array[0..16384 * 1024 - 1] of BYTE;
    PByteArray16M = ^TByteArray16M;

function  Byte2Hex  (b: BYTE): String;
function  Word2Hex  (w: WORD): String;
function  Dword2Hex (d: DWORD): String;
function  PtrToStr  (p: Pointer): String;

procedure Buff2HexRL (var buff; nb: Integer; ret: PChar);
procedure Int2Hex (i: Integer; ret: PChar);
function  itos(i: Int64): ShortString;


implementation


const hash : PChar = ('0123456789ABCDEF');


procedure Buff2HexRL (var buff; nb: Integer; ret: PChar);
var
   n, nc: Integer;
   b: BYTE;
   bs: PByteArray16M;

begin
 bs := @buff;
 nc := 0;

 for n := nb - 1 downto 0 do
  begin
   b := bs[n];
   ret [nc] := hash [b shr $4];
   Inc (nc);
   ret [nc] := hash [b and $F];
   Inc (nc);
  end;

 ret[nc] := #0;
end;

procedure Int2Hex (i: Integer; ret: PChar);
begin
 Buff2HexRL(i, 4, ret);
end;




function Byte2Hex (b: BYTE): String;

begin
 Byte2Hex := hash [b shr 4] + hash [b and 15];
end;

function  Word2Hex(w: WORD): String;
begin
 result := Byte2Hex (Hi (w)) + Byte2Hex (Lo (w));
end;

function  Dword2Hex(d: DWORD): String;
begin
 result := Word2Hex (HiWord (d)) + Word2Hex (LoWord (d));
end;

function  PtrToStr (p: Pointer): String;
begin
 result := DWORD2HEX ( DWORD (P) );
end;


function itos(i: Int64): ShortString;
begin
 Str (i, result);
end;


end.
