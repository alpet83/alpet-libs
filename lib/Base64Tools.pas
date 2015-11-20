unit Base64Tools;

interface
uses Windows, SysUtils, Soap.EncdDecd;


function  Base64LineW ( pData: Pointer; cb: Integer ): WideString;
function  Base64LineA ( pData: Pointer; cb: Integer ): AnsiString;
function  Base64LineU ( pData: Pointer; cb: Integer ): UTF8String;

function  Base64PackA ( const sText: AnsiString ): String; inline;
function  Base64PackW ( const sText: WideString ): String; inline;


function  Base64UnpackA ( const sPacked: String ): AnsiString;
function  Base64UnpackW ( const sPacked: String ): WideString;



implementation

function AnsiCharPos ( ch: AnsiChar; const s: RawByteString ): Integer;
var
   p, i, l: Integer;
begin
 result := 0;

 l := Length (s);
 i := 1;
 p := 1;

 // sample 7 chars: [4], 5, 6, 7
 while ( i < l ) do
  begin

   if ( s [i + 0] = ch ) then result := i + 0 else
   if ( s [i + 1] = ch ) then result := i + 1 else
   if ( s [i + 2] = ch ) then result := i + 2 else
   if ( s [i + 3] = ch ) then result := i + 3 else
        begin
         p := i + 1;
         Inc (i, 4);
         continue;
        end;

   exit;
  end;
 for i := p to Length(s) do
   if s [i] = ch then
    begin
     result := i;
     break;
    end;
end;


function  Base64LineA ( pData: Pointer; cb: Integer ): AnsiString; // primary variant, due chars remove native strings
var
   i: Integer;
begin
 result := EncodeBase64 ( pData, cb );
 repeat
  i := AnsiCharPos ( #13, result );
  if i = 0 then
     i := AnsiCharPos ( #10, result );
  if i > 0 then
     Delete ( result, i, 1 );
 until ( i = 0 );
end; // Base64LineW

function  Base64LineW ( pData: Pointer; cb: Integer ): WideString;
begin
 result := WideString ( Base64LineA (pData, cb) );
end;

function  Base64LineU ( pData: Pointer; cb: Integer ): UTF8String;
begin
 result := UTF8Encode ( Base64LineW (pData, cb)  );
end;

function  Base64PackA ( const sText: AnsiString ): String;
begin
 result := Base64LineW ( @sText[1], Length(sText) );
end;

function  Base64PackW ( const sText: WideString ): String;
begin
 result := Base64LineW ( @sText[1], Length(sText) * sizeof(WideChar) );
end;

function  Base64UnpackA ( const sPacked: String ): AnsiString;
var
   b: TBytes;
begin
 b := DecodeBase64 ( AnsiString (sPacked) );
 SetLength ( result, Length(b) );
 Move ( b[0], result[1], Length (b) );
 SetLength (b, 0);
end;


function  Base64UnpackW ( const sPacked: String ): WideString;
var
   b: TBytes;
begin
 b := DecodeBase64 ( AnsiString (sPacked) );
 SetLength ( result, Length(b) div sizeof (WideChar) );
 Move ( b[0], result[1], Length (b) );
 SetLength (b, 0);
end;

end.
