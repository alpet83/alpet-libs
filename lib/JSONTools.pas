unit JSONTools;

interface
uses Windows, Classes, SysUtils, StrUtils, StrClasses, DBXJSON;




function JSONParse ( rs: UTF8String ): TJSONObject; // функция создает объект, который надо уничтожить после пользования!
function JSONScan ( jo: TJSONObject; path: String ): TJSONObject;

function JSONStrVal ( jp: TJSONPair ): String; inline;
function JSONFloatVal ( jp: TJSONPair ): Double; inline;
function JSONIntVal ( jp: TJSONPair ): Int64; inline;

function JSONStrField ( jo: TJSONObject; const pair_name: String ): String; inline;


implementation
uses Misc;

function JSONScan ( jo: TJSONObject; path: String ): TJSONObject;
var
   tmp: TStrings;
   tag: String;
    jp: TJSONPair;
     n: Integer;
     i: Integer;
begin
 result := nil;

 tmp := TStringList.Create;
 tmp.Delimiter := '/';
 tmp.DelimitedText := path;

 for n := 0 to tmp.Count - 1 do
  begin
   tag := Trim (tmp [n]);
   if tag = '' then continue;
   for i := 0 to jo.Size - 1 do
    begin
     jp := jo.Get (i);
     if jp.JsonString.Value <> tag then continue;
     jo := TJSONObject ( jp.JsonValue );
     if n = tmp.Count - 1 then
        result := jo;
     break;
    end;
  end;


end;

function JSONStrVal ( jp: TJSONPair ): String; inline;
begin
 result := '';
 if Assigned (jp) then
    result := jp.JsonValue.Value;
end;


function JSONFloatVal ( jp: TJSONPair ): Double; inline;
begin
 result := atof ( jp.JsonValue.Value );
end;

function JSONIntVal ( jp: TJSONPair ): Int64; inline;
begin
 result := atoi ( jp.JsonValue.Value );
end;


function JSONParse(rs: UTF8String): TJSONObject;
var
  data: TBytes;
    jv: TJSONValue;
     i: Integer;

begin
 result := nil;
 i := Pos('{', UTF8ToString(rs) );
 if i = 0 then exit;

 SetLength (data, Length(rs));
 Move ( rs[1], data[0], Length(data) );

 try
  if i > 1 then
     Delete (rs, 1, i - 1);

  jv := TJSONObject.ParseJSONValue ( rs );
  if Assigned (jv) and ( jv is TJSONObject ) then
     result := TJSONObject (jv)
  else
     jv.Free;

 finally
  SetLength (data, 0);
 end;

end;



function JSONStrField ( jo: TJSONObject; const pair_name: String ): String;
begin
 result := JSONStrVal ( jo.Get(pair_name) );
end;

end.
