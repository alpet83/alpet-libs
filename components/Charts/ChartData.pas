unit ChartData;

interface
uses Windows, SysUtils, Classes, Controls, Graphics, UTITypes, ExtCtrls, ContNrs, UNIArray, DateTimeTools, SMTypes;


const
     STDD_OPEN = 1;
     STDD_LOW  = 2;
     STDD_HIGH = 4;
     STDD_CLOSE = 8;
     STDD_VOLUME = 16;





type

    TFloatRange = record
     minv, maxv: Single
    end;

    TDataRange = record
     iFirst, iLast: Integer;
    end;



procedure ExtentRange (var rng: TFloatRange; const src: TFloatRange); overload;
procedure ExtentRange (var rng: TFloatRange; v: Single); overload;

procedure InitRange (var rng: TFloatRange);

function  MakeInstrData(AOwner: TObject): TDataMap;
procedure AddInstrData(map: TDataMap; instr_index: Integer);

implementation
uses Math, Misc;


procedure ExtentRange (var rng: TFloatRange; v: Single); overload;
begin
 if isNan (v) then
  begin
   PrintError ('ExtentRange: v is NaN');
   exit;
  end;
 try
  rng.minv := Min (rng.minv, v);
  rng.maxv := Max (rng.maxv, v);
 except
  on E: Exception do
    begin
     PrintError ('In ExtentRange catched exception: ' + e.Message );
     Sleep(50);
     with rng do  ODS ( Format('#DBG_EXCEPT: minv = %.5f, maxv = %.5f, v = %.f ', [minv, maxv, v]) );
    end;
 end;
end; // ExtentRange

procedure ExtentRange (var rng: TFloatRange; const src: TFloatRange); overload;
begin
 if src.minv <= src.maxv then
  begin
   ExtentRange (rng, src.minv);
   ExtentRange (rng, src.maxv);
  end; 
end;

procedure InitRange (var rng: TFloatRange);
begin
 rng.minv := +INFINITE;
 rng.maxv := -INFINITE;
end;


function MakeInstrData;
begin
 result := TDataMap.Create (AOwner);
 result.Name := 'InstrData';
 AddInstrData(result, 0);
 // ODS(Format('[~T]. #MEMORY: Created InstrData $%p', [Pointer(result)]));
end;

procedure AddInstrData(map: TDataMap; instr_index: Integer);
const
     STD_STREAM = DSF_BARS_DATA or DSF_DATA_STREAM;

var suffix: String;
    date: TDateMap;

procedure RegData32 (const name: String; fset: DWORD; is_float: Boolean = TRUE);
var ma: TArray32;
begin
 ma := map.AddArray32 (name + suffix, is_float);
 ma.Flags := fset;
 ma.Tags ['INSTR_ALIAS'] := map.Values['INSTR_ALIAS' + suffix];
end;

begin
 suffix := '';
 if instr_index > 0 then
    suffix := IntToStr(instr_index + 1);
 if map.IndexOfName('INSTR_ALIAS' + suffix ) < 0 then
    map.Add('INSTR_ALIAS' + suffix + '=?');

 RegData32 ('OPEN', STD_STREAM);
 RegData32 ('HIGH', STD_STREAM);
 RegData32 ('LOW', STD_STREAM);
 RegData32 ('CLOSE', STD_STREAM);
 RegData32 ('VOLUME', STD_STREAM);
 RegData32 ('VPRICE', STD_STREAM);
 RegData32 ('TIME_OPEN',  STD_STREAM or DSF_TIME_STREAM, FALSE);
 RegData32 ('TIME_CLOSE', STD_STREAM or DSF_TIME_STREAM, FALSE);
 date := TDateMap.Create;
 date.Flags := DSF_DATE_STREAM;
 map.AddObject ('DATE' + suffix, date);
end;


end.
