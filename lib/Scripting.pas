unit Scripting;
{ Модуль описания базовых классов, на основе которых можно выполнять сценарии. }
interface
uses Windows, SysUtils, Classes, StrUtils, StrClasses, WThreads, UNIArray, Misc, ContNrs;


const
     MAXINT64 = $7FFFFFFFFFFFFFFF;
     SVT_NONE    = $0000;
     SVT_INTEGER = $0001;
     SVT_POINTER = $0002;
     SVT_FLOAT   = $0003;
     SVT_NUMBER  = SVT_FLOAT;
     SVT_BOOLEAN = $0004;
     SVT_STRING  = $0005;
     SVT_USTRING = $0007;
     SVT_NIL     = $0008;        // special  value
     SVT_UNTYPED = SVT_NIL;
     SVT_AUTO    = $1000000;


     SVT_COMPLEX = $0100;
     SVT_TABLE   = $0110;
     SVT_ARRAY   = $0120;        // for combination

     SVT_SIZE_1B  = $0100;
     SVT_SIZE_2B  = $0200;
     SVT_SIZE_4B  = $0400;
     SVT_SIZE_8B  = $0800;
     SVT_SIZE_10B = $0A00;

     SVT_UPDATED  = $80000000;

     MAX_VARS = 256 * 1024;
     MAX_VNAME_LEN = 21;

type
     TScriptEngine = class;
     TScriptVarTable = class;
     TScriptVarArray = class;

     TScriptVar = packed record
      vname: String;
      vtype: DWORD;
      astr: AnsiString;
      wstr: WideString;
      fval: Extended;

      procedure          Init (const sv: TScriptVar);
      procedure          Decode   (s: String);
      procedure          SetBool  (b: Boolean);
      procedure          SetInt   (i: Int64);
      procedure          SetFloat (f: Extended);
      procedure          SetStr   (s: String);
      procedure          SetPtr   (p: Pointer);
      procedure          Empty;

      case BYTE of
         1: (ival: Int64;);
         2: (pval: Pointer);
         4: (bval: Boolean);
         5: (objv: TObject);  // unknow object?
         7: (carr: array [0..7] of AnsiChar);
       $10: (varr: TScriptVarArray);
       $11: (vtab: TScriptVarTable);
     end; // TScriptVar

     PScriptVar = ^TScriptVar;

     TSVarsArray = packed array [0..MAX_VARS - 1] of TScriptVar;
     PSVarsArray = ^TSVarsArray;

     // общий класс для массивов, таблиц и записей
     TScriptVarComplex = class (TMatrixArray)
     private
      FNested: Boolean;
      FCollector: TObjectList;          // для всех вложенных объектов
      FOwner: TScriptVarComplex;    // по отношению к кому вложенный
     public
      { props }
      property  Nested: Boolean read FNested write FNested;
      property  Collector: TObjectList read FCollector;
      property  Owner: TScriptVarComplex read FOwner write FOwner;
      { C & D }
      constructor       Create (nRowSize: Integer; bRoot: Boolean);
      destructor        Destroy; override;

      { methods }
      procedure         Clear; override;
      procedure         CleanVar (psv: PScriptVar); virtual;
      procedure         Ownerize(obj: TObject); virtual;
      function          Root: TScriptVarComplex;
     end; // TScriptVarComplex

     // Массив переменных
     TScriptVarArray = class (TScriptVarComplex)
     private
      function          GetItems: PSVarsArray; inline;
     public
      property          Items: PSVarsArray read GetItems;

      { C & D }
      constructor       Create (bRoot: Boolean);
      destructor        Destroy; override;

      { Methods }
      function          Add: PScriptVar;

      procedure         DeleteItems (iFirst, nCount: Integer); override;

      procedure         LoadStrings (src: TStrings);
      procedure         LoadObjects (src: TStrings);
     end;

     // Список переменных
     TScriptVarList = class (TScriptVarArray)
     private
      FEngine: TScriptEngine;
      FNames: TStrMap;
      FAutoRegVars: Boolean;


      function GetVariable(index: String): PScriptVar;
     public

      { props }
      property          AutoRegVars: Boolean read FAutoRegVars write FAutoRegVars;
      property          Engine: TScriptEngine read FEngine;

      property          Names: TStrMap read FNames;
      property          Vars[index: String]: PScriptVar read GetVariable;

      { C & D }
      constructor       Create (AEngine: TScriptEngine);
      destructor        Destroy; override;

      { methods }
      function          AddVar(sName: String; vtype: DWORD = SVT_INTEGER; ptmpl: PScriptVar = nil): PScriptVar; virtual;
      procedure         Clear; override;
      function          FindVar(const sName: String): Integer; virtual;
      procedure         RemoveVar (const sName: String); virtual;
      procedure         ReIndexVars;

     end; // TScriptVarList

     TSyncType = ( SYNC_READ, SYNC_WRITE, SYNC_WRITE_FORCED );




     TScriptVarPair = record
      key, value: TScriptVar; // подразумевается что ключ или строка, или число.
      // nested: TScriptVarComplex; // накуя?
     end; // TScriptVarPair
     PScriptVarPair = ^TScriptVarPair;

     TVarPairsArray = packed array [0..256 * 1024 - 1] of TScriptVarPair;
     PVarPairsArray = ^TVarPairsArray;

     // класс ассоциативного массива (таблицы)
     TScriptVarTable = class (TScriptVarComplex)
     private
      FMap: TStrMap;   // для поиска по строковым ключам.

      function          GetPairs: PVarPairsArray;
      function          GetValue(const sk: String): PScriptVar;
     public
      { props }
      property          Pairs: PVarPairsArray read GetPairs;
      property          Values[const index: String]: PScriptVar read GetValue;
      { C & D }
      constructor       Create(bRoot: Boolean = FALSE);
      destructor        Destroy; override;
      { methods }
      function          Add: PScriptVarPair;
      procedure         DeleteItems (iFirst, nCount: Integer); override;
      // возвращает TRUE, если таблица имеет непрерывные числовые индексы, начинающиеся с iFirst
      function          IsArray(iFirst: Integer): Boolean; virtual;

      procedure         LoadMap (src: TStrMap); // загрузка строкового ассоциативного массива

      function          ToArray: TScriptVarArray; virtual; // тупо складывает values в массив
      procedure         UpdateMap; virtual;

     end; // TScriptVarTable

     // класс скриптового движка, инкапсулирует базовые функции. Для наследования!
     // в основе представляет собой хранилище текста сценария.
     TScriptEngine = class (TStrMap)
     protected
      FLogOutput: TStrings;             //  для вывода отладочных сообщений из сценария
       FSupCodes: String;                 // поддерживаемые коды сценариев через запятую
        FVarList: TScriptVarList;

      sltmp: TStrMap;
     public

      { methods }
      property  LogOutput: TStrings read FLogOutput write FLogOutput; // связывается
      property  SupportCodes: String read FSupCodes;
      property  VarList: TScriptVarList read FVarList;

      { C & D }
      constructor       Create;
      destructor        Destroy; override;
      { methods }

      function          Add (const s: String): Integer; override;
      procedure         Init; virtual; // инициализация движка, сброс состояния

      procedure         LogMsg (msg: String; dupConsole: Boolean = TRUE); virtual;


      procedure         CallFunc(const fname: String); virtual; abstract; // вызов функции без параметров
      procedure         CallFuncEx (const fname, in_params, out_params: String); virtual; // вызов с параметрами

      function          RegFunc(const fname: String; pfunc: Pointer): Boolean; virtual; abstract; // регистрация функции в VM


      procedure         RegAllVars (bRegister: Boolean = TRUE); virtual;
      procedure         RegVar (sv: PScriptVar; bRegister: Boolean = TRUE); virtual; abstract;

      procedure         SyncVars (synct: TSyncType); virtual; // считывание или запись переменных из/в скриптовую машину


      procedure         Execute; virtual;       // выполнить сценарий

      // io-ops
      function          ReadArray (const sName: String; idx: Integer = 0): TScriptVarArray; virtual; abstract;
      function          ReadTable (const sName: String; idx: Integer = 0): TScriptVarTable; virtual; abstract;

      procedure         WriteArray (const sName: String; varr: TScriptVarArray; idx: Integer = 0); virtual; abstract;
      procedure         WriteTable (const sName: String; varr: TScriptVarTable; idx: Integer = 0); virtual; abstract;

      // доступ к глобальным и зарегистрированным переменным
      procedure         ReadVar (sv: PScriptVar); virtual; abstract;
      procedure         WriteVar (sv: PScriptVar); virtual; abstract;


      function          VarGet(const sName: String): PScriptVar;
      function          VarSet(const sName: String;  vnum: Double; bInt: Boolean = FALSE): Boolean; overload;
      function          VarSet(const sName: String;  vstr: String): Boolean; overload;
      function          VarSet(const sName: String;   vbl: Boolean): Boolean; overload;
      function          VarSet(const sName: String;  vptr: Pointer): Boolean; overload;

      function          VarType (const sName: String ): DWORD; virtual; abstract;

     end; // TScriptEngine

procedure InitScriptVar(const sName: String; vtype: DWORD; var sv: TScriptVar; AOwner: TScriptVarComplex);

implementation

procedure InitScriptVar(const sName: String; vtype: DWORD; var sv: TScriptVar; AOwner: TScriptVarComplex);
begin
 FillChar(sv, sizeof (sv), 0);
 sv.vname := sName;
 sv.vtype := vtype;
 sv.astr := '';
 sv.wstr := '';
 if (sv.pval = nil) and (vtype and SVT_COMPLEX <> 0) then
   begin
    case vtype of
     SVT_ARRAY: sv.varr := TScriptVarArray.Create(FALSE);
     SVT_TABLE: sv.vtab := TScriptVarTable.Create(FALSE);
    end;

    AOwner.Ownerize (sv.objv);
   end;

end;

{ TScriptVarList }

function TScriptVarList.AddVar(sName: String; vtype: DWORD; ptmpl: PScriptVar): PScriptVar;
var
    old_items: Pointer;
            i: Integer;
begin
 sName := Trim (sName);
 sName := AnsiReplaceStr(sName, ' ', '_');

 i := FindVar (sName);

 if i >= 0 then
   begin
    result := @Items [i];
    Assert ( result.vname = sName, Format('AddVar/FindVar: result.vname = %s <> sName = %s ', [result.vname, sName]) );
    exit;
   end;
 old_items := Items;
 result := AddRow;

 if ptmpl <> nil then
   result^ := ptmpl^
 else
   InitScriptVar(sName, vtype, result^, self);

 if old_items = Items then // data block not moved
    Names.AddObject ( sName, TObject (result) )
 else
    ReIndexVars;

 if AutoRegVars and Assigned (Engine) then
    Engine.RegVar (result, TRUE);
end;

procedure TScriptVarList.Clear;
begin
 inherited;
 FNames.Clear;
end;

constructor TScriptVarList.Create;
begin
 FEngine := AEngine;
 FNames := TStrMap.Create (self);
 FNames.Sorted := TRUE;
 FNames.CaseSensitive := FALSE;
 inherited Create (TRUE);
end;


destructor TScriptVarList.Destroy;
begin
 if Count > 0 then DeleteItems (0, Count);
 FNames.Free;
 inherited;
end;

function TScriptVarList.FindVar(const sName: String): Integer;
var pv: PScriptVar;
    i: Integer;
begin
 result := -1;
 Names.Lock('FindVar');
 try
  i := Names.IndexOf(sName);
  if i < 0 then exit;
  pv := PScriptVar ( Names.Objects[i] ); // strong typecast
  i :=  DWORD (pv) - DWORD(Items);       // разница в адресах
  result := i div sizeof (TScriptVar);        // индекс
 finally
  Names.Unlock;
 end;
end; // FindVar


function TScriptVarList.GetVariable(index: String): PScriptVar;
var i: Integer;
begin
 i := FindVar (index);
 result := nil;
 if (i >= 0) then result := @Items[i];
end; // GetVariable

procedure TScriptVarList.ReIndexVars;
var n: Integer;
begin
 Names.Lock('ReIndex');
 try
  Names.Clear;
  for n := 0 to Count - 1 do
     Names.AddObject ( String(Items[n].vname), TObject ( @Items[n] ) );
 finally
  Names.Unlock;
 end;
end; // ReIndexVars

procedure TScriptVarList.RemoveVar(const sName: String);
var i: Integer;
begin
 i := FindVar (sName);
 if i < 0 then exit;
 if AutoRegVars and Assigned (Engine) then
    Engine.RegVar ( @Items[i], FALSE);
 i := FNames.IndexOf (sName);
 if i >= 0 then FNames.Delete (i);
 DeleteItems(i, 1);
end; // RemoveVar

{ TScriptVarList }

function TScriptVarArray.Add: PScriptVar;
begin
 result := AddRow ();
 ZeroMemory (result, sizeof (TScriptVar));
 result.Empty;
end;

constructor TScriptVarArray.Create(bRoot: Boolean);
begin
 inherited Create ( sizeof (TScriptVar), bRoot );
end;

procedure TScriptVarArray.DeleteItems(iFirst, nCount: Integer);
var i: Integer;
begin
 // освобождение памяти под строки
  for i := iFirst to iFirst + nCount - 1 do
           CleanVar ( @Items[i] );
  inherited;
end; // DeleteItems

destructor TScriptVarArray.Destroy;
begin
 DeleteItems (0, Count);
 inherited;
end;



function TScriptVarArray.GetItems: PSVarsArray;
begin
 result := Memory;
end;

procedure TScriptVarArray.LoadObjects(src: TStrings);
var
   n: Integer;
begin
 for n := 0 to src.Count - 1 do
     Add.SetPtr ( src.Objects [n] );

end;

procedure TScriptVarArray.LoadStrings(src: TStrings);
var
   n: Integer;
begin
 for n := 0 to src.Count - 1 do
     Add.Decode ( src [n] );
end;

{ TScriptEngine }

function TScriptEngine.Add(const s: String): Integer;
var  sv: String;
begin
 sv := AnsiReplaceStr ( s, '%EOL', #13#10 );
 sv := AnsiReplaceStr ( sv, '%EOF', #26 );
 sv := AnsiReplaceStr ( sv, '%TAB', #9 );

 result := inherited Add ( sv );
end;

procedure TScriptEngine.CallFuncEx(const fname, in_params, out_params: String);
begin
 // nothing actions here?
end;

constructor TScriptEngine.Create;
begin
 FVarList := TScriptVarList.Create (self);
 sltmp := TStrMap.Create(self);
 inherited Create (nil);
end; // create

destructor TScriptEngine.Destroy;
begin
 FVarList.Free;
 sltmp.Free;
 inherited;
end;

procedure TScriptEngine.Execute;
begin
 // abstract method?
end;

procedure TScriptEngine.Init;
begin
 ///ddd
 // VarList.Clear;
end;

procedure TScriptEngine.LogMsg(msg: String; dupConsole: Boolean = TRUE);
begin
 if dupConsole then
    ODS (msg);

 msg := RemoveColorTags ( InfoFmt (msg) );

 if Assigned (FLogOutput) then
    FLogOutput.Add (msg);
end;

procedure TScriptEngine.RegAllVars;
var n: Integer;
begin
 for n := 0 to VarList.Count - 1 do
     RegVar ( @VarList.Items [n], bRegister);
end; // RegAllVars





procedure TScriptEngine.SyncVars;
var n: Integer;
    psv: PScriptVar;
begin
 for n := 0 to VarList.Count - 1 do
  begin
   psv := @VarList.Items[n];
   if ( psv.vtype and SVT_UPDATED <> 0 ) and (synct = SYNC_WRITE ) or ( synct = SYNC_WRITE_FORCED ) then
        WriteVar (psv);
   if ( synct = SYNC_READ ) then ReadVar (psv);
   psv.vtype := psv.vtype and (not SVT_UPDATED);
  end;
end; // SyncVars

function TScriptEngine.VarGet(const sName: String): PScriptVar;
begin
 result := VarList.Vars [sName];
 if result = nil then exit;
 ReadVar(result);
end;// VarGet


function TScriptEngine.VarSet(const sName: String; vptr: Pointer): Boolean;
var psv: PScriptVar;
begin
 psv := VarList.Vars [sName];
 if psv = nil then
    psv := VarList.AddVar (sName, SVT_POINTER);
 psv.pval := vptr;
 WriteVar(psv);
 result := TRUE;
end;// VarSet pointer

function TScriptEngine.VarSet(const sName: String; vnum: Double; bInt: Boolean ): Boolean;
var psv: PScriptVar;
begin
 psv := VarList.Vars [sName];
 if psv = nil then
    psv := VarList.AddVar (sName, SVT_NUMBER);
 psv.fval := vnum;
 // check for i64 compatibilty: vnum >= $80... and vnum $7f..
 if (vnum >= (- MAXINT64 - 1) ) and (vnum <= + MAXINT64) then
     psv.ival := Round (vnum);
 if bInt and (psv.ival = psv.fval) then
    psv.vtype := SVT_INTEGER;

 WriteVar(psv);
 result := TRUE;        // TODO: may add error check
end;// VarSet number

function TScriptEngine.VarSet(const sName: String; vstr: String): Boolean;
var psv: PScriptVar;
begin
 psv := VarList.Vars [sName];
 if psv = nil then
    psv := VarList.AddVar (sName, SVT_STRING);
 psv.astr := AnsiString (vstr);
 psv.wstr := WideString (vstr);
 WriteVar(psv);
 result := TRUE;
end;// VarSet string

function TScriptEngine.VarSet(const sName: String; vbl: Boolean): Boolean;
var psv: PScriptVar;
begin
 psv := VarList.Vars [sName];
 if psv = nil then
    psv := VarList.AddVar (sName, SVT_BOOLEAN);
 psv.bval := vbl;
 WriteVar(psv);
 result := TRUE;
end;// VarSet bool





{ TScriptVarComplex }

procedure TScriptVarComplex.CleanVar(psv: PScriptVar);
begin
 with psv^ do
     begin
      if ( vtype and SVT_COMPLEX <> 0 ) and ( Assigned (objv) ) then
        try
         if  Assigned (Root.Collector) then
           Root.Collector.Remove (objv)
         else
           objv.Free;
         objv := nil;

        except
         on E: Exception do
            OnExceptLog ('TScriptVarComplex.CleanVar ' + psv.vname, E);

        end;
      Empty;
     end; // with
end; // CleanVar

procedure TScriptVarComplex.Clear;
begin
 DeleteItems (0, Count);
 inherited;
 if Assigned (Collector) then Collector.Clear;
end;

constructor TScriptVarComplex.Create(nRowSize: Integer; bRoot: Boolean);
begin
 inherited Create (nRowSize);
 if bRoot then
    FCollector := TObjectList.Create (TRUE);
end;

destructor TScriptVarComplex.Destroy;
begin
 inherited;
 if Assigned (FCollector) then
    FCollector.Free;
end;

procedure TScriptVarComplex.Ownerize(obj: TObject);
begin
 if Assigned (Collector) then
    begin
     if obj is TScriptVarComplex then
        TScriptVarComplex (obj).Owner := self;
     Collector.Add (obj);
    end
   else
    if self.Root <> self then Root.Ownerize (obj);
end; // Ownerize

function TScriptVarComplex.Root: TScriptVarComplex;
begin
 result := self;
 if Owner <> nil then result := Owner.Root;
 if (result = self) and (FCollector = nil) then
     FCollector := TObjectList.Create (TRUE); // по требованию так сказать
end;



{ TScriptVarTable }

function TScriptVarTable.Add: PScriptVarPair;
begin
 result := AddRow ();
 ZeroMemory (result, sizeof (TScriptVarPair));
 result.key.Empty;
 result.value.Empty;
end;

constructor TScriptVarTable.Create(bRoot: Boolean);
begin
 inherited Create (sizeof (TScriptVarPair), bRoot);
 FMap := TStrMap.Create (self);
end;

procedure TScriptVarTable.DeleteItems(iFirst, nCount: Integer);
var i: Integer;
begin
  for i := iFirst to iFirst + nCount - 1 do
    with Pairs[i] do
     begin
      CleanVar ( @key );
      CleanVar ( @value );
     end; // with
  inherited;
end; // DeleteItems


destructor TScriptVarTable.Destroy;
begin
 FMap.Free;
 inherited;
end;

function TScriptVarTable.GetPairs: PVarPairsArray;
begin
 result := Memory;
end;

function TScriptVarTable.GetValue(const sk: String): PScriptVar;
var n: Integer;
begin
 result := nil;
 n := FMap.IndexOf (sk);
 if n < 0 then exit;
 result := @Pairs[n].value;
end;

function TScriptVarTable.IsArray(iFirst: Integer): Boolean;
var n, i: Integer;
    ik, fk: Boolean;
begin
 result := TRUE;
 for n := 0 to Count - 1 do
 with pairs [n] do
  begin
   i := n + iFirst;
   ik := (key.vtype = SVT_INTEGER) and (key.ival = i);
   fk := (key.vtype = SVT_FLOAT) and (key.fval = i);
   result := result and ( ik or fk );
   if not result then exit;
  end;

end;

procedure TScriptVarTable.LoadMap(src: TStrMap);
var
   vp: PScriptVarPair;
    n: Integer;

begin
 // добавление пар ключ=значение
 for n := 0 to src.Count - 1 do
 if Pos('=', src [n]) > 1 then
  begin
   vp := Add;
   vp.key.SetStr( src.Names [n] );
   vp.value.Decode ( src.ValueFromIndex [n] );
  end;

 UpdateMap;
end;

function TScriptVarTable.ToArray: TScriptVarArray;
var n: Integer;
    psva: PSVarsArray;
begin
 result := TScriptVarArray.Create (FALSE);
 if Count = 0 then exit;
 psva := result.AddRows(Count);
 FillChar (psva^, Count * sizeof(TScriptVar), 0);
 for n := 0 to Count - 1 do
     psva[n].Init ( Pairs[n].value );

end; // ToArray

procedure TScriptVarTable.UpdateMap;
var i: Integer;
    k: String;
begin
 // TODO: rescan pairs - needed after resizing and adding newest pairs
 FMap.Lock;
 FMap.Clear;
 try
  for i := 0 to Count - 1 do
  with Pairs [i] do
   begin
    k := '';
    case key.vtype and $FF of
     SVT_INTEGER: k := IntToStr (key.ival);
       SVT_FLOAT: k := FormatFloat ('0.########', key.fval);
      SVT_STRING: k := String (key.astr);
     SVT_USTRING: k := key.wstr;
     SVT_POINTER: k := IntToHex ( DWORD(key.pval),  8);
     SVT_BOOLEAN: if key.bval then k := 'TRUE' else k := 'FALSE';
    end;

    if k <> '' then FMap.AddObject (k, TObject(@value));
   end;
 finally
  FMap.Unlock;
 end;
end; // UpdateMap

function NotNil (pdata: Pointer): Boolean;
begin
 result := PDWORD (pdata) <> nil;
end;

{ TScriptVar }


procedure TScriptVar.Decode(s: String);

const
   digits = ['0'..'9'];
   hex = ['a'..'f', 'A'..'F'];

var
   dc: Integer;
   pc: Integer;
   sc: Integer;
   hx: Boolean;
   ng: Boolean;
   ls: String;
    c: CHAR;

    f: Integer;
    n: Integer;
begin
 if Length (s) = 0 then
   begin
    SetStr (s);
    exit;
   end;


 hx := ( s [1] = '$' );
 ng := ( s [1] = '-' );

 dc := 0;
 pc := 0;
 sc := 0;

 f := 1;
 if hx or ng then f := 2;


 for n := f to Length (s) do
  begin
   c := s [n];

   if CharInSet( c, digits ) or                   // detect number
      ( CharInSet( c, hex ) and hx ) or           // detect hexadecimal
      ( ( UpCase(c) = 'E' ) and ( dc > 0 ) ) then // detect exponentional digits
      Inc (dc)
   else
   if ( c = '.' ) or ( c = ',' ) then
     begin
      Inc (pc);
      if pc > 1 then break;
     end
   else
     begin
      Inc (sc);
      break; // single symbol detected
     end;
  end;

 // if symbols count > 0
 ls := LowerCase ( Trim(s) );
 if ( ls = 'true' ) or ( ls = 'false' ) then
   begin
    SetBool ( ls = 'true' );
    exit;
   end;

 if sc <> 0 then
   begin
    SetStr (s);
    exit;
   end;

 if ( pc = 1 ) and ( dc > 0 ) and ( not hx ) then
   begin
    SetFloat ( atof (s) );
    exit;
   end;

 if ( pc = 0 ) and ( dc > 0 ) then
   begin
    SetInt ( atoi (s) );
    exit;
   end;


 SetStr (s);
end;

procedure TScriptVar.Empty;
begin
 SetLength (vname, 0);
 SetLength (astr, 0);
 SetLength (wstr, 0);
end;

procedure TScriptVar.Init(const sv: TScriptVar);
begin
 vtype := sv.vtype;
 vname := sv.vname;
 astr := sv.astr;
 wstr := sv.wstr;
 ival := sv.ival;
 fval := sv.fval;
end;

procedure TScriptVar.SetBool(b: Boolean);
begin
 if (vtype = 0) then vtype := SVT_BOOLEAN or SVT_SIZE_4B;
 bval := b;
end;

procedure TScriptVar.SetFloat(f: Extended);
begin
 if (vtype = 0) then vtype := SVT_FLOAT or SVT_SIZE_8B;
 fval := f;
end;

procedure TScriptVar.SetInt(i: Int64);
begin
 if (vtype = 0) then vtype := SVT_INTEGER or SVT_SIZE_4B;
 ival := i;
end;

procedure TScriptVar.SetPtr(p: Pointer);
begin
 if (vtype = 0) then
     vtype := SVT_POINTER or SVT_SIZE_4B;
 pval := p;
end;

procedure TScriptVar.SetStr(s: String);
begin
 if (vtype = 0) then vtype := SVT_USTRING;

 SetLength (wstr, Length(s));
 SetLength (astr, Length(s));
 wstr := s;
 astr := AnsiString (s);
end;

end.
