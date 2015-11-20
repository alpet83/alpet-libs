unit StrClasses;

interface
uses Windows, SysUtils, StrUtils, Classes, ContNrs, SyncObjs, FastSync, ArrayTypes, Misc;

{$WARN SYMBOL_PLATFORM OFF}
const
    SPF_HIDESPACES  = 1;
    SPF_AUTO_UNHIDE = 2;
    SPF_AUTO_TRIM   = 4;
    NDF_MAKE_COPY   = 1;

type
    TFloat = Extended;

    TSortBy = (SortByName, SortByExt, SortByAccessDate, SortByCreateDate, SortByUpdateDate, SortBySize);

    TNamedData = class
    private
      FTypeName: String;
     FDataFlags: DWORD;
      FItemSize: NativeUInt;
       FDataPtr: Pointer;
    FItemsCount: Integer;
      FDataSize: NativeUInt;
    protected


    public

     property  DataPtr: Pointer read FDataPtr;
     property  DataSize: NativeUInt read FDataSize;
     property  TypeName: String read FTypeName write FTypeName;
     property  ItemsCount: Integer read FItemsCount;
     property  ItemSize: NativeUInt read FItemSize;


     { C & D }
     constructor    Create ( const ATypeName: String; buff: Pointer; cbSize, flags: NativeUInt );
     destructor     Destroy; override;


    end;

    TSimpleString = class
    public
     s: String;
    end;

    TStrTag32 = record
    case BYTE of
     0: (iValue: Integer);
     1: (fValue: Single);
     2: (pValue: Pointer);
     3: (oValue: TObject);
    end; // TStrTag32

    TStrMap = class (TStringList)
    private
     FName: String;
     FFloatFormat: String; // for SetFloat
     FAutoHideSP: Boolean;
     FAccessing: Integer;
     FStringsUpdated: Boolean;

     function           GetIntegerTag(nIndex: Integer): Integer; inline;
     procedure          SetIntegerTag(nIndex, v: Integer);       inline;
     function           GetFloatTag (nIndex: Integer): Single;   inline;
     procedure          SetFloatTag (nIndex: Integer; f: Single); inline;

     function           GetAsFloat(const Index: Integer): TFloat;              inline;
     function           GetAsInteger(const Index: Integer): Integer;           inline;
     procedure          SetFloat(const Index: Integer; const Value: TFloat);   inline;
     procedure          SetInteger(const Index, Value: Integer);               inline;

     function           GetFloatVal (const sIndex: String): TFloat;   inline;
     function           GetIntVal   (const sIndex: String): Int64;  inline;

     procedure          SetFloatVal   (const sIndex: String; fVal: TFloat); inline;
     procedure          SetIntVal   (const sIndex: String; nVal: Int64);  inline;
     function           GetDataBlock(const Index: Integer): TDataBlock;

    protected
         f_share: TCritSection;
      spin_count: Integer; // f_share spin_lock

          FOwner: TObject;
          FMutex: THandle;
       FMutexCtx: String;
       FMutexOwn: DWORD;
       FRefCount: Integer;
      FReadIndex: Integer;
        ref_list: TStrings;



     { methods }

     function           sc_share: TCritSection;

     function           GetMutex: THandle;




    public

     tagValues: array [0..7] of Integer;

     FloatFmt: String;  // template for float-to-string conversion



     // присваивание строковому слоту
     property           Accessing: Integer read FAccessing write FAccessing;

     property           AsFloat[const Index: Integer]: TFloat read GetAsFloat write SetFloat;
     property           AsInteger[const Index: Integer]: Integer read GetAsInteger write SetInteger;
     property           AutoHideSP: Boolean read FAutoHideSP write FAutoHideSP;
     property           Data [const Index: Integer]: TDataBlock read GetDataBlock;

     function           FindAdd (const s: String; obj: TObject = nil): Integer; virtual;

     property           FloatFormat: String read FFloatFormat write FFloatFormat;
     // присваивание значению в слоте
     property           FloatValues [const Index: String]: TFloat read GetFloatVal write SetFloatVal;

     property           IntValues[const Index: String]: Int64 read GetIntVal write SetIntVal;

     property           I32Tags[Index: Integer]: Integer read GetIntegerTag write SetIntegerTag;
     property           F32Tags[Index: Integer]: Single read GetFloatTag write SetFloatTag;

     property           Owner: TObject read FOwner;
     property           Name: String read FName write FName;
     property           ReadIndex: Integer read FReadIndex write FReadIndex;
     property           RefCount: Integer read FRefCount;
     property           StringsUpdated: Boolean read FStringsUpdated write FStringsUpdated;

     { C & D }
     constructor        Create (AOwner: TObject = nil);
     destructor         Destroy; override;

     { Methods }
     function           Add(const s: String): Integer; override;
     function           AddAnsi(const sAnsi: AnsiString): Integer; virtual;
     function           AddAsync(const s: String; obj: TObject = nil): Integer; virtual;
     procedure          AppendToFile (const sFileName: String); virtual;
     function           AutoLine(n: Integer): String;
     function           Content: String;
     function           DumpLockHistory: String;

     function           FindDBObj (const sIndex: String; bHideSP: Boolean = FALSE): TDataBlock;
     function           FindObject (sIndex: String; bHideSP: Boolean =  FALSE; addClass: TClass = nil): TObject; inline;
     function           FindSub (const s: String; iFrom: Integer = 0; iTo: Integer = -1 ): Integer;

     procedure          FreeData; virtual;
     procedure          FreeObjects; virtual;

     function           HasKey(const k: String): Boolean; inline;


     function           InRange (nIndex: Integer): Boolean;
     procedure          InsertStrings (src: TStrings; Index: Integer);
     function           IndexOfObjEx ( obj: TObject; iFrom, iTo: Integer ): Integer; virtual;
     function           IndexOfObject ( obj: TObject ): Integer; override;

     function           Join (chDelim: Char): String;
     procedure          MixStrings ( const fmt: String; a, b: TStrings );
     function           MemUsage: DWORD;
     function           MoreItems (sample: TStrings): Boolean; // возвращает true, если количество строк больше, чем в сэмпле
     function           Read: String; inline;
     function           Remove (const s: String): Boolean;
     procedure          Reverse;
     procedure          SetLockTimeoutWarns (w: Boolean);
     procedure          SetSyncObjectName (const sName: String);
     procedure          Split (chDelim: CHAR; src: String; flags: DWORD = 1); inline;
     function           Tail (nLines: Integer; const delim: String = #13#10): String;
     function           LastStr: String; inline;

     procedure          OnAttach (Sender: TObject; id_obj: String); virtual;
     procedure          OnDetach (Sender: TObject; id_obj: String); virtual;


     procedure          ParseIniSections (rlst: TStrings);
     procedure          ParseSection(const sect: String; rlst: TStrings);

     procedure          UpdateValue (const skey, sval: String);
     { sharing tools }
     function           MutexLock (const ctx: String; dwWaitTimeout, dwMaxTimeout: DWORD): Boolean;
     procedure          MutexUnlock;

     procedure          Lock(const ctx: String = ''; dwTimeout: DWORD = 200);
     function           TryLock (const ctx: String = ''; timeOut: Integer = 5000): Boolean;
     procedure          Unlock;
    end;


    TFileSearchRec = class
    public
     FileName: String;
     FileSize: Int64;
     Attr: DWORD;
     acTime, crTime, lwTime: TDateTime;

    end; // TFileSearchRec

    TFileList = class (TStrMap)
    private
     FSortBy: TSortBy;
     FDirsFirst: Boolean;
     FLastPath: String;
     function           GetItem(index: Integer): TFileSearchRec;
    public
     { props }
     property           SortBy: TSortBy read FSortBy write FSortBy;
     property           DirsFirst: Boolean read FDirsFirst write FDirsFirst;
     property           Items[index: Integer]: TFileSearchRec read GetItem;
     property           LastPath: String read FLastPath;
     { C & D }
     constructor        Create (AOwner: TObject = nil);
     { methods }
     procedure          FindFiles (const Path: String; FAttrs: Integer = faAnyFile);
    end;


    TStringTags = class (TStrMap)
    private

    public

     property   Values; default;
    end;


    TProgramParams = class (TStrMap)
    published
    public
     property Values; default;
     { C & D }
     constructor        Create;

     procedure Dump;
    end; // TProgramParams


    TScheduleItem = record
     dt: TDateTime;
     scmd: String;
     aobj: TObject;
    end; // TScheduleItem

    // список расписания, например комманд. Предполагает возможность указания даты и время
    TScheduleList = class (TStrMap)
    private
     function           GetItem (nItem: Integer): TScheduleItem;
     function           ItemTimePart (nItem: Integer): String;
    public
     { props }
     property           Items[index: Integer]: TScheduleItem read GetItem;
     { methods }
     // добавление с учетом сортировки по времени
     function           InsertItemByTime (const sLine: String; aobj: TObject = nil): Integer;


    end; // TScheduleList

function  MakeStrMap (const sFlags: String): TStrMap;
function  StrExtent (const s: String; nChars: Integer): String;

function  StrSplit(const s: String; chDelim: CHAR; pResult: Pointer; rType: TVarType; max_items: Integer): Integer;

procedure SplitPair(chDelim: CHAR; const s: String; var a, b: String);

function  DelSubstr (const s, subs: String): String;

function  PutFileContents (const sFileName, sText: String): Boolean;

procedure FreeListData    (sl: TStrings);
procedure FreeListObjects (sl: TStrings);

function  FastReplaceString (const AText, AFrom, AReplace: String): String;

procedure FillZero (var s: String; cnt: Integer);

function  SearchFiles (const path: String; Attr: DWORD = faAnyFile; Sort: TSortBy = SortByName): TFileList;

function  GetDirCT (const vn, def: String): TDateTime;



implementation
uses Math, UNIArray, DateTimeTools;


function GetDirCT (const vn, def: String): TDateTime;
var
   sn: String;
   sd: String;
   se: String;
   fl: TFileList;
    i: Integer;

begin
 result := 0;

 sn := GetEnvironmentVariable (vn);
 if sn = '' then
    sn := GetEnvironmentVariable ('SystemDrive') + '\' + def;

 if not DirectoryExists (sn) then exit;

 sd := ExtractFileDrive (sn);
 sd := AddSlash (sd);

 fl := TFileList.Create;
 try
   fl.FindFiles ( sd + '*.*' );

   sn := UpperCase (sn);

   for i := 0 to fl.Count - 1 do
    begin
     se := sd + UpperCase (fl [i]);
     if se <> sn then continue;
     result := fl.Items [i].crTime;
     break;
    end;
 finally
  fl.Free;
 end;
end;


function  DelSubstr (const s, subs: String): String;
begin
 result := s;
 if subs <> '' then
    result := AnsiReplaceStr (s, subs, '');
end;

procedure FillZero (var s: String; cnt: Integer);
begin
 cnt := Min ( cnt, Length(s) );
 FillChar (s [1], cnt * sizeof (s[1]), 0);
end;


function SearchFiles (const path: String; Attr: DWORD = faAnyFile; Sort: TSortBy = SortByName): TFileList;
begin
 result := TFileList.Create;
 result.SortBy := Sort;
 result.FindFiles (path, Attr);
end;



function  FastReplaceString (const AText, AFrom, AReplace: String): String;

   function found (const a, b: String; n, lc: Integer): Boolean; inline;
   var i: Integer;
   begin
    result := TRUE;
    for i := 1 to lc do
      if (a [n + i] <> b [i]) then result := FALSE;
   end;


var
   n, lchk, lsrc: Integer;

begin
 result := '';
 if AText = '' then exit;

 lchk := Length (AFrom);
 lsrc := Length (AText);
 // lrep := Length (AReplace);

 n := 1;


 while (n < lsrc) do
  begin
   if found (AText, AFrom, n - 1, lchk) then
    begin
     result := result + AReplace;
     Inc (n, lchk);
    end
   else
    begin
     result := result + AText [n];
     Inc (n);
    end;

  end;


end;

function PutFileContents (const sFileName, sText: String): Boolean;
var
   ftxt: Text;
begin
 result := FALSE;
 AssignFile (ftxt, sFileName);
 {$I-}
 ReWrite (ftxt);
 if IOResult <> 0 then exit;
 WriteLn (ftxt, sText);
 CloseFile (ftxt);
 {$I+}
 result := TRUE;
end;


procedure FreeListData (sl: TStrings);
var
   n: Integer;
   p: Pointer;

begin
 for n := sl.Count - 1 downto 0 do
  begin
   p := sl.Objects[n];
   FreeMem (p);
   sl.Objects[n] := nil;
  end;
end; // FreeListObjects


procedure FreeListObjects (sl: TStrings);
var n: Integer;
begin
 for n := sl.Count - 1 downto 0 do
  begin
   sl.Objects[n].Free;
   sl.Objects[n] := nil;
  end;
end; // FreeListObjects


procedure SplitPair(chDelim: CHAR; const s: String; var a, b: String);
var
   p: Integer;
begin
 a := s;
 b := '';
 p := Pos(chDelim, s);
 if p <= 0 then exit;
 a := Copy(s, 1, p - 1);  // abc@
 b := Copy(s, p + 1, Length(s) - p); // abc@def
end;

function   StrSplit(const s: String; chDelim: CHAR; pResult: Pointer; rType: TVarType; max_items: Integer): Integer;
var sltmp: TStrMap;
    n: Integer;
begin
 sltmp := TStrMap.Create;
 try
  sltmp.Split(chDelim, s);
  result := Min(max_items, sltmp.Count);
   case rType of
    varInteger,
    varLongWord: for n := 0 to result - 1 do PIntArray(pResult)[n] := Integer ( atoi (sltmp [n]) );
      varDouble: for n := 0 to result - 1 do PDoubleArray(pResult)[n] := atof (sltmp [n]);
      varSingle: for n := 0 to result - 1 do PSingleArray(pResult)[n] := atof (sltmp [n]);

   end;
 finally
  sltmp.Free;
 end;
end; // StrSplit

function  StrExtent;
begin
 result := s;
 if nChars > 0 then
    while Length(result) < nChars do result := result + ' '
 else
    while Length(result) < Abs(nChars) do result := ' '  + result;
end;

function  MakeStrMap;

begin
 result := TStrMap.Create;
 result.OwnsObjects := ( Pos('O', sFlags) > 0 );
 result.Sorted := ( Pos('S', sFlags) > 0 );
end;

{ TStrMap }

function TStrMap.Add(const s: String): Integer;
begin
 if AutoHideSP then
   result := inherited Add( HideSP(s) )
 else
   result := inherited Add(s);

 FStringsUpdated := TRUE;
end;

function TStrMap.AddAnsi(const sAnsi: AnsiString): Integer;
begin
 result := Add (String (sAnsi));
end;


function TStrMap.AddAsync(const s: String; obj: TObject): Integer;
begin
 Lock;
 try
  result := AddObject (s, obj);
  FStringsUpdated := TRUE;
 finally
  Unlock;
 end;
end; // AddAsync


procedure TStrMap.AppendToFile(const sFileName: String);
var
   ftxt: System.Text;
   s: String;
begin
  s := Trim(Text);
  if s = '' then exit;

  if FileExists (sFileName) then
    begin
     AssignFile (ftxt, sFileName);
     {$I-}
     System.Append (ftxt);
     if IOresult = 0 then
        WriteLn (ftxt, s);
     CloseFile(ftxt);
    end
  else
    SaveToFile (sFileName);
end;



function TStrMap.AutoLine(n: Integer): String;
begin
 result := self [n];
 if Pos(NameValueSeparator, result) > 0 then
        result := Names [n];

end;

function TStrMap.Content: String;
var
   n: Integer;
begin
 result := '';

 for n := 0 to Count - 1 do
     result := result + self [n];
end;

constructor TStrMap.Create;
begin
 FloatFmt := '';
 FOwner := AOwner;
 FloatFormat := '0.0###';
 inherited Create;
end;

destructor TStrMap.Destroy;
var rc, n: Integer;
    s: String;
    o: TObject;
begin
 rc := 0;
 if FMutex <> 0 then
    MutexLock ('.Destroy', 500, 500);

 if ( ref_list <> nil ) then rc := ref_list.Count;
 if (rc > 0) then
  begin
   s := '';
   for n := 0 to ref_list.Count - 1 do
         begin
          s :=s + Format(#13#10#9'%d. referer =~C0F %s~C07 ', [n + 1, ref_list[n]] );
          o := ref_list.Objects [n];
          if o <> nil then
             s := s + Format (', object class =~C0A %s~C07, at~C0D $%p~C07', [o.ClassName, Pointer(o)]);
         end;
   PrintError(ClassName + '.Destroy: RefCount = ' + IntToStr(rc) + '  > 0');
   ODS (#9#9' Dumping referers: ' + s);
  end;


 CloseHandle (FMutex);
 Clear;
 ref_list.Free;
 FreeAndNil (f_share);
 ref_list := nil;
 inherited;
end;

function TStrMap.DumpLockHistory: String;
begin
 if Assigned (sc_share.History) then
    result := sc_share.History.Text
 else
    result := sc_share.Dump;
end;

function TStrMap.FindAdd(const s: String; obj: TObject): Integer;
begin
 result := IndexOf (s);
 if result >= 0 then
    Objects [result] := obj
 else
    result := AddObject (s, obj);
end; // FindAdd


function TStrMap.FindDBObj(const sIndex: String; bHideSP: Boolean): TDataBlock;
begin
 result := TDataBlock ( FindObject ( sIndex, bHideSP ) );
end;



function TStrMap.FindObject(sIndex: String; bHideSP: Boolean; addClass: TClass): TObject;
var i: Integer;
begin
 if bHideSP then sIndex := HideSP (sIndex);
 i := IndexOf (sIndex);

 result := nil;
 if i >= 0 then
    result := Objects [i]
 else
  if addClass <> nil then
    begin
     result := addClass.Create;
     AddObject (sIndex, result);
    end;
end;

function TStrMap.FindSub(const s: String; iFrom: Integer; iTo: Integer ): Integer;
var
   n: Integer;
begin
 result := -1;

 if iTo < 0 then Inc (iTo, Count);

 for n := iFrom to iTo do
   if Pos (s, self[n]) > 0 then
      begin
       result := n;
       exit;
      end;
end;

procedure TStrMap.FreeData;
var
   n: Integer;
   p: Pointer;
begin
 for n := 0 to Count - 1 do
  begin
   p := Objects [n];
   Objects [n] := nil;
   FreeMem (p);
  end;

end;

procedure TStrMap.FreeObjects;
var n, nFree: Integer;

begin
 try
  nFree := 0;
   for n := Count - 1 downto 0 do
     begin
      if Objects[n] = nil then continue;
      if not OwnsObjects then Objects [n].Free;
      Inc (nFree);
     end;
   for n := Count - 1 downto 0 do Objects[n] := nil;

  // next line - disabled
  if nFree < 0 then ODS (Format('[~T]. ' + ClassName + '($%p).FreeObjects: released %d objects', [Pointer(self), nFree]), 1);
 except
  on E: Exception do
    PrintError ('Exception catched in TStrMap.FreeObjects: ' + E.Message);
 end
end; // FreeObjects

function TStrMap.GetFloatVal;
begin
 result := atof (Values [sIndex]);
end; // GetFloatVal

function TStrMap.GetIntVal;
var s: String;
begin
 s := Values [sIndex]; // ( Pos('$', s) > 0 ) or
 // if ( Pos( '.', s ) + Pos( ',', s ) = 0 )   then
 result := atoi (s)
 // else result := Round ( atof (s) );
end;


function TStrMap.GetMutex: THandle;
begin
 if FMutex = 0 then
  begin
   FMutex := CreateMutex (nil, FALSE, PChar (ClassName + '@' + IntToHex (DWORD(self), 7)) );
   FMutexCtx := InfoFmt('[~T/~I]: Create', Now);
   FMutexOwn := GetCurrentThreadID;
  end;

 result := FMutex;
end;

function TStrMap.HasKey(const k: String): Boolean;
begin
 result := ( IndexOfName (k) >= 0 );
end;

function TStrMap.IndexOfObject(obj: TObject): Integer;
begin
 result := IndexOfObjEx ( obj, 0, Count - 1 );
end;

function TStrMap.IndexOfObjEx (obj: TObject; iFrom, iTo: Integer): Integer;
var
   n: Integer;
begin
 result := -1;
 for n := iFrom to iTo do
  if Objects [n] = obj then
    begin
     result := n;
     exit;
    end;
end;

function TStrMap.InRange(nIndex: Integer): Boolean;
begin
 result := (nIndex >= 0) and (nIndex < Count);
end;

procedure TStrMap.InsertStrings(src: TStrings; Index: Integer);
var
   tmp: TStrMap;
   sav: Boolean;
     n: Integer;
begin
 if Index = 0 then
  begin
   // fast method
   tmp := TStrMap.Create();
   tmp.AddStrings(src);
   tmp.AddStrings(self);

   sav := OwnsObjects;
   OwnsObjects := FALSE;

   Clear;
   AddStrings(tmp);

   OwnsObjects := sav;

   tmp.Free;
  end
 else
  for n := 0 to src.Count - 1 do
      InsertObject (n + Index, src [n], src.Objects [n]);
end;

function TStrMap.Join(chDelim: Char): String;
begin
 QuoteChar := '"';
 Delimiter := chDelim;
 result := UnhideSP(DelimitedText);
end;

// GetIntVal

function TStrMap.LastStr: String;
begin
 result := '';
 if Count > 0 then result := self[Count - 1];
end;

procedure TStrMap.Lock;
begin
 sc_share.Lock ( ctx, dwTimeout );
 //BeginUpdate;
end;

function TStrMap.MemUsage: DWORD;
var
   n: Integer;
begin
 result := InstanceSize;
 for n := 0 to Count - 1 do
   begin
    Inc (result, Length(self[n]) * sizeof(Char) + 8);

    if Objects [n] <> nil then Inc (result, Objects [n].InstanceSize);
   end;
end;

procedure TStrMap.MixStrings(const fmt: String; a, b: TStrings);
var
   n: Integer;
   s: String;
begin
 for n := 0 to Min (a.Count, b.Count) - 1 do
  begin
   s := Format( fmt, [a [n], b [n]] );
   Add ( s );
  end;

end;

function TStrMap.MoreItems(sample: TStrings): Boolean;
var n: Integer;
begin
 result := FALSE;
 for n := 0 to Count - 1 do
  if sample.IndexOf ( self [n] ) < 0 then
     begin
      result := TRUE;
      break;
     end; // for-if

end; // MoreItems

function TStrMap.MutexLock(const ctx: String; dwWaitTimeout, dwMaxTimeout: DWORD): Boolean;
var
   res,  dstart: DWORD;
   cpu: TContext;
begin
 dstart := GetTickCount;
 result := ( GetCurrentThreadID = FMutexOwn );

 SetLastError (0);

 if not result  then
 while ( GetMutex <> 0 ) and ( dwMaxTimeout > GetTickCount - dstart ) do
  begin

   res := WaitForSingleObject ( FMutex, dwWaitTimeout );
   if (WAIT_OBJECT_0 = res) or (WAIT_ABANDONED = res) then
     begin
      FMutexCtx := InfoFmt('[~T/~I]: ', Now) + ctx;
      FMutexOwn := GetCurrentThreadID;
      result := TRUE;
      exit;
     end;

   if res = WAIT_TIMEOUT then
     begin
      FillChar (cpu, sizeof (cpu), 0);
      cpu.ContextFlags := CONTEXT_INTEGER or CONTEXT_CONTROL;

      ODS('[~T/~I].~C0C #WARN: MutexLock timeout for ~C0F' + self.Name + '@' + ClassName + '~C0C =~C07 ' + IntToStr (GetTickCount - dstart) +
                '~C0C ms, context =~C0F ' + ctx + ' ~C0C, active_ctx =~C0F ' + FMutexCtx + '~C0C, active_ownr =~C0F ' + IntToStr(FMutexOwn) + '~C07');
      Sleep(100);
      OnExceptLog ('MutexLock', EExternalException.CreateFmt('Mutex acquire timeout %d', [GetTickCount - dstart]), FALSE, @cpu);
     end
   else
      PrintError('WaitForSingleObject returned ' + IntToStr (res) + ', error: ' + Err2Str(GetLastError));
  end;


end;

procedure TStrMap.MutexUnlock;
begin
 ReleaseMutex (GetMutex);
end;



procedure TStrMap.Unlock;
begin
 //EndUpdate;
 Assert ( f_share <> nil, ClassName + '.Unlock: f_share = nil' );
 f_share.Unlock;
end;



procedure TStrMap.UpdateValue(const skey, sval: String);
var i: Integer;
begin
 i := IndexOfName (skey);
 if Sorted then
  begin
   if (i >= 0) then Delete (i);
  end
 else
  if i >= 0 then Values[skey] := sval;
 if i < 0 then
    Add (skey + NameValueSeparator + sval);

 FStringsUpdated := TRUE;
end; // UpdateValue

procedure TStrMap.OnAttach(Sender: TObject; id_obj: String);
begin
 if ref_list = nil then ref_list := TStringList.Create;
 if id_obj <> '' then ref_list.AddObject (id_obj, Sender);
 Inc (FRefCount);
end;

procedure TStrMap.OnDetach(Sender: TObject; id_obj: String);
var i: Integer;
begin
 Dec (FRefCount);
 i := ref_list.IndexOf(id_obj);
 if i >= 0 then ref_list.Delete(i);
end;


procedure TStrMap.ParseIniSections(rlst: TStrings);
var
   n, p: Integer;
   s: String;
begin
 rlst.Clear;
 for n := 0 to Count - 1 do
  begin
   s := Strings [n];
   if Pos ('[', s) <> 1 then continue;

   p := Pos (']', s);

   s := Copy (s, 2, p - 2);

   rlst.Add (s);
  end; // parse loop
end;

procedure TStrMap.ParseSection(const sect: String; rlst: TStrings);
var
   n: Integer;
   s: String;
   save: Boolean;

begin
 rlst.Clear;
 save := FALSE;

 for n := 0 to Count - 1 do
  begin
   s := Trim ( Strings [n] );
   // при нахождении последующей секции - прервать добавление
   if ( Pos ('[', s) = 1 ) and save then exit;
   if save then
      rlst.Add (s);

   if Pos ('[' + sect + ']', s) = 1 then
      save := TRUE; // со след. строки добавлять
 end;

end;


function TStrMap.Read: String;
begin
 if ReadIndex < Count then
   begin
    result := self[ReadIndex];
    Inc (FReadIndex);
   end
 else
    result := '';
end; // Read

function TStrMap.Remove(const s: String): Boolean;
var
   i: Integer;
begin
 i := IndexOf (s);
 result := ( i >= 0 );
 if result then
   begin
    Delete (i);
    FStringsUpdated :=  TRUE;
   end;
end;



procedure TStrMap.Reverse;
var
   a, b: Integer;
begin
 a := 0;
 b := Count - 1;

 while (a < b) do
  begin
   Exchange(a, b);
   Inc (a);
   Dec (b);
  end;


end;

function TStrMap.sc_share: TCritSection;
begin
 if f_share = nil then
   begin
    f_share := TCritSection.Create ( ClassName + '(' + FormatPtr(self) + ').sc_share', spin_count );
    f_share.msg_pass := TRUE;
   end;
 result := f_share;
end;

procedure TStrMap.SetFloatVal(const sIndex: String; fVal: TFloat);
var s: String;
begin
 s := FormatFloat (FloatFmt, fVal);
 s := AnsiReplaceStr (s, ',', '.');
 if IndexOfName (sIndex) >= 0 then
    Values[sIndex] := s
 else
    Add (sIndex + '=' + s);
end; // SetFloatVal

procedure TStrMap.SetIntVal(const sIndex: String; nVal: Int64);
begin
 if IndexOfName (sIndex) >= 0 then
    Values[sIndex] := IntToStr (nVal)
 else
    Add (sIndex + '=' + IntToStr (nVal));
end;

procedure TStrMap.SetLockTimeoutWarns(w: Boolean);
begin
 sc_share.msg_pass := w;
end;

procedure TStrMap.SetSyncObjectName(const sName: String);
begin
 sc_share.Name := sName;
end;

function TStrMap.GetIntegerTag(nIndex: Integer): Integer;
begin
 result := TStrTag32 ( Objects[nIndex] ).iValue;
end;

procedure TStrMap.SetInteger(const Index, Value: Integer);
begin
 Strings[Index] := IntToStr (Value);
end;

procedure TStrMap.SetIntegerTag(nIndex: Integer; v: Integer);
var
   t: TStrTag32;
begin
 t.iValue := v;
 Objects [nIndex] := t.oValue;
end;


function TStrMap.GetAsFloat(const Index: Integer): TFloat;
begin
 if Index >= Count then
    result := 0
 else
    result := atof ( Strings [Index] );
end;

function TStrMap.GetAsInteger(const Index: Integer): Integer;
begin
 result := atoi ( Strings [Index] );
end;

function TStrMap.GetDataBlock(const Index: Integer): TDataBlock;
begin
 result := TDataBlock ( Objects [index] );
end;

function TStrMap.GetFloatTag(nIndex: Integer): Single;
begin
 result := TStrTag32 ( Objects[nIndex] ).fValue;
end;

procedure TStrMap.SetFloat(const Index: Integer; const Value: TFloat);
begin
 Strings [Index] := FormatFloat(FloatFormat,  value);
end;

procedure TStrMap.SetFloatTag(nIndex: Integer; f: Single);
var
   t: TStrTag32;
begin
 t.fValue := f;
 Objects [nIndex] := t.oValue;
end;

// SetIntVal

procedure TStrMap.Split(chDelim: CHAR; src: String; flags: DWORD);
var n: Integer;
begin
 Delimiter := chDelim;
 if flags and SPF_HIDESPACES <> 0 then src := HideSP (src);


 DelimitedText := src;


 if flags and SPF_AUTO_UNHIDE <> 0 then
  begin
   Sorted := FALSE;
   for n := 0 to Count - 1 do
       Strings [n] := UnhideSP (src);
  end;

 if flags and SPF_AUTO_TRIM <> 0 then
  begin
   Sorted := FALSE;
   for n := 0 to Count - 1 do
       Strings [n] := Trim( Strings [n] );
  end;
 ReadIndex := 0;
end; // Split

function TStrMap.Tail(nLines: Integer; const delim: String): String;
var n, l: Integer;
begin
 result := '';
 l := Count - 1;
 Lock ('Tail');
 for n := Max (0, Count - nLines) to l do
   begin
    result := result + self[n];
    if n < l then result := result + delim;
   end;

 Unlock;
end;

function TStrMap.TryLock(const ctx: String; timeOut: Integer): Boolean;
begin
 result := FALSE;
 try
  result := sc_share.TryLock(ctx + '.TryLock', timeOut)
 except
  on E: Exception do
     OnExceptLog (FName + '.TryLock', E);
 end;
end;


{ TFileList }

constructor TFileList.Create(AOwner: TObject);
begin
 inherited Create (AOwner);
 SortBy := SortByName;
 OwnsObjects := TRUE;
end;

function FileTimeToLocalDateTime (ft: TFileTime): TDateTime;
var
   lft: TFileTime;
   st: TSystemTime;

begin
 FileTimeToLocalFileTime ( ft, lft );
 FileTimeToSystemTime (  lft, st);
 result := SystemTimeToDateTime (st);
end; // FileTimeToLocalDateTime


procedure TFileList.FindFiles;
const
   time_fmt = 'yyyy.mm.dd hh:nn:ss.zzz';

var
   sr: TSearchRec;
   fsr: TFileSearchRec;
   n: Integer;
   c: String;
begin
 Clear;
 FLastPath := Path;

 if FindFirst(Path, FAttrs, sr) = 0 then
    begin
      repeat
       fsr := TFileSearchRec.Create;
       fsr.FileName := sr.Name;
       fsr.FileSize := sr.Size;
       fsr.Attr := sr.Attr;

       // конвертирование времени
       fsr.acTime := FileTimeToLocalDateTime (sr.FindData.ftLastAccessTime);
       fsr.crTime := FileTimeToLocalDateTime (sr.FindData.ftCreationTime);
       fsr.lwTime := FileTimeToLocalDateTime (sr.FindData.ftLastWriteTime);

       c := sr.Name;

       case SortBy of
               SortByExt: c := ExtractFileExt (sr.Name) + '.' + ExtractFileName (sr.Name);
        SortByAccessDate: c := FormatDateTime (time_fmt, fsr.acTime) + #9 + sr.Name;
        SortByCreateDate: c := FormatDateTime (time_fmt, fsr.crTime) + #9 + sr.Name;
        SortByUpdateDate: c := FormatDateTime (time_fmt, fsr.lwTime) + #9 + sr.Name;
              SortBySize: c := IntToHex (fsr.FileSize, 16) + #9 + sr.Name;
       end;

       if DirsFirst and (fsr.Attr and faDirectory <> 0) then
          c := #9 + c;
       AddObject (c, fsr);
      until FindNext(sr) <> 0;
      FindClose(sr);
      Sort;
    end; // if first found

 if Count > 0 then
   for n := 0 to Count - 1 do
    self [n] := Items [n].FileName; // Вернуть имена вместо кода сортировки


end;

function TFileList.GetItem(index: Integer): TFileSearchRec;
begin
 result := TFileSearchRec ( Objects [index] );
end;

{ TProgramParams }

constructor TProgramParams.Create;
 function IsKey(const s: String): Boolean;
 begin
  result := (s <> '') and ( (s[1] = '-') or (s[1] = '/') );
 end;


var n: Integer;
    k, s: String;
begin
 inherited Create(nil);
 CaseSensitive := FALSE;

 n := 1;

 while ( n <= ParamCount ) do
  begin
   k := ParamStr(n);
   s := '';
   ODS ('#DBG: ParamStr (' + IntToStr(n) + ') = ' + k);
   Inc (n);

   if n <= ParamCount then
      s := ParamStr(n);



   if IsKey(k) then
    begin
     // очистка ключа от префикса
     while (k <> '') and ( CharInSet(k[1], ['-', '/'] ) ) do
            System.Delete(k, 1, 1);

     // если следующий параметр ключ или неопределен
     if IsKey(s) or (s = '') then
        k := k + '=1'
     else
       begin
        // следующий параметр значение
        k := k + '=' + s;
        ODS ('#DBG: ParamStr (' + IntToStr(n) + ') = ' + s);
        Inc (n);
       end;


     k := AnsiReplaceStr (k, '~', '-');
     Add(k);
    end
   else
    if ( Pos('=', k) > 0 ) then      Add (k);

  end;

  if log_verbose < 7 then exit;


end; // TProgramParams.Create

function Str2Time(dts: String): TDateTime;
var ds: String;
    ts: TTimeStamp;
begin
 ds := StrTok(dts, [' ']);
 ts := StrDateTimeDecode (ds, dts);
 result := TimeStampToDateTime(ts);
end; // Str2Time

procedure TProgramParams.Dump;
var
   n: Integer;
   k: String;
begin
  ODS('[~T]. #DBG: Program params dump:');
  for n := 0 to Count - 1 do
   begin
    k := Names [n];

    ODS ( #9#9 + k + ' = ' + Values[k] );

   end;

end;

{ TScheduleList }

function TScheduleList.GetItem(nItem: Integer): TScheduleItem;
var k: String;

begin
 result.dt := 0;
 result.scmd := '';
 result.aobj := nil;
 if (nItem < 0) or (nItem >= Count) then exit;
 k := ItemTimePart (nItem);
 result.dt := Str2Time (k);
 result.scmd := ValueFromIndex[nItem];
 result.aobj := Objects [nItem];
end;



function TScheduleList.InsertItemByTime (const sLine: String; aobj: TObject): Integer;
var
   i: Integer;
   s: String;
   dtref: TDateTime;
begin
 result := -1;
 // замена по шаблону
 s := AnsiReplaceStr ( sLine, '$TOD', FormatDateTime('dd.mm.yy', Now) );
 s := AnsiReplaceStr ( s, '$TOM', FormatDateTime('dd.mm.yy', Now + 1) );
 if Pos ('=', s) <= 0 then exit;
 result := AddObject ( s, aobj);
 if Count <= 1 then exit;
 s := ItemTimePart (result);
 dtref := Str2Time (s);
 // поиск временной метки, с большим или равным временем относительно dtref
 for i := 0 to Count - 1 do
  begin
   if i = result then exit;     // дальше отодвигать нет смысла!
   s := ItemTimePart (i);
   if dtref > Str2Time (s) then continue;
   Exchange(i, result);
   result := i;
   break;
  end;

end;  // InsertItemByTime

function TScheduleList.ItemTimePart(nItem: Integer): String;
begin
 result := AnsiReplaceStr( Names[nItem], '_', ' ' );
 result := Trim( result );
end; // ItemTimePart

{ TNameData }

constructor TNamedData.Create(const ATypeName: String; buff: Pointer; cbSize, flags: NativeUInt);
begin
 FTypeName := ATypeName;
 FDataFlags := DWORD(flags);
 FDataSize := cbSize;

 FItemsCount := 1;
 FItemSize := cbSize;

 if flags and NDF_MAKE_COPY <> 0 then
   begin
    GetMem (FDataPtr, cbSize);
    CopyMemory (FDataPtr, buff, cbSize);
   end
  else
    FDataPtr := buff;


end;

destructor TNamedData.Destroy;
begin
 if FDataFlags and NDF_MAKE_COPY <> 0 then
    FreeMem (FDataPtr, DataSize);
 inherited;
end;

end.
