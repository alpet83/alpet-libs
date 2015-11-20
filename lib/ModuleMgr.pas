unit ModuleMgr;

interface

uses Windows, SysUtils, Classes, StrUtils, ContNrs;

const
     MRQ_INITIALIZE           = $0000040F;
     MRQ_FINALIZE             = $80000500;


     MMF_WARN_DEP             = $00400000;
type

    TModuleDescriptor = class;
    TModuleDescList = class;
    TModuleStatus = (MST_UNKNOWN, MST_INITIALIZED, MST_FINALIZED);
    TModuleRqsHandler = function (md: TModuleDescriptor; rqs, flags: DWORD): Boolean;

    TSimpleCallback = procedure (src: TModuleDescriptor); stdcall;



    // класс олицетворени€ дельфийского юнита, с набором глобальных и значимых переменных
    TModuleDescriptor = class
    private
    FModule: String;
     procedure SetStatus(const Value: TModuleStatus);
    protected
     FStatus: TModuleStatus;
     FName, FDepList: String;
     FPriority: Integer;
     FOwner: TModuleDescList;
     FOnRequest: TModuleRqsHandler;
     FGlobals: TObjectList;             // global objects for auto-destroy

     cb_finalize: array [0..15] of TSimpleCallback;

    public

     property              Globals: TObjectList read FGlobals;

     property              Status: TModuleStatus read FStatus write SetStatus;
     property              Module: String read FModule;
     property              Name: String read FName;
     property              DepList: String read FDepList;
     property              Priority: Integer read FPriority write FPriority;
     property              Owner: TModuleDescList read FOwner;

     property              OnRequest: TModuleRqsHandler read FOnRequest;


     { C & D }
     constructor           Create (const AName, ADepList: String; AOnRequest: TModuleRqsHandler);
     destructor            Destroy; override;
     { methods }
     function              AddCallback (list: String; cb: TSimpleCallback): Boolean;
     procedure             CleanGlobals; // позвол€ет удал€ть глобальные объекты в отладочном режиме
     function              DependedFrom(const sName: String): Boolean;

     function              Initialize (flags: DWORD): Boolean;
     function              Finalize (flags: DWORD): Boolean;
    end; // TModuleDescriptor

    TModuleDescList = class (TObjectList)
    private
     FModule: String;
     function             GetItem(nIndex: Integer): TModuleDescriptor;
    public
     { props }
     property              Items [nIndex: Integer]: TModuleDescriptor read GetItem;
     property              Module: String read FModule;

     { C & D }
     constructor           Create;

     { methods }
     function              Add (MDesc: TModuleDescriptor): Integer;
     function              FindDesc (const sName: String): TModuleDescriptor;

     procedure             GlobalFinalize (flags: DWORD = 0);


     procedure             InitializeOne (const sName: String; flags: DWORD = 0);
     procedure             FinalizeOne (const sName: String; flags: DWORD = 0);

    end; // TModuleDescList


function GetModuleDescList: TModuleDescList; stdcall; // дл€ экспорта
function MainModuleDescList: TModuleDescList;  // основной находитс€ или локально, или у EXE


procedure InitializeModule (const sName: String; flags: DWORD = 0);
procedure FinalizeModule (const sName: String; flags: DWORD = 0);

function  FindModuleDesc (const sName: String): TModuleDescriptor;
function  RegModule (const AModName, ADepList: String; AOnRqs: TModuleRqsHandler): TModuleDescriptor;

function ModuleFileName (hInst: DWORD): String;


implementation
uses Misc;

var
    gModuleDescList: TModuleDescList = nil; // локальный список модулей


function ModuleFileName (hInst: DWORD): String;
var
   tmp: array [0..261] of AnsiChar;
   sa: AnsiString;
begin
 sa := '';
 FillChar (tmp, sizeof(tmp), 0);
 GetModuleFileNameA (hInst, @tmp, sizeof(tmp) );
 SetString (sa, tmp, StrLen (tmp));
 result := Trim ( String (sa) );
end; // ModuleFileName


function GetModuleDescList: TModuleDescList; stdcall;
begin
 if gModuleDescList = nil then
    gModuleDescList := TModuleDescList.Create;
 result := gModuleDescList;
end;

function MainModuleDescList: TModuleDescList;
var
   exe: String;
   hExe: THandle;
   gmdl: function: TModuleDescList; stdcall;
begin
 result := gModuleDescList;
 exe := ModuleFileName (0);
 if exe = '' then exit;
 hExe := GetModuleHandle ( PChar (exe) );
 if hExe = 0 then exit;
 gmdl := GetProcAddress (hExe, 'GetModuleDescList');
 if Assigned (gmdl) then
    result := gmdl;
end;

procedure InitializeModule (const sName: String; flags: DWORD = 0);
begin
 if not gGlobalStop then
    gModuleDescList.InitializeOne(sName, flags);
end;

procedure FinalizeModule (const sName: String; flags: DWORD = 0);
begin
 if sName = '~ALL' then
    gModuleDescList.GlobalFinalize(flags)
 else
    gModuleDescList.FinalizeOne(sName, flags);
end;

function RegModule (const AModName, ADepList: String; AOnRqs: TModuleRqsHandler): TModuleDescriptor;

begin
 result := GetModuleDescList.FindDesc (AModName);
 if result <> nil then exit;
 result := TModuleDescriptor.Create (AModName, ADepList, AOnRqs);
 result.FModule := gModuleDescList.Module;
 gModuleDescList.Add (result);
end; // RegModule

function  FindModuleDesc (const sName: String): TModuleDescriptor;
begin
 result := gModuleDescList.FindDesc (sName);
end;

{ TModuleDescriptor }

procedure TModuleDescriptor.CleanGlobals;
begin
 try
  Globals.Clear; // default
 except
  on E: Exception do
     OnExceptLog ('CleanGlobals for module ' + Name, E);
 end;
end; // CleanGlobals

constructor TModuleDescriptor.Create;
begin
 FName := AName;
 FStatus := MST_UNKNOWN;
 FDepList := ADepList;
 FPriority := 1000;
 FOnRequest := AOnRequest;
 FGlobals := TObjectList.Create (TRUE);
end;

destructor TModuleDescriptor.Destroy;
begin
 CleanGlobals;
 FGlobals.Free;
 inherited;
end;


function  TModuleDescriptor.AddCallback (list: String; cb: TSimpleCallback): Boolean;
var n: Integer;
begin
 result := FALSE;
 if not Assigned (cb) then exit;

 if list = 'finalize' then
  for n := 0 to High (cb_finalize) do
   if not Assigned ( cb_finalize [n] ) then
    begin
     cb_finalize [n] := cb;
     if @cb_finalize [n] = nil then exit;
     result := TRUE;
     break;
    end;
end;

function TModuleDescriptor.DependedFrom(const sName: String): Boolean;
var
   sltmp: TStringList;
begin
 sltmp := TStringList.Create;
 sltmp.CaseSensitive := FALSE;
 sltmp.CommaText := DepList;
 sltmp.Sort;
 result := ( sltmp.IndexOf(sName) >= 0 );
 sltmp.Free;
end;



function TModuleDescriptor.Finalize;
var
   n: Integer;
begin
 result := (Status <> MST_FINALIZED);
 if ( not result ) or ( not Assigned (OnRequest) ) then exit;
 if ( GetConsoleWindow <> 0 ) and con_enabled then
    ODS('[~T]. #DBG: Module ~C0A' + Module + ':' + Name + '~C07 now finalize...');

 for n := 0 to High (cb_finalize) do
     if Assigned ( cb_finalize [n] ) then
                   cb_finalize [n] (self);
 result := OnRequest(self, MRQ_FINALIZE, flags);
 if result then FStatus := MST_FINALIZED;
 CleanGlobals;
end;

function TModuleDescriptor.Initialize;
begin
 result := (Status <> MST_INITIALIZED);
 if ( not result ) or ( not Assigned (OnRequest) ) then exit;
 if ( LowerCase(Name) <> 'misc') and (con_enabled) then
     ODS('[~T]. #DBG: Module ~C0A' + Name + '~C07 now initialize...');
 result := OnRequest(self, MRQ_INITIALIZE, flags);
 if result then FStatus := MST_INITIALIZED;
end;

procedure TModuleDescriptor.SetStatus(const Value: TModuleStatus);
begin
  FStatus := Value;
end;

{ TModuleDescList }

function TModuleDescList.Add(MDesc: TModuleDescriptor): Integer;
begin
 MDesc.FOwner := self;
 result := TObjectList (self).Add(MDesc);
end;

constructor TModuleDescList.Create;
var
   buff: array [0..261] of CHAR;
begin
 inherited Create (TRUE);
 GetModuleFileName (HInstance, buff, sizeof (buff) -1);
 FModule := Format('$%x', [HInstance]) + ' - ' + buff;
end;


function TModuleDescList.FindDesc(const sName: String): TModuleDescriptor;
var n: Integer;
begin
 result := nil;
 for n := 0 to Count - 1 do
   if UpperCase (sName) = UpperCase (Items [n].Name) then
     begin
      result := Items [n];
      exit;
     end;
end; // FindModule

function TModuleDescList.GetItem(nIndex: Integer): TModuleDescriptor;
begin
 result := TObjectList(self).Items [nIndex] as TModuleDescriptor;
end;

procedure TModuleDescList.GlobalFinalize;
var n: Integer;
begin
 // TODO: incomplete logic
 for n := 0 to Count - 1 do
  if Items [n] <> nil then
     FinalizeOne ( Items [n].Name );
end;

procedure TModuleDescList.InitializeOne(const sName: String; flags: DWORD);
var
   md: TModuleDescriptor;
begin
 // TODO: incomplete logic
 md := FindDesc (sName);
 if md <> nil then md.Initialize (flags);
end;

procedure TModuleDescList.FinalizeOne(const sName: String; flags: DWORD);
var
   md: TModuleDescriptor;
   n: Integer;
begin
 // TODO: incomplete logic
 { Ќеобходимо поискать модули завис€щие от данного, и если они активны - возмутитьс€.
 }

 md := FindDesc (sName);
 if (md = nil) or (md.Status = MST_FINALIZED) then exit;

 for n := 0 to Count - 1 do
  with Items [n] do
   if  (Status = MST_INITIALIZED) and ( DependedFrom(sName) ) then
     begin
      if MMF_WARN_DEP and flags <> 0 then PrintError('Module ' + Name + ' depended from ' + sName + ' - finalized automaticaly');
      FinalizeOne (Name); // WARN: Recursive
     end;

 md.Finalize (flags);
end;

initialization
 GetModuleDescList;
finalization
 FreeAndNil (gModuleDescList);
end.
