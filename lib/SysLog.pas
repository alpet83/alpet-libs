unit SysLog;

interface
uses Windows, Classes, SysUtils, Misc, MMSystem;


procedure  AddEventMsg(sMsg: String);
procedure  GetEventMessages (slout: TStrings); // добавить сообщения из локального списка, и очистить его

implementation
uses ModuleMgr, WThreads;

var
   g_share: RTL_CRITICAL_SECTION;
   slMsgs: TStrings;

procedure  AddEventMsg;
var n: Integer;
begin
 if slMsgs = nil then exit;
 DbgEnterCS (g_share, 'AEM');
 try
  repeat
   n := Pos ('~C', sMsg);
   if n <= 0 then break;
   Delete (sMsg, n, 4);
  until FALSE;
  slMsgs.Add (InfoFmt(sMsg));
 finally
  LeaveCriticalSection (g_share);
 end;
end; // AddEventMsg

procedure  GetEventMessages;
var n: Integer;
begin
 if slMsgs = nil then exit;
 DbgEnterCS (g_share, 'GEM');
 try
  for n := 0 to slMsgs.Count - 1 do
    slout.Add (slMsgs[n]);
  slMsgs.Clear;
 finally
  LeaveCriticalSection (g_share);
 end;
end; // GetEventMessags

function OnModuleRqs (md: TModuleDescriptor; rqs, flags: DWORD): Boolean;
begin
 result := FALSE;
 case rqs of
  MRQ_INITIALIZE:
   begin
    result := (slMsgs = nil);
    if not result then exit;
    slMsgs := TStringList.Create;
    md.Globals.Add (slMsgs);
    InitializeCriticalSection(g_share);
   end;
  MRQ_FINALIZE:
   begin
    result := (slMsgs <> nil);
    DeleteCriticalSection(g_share);
    slMsgs := nil;
   end
 end; // case
end; // OnModuleRqs



initialization
 {
 }
 RegModule ('SysLog', 'misc,ModuleMgr', OnModuleRqs);
 InitializeModule ('SysLog');
finalization
 FinalizeModule ('SysLog');
end.
