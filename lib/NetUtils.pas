unit NetUtils;

interface
Uses Windows, SysUtils, Classes, Misc, NetAPI, DateTimeTools, WSASock;

function BasicAuthorize(wcon: TClientCon; const sRegStr, sLogin, sPasswd: String): Boolean;

procedure DownloadRequest (clcon: TClientCon; const subj: String; const extra: String = '');

implementation

function BasicAuthorize;
var
    sMsg: String;
    pt: TProfileTimer;
begin
 result := FALSE;
 ODS ('[~T].~C0F Auth-request ' + sRegStr + ' sending...~C07');
 Sleep(10);
 wcon.auto_wakeup := FALSE;
 wcon.SendMsgPacket('BEGIN_AUTH');
 wcon.SendMsgPacket(sRegStr);
 wcon.SendMsgPacket('LOGIN=' + sLogin); // login & passwd limited to 8 chars!
 wcon.SendMsgPacket('PWD=' + sPasswd);
 wcon.SendMsgPacket('END_AUTHRQ');
 ODS ('[~T].~C0F Waiting auth-response...~C07');
 wcon.Flush;
 wcon.WaitFlush(20);
 pt := TProfileTimer.Create;
 pt.Start;

 // TODO: Ќормальна€ авторизаци€ должна быть защищеной
 repeat
  sMsg := '';
  if (wcon.Ready) then
   begin
    if (wcon.Socket.AsyncMode = WSA_SYNC_MODE) then
        wcon.TransferRecv
    else
    if wcon.OutcomingBytes > 0 then
      begin
       // wcon.WakeUp;  // дл€ скорейшего получени€ данных
       ODS('[~T]. #NETDBG(BasicAuthorize): Outcoming data in buff = ' + IntToStr(wcon.OutcomingBytes));
       wcon.Flush;
      end;

   end
  else break;

  if (wcon.IncomingBytes >= 64) or ( wcon.WaitPacketRecv(150) < 10 ) then
   begin
    sMsg := wcon.RecvMsgPacket;
    ODS ('[~T]. #ARESP: ' + sMsg);
   end;

  if sMsg = 'AUTH=OK' then
           begin
            result := TRUE;
            break;
           end;
  if sMsg = 'AUTH=FAILED' then
            break;
  XSleep(1);

 until  pt.Elapsed > 15000 ;
 wcon.auto_wakeup := TRUE;
 pt.Free;
end; // Authorize



procedure DownloadRequest (clcon: TClientCon; const subj, extra: String);
var tmpl: TStrings;
       n: Integer; 
begin
 clcon.SendMsgPacket('START_RQ');
 clcon.SendMsgPacket('RQS=DOWNLOAD');
 clcon.SendMsgPacket('SUBJ=' + subj);
 if (extra <> '') then
   begin
    tmpl := TStringList.Create;
    tmpl.Delimiter := ';';
    tmpl.DelimitedText := extra;
    for n := 0 to tmpl.Count - 1 do
     if tmpl[n] <> '' then
        clcon.SendMsgPacket(tmpl[n]);
    tmpl.Free;
   end;
 clcon.SendMsgPacket('END_RQ');
 clcon.WaitFlush(10);
end; // DownloadRequest




end.
