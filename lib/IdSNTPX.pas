{
  $Project$
  $Workfile$
  $Revision$
  $DateUTC$
  $Id$

  This file is part of the Indy (Internet Direct) project, and is offered
  under the dual-licensing agreement described on the Indy website.
  (http://www.indyproject.org/)

  Copyright:
   (c) 1993-2005, Chad Z. Hower and the Indy Pit Crew. All rights reserved.
}
{
  $Log$
}
{
  Rev 1.6    2/8/2005 6:28:02 AM  JPMugaas
  Should now work properly.  I omitted a feild when outputting bytes from the
  packet object.  OOPS!!!

  Rev 1.5    6/1/2004 9:09:00 PM  DSiders
  Correct calculation for RoundTripDelay as per RFC 2030 errata.

  Rev 1.4    2/9/2004 11:26:46 AM  JPMugaas
  Fixed some bugs reading the time.  SHould work.

  Rev 1.3    2/8/2004 4:15:54 PM  JPMugaas
  SNTP ported to DotNET.

  Rev 1.2    2004.02.03 5:44:24 PM  czhower
  Name changes

  Rev 1.1    1/21/2004 4:03:42 PM  JPMugaas
  InitComponent

  Rev 1.0    11/13/2002 08:01:12 AM  JPMugaas

  2002 Jan 21 Don
    Added suggestions from R. Brian Lindahl.
    Added CheckStratum property.
    Modified Disregard to use CheckStratum property.
    Modified GetAdjustmentTime to ignore optional NTP authentication in response.

  2002 Jan 3 Don
    Corrected errors introduced in previous revision.
    Added TIdSNTP.Create to assign port number for the SNTP protocol.

  2002 Jan 3 Don
    Corrected error in TIdSNTP.GetDateTime as per Bug Report
    http://sourceforge.net/tracker/?func=detail&atid=431491&aid=498843&group_id=41862

  2001 Sep 4 Don
    Corrected error in Flip() as reported on BCB newsgroup

  2000 Apr 21 Kudzu
    Updated to match UDP core changes

  2000 Mar 28 Hadi
    Continued conversion to Indy

	2000 Mar 24 Kudzu
    Converted to Indy

	2000 Jan 13 MTL
  	Moved to new Palette Tab scheme (Winshoes Clients)
    1999
}

unit IdSNTPX;

{*
  Winshoe SNTP (Simple Network Time Protocol)
  Behaves more or less according to RFC-2030
  R. Brian Lindahl - Original Author
*}

interface
{$I IdCompilerDefines.inc}

uses
  Classes,
  IdGlobal,
  IdUDPClient, IdSocketHandle, IdUDPServer;

const
  NTPMaxInt = 4294967297.0;
  NTPVersion = 3;


type
  // NTP Datagram format
  TNTPGram = packed record
    Flags: byte;
    Stratum: byte;
    Poll: byte;
    Prec: ShortInt;
    RootDelay: LongWord;
    RootDispersion: LongWord;
    RefID: LongWord;
    Ref1: LongWord;
    Ref2: LongWord;
    Org1: LongWord;
    Org2: LongWord;
    Rcv1: LongWord;
    Rcv2: LongWord;
    Xmit1: LongWord;
    Xmit2: LongWord;
  end;

  TDateRec32 = packed record
   case BYTE of
    0: ( dd, mm: byte; yyyy: word );
    1: ( date32: LongWord );
  end;

  TTimeRec32 = packed record
   case BYTE of
    0: ( tzb: shortInt; hh, mm, ss: BYTE; );
    1: ( time32: LongWord );
  end;

  TTimeFunction = function: TDateTime; // SysUtils.Now as default
  TSyncStatProc = procedure (const ip: String; port: WORD; pt, dt: TDateTime) of object;


  TIdSNTP = class(TIdUDPClient)
  private
    FServerTime: TTimeRec32;
    FServerDate: TDateRec32;
    FStratum: Integer;
    FTimeFunc: TTimeFunction;
  protected
    FDestinationTimestamp: TDateTime;   // Destination Timestamp   T4   time reply received by client
    FLocalClockOffset: TDateTime;       // = ((T2 - T1) + (T3 - T4)) / 2
    FOriginateTimestamp: TDateTime;     // Originate Timestamp     T1   time request sent by client
    FReceiveTimestamp: TDateTime;       // Receive Timestamp       T2   time request received by server
    FRoundTripDelay: TDateTime;         // = (T4 - T1) - (T2 - T3)
    FTransmitTimestamp: TDateTime;      // Transmit Timestamp      T3   time reply sent by server
    FCheckStratum: Boolean;

    function Disregard(const ANTPMessage: TNTPGram): Boolean;
    function GetAdjustmentTime: TDateTime; inline;
    function GetDateTime: TDateTime;
    procedure InitComponent; override;
  public
    ds: String;
    mcs: SmallInt;

    function SyncTime: Boolean;        // get datetime and adjust if needed
    //
    property AdjustmentTime: TDateTime read GetAdjustmentTime;
    property DateTime: TDateTime read GetDateTime;
    property OriginateTimestamp: TDateTime read FOriginateTimestamp;
    property ServerDate: TDateRec32 read FServerDate;
    property ServerTime: TTimeRec32 read FServerTime;
    property Stratum: Integer read FStratum;
    property TimeFunc: TTimeFunction read FTimeFunc write FTimeFunc;
    property TransmitTimestamp: TDateTime read FTransmitTimestamp;
    property RoundTripDelay: TDateTime read FRoundTripDelay;
    property CheckStratum: Boolean read FCheckStratum write FCheckStratum default True;
  end;


  // TIdSNTPServer added by alpet 16.10.2010
  TIdSNTPServer = class(TIdUDPServer)
  private
    FTimeFunc: TTimeFunction;
    FClockPrecision: ShortInt;
    FRefTimeStamp: TDateTime;
    FOnSyncStat: TSyncStatProc;
    FStratum: Integer;
    FInTraffic: Int64;
    FOutTraffic: Int64;

  protected
    { props }
    property  OnUDPRead; // hide as internal handler
    procedure DoUDPRead (AThread: TIdUDPListenerThread; const AData: TIdBytes; ABinding: TIdSocketHandle); override; // reply
    { methods }
    procedure InitComponent; override;
  public
   { props }

   property  InTraffic: Int64 read FInTraffic;
   property  OutTraffic: Int64 read FOutTraffic;
   property  TimeFunc: TTimeFunction read FTimeFunc write FTimeFunc;
   property  RefTimeStamp: TDateTime read FRefTimeStamp write FRefTimeStamp; // when is sync?

  published
   { props }
   property  ClockPrecision: ShortInt read FClockPrecision write FClockPrecision;
   property  OnSyncStat: TSyncStatProc read FOnSyncStat write FOnSyncStat;
   property  Stratum: Integer read FStratum write FStratum;

  end; // TIdSNTPServer



implementation

uses
  {$IFDEF USE_VCL_POSIX}
  PosixSysTime,
  PosixTime,
  {$ENDIF}
  {$IFDEF WIN32_OR_WIN64_OR_WINCE}
  Windows,
  {$ENDIF}

  IdGlobalProtocols,
  IdAssignedNumbers,
  IdStack, Misc,
  SysUtils;



const
  DT_ONE_SECOND  = ( 1.0 / 24.0 ) / 3600.0;
  DT_ONE_MSEC    = DT_ONE_SECOND / 1000.0;
  DT_ONE_MCS     = DT_ONE_MSEC / 1000;

  PEER_NTP_EX    = $FEFEFEFE;


procedure DateTimeToNTP(ADateTime: TDateTime; var Second, Fraction: LongWord);
var
  Value1, Value2: Double;
begin
  Value1 := (ADateTime + TimeZoneBias - 2) * 86400;
  Value2 := Value1;

  if Value2 > NTPMaxInt then
  begin
    Value2 := Value2 - NTPMaxInt;
  end;

  Second := LongWord( Trunc(Value2) );
  Value2 := Frac (Value1) * NTPMaxInt;

  if Value2 > NTPMaxInt then
  begin
    Value2 := Value2 - NTPMaxInt;
  end;

  Fraction := Trunc(Value2);
end;

function NTPToDateTime(Second, Fraction: LongWord; RealDate, RealTime: LongWord): TDateTime;
var
  Value1: Double;
  Value2: Double;
  dr: TDateRec32;
  tr: TTimeRec32;
begin
  Value1 := Second;

  if Value1 < 0 then
  begin
    Value1 := NTPMaxInt + Value1 - 1;
  end;

  Value2 := Fraction;

  if Value2 < 0 then
  begin
    Value2 := NTPMaxInt + Value2 - 1;
  end;


  Value2 := Trunc ( 1e6 * Value2 / NTPMaxInt ) / 1e6;
  Result := ( (Value1 + Value2) / 86400.0 ) + 2 - TimeZoneBias;



  if ( RealDate > 0 ) and ( Abs (result - RealDate ) > 1 ) then
    begin
     dr.date32 := RealDate;
     tr.time32 := RealTime;

     Value1 := EncodeDate ( dr.yyyy, dr.mm, dr.dd ) + EncodeTime (tr.hh, tr.mm, tr.ss, 0);
     if (dr.yyyy >= 2010) and ( Abs (Value1 - result) > 1 ) then
       begin
        OutputDebugString ( PChar ('Correcting date [' + FormatDateTime ('dd mmm yyyy hh:mm:ss', result) + '] year to [' +
                 FormatDateTime ('dd mmm yyyy hh:mm:ss', Value1) + '] +/- TimeZoneBias = ' + IntToStr(tr.tzb) ));
        result := Value1;
       end;
    end;


end;

{ TIdSNTP }

procedure TIdSNTP.InitComponent;
begin
  inherited;
  FPort := IdPORT_SNTP;
  FCheckStratum := True;
  TimeFunc := SysUtils.Now;
end;


function TIdSNTP.Disregard(const ANTPMessage: TNTPGram): Boolean;
var
  LvStratum: Byte;
  LvLeapIndicator: Byte;
begin
  LvLeapIndicator := (ANTPMessage.Flags and $C0) shr 6;
  LvStratum := ANTPMessage.Stratum;

  Result := (LvLeapIndicator = 3) or
    (((Int(FTransmitTimestamp)) = 0.0) and (Frac(FTransmitTimestamp) = 0.0));

  // DS ignore NTPGram when stratum is used, and value is reserved or unspecified
  if FCheckStratum and ((LvStratum > 15) or (LvStratum = 0)) then
  begin
    Result := True;
  end;
end;


function TIdSNTP.GetAdjustmentTime: TDateTime;
begin
  Result := FLocalClockOffset;
end;


function TIdSNTP.GetDateTime: TDateTime;
var
  LNTPDataGram: TNTPGram;
  LBuffer: TIdBytes;
  rb: Integer;

begin

  // DS default result is an empty TDateTime value
  Result := 0.0;

  SetLength(LBuffer, SizeOf(TNTPGram));
  FillBytes(LBuffer, SizeOf(TNTPGram), $00);

  LBuffer[0] := $1B; // 0 + 3 shl 2 + 3 shl 5
  DateTimeToNTP(TimeFunc, LNTPDataGram.Xmit1, LNTPDataGram.Xmit2);

  CopyTIdLongWord(GStack.HostToNetwork(PEER_NTP_EX), LBuffer, 12); // htonl DWORD 1 = ref ID
  CopyTIdLongWord(GStack.HostToNetwork(LNTPDataGram.Xmit1), LBuffer, 40); // htonl DWORD 1 = int seconds
  CopyTIdLongWord(GStack.HostToNetwork(LNTPDataGram.Xmit2), LBuffer, 44); // htonl DWORD 2 = frac part

  if dbg_present then ODS('[~TP/~B]. #PERF: UDP request pending...');
  SendBuffer(LBuffer);
  asm
   pause
   pause
  end;

  SetLength(LBuffer, SizeOf(TNTPGram) + 8);

  FillBytes(LBuffer, SizeOf(TNTPGram), $00);

  rb := ReceiveBuffer(LBuffer);

  // DS response may contain optional NTP authentication scheme info not in NTPGram
  if (rb > 0) and ( Length(LBuffer) >= SizeOf(TNTPGram) ) then
  begin
    FDestinationTimeStamp := TimeFunc;


    // DS copy result data back into NTPDataGram
    // DS ignore optional NTP authentication scheme info in response

    LNTPDataGram.Flags            := LBuffer[0];
    LNTPDataGram.Stratum          := LBuffer[1];
    FStratum := LNTPDataGram.Stratum;
    {
    LNTPDataGram.Poll             := LBuffer[2];
    LNTPDataGram.Prec             := LBuffer[3];

    LNTPDataGram.RootDelay        := GStack.NetworkToHost(BytesToLongWord(LBuffer, 4));
    LNTPDataGram.RootDispersion   := GStack.NetworkToHost(BytesToLongWord(LBuffer, 8));

    LNTPDataGram.Ref1             := GStack.NetworkToHost(BytesToLongWord(LBuffer, 16));
    LNTPDataGram.Ref2             := GStack.NetworkToHost(BytesToLongWord(LBuffer, 20));}

    LNTPDataGram.RefID            := GStack.NetworkToHost(BytesToLongWord(LBuffer, 12));

    LNTPDataGram.Org1             := GStack.NetworkToHost(BytesToLongWord(LBuffer, 24));
    LNTPDataGram.Org2             := GStack.NetworkToHost(BytesToLongWord(LBuffer, 28));
    LNTPDataGram.Rcv1             := GStack.NetworkToHost(BytesToLongWord(LBuffer, 32));
    LNTPDataGram.Rcv2             := GStack.NetworkToHost(BytesToLongWord(LBuffer, 36));
    LNTPDataGram.Xmit1            := GStack.NetworkToHost(BytesToLongWord(LBuffer, 40));
    LNTPDataGram.Xmit2            := GStack.NetworkToHost(BytesToLongWord(LBuffer, 44));

    FServerDate.date32 := 0;
    FServerTime.time32 := 0;

    // Experimental DATE detalization (compatible with most NTP servers).
    if ( Length(LBuffer) >= sizeof (TNTPGram) + 8 ) and ( LNTPDataGram.RefID = $01010101 ) then
      begin
       FServerDate.date32 := GStack.NetworkToHost( BytesToLongWord(LBuffer, sizeof(TNTPGram) + 0) );
       FServerTime.time32 := GStack.NetworkToHost( BytesToLongWord(LBuffer, sizeof(TNTPGram) + 4) ); // read last DWORD if possible

      end;

    // recvts == transts !!
    FOriginateTimeStamp := NTPToDateTime(LNTPDataGram.Org1, LNTPDataGram.Org2,   ServerDate.date32, ServerTime.time32);
    FReceiveTimestamp   := NTPToDateTime(LNTPDataGram.Rcv1, LNTPDataGram.Rcv2,   ServerDate.date32, ServerTime.time32);
    FTransmitTimestamp  := NTPToDateTime(LNTPDataGram.Xmit1, LNTPDataGram.Xmit2, ServerDate.date32, ServerTime.time32);

    if (FServerDate.date32 > 0) and ( Abs ( FOriginateTimeStamp - FDestinationTimeStamp ) > 1 ) then
        FDestinationTimeStamp := Trunc (FOriginateTimeStamp) + Frac (FDestinationTimeStamp); // correct possible round errors


    (*
    {$IFOPT D+}
    OutputDebugString (PChar ('Transmit time stamp: ' + FormatDateTime ('dd mmm yyyy hh:nn:ss.zzz', FTransmitTimestamp)));
    {$ENDIF}
    *)

    // corrected as per RFC 2030 errata
    FRoundTripDelay := (FDestinationTimestamp - FOriginateTimestamp) -
      (FTransmitTimestamp - FReceiveTimestamp);

    FLocalClockOffset := ((FReceiveTimestamp - FOriginateTimestamp) +
      (FTransmitTimestamp - FDestinationTimestamp + mcs * DT_ONE_MCS)) / 2; // adjusted thread context switch

    // DS update date/time when NTP datagram is not ignored
    if not Disregard(LNTPDataGram) then begin
      Result := FTransmitTimestamp;
    end;
  end;

end;

function TIdSNTP.SyncTime: Boolean;
begin
  Result := DateTime <> 0.0;
  if Result then begin
    Result := IndySetLocalTime(FOriginateTimestamp + FRoundTripDelay + FLocalClockOffset);
  end;
end;

{ TIdSNTPServer }

procedure TIdSNTPServer.DoUDPRead;


var
  LNTPDataGram: TNTPGram;
  LBuffer: TIdBytes;
  tms, recvts, peerts: TDateTime;

  cfg, peerID: DWORD;
  st: TSystemTime;
  dr: TDateRec32;
  tr: TTimeRec32;
begin
  tms := TimeFunc;

  SetLength(LBuffer, Length(AData));
  Move (AData[0], LBuffer[0], Length(AData));
  Inc (FInTraffic, Length(AData));

  // ReceiveBuffer(LBuffer); // Receive dgram
  if Length(AData) < SizeOf(TNTPGram) then
   begin
     if Assigned (OnSyncStat) then
        OnSyncStat ( ABinding.PeerIP, Length(AData), -1, Now );
    exit; // wrong request / version mistmath
   end;

  Move (AData [0], LNTPDataGram, sizeof (TNTPGram)); // copy all secondary fieldso

  Move (LNTPDataGram, cfg, 4);

  peerID := GStack.NetworkToHost(BytesToLongWord(AData, 12));

  recvts := TimeFunc;

  if AThread.Priority <> tpHigher then
     AThread.Priority := tpHigher;

  if (LNTPDataGram.Flags and 7) = 3 then // check mode fields, 1st three bits
      LNTPDataGram.Flags := 4  // if request from client
  else
      LNTPDataGram.Flags := 2; // passive (!)

  LNTPDataGram.Flags := LNTPDataGram.Flags + NTPVersion shl 3;
  LNTPDataGram.Stratum := Stratum;      // stratum 2
  LNTPDataGram.Poll := 7;      // registration 128 sec
  LNTPDataGram.Prec :=  ClockPrecision;
  LNTPDataGram.RefID := $01010101; // 1.1.1.1 ? GPS ? Atom clock

  LBuffer[0] := LNTPDataGram.Flags;
  LBuffer[1] := LNTPDataGram.Stratum;
  LBuffer[2] := LNTPDataGram.Poll;
  LBuffer[3] := LNTPDataGram.Prec;




  //  originate timestamp
  LNTPDataGram.Org1            := GStack.NetworkToHost(BytesToLongWord(LBuffer, 40)); // xmit.int
  LNTPDataGram.Org2            := GStack.NetworkToHost(BytesToLongWord(LBuffer, 44)); // xmit.frac

  peerts := NTPToDateTime (LNTPDataGram.Org1, LNTPDataGram.Org1, 0, 0);


  DateTimeToNTP (recvts, LNTPDataGram.Rcv1, LNTPDataGram.Rcv2);
  DateTimeToNTP (recvts, LNTPDataGram.Xmit1, LNTPDataGram.Xmit2);  // send and recv - equal
  DateTimeToNTP (RefTimestamp, LNTPDataGram.Ref1, LNTPDataGram.Ref2);

  if peerID = PEER_NTP_EX then
   begin
    SetLength(LBuffer, SizeOf(TNTPGram) + 8); // experiment: resize buffer, and include local date info
    DateTimeToSystemTime (recvts, st);
    dr.dd := st.wDay;
    dr.mm := st.wMonth;
    dr.yyyy := st.wYear;
    tr.tzb := Trunc ( TimeZoneBias / (1 / 24) );
    tr.hh := st.wHour;
    tr.mm := st.wMinute;
    tr.ss := st.wSecond;

    CopyTIdLongWord( GStack.HostToNetwork( dr.date32 ), LBuffer, sizeof(TNTPGram) + 0 );
    CopyTIdLongWord( GStack.HostToNetwork( tr.time32 ), LBuffer, sizeof(TNTPGram) + 4 );
   end;

 (* {$IFOPT D+}
  recvts := NTPToDateTime(LNTPDataGram.Org1, LNTPDataGram.Org2);
  OutputDebugString (PChar ( 'Originate time stamp: ' + FormatDateTime ('dd mmm yyyy hh:nn:ss.zzz', recvts) +
                        ', peer = ' + ABinding.PeerIP + ':' + IntToStr (ABinding.PeerPort) ));
  {$ENDIF} *)

  CopyTIdLongWord(GStack.HostToNetwork(LNTPDataGram.RefID), LBuffer, 12); // refId
  CopyTIdLongWord(GStack.HostToNetwork(LNTPDataGram.Ref1),  LBuffer, 16); // htonl DWORD 1 = ref.int
  CopyTIdLongWord(GStack.HostToNetwork(LNTPDataGram.Ref2),  LBuffer, 20); // htonl DWORD 2 = ref.frac
  CopyTIdLongWord(GStack.HostToNetwork(LNTPDataGram.Org1),  LBuffer, 24); // htonl DWORD 1 = org.int
  CopyTIdLongWord(GStack.HostToNetwork(LNTPDataGram.Org2),  LBuffer, 28); // htonl DWORD 2 = org.frac
  CopyTIdLongWord(GStack.HostToNetwork(LNTPDataGram.Rcv1),  LBuffer, 32); // htonl DWORD 1 = rcv.int
  CopyTIdLongWord(GStack.HostToNetwork(LNTPDataGram.Rcv2),  LBuffer, 36); // htonl DWORD 2 = rcv.frac
  CopyTIdLongWord(GStack.HostToNetwork(LNTPDataGram.Xmit1), LBuffer, 40); // htonl DWORD 1 = xmit.int
  CopyTIdLongWord(GStack.HostToNetwork(LNTPDataGram.Xmit2), LBuffer, 44); // htonl DWORD 2 = xmit.frac


  Inc (FOutTraffic, Length(LBuffer));

  tms := TimeFunc - tms;
  ABinding.SendTo ( ABinding.PeerIP, ABinding.PeerPort, LBuffer );

  if dbg_present then ODS ('[~TP/~B]. #PERF: response time (mcs) = ' + ftow (tms / DT_ONE_MCS) );

  if Assigned (OnSyncStat) then
     OnSyncStat ( ABinding.PeerIP + ' $' + IntToHex(PeerID, 8), ABinding.PeerPort, peerts, recvts );
end;



procedure TIdSNTPServer.InitComponent;
begin
  inherited;
  FPort := IdPORT_SNTP; // listen 123 as default
  DefaultPort := IdPORT_SNTP;
  OnUDPRead := nil;
  TimeFunc := SysUtils.Now;
  ClockPrecision := -10;

  ThreadedEvent := TRUE; // without this, precision drop bellow 1 second
  Stratum := 2;
end;

end.
