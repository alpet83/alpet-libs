unit NetApi;

interface
Uses Windows, Classes, StrClasses, SysUtils, WinSock, WSASock, NBBuff, WThreads, Math, FastSync, ContNrs,
        Messages, DateTimeTools, UNIArray, SyncObjs, WinHeap, MemStat, BaseLib;

const
   NET_CACHE_SIZE = 512 * 1024;

   PACKET32_SIGNATURE = $AA20; // version AA
   PACKET64_SIGNATURE = $AA40; // version AA
   // теги пакета являются битовыми комбинациями
   PKTAG_MESSAGE  = $0100;
   PKTAG_DATA     = $0200;
   PKTAG_LASTPT   = $0400;
   PKTAG_PREFETCH = $0800;
   PKTAG_SYSTEM   = $1000;

   UPLOAD_PREFIX = 'UPLOAD=';

   NMSG_RECVOK        = 'RECV_OK';
   NMSG_RECV_BREAKED  = 'RECV_BREAKED';
   NMSG_RECVPT_FAILED = 'RECVPT_FAILED';
   NMSG_EOSDATA = 'EOS_DATA';   // end of stream
   NMSG_SENDERR = 'SEND_ERROR'; //

   {   Библиотека высокоуровневых сетевых подключений через TCP-сокеты.

      Режимы работы:
       1. Передача простого и "толстого" пакета.
       2. Передача крупного объема данных, в порциях. Каждая порция сопровождается заголовочным пакетом.

       Обработка отправки данных - единичным и заголовочным пакетам присваивается уникальный для соединения номер.
       При этом для как для самого пакета, так и для отправляемых данных рассчитывается контрольная сумма.

       У каждого соединения, для временного сохранения отправленных данных, используется список порций и список пакетов.
       Порции из него изымают, по прохождению некоторого времени. Точно такой-же список используется для сборки
       принятых данных, однако пакеты в него не попадают.

       Реакции на ошибки:
        * Искаженные данные пакета, или порции данных - принимающая сторона просит "повторить".
        * Пропуск пакета - приходит пакет с номером, превышающим ожидаемый: отправляется запрос на повтор передачи
          данного пакета.

          Первичная реализация, в режиме синхронной передачи порций.
          -----------------------------------------------------------------------------------------
          В процессе передачи данных, идет постоянная оценка ответа о "плохой порции". В случае прихода
          этого ответа, порция отправляется повторно. Ожидание ответа успешного приема, производится после
          отправки каждого набора порций.

          Проблема асинхронного решения: часто отправляется слишком много данных, и корректирующую посылку становится
          сложно впихнуть в отправленный объем.
           ВАРИАНТ СЛОЖНОГО РЕШЕНИЯ: Порции принимаются отдельным механизмом, например при парсинге пакетов, и выдаются
           лишь по запросу функции RecvData. Требуется разработка идеи и время на реализацию серьезной переделки либы.




       ВАЖНО:
         На приемной стороне, перед механизмом выдачи любых данных, должна использоваться сериализация пакетов.
        Ситуация с пропуском или искажением пакета, является исключительной - все поступающие данные, должны игнорироватся
        до получения пропущенного пакета. Игнорируются так-же пакеты, уже принятые по состоянию на "сейчас".
         TODO/ВРЕМЕННОЕ РЕШЕНИЕ - пропуск пакета сопровождается красным ругательством.


       РАСШИРЕННЫЕ РЕЖИМЫ РАБОТЫ:
         Избыточная передача пакетов - каждый пакет передается дважды, что увеличивает траффик, но относительно позволяет
        избежать переотправки пакетов.
   }



type
    TPacket32 = packed record
      signature: WORD;          // signature & size complexed
      tag_ident: WORD;          // описатель типа данных/пакета [flags/set]
      usr_value: DWORD;
      body_size: DWORD;         // размер данных, следующих за пакетом
      body_crc32: DWORD;        // body-protection CSUMM
      { ------------ next 16 bytes -------------- }
       packet_no: Integer;      // connection-depended packet number
      package_no: Integer;
      portion_no: Integer;
       pk_crc32: DWORD;          // packet_protection
    end; // TPacket16


    PPacket32 = ^TPacket32;

    TMsgPacket = packed record
      hdr: TPacket32;
      msg: array [0..31] of AnsiChar;
    end; // TMsgPacket

    PMsgPacket = ^TMsgPacket;

    TNetThread = class;
    TNetServerThread = class;
    TClientCon = class;

    {
       Реорганизация сетевой библиотеки.
       Новый вариант: Каждое клиентское подключение, теперь перестает быть потоком, но
       добавляется поток который может поддерживать произвольное количество подключений,
       и потоковый пул для обработки событий каждого подключения.

    }


    {  Класс списка порций данных.
         Позволяет разбить некоторые данных, на порции заданного размера,
       и собрать из порций поток обратно. Используется в технике безопасной
       пересылки данных (с досылкой поврежденных порций).
         Механизм сетевой пересылки при этом разбивает некоторый объем данных
       на отдельные порции (к примеру 16кб). Все порции отправляются вместе
       с контрольными суммами, после чего наступает этап сверки. Приемная сторона
       получив сообщение о начале сверки, отправляет номера порций для которых
       данные дошли поврежденными, если таковые есть, иначе сигнал о прикращении
       приема данных (успешное завершение) этого набора порций. 

    }
    TPortionInfo = packed record
     crc32: DWORD;
     status: SmallInt;             // 0 = not loaded, 1 = loaded, < 0 = failed
     data_size: WORD;              // <= 64KiB
     package_no: Integer;          // номер посылки (набора порций)
     portion_no: Integer;          // номер порции глобальный относительно соединения
    end; // TPortionInfo
    PPortionInfo = ^TPortionInfo;

    TPackageInfo = packed record
     package_no: Integer;
     recv_sz: Integer;              // кол-во принятых байт
    end;
    PPackageInfo = ^TPackageInfo;

    TPortionList = class(TList)
    protected
     FPtSize: Integer;
     FHeap: TWindowsHeap;
     last_pt_no: Integer;
     last_good_no: Integer;
     sc_share: TCritSection;
     FMemUsage: Integer;

     evt_pt_recv: TEvent;
     evt_pk_recv: TEvent;
     FCon: TClientCon;
     pkinf_list: TList;
     function           AddPackageInfo(npk: Integer): PPackageInfo;
     function           FindPackageInfo(npk: Integer): PPackageInfo;
     function           GetItem(nIndex: Integer): PPortionInfo;
    public
     recv_count: Integer;
     good_count: Integer;
     wait_package: Integer;
     wpk_size: Integer;


     property           Items[Index: Integer]: PPortionInfo read GetItem; default;
     property           PortionSize: Integer read FPtSize;
     property           MemUsage: Integer read FMemUsage;
     { C & D }
     constructor        Create(nPortionSize: Integer = 16384);
     destructor         Destroy; override;
     { Methods }
     function           AddPortion: PPortionInfo;
     function           CheckData: Integer; // возвращает количество принятых данных
     procedure          Clear; override;
     procedure          CompletePackage(n_pack: Integer);
     function           FindPortion(pt_no: Integer; pIndex: PInteger = nil): PPortionInfo;
     function           GetIndex(pt_no, i: Integer): Integer;
     function           JoinData(dst: TStream; n_pack, need_sz: Integer): Integer;
     procedure          SplitData(src: TStream; nSize, n_pack: Integer);
     function           PackageSize(n_pack: Integer): Integer;
     procedure          RemovePortion(pinf: PPortionInfo);
     function           SerialCheck: Integer;
     procedure          Optimize ();
     function           RecvPortion(phdr: PPacket32; rcv_bytes: Integer): Integer;
     { Sync }
     procedure          Lock(const ctx: String);
     procedure          Unlock;
    end; { TPortionList }

    { Класс механизма сборки данных, и обеспечения возможности
      их повторной передачи/приемки. }
    TDataCollector = class(TNBBuff)
    protected
     FPtList: TPortionList;
     FCon: TClientCon;
    public
     property           PtList: TPortionList read FPtList;

     { C & D }
     constructor        Create (AOwner: TClientCon);
     destructor         Destroy; override;
     { Methods }


    end; // TDataCollector

    TClientCon = class
    private
      wait_errors: Integer;
      last_recv_no: Integer;     // номер последнего принятого пакета
      last_send_no: Integer;     // номер последнего отправленного пакета
      last_pack_no: Integer;     // номер последней отправленной посылки
      total_recv: Integer;

      need_disconnect: Boolean;  // флаг для отложенного отключения
      procedure OnDestroy(Sender: TObject);
      function  SendPortions(n_pass: Integer; n_bad: Integer = -1): Integer;
      function  ParsePackets(pdata: Pointer; nSize, prv_rest: Integer): Integer;
    protected
      FName, FLogin: String;
      FSocket: TTCPSocket;
      out_buff: TNBBuff;
      recv_log: TStrings;
      sc_transfer: TCritSection;
      sc_send: TCritSection;
      cache: array of Byte;
      FLastSocket: Integer;
      FOwner: TNetThread;
      FNextOwner: TNetThread; //  следующая нить обработки (подключаяемая сейчас)
      FSrvThread: TNetServerThread;
      FDestroyTimer: Integer;
      WakeUpMsgCount: Integer;
      FWorkState: Integer;
      FLastState: TSocketState;
      FTransTimer: TProfileTimer;
      WUSendCount: Integer;
      in_data: TDataCollector;
      out_data: TDataCollector;
      need_opt_in: Integer;
      need_opt_out: Integer;
      max_transfer: Integer;    // экстремум передачи в периоде
      target_recv: Integer;
      trans_ctx: String;
      err_flags: DWORD;
      send_owner: DWORD;
      recv_owner: DWORD;


      function          TransferData(nEvents: LongInt = 0): Integer; virtual;
      procedure         OnError; virtual;

      procedure         SendLock(const ctx_msg: String); virtual;
      procedure         SendUnlock; virtual;
    public
      lp_usrval: DWORD;
      OnDataArrive: TNotifyEvent;
      OnDisconnect: TNotifyEvent;
      TransferUsage: Integer;
      OwnedByServer: Boolean;           // после разрыва, такое подключение обычно убивается
      auto_wakeup: Boolean;      // пробуждать поток при отправке данных автоматически

      referers: array [0..7] of TObject;

      { props }
      property          Name: String read FName write FName;
      property          Login: String read FLogin write FLogin;


      property          DestroyTimer: Integer read FDestroyTimer write FDestroyTimer;


      property          LastRecieved: Integer read last_recv_no;
      property          LastSocket: Integer read FLastSocket;
      property          Owner: TNetThread read FOwner;
      property          Socket: TTCPSocket read FSocket;
      property          SrvThread: TNetServerThread read FSrvThread;
      property          WorkState: Integer read FWorkState write FWorkState;

      { C & D }
      constructor       Create (const sName: String);
      destructor        Destroy; override;

      { methods }

      procedure         AssignSocket (S: TTCPSocket);
      procedure         BreakCon;
      function          CheckNetEvents: LongInt;
      function          CheckPacket(p32: PPacket32): Boolean;
      function          CheckPacketOrder(p32: PPacket32; bSave: Boolean = TRUE): Boolean;
      function          Connect (const host: String; port: Integer): Boolean; virtual;

      procedure         Disconnect (bNeedShutdown: Boolean = TRUE);

      procedure         Flush;

      function          GetDataType: WORD;
      function          GetPkType: WORD;

      function          GetError: Integer;

      procedure         InitPacket(p32: PPacket32; tag: WORD; usrv: Integer = 0);
      function          IncomingBytes: Integer;
      function          OutcomingBytes: Integer;


      procedure          LockInBuff(const ctx_msg: String);
      procedure          UnlockInBuff;

      function          DataInBuff: Boolean;
      function          MsgInBuff: Boolean;

      function          Ready: Boolean; virtual;
      function          Recv(pBuff: Pointer; nBytes: Integer; peekMode: Boolean = FALSE): Integer;
      function          Send(pBuff: Pointer; nBytes: Integer; bWakeUp: Boolean = TRUE; bLock: Boolean = TRUE): Integer;

      function          RecvData(buff: TStream; const sFirstMsg: String): String;
      function          SendData(const sDataDesc: String; buff: TStream): Integer;

      function          RecvPacket(p32: PPacket32; peekMode: Boolean = FALSE): Integer; // returns 0, 16, 32
      function          SendPacket(p32: PPacket32; bWakeUp: Boolean = TRUE): Boolean; // auto set packet type by size

      function          RecvMsgPacket (peekMode: Boolean = FALSE; pmsg: PMsgPacket = nil): String;
      function          SendMsgPacket (msg: String; uv: Integer = 0; bWakeUp: Boolean = TRUE): Boolean;

      function          RecvRequest(rql : TStrMap; const msg_stop: String = 'END_RQ'): Boolean;

      procedure         ResendPortion(np: Integer);

      function          TransferRecv: Integer;
      function          TransferSend: Integer;

      procedure         WaitFlush(timeOut: DWORD = 1500);
      function          WaitPacketRecv(timeOut: DWORD = 50): DWORD;
      function          WaitPackageRecv(n_pack, pack_sz: Integer; timeOut: DWORD = 500): DWORD;
      function          WaitPortionRecv(timeOut: DWORD = 50; npt: Integer = -1): DWORD;

      procedure         WakeUp (bForce: Boolean = FALSE);

      // procedure         WorkProc; override;

    end; // TClientCon

    TNetThread = class (TWorkerThread)
    protected

     FConnections: TObjectList;  // список назначенных подключений
     FOwner: TNetServerThread;
     RState: Integer;

     function           GetConnection(nIndex: Integer): TClientCon;
     function           ProcessMessage(uMsg: DWORD; wParam, lParam: Integer): Integer; override;
     function           ProcessRequest(const rqs: String; rqobj: TObject): Integer; override;
    public

     property           Connections[Index: Integer]: TClientCon read GetConnection;
     property           SrvThread: TNetServerThread read FOwner;

     { C & D }
     constructor        Create (CreateSuspended: Boolean; const name: String);
     destructor         Destroy; override;
     { methods }
     procedure          AddCon (clcon: TClientCon);
     function           ConCount: Integer;
     function           FindCon(nSocket: Integer): TClientCon;
     function           PeekMessages(uMsg, flags: DWORD): Integer; 
     procedure          RemoveCon(clcon: TClientCon); // освобождение потока от задания
     procedure          WorkProc; override;
    end; // TNetThread

    TNetServerThread = class (TWorkerThread)
    private
      procedure         AcceptCon;
      function          MakeNetThread(const sName: String): TWorkerThread;
      procedure         ProcessEvents(evts: Integer);

    protected
      FAsgList: TList;   // список соединений связанных или связываемых с нитями обработки
      FConList: TObjectList;  // список осуществленных клиентских подключений
      FClosedList: TList;     // список закрытых и умирающих подключений
         FPool: TWorkThreadPool;
       FSocket: TTCPSocket;
      pass_cnt: Integer;
       FStrError: String;
      function          GetClientCon (nIndex: Integer): TClientCon;
      procedure         OleExec; override;
      function          ProcessMessage(uMsg: DWORD; wParam, lParam: Integer): Integer; override;
      function          FindCon(s: TSocket): TClientCon;
      procedure         OnTimer (idTimer: Integer); override;
    public


      OnClientConnect: TNotifyEvent;

      property          ClientCons [Index: Integer]: TClientCon read GetClientCon;
      property          StrError: String read FStrError;


      constructor       Create (CreateSuspended: Boolean; const name: String);
      destructor        Destroy; override;

      { Methods }
      procedure         AddCon (clcon: TClientCon);     // добавление существующего подключения
      function          AllocateWorker: TNetThread;
      function          ConCount: Integer;
      function          IsOwned (clcon: TClientCon): Boolean;
      function          StartServer(const Host: String; port: Integer): Boolean;
      procedure         CloseClients; // закрытие всех клиентских подключений
      procedure         OnClientDisconnect(Sender: TObject);
      procedure         OnClientReleased (clcon: TClientCon);
      procedure         OnThreadReleased (clcon: TClientCon; nt: TNetThread);

      function          Ready: Boolean;
      // function          WaitEvents(timeOut: DWORD): DWORD;
      procedure         WorkProc; override;

    end; // TNetServerThread

var
   WM_WSAMSG: DWORD = $F001;
   net_verbose: Integer = 0;

function           CheckPortion(pinf: PPortionInfo): Boolean;

function           TransferOrWaitData(clcon: TClientCon; timeOut: DWORD): Integer;


// ============================================================================================================= //
implementation
// ============================================================================================================= //
uses Misc;

type
   TMsgItem = packed record
    uMsg: WORD;
    msg_cnt: WORD;  // сколько раз попадалось сообщение в очереди
    socket: Integer;
   case BYTE of
    0: (evts, error: WORD);
    1: (lParam: Integer);
   end; // TMsgItem
   PMsgItem = ^TMsgItem;


procedure SetMsgItem(pmi: PMsgItem; uMsg: WORD; socket, lParam: Integer);
begin
 pmi.uMsg := uMsg;
 pmi.msg_cnt := 1;
 pmi.socket := socket;
 pmi.lParam := lParam;
end;
// ============================================================================================================= //

function  TransferOrWaitData(clcon: TClientCon; timeOut: DWORD): Integer;
begin
 if (clcon.Owner = nil) or (clcon.Owner.ThreadId = GetCurrentThreadId) then
    repeat
     if clcon.TransferRecv = 0 then Sleep(20);
     Dec (timeOut, Min(timeOut, 20));
    until (timeOut = 0)
 else
    begin
     Sleep(1);
     clcon.WaitPacketRecv(timeOut);
    end;
 result := clcon.IncomingBytes;   
end; // TransferOrWaitData


function GetPtData(pinf: PPortionInfo): Pointer;
begin
 result := RelativePtr (pinf, sizeof(TPortionInfo));
end;

{ TClientCon }

procedure TClientCon.AssignSocket;
begin
 if Assigned (FSocket) then  FSocket.Free; // previous close
 FSocket := S;
end;

function TClientCon.Connect(const host: String; port: Integer): Boolean;
begin
 if Assigned (FSocket) then  FSocket.Free; // previous close
 need_disconnect := FALSE;
 FSocket := TTCPSocket.Create;
 result := FSocket.connect(host, port);
 FLastSocket := FSocket.socket;
 FLastState := FSocket.State;

end; // Connect

constructor TClientCon.Create(const sName: String);
begin
 sc_transfer := TCritSection.Create('TClientCon.sc_transfer');
 sc_send := TCritSection.Create('TClientCon.sc_send');
 in_data := TDataCollector.Create(self);
 out_data := TDataCollector.Create(self);
 recv_log := TStringList.Create;
 auto_wakeup := TRUE;
 // evt_pk_recv := TEvent.Create(nil, TRUE, FALSE, '');
 
 FName := sName;
 FDestroyTimer := MAXINT;
 FTransTimer := TProfileTimer.Create;
 out_buff := TNBBuff.Create ();
 TransferUsage := 0;
 SetLength (cache, NET_CACHE_SIZE);
end; // Create

destructor TClientCon.Destroy;
begin
 sc_transfer.TryLock('.Destroy', 500);
 DestroyTimer := Min (DestroyTimer, 10);
 if Assigned(Socket) then
    Disconnect();

 if (FLastState <> SST_CLOSED) and Assigned(OnDisconnect) then OnDisconnect(self);


 while (DestroyTimer > 0) do
  begin
   Dec (FDestroyTimer);
   Sleep (100);
  end;

 SafeExec (OnDestroy);
 out_buff.Free;
 in_data.Free;
 out_data.Free;
 //evt_pk_recv.Free;
 sc_transfer.Free;
 FTransTimer.Free;
 SetLength (cache, 0);
 sc_send.Free;
 recv_log.Free;
 ODS ('[~T]. ~C0B#DBG: Уничтожен объект подключения $' + PtrToStr (self) + '.' + Name + '~C07');
 inherited;
end; // Destroy

procedure TClientCon.OnDestroy(Sender: TObject);
begin
  if Assigned (FSrvThread) then
     FSrvThread.OnClientReleased(self);
  if Assigned (FOwner) then
     FOwner.RemoveCon (self); // unuse thread
end; // OnDestroy

procedure TClientCon.Disconnect;
var
   SLnk: TTCPSocket;
   timeOut: Integer;
begin
 need_disconnect := FALSE;
 if self = nil then
  begin
   PrintError ('Trying to disconnect destroyed connection object...');
   exit;
  end;

 Name := '¤¤¤' + Name + '¤¤¤';
 WorkState := 0;
 if Assigned (FSrvThread) then
    FSrvThread.OnClientDisconnect(self);


 if Assigned (FOwner) and (not FOwner.Stopped) then
   begin
    timeOut := 0;
    FOwner.RemoveCon (self);
    if bNeedShutdown and Assigned (FSocket)  then
      begin
       FSocket.shutdown();
       bNeedShutdown := FALSE;
      end;

    while (TransferUsage > 0) and (timeOut < 1000) do
     begin
      Sleep(1);
      Inc (timeOut);
     end;
   end;
 FOwner := nil;
 if Socket <> nil then FLastSocket := Socket.socket;

 if Assigned (OnDisconnect) then OnDisconnect (self);

 FLastState := SST_CLOSED;


 sc_transfer.TryLock('Disconnect', 1500);
 try
   SLnk := Ptr (  InterlockedExchange (Integer(FSocket), 0) );
   FSocket := nil;
   if (SLnk <> nil) then
    begin
     SLnk.SurviveTime := 0;
     if bNeedShutdown  then
        SLnk.shutdown();
     SLnk.close;
     SleepEx(5, TRUE); // TODO: надо это заменить на что-то
     SLnk.Free;
    end;
 finally
  sc_transfer.Unlock;
 end;
end; // Disconnect

function TClientCon.GetError: Integer;
begin
 if Assigned (FSocket) then
    result := FSocket.Error
 else
    result := -150;
end; // GetError

function TClientCon.IncomingBytes: Integer;
begin
 result := in_data.GetCount;
end; // IncomingBytes

function TClientCon.OutcomingBytes: Integer;
begin
 result := out_buff.GetCount;
end; // OutcomingBytes


function TClientCon.Ready: Boolean;
begin
 result := (DestroyTimer > 50) and (not need_disconnect);
 if not result then exit;
 result := ( Assigned (FSocket) and FSocket.connected );
end; // Ready

function TClientCon.Recv;
begin
 nBytes := Min (nBytes, in_data.GetCount);
 result := nBytes;
 if nBytes > 0 then in_data.Read(pBuff, nBytes, peekMode);
end; // Recv

function TClientCon.RecvPacket(p32: PPacket32; peekMode: Boolean): Integer;
var
   rcin: Integer;
begin
 result := 0;
 rcin := in_data.GetCount;
 if rcin < sizeof (TPacket32) then exit; // impossible!?
 in_data.Read(p32, sizeof(TPacket32), TRUE); // this may be not a packet
 if CheckPacket(p32) then
  case p32.signature of
   PACKET32_SIGNATURE:
       result := in_data.Read(p32, sizeof(TPacket32), peekMode);
   else result := 0;
  end; // case
 CheckPacketOrder(p32, not peekMode);
end; // RecvPacket

function TClientCon.Send(pBuff: Pointer; nBytes: Integer; bWakeUp, bLock: Boolean): Integer;
begin
 target_recv := 0;
 if bLock then
  begin
   SendLock('TClientCon.Send');
   try
    out_buff.Write(pBuff, nBytes);
    result := nBytes;
   finally
    SendUnlock;
   end;
  end
 else
  begin
   out_buff.Write(pBuff, nBytes);
   result := nBytes;
  end;
 if (auto_wakeup) and (WUSendCount <= 0) or bWakeUp then WakeUp;
end; // Send

function TClientCon.SendMsgPacket(msg: String; uv: Integer = 0; bWakeUp: Boolean = TRUE): Boolean;
var pkmsg: TMsgPacket;
    len: Integer;
begin
 result := FALSE;
 msg := Trim (msg);
 if (msg = '') then
  begin
   ODS('[~T].~C0C #WARN(SendMsgPacket): Void message sending - request ignored~C07');
   exit;
  end;
 FillChar (pkmsg, sizeof (pkmsg), 0);
 len := Min(31, Length(msg));
 pkmsg.hdr.signature := PACKET64_SIGNATURE;
 pkmsg.hdr.usr_value := uv;
 pkmsg.hdr.tag_ident := PKTAG_MESSAGE;
 pkmsg.hdr.packet_no := InterlockedIncrement (last_send_no);

 SetStrZ (pkmsg.msg, Copy (msg, 1, len), 31);
 if len < 27 then
   begin
    PWORD(@ pkmsg.msg[30] )^ := pkmsg.hdr.packet_no and $FFFF;       // marker = packet_no
   end;
 // 2-blocks for CRC32
 pkmsg.hdr.pk_crc32 := CalcCRC32(@pkmsg, sizeof(pkmsg.hdr) - 4);
 pkmsg.hdr.pk_crc32 := CalcCRC32(@pkmsg.msg, sizeof(pkmsg.msg), pkmsg.hdr.pk_crc32);
 result := Send (@pkmsg, sizeof (TMsgPacket), bWakeUp) = sizeof (TMsgPacket);
 out_data.Write(@pkmsg, sizeof(TMsgPacket)); // запомнить для восстановления!
end;

function TClientCon.SendPacket(p32: PPacket32; bWakeUp: Boolean = TRUE): Boolean;
var
   wsize: Integer;
begin
 // TODO: номер пакета и прочее
 p32.signature := PACKET32_SIGNATURE;
 p32.packet_no := InterlockedIncrement(last_send_no);
 p32.pk_crc32 := CalcCRC32(P32, sizeof(TPacket32) - 4);
 wsize := sizeof (TPacket32);
 if p32.packet_no = 1145  then
  begin
   ODS(Format('[~T]. Sending packet #%d, body_size = %d ', [p32.packet_no, p32.body_size] ));
   //Sleep(50);
  end;

 result := Send (p32, wsize, bWakeUp, send_owner <> GetCurrentThreadId) = wsize;
 out_data.Write(p32, sizeof (TPacket32));       // сохранить для возможности восстановления
end; // SendPacket


procedure TClientCon.BreakCon;
begin
 if not Assigned (Socket) then exit;
 Socket.shutdown();
 Socket.WaitFor(10);
 Socket.close;
end;

function TClientCon.CheckNetEvents;

begin
 // чтение входящих данных
 result := FSocket.get_events;
 if result < 0 then OnError;
 if result AND FD_CLOSE <> 0 then Disconnect (FALSE);
end; //

function TClientCon.TransferData;
var
   rb, wb: Integer;
   r_sum, s_sum: Integer;
    s_skip: Integer;
    w_loops: Integer;
   elapsed: Single;
         s: String;
      sobj: TTCPSocket;
begin
 result := 0;
 if not Ready then
  begin
   while out_buff.GetCount > 0 do
         out_buff.Read (@cache[0], Min (out_buff.GetCount, NET_CACHE_SIZE) );
   exit;
  end;
 wait_errors := 0;
 sobj := FSocket;
 Inc(TransferUsage);
 sc_transfer.Lock('.TransferData', 20);
 try
  if Assigned (sobj) then
    begin
     s_skip := 0;
     w_loops := 0;
     FTransTimer.Start;
     s_sum := 0;
     r_sum := 0;
     sobj.reset_event;
     // цикл приема-отправки
     repeat
      rb := TransferRecv();
      if rb > 0 then Inc (r_sum, rb);
      wb := TransferSend();
      if wb > 0 then Inc (s_sum, wb);
      if (FTransTimer.Elapsed > 500)  then break;
      Inc (w_loops);
      if Assigned(Owner) then Owner.PeekMessages(WM_WSAMSG, PM_REMOVE);
     until (rb = 0) and (wb < 0);

     if (IncomingBytes > 0) then
      try
       if Assigned (OnDataArrive) then OnDataArrive (self);
      except
       on E: Exception do
          PrintError('TClientCon.TransferData: Exception catched while processing event OnDataArrive');
      end;

     elapsed := FTransTimer.Elapsed;
     result := r_sum + s_sum;
     if (result > 0) and (Owner <> nil) then Owner.SetBusy;

     if (net_verbose > 3) and ( (Elapsed >= 20) or (w_loops > 10) ) then
       begin
        s := '[~T].~C0F #PROF:~C0A TransferData[~C0E' + Name + '~C0A]: time = ~C0D'+ FormatFloat ('0.0', elapsed);
        s := s + ' ms~C0A, received = ~C0D' + IntToStr(r_sum) + '~C0A, sended = ~C0D' + IntToStr(s_sum);
        s := s + '~C0A, skip cnt = ~C0D' + IntToStr (s_skip) + '~C07';
        s := s + Format('~C0A, w_loops =~C0D %d~C07, w_err =~C0D %d~C07', [w_loops, wait_errors]);
        ODS(s);
       end;
  end;      
 finally
  sc_transfer.Unlock;
  Dec (transferUsage);
 end;
 if WakeupMsgCount > 0 then Dec (WakeupMsgCount);
end; // TransferData





procedure TClientCon.OnError;
begin
 case FSocket.Error of
  WSAEWOULDBLOCK:
             begin
              if Assigned (FSocket) then FSocket.WaitFor(5);
              Inc (wait_errors);
             end;
 {WSAENETDOWN, WSAENOTCONN, WSAECONNREFUSED, WSAECONNRESET,
  WSAESHUTDOWN, WSAEHOSTDOWN, WSAEDISCON }
  else
    if Assigned (FOwner) then    
       need_disconnect := TRUE
    else
       Disconnect(FALSE);   
 end;
end;

function TClientCon.RecvMsgPacket(peekMode: Boolean; pmsg: PMsgPacket): String;
var pkmsg: TMsgPacket;
begin
 if pmsg = nil then pmsg := @pkmsg;
 result := '';
 if ( IncomingBytes >= sizeof (TMsgPacket) )  then
   begin
    if ( Recv(pmsg, sizeof (TMsgPacket), peekMode) = sizeof (TMsgPacket) ) and
         CheckPacket (@pmsg.hdr) and ( pmsg.hdr.tag_ident and PKTAG_MESSAGE <> 0) then
        begin
         result := String ( pmsg.msg );
         CheckPacketOrder(@pmsg.hdr, not peekMode);
        end;
   end;
end; // RecvMsgPacket

procedure TClientCon.WaitFlush(timeOut: DWORD);

begin
 if out_buff.GetCount > 0 then
    out_buff.WaitReadAll(timeOut);

end; // WaitFlush





function TClientCon.RecvData(buff: TStream; const sFirstMsg: String): String;
var
   p32: TPacket32;
   pkmsg: TMsgPacket;
   rc, recv_sum: Integer;
   n_pack, n_skip: Integer;
   n_pass: Integer;
   pack_sz: Integer;
   recv_need: Integer;
   ptList: TPortionList;
   pt, rt: TProfileTimer;
   e: Single;
   msg: String;
begin
 result := '#ERROR:Bad first msg';
 if Pos(UPLOAD_PREFIX, sFirstMsg) <= 0 then exit;
 RecvMsgPacket(FALSE, @pkmsg);  // окончательный прием сообщения, с дополнительными данными
 recv_sum := 0;
 recv_need := pkmsg.hdr.usr_value;
 result := StrSuffix(sFirstMsg, UPLOAD_PREFIX);
 lp_usrval := pkmsg.hdr.usr_value;

 // if net_verbose then ODS('[~T].~C0F #NETDBG: Начат прием данных ' + result + '~C07');
 ptList := in_data.PtList;
 pt := TProfileTimer.Create;
 rt := TProfileTimer.Create;
 n_pass := 0;
 try
  pt.Start;
  repeat
   // получение PREFETCH пакета, с указанием объема и номера посылки или EOS-сообщения
   if IncomingBytes < 32 then
      WaitPacketRecv(500);
   e := pt.Elapsed;
   if e >= 10 then
      ODS(Format('[~T].~C0B #PROF: wait-prefetch packet #%d = %.2f ms ~C07', [n_pass, e]) );
   
   if (MsgInBuff) then
     begin
      // сообщения в процессе приемки данных не ожидаются
      msg := RecvMsgPacket(TRUE);
      if (msg = NMSG_EOSDATA) or (msg = NMSG_SENDERR) then RecvMsgPacket;
      break;
     end;

   FillChar(p32, sizeof(p32), 0);

   if (RecvPacket(@p32) = 32) and
      (p32.tag_ident and PKTAG_PREFETCH <> 0) then
     begin
      lp_usrval := pkmsg.hdr.usr_value;
      Inc (n_pass);
      n_pack := p32.package_no;
      pack_sz := p32.usr_value;

      n_skip := 0;
      pt.Start;
      while (n_skip < 100) and (Ready) do
        begin
         rc := ptList.PackageSize(n_pack);
         if (rc >= pack_sz) then break;

         if WaitPackageRecv (n_pack, pack_sz) = 0 then
             break
         else
             Inc(n_skip);
        end;
      if not Ready then break;
        

      e := pt.Elapsed;
      if e >= 10 then
                ODS(Format('[~T].~C0B #PROF: wait package = %.2f ms ~C07', [e]) );

      if (n_skip >= 100) then
       begin
        rc := ptList.PackageSize(n_pack);
        PrintError(
          Format('Timeout for package arrive. Not recv_sz = %d, pack_sz= %d, recv_sz= %d',
                 [recv_need - recv_sum, pack_sz, rc]));
        break;
       end;


      // линковка данных, для данной посылки
      rc := ptList.JoinData(buff, n_pack, pack_sz);
      if (rc = pack_sz) then
       begin
        ptList.CompletePackage(n_pack);
        Inc(recv_sum, rc);
        SendMsgPacket(NMSG_RECVOK, n_pack);
       end
      else
       begin
        SendMsgPacket(NMSG_RECV_BREAKED);
        PrintError(Format('При сборке посылки #%d, получено иное количество данных %d <> %d',
                          [n_pack, rc, pack_sz]));
        result := '#ERROR:JoinData failed';
        buff.Size := 0;                          
        break;
       end;
       pt.Start; 
     end;
  until (FALSE);
  while (GetDataType and PKTAG_PREFETCH <> 0) do
      begin
       RecvPacket(@p32);
       ODS('[~T].~C0C #WARN: out-recv prefetch packet for package #' +
                IntToStr(p32.package_no) + '~C07');
      end;
 finally
  e := rt.Elapsed;
  if e > 100 then
   ODS(Format('[~T].~C09 #PERF: Данные %s приняты в количестве %d за %.2f мсек~C07',
        [result, recv_sum, e]));
  pt.Free;
  rt.Free;
 end;
end; // RecvData

var
  planed_errors: Integer = 0;

function TClientCon.SendPortions;
var
   np, last: Integer;
   pinf: PPortionInfo;
   pdta: Pointer;
   pk: TPacket32;
   ptListOut: TPortionList;
   lpt: TProfileTimer;
   et, ct: Double;
   awk, bLock: Boolean;
begin
 ptListOut := out_data.ptList;
 result := 0;
 if (not Ready) or (ptListOut.Count = 0) then
   begin
    PrintError('SendPortions: Connection not ready or no portions for send');
    exit;
   end;

 bLock := send_owner <> GetCurrentThreadId;
 if bLock then
    SendLock('SendPortions');

 lpt := TProfileTimer.Create;
 et := 0;
 ct := 0;
 awk := auto_wakeup;
 auto_wakeup := FALSE;

 try
   FillChar (pk, sizeof (pk), 0);

   pk.tag_ident := PKTAG_DATA;
   last := ptListOut.Count - 1;
   // Если вторичный цикл отправок - не все посылки проверять. Важно, чтобы с последней ушел так-же флаг PKTAG_LAST
   if (n_pass > 0) then
    begin
     while (last > 0) do
      if (ptListOut [last].status <= 0) then break else Dec (last);
     if (n_bad >= 0) then
       begin
        // к настоящему порция может быть уже отправленна
        pinf := ptListOut.FindPortion(n_bad);
        if (pinf <> nil) and (pinf.status > 0) then exit;
       end;
    end;

   for np := 0 to last do
    begin
     pinf := ptListOut[np];

     if (pinf = nil) then
       begin
        PrintError('Portion #' + IntToStr(np) + ' = nil');
        break;
       end;
     if (pinf.portion_no = MAXINT) then continue;  
     if (pinf.data_size <= 0) then
       begin
        PrintError('Portion #' + IntToStr(np) + ', data_size = 0');
        break;
       end;
     pdta := GetPtData (pinf);

     if (pinf.status = 1) then
       begin
        // if n_pass = 0 then ODS(Format('[~T]. Portion #%d, status = %d, pass = %d [already sended?]', [np, pinf.status, n_pass]));
        continue; // уже отправлялась!
       end;

     if (pinf.status < 0) then
        ODS('[~T]. Переотправка поврежденной порции #' + IntToStr(pinf.portion_no));
     if np = last then
        pk.tag_ident := pk.tag_ident or PKTAG_LASTPT;
     pk.body_size := pinf.data_size;
     pk.body_crc32 := pinf.crc32;
     pk.portion_no := pinf.portion_no;
     pk.package_no := pinf.package_no;
     // sending header of data stream, after one block
     if (pinf.data_size > 0) and (planed_errors > 0) and (np = 4 + Random(5)) then
      begin
       Dec(planed_errors);
       ODS('[~T.]~C0F #DEBUG: Намеренное повреждение порции данных (CRC)~C0D #' + IntToStr(pinf.portion_no) + '~C07');
       pk.body_crc32 := $EBB0B;
      end;

     if pk.portion_no = 1039 then
       begin
        ODS('[~T]. ~C0A Sending portion #1039, data_size = ' + IntToStr(pinf.data_size) + '~C07');
        //Sleep(1000);
       end;
     // sending portion header-packet
     SendPacket(@pk, FALSE);
     lpt.Start;
     Send(pdta, pinf.data_size, TRUE, FALSE); //
     et := et + lpt.CPUElapsed;
     ct := ct + lpt.ClocksElapsed;
     // ODS(Format('[~T]. ', []));
     if OutcomingBytes > 512 * 1024 then WakeUp;
     pinf.status := 1; // пока значится отправленной
     Inc (result);
    end;
   Flush;
  finally
   if bLock then
      SendUnlock;
   if et > 8 then
     ODS(Format('[~T].~C0E #PROF: SendPortions - CPUTime =~C0F %.2f' +
                '~C0E, MegaClocks =~C0F %.2f~C07', [et, ct]));
   lpt.Free;
   auto_wakeup := awk;
  end;
end; // SendPortions


function TClientCon.SendData(const sDataDesc: String;  buff: TStream): Integer;


var
   pk: TPacket32;
   n_pass: Integer;
   rpos, send_need, send_bytes, t_sum: Integer;
   pt, lpt: TProfileTimer;
   et: Double;
begin
  Randomize;
  result := 0;
  if not Ready then exit;
  pt := TProfileTimer.Create;
  lpt := TProfileTimer.Create;
  pt.Start;
  t_sum := 0;
  et := 0;
  Inc(TransferUsage);
  SendLock('TClientCon.SendData');
  send_owner := GetCurrentThreadId;
  out_buff.write_owner := send_owner;
  out_data.write_owner := send_owner;
  try
    buff.Seek(0, soFromBeginning);
    send_need := buff.Size;
    max_transfer := Max(max_transfer, send_need);
    try
      WaitFlush;
      // отправка стартового пакета
      SendMsgPacket (UPLOAD_PREFIX + sDataDesc, send_need, FALSE);
      // if net_verbose then  ODS('[~T]. #NETDBG: sending upload-start packet');
      // -- Главный цикл отправки данных
      Repeat
       rpos := buff.Position;
       send_bytes := Min( 512 * 1024, buff.Size - rpos);
       if (send_bytes = 0) then break;  // отправка всех данных полностью завершена

       InitPacket(@pk, PKTAG_PREFETCH, send_bytes);
       pk.package_no := InterlockedIncrement(last_pack_no);
       // из буфера считываются и разбиваются на порции данные, вплоть до лимита/исчерпания(NEXT)
       out_data.ptList.SplitData(buff, send_bytes, pk.package_no);

       // отправка упреждающего пакета
       {if net_verbose then
             ODS('[~T].~C0B #NETDBG: sending prefetch packet for package #' +
              IntToStr(pk.package_no) + '~C07');{}
       SendPacket(@pk, FALSE);


       lpt.Start;
       n_pass := 0;
       if SendPortions (n_pass) <= 0 then
          begin
           PrintError('SendPortions returns 0, pass= ' + IntToStr(n_pass));
           break;
          end;
       et := et + lpt.CPUElapsed;
       // WaitFlush;
       Inc (t_sum, send_bytes);
      Until (t_sum >= buff.Size) or (not Ready);
      // -------------------------------------------------------------------------------- //
      result := t_sum;
      if t_sum <> buff.Size then
        begin
         PrintError (Format ('Отправленно данных %s = %d из %d', [sDataDesc, t_sum, buff.Size]));
         SendMsgPacket(NMSG_SENDERR);
        end
      else
         SendMsgPacket(NMSG_EOSDATA);
      WaitFlush;
    except
     on E: Exception do
          PrintError ('TClientCon.SendData cause exception = ' + E.Message);
    end;
  finally
   send_owner := 0;
   out_buff.write_owner := 0;
   out_data.write_owner := 0;
   SendUnlock;
   Dec(TransferUsage);
   if pt.Elapsed >= 1 then
      ODS(Format('[~T].~C0F #PROF: SendData ~C0D %d~C0F bytes / ~C0D %.2f~C0F ms.' +
                 ' CPU_usage = ~C0D %.2f~C0F ms. CheckPart = ~C0D %.2f~C07 ~C07',
                                 [t_sum, pt.Elapsed, pt.CPUElapsed, et]));
   pt.Free;
   lpt.Free;        
  end;
  WakeUp;
end;

function TClientCon.RecvRequest;
var s: String;
    n: Integer;
    pt: TProfileTimer;
    el: Double;
begin
 rql.Clear; // previous request kill
 n := 0;
 pt := TProfileTimer.Create;
 pt.Start;
 el := 0;
 try
   repeat
    s := '';
    if (FOwner = nil) then
        WakeUp
    else
        FOwner.SetBusy();

    if MsgInBuff then
     begin
      s := RecvMsgPacket (TRUE);
      if (n = 0) and (s <> 'START_RQ') then break;
      s := RecvMsgPacket (FALSE);
      if s = 'START_RQ' then
         pt.Start;
      Inc (n);
     end;

    if s <> '' then
                rql.Add (s)
    else
      begin
       if (Owner = nil) or (Owner.ThreadID = GetCurrentThreadId) then
        begin
         TransferRecv;
         if (IncomingBytes < 32) then Socket.WaitFor(10);
        end
       else
        begin
         WaitPacketRecv (100);
        end;
      end;
    el := pt.Elapsed;
    if (el > 1000) then
           PrintError('Timeout for receive multi-line request. Elapsed (ms) = ' +
                FormatFloat('0.0', el) + '. Transfer context = ' + trans_ctx);
   until (s = msg_stop) or (el > 15000);
 finally
  pt.Free;
 end;
 result := (s = msg_stop);
end; // RecvRequest

function TClientCon.GetDataType: WORD;
var pk: TPacket32;
begin
 result := 0;
 if Recv(@pk, 32, TRUE) > 0 then
   result := pk.tag_ident;
end; // GetDataType

function TClientCon.GetPkType: WORD;
var pk: TPacket32;
begin
 result := 0;
 if Recv(@pk, 32, TRUE) > 0 then
    result := pk.signature;
end; // GetPkType



procedure TClientCon.Flush;
begin
 if Assigned (FOwner) then
    FOwner.AddRequest('FLUSH', self, 1)
end;

procedure TClientCon.WakeUp;
var
   owc: TNetThread;
   sobj: TTCPSocket;
begin
 if (WUSendCount > 0) or (not Ready) then exit;
 Inc (WUSendCount);
 owc := FOwner;
 sobj := Socket;
 if Assigned (sobj) then
    bForce := bForce or (OutcomingBytes > 0) or (sobj.ready_recv > 0);

 if Assigned (owc) and bForce then
   begin
    if Assigned (Socket) then
       owc.PostMessage(WM_WSAMSG + 1, Socket.socket, 0)
    else
       owc.AddRequest('WAKEUP', self);
   end;
end; // WakeUp

procedure TClientCon.SendLock;
begin
 sc_send.Lock( TSafeStr ( 'SendLock:' + ctx_msg ), 10);
 out_data.ptList.Lock('SendLock:' + ctx_msg);
 out_buff.LockWrite('SendLock:' + ctx_msg);
end;

procedure TClientCon.SendUnlock;
begin
 out_buff.UnlockWrite();
 out_data.ptList.Unlock;
 sc_send.Unlock;
end;

function TClientCon.ParsePackets(pdata: Pointer; nSize, prv_rest: Integer): Integer;
var
   psrc: PByteArray absolute pdata;
   ppk: PPacket32;
   pmsg: PMsgPacket absolute ppk;
   s: String;
   dt, fp, i, ofst, rb: Integer;
   data_recv: Boolean;
begin
 ofst := 0;
 dt := 0;
 rb := -1;
 data_recv := FALSE;
 try
   while (ofst + sizeof(TPacket32) <= nSize) do
    begin
     ppk := @psrc[ofst];
     case ppk.signature of
      PACKET32_SIGNATURE:
        begin
         dt := 1;
         Inc (ofst, sizeof(TPacket32));
         rb := 0;
         if net_verbose > 10 then
            AddMsgToList(recv_log,
                Format(#9'[~T]. P32 #%d tag = %x, body_size = %d, portion = #%d',
                                [ppk.packet_no, ppk.tag_ident, ppk.body_size, ppk.portion_no]));
         // системный или пакет данных, всякое может быть...
         if (ppk.tag_ident and PKTAG_DATA <> 0) then // полная прозрачная обработка
           begin
            dt := 2;
            if (not data_recv) and
               (in_data.write_owner <> GetCurrentThreadId) then
             begin
              data_recv := TRUE;
              in_data.PtList.Lock('ParsePackets');
             end;
            // функции приемки указывается, сколько данных осталось в кэш-буфере
            rb := in_data.ptList.RecvPortion (ppk, nSize - ofst);
           end
         else
            in_data.Write(ppk, sizeof(TPacket32));

         Inc (ofst, rb); // #WARN: после этого инкремента, смещение может выйти далеко за границы кэша
         result := Max(0, nSize - ofst);
         if (result > 0) and (result < 32) then
             ODS('[~T]. #NETDBG: Rest after RecvPortion = ' + IntToStr(result));
         err_flags := 0;
        end;
      PACKET64_SIGNATURE:
        begin
         if (ofst + sizeof (TMsgPacket) > nSize) then break;
         dt := 3;
         Inc (ofst, sizeof (TMsgPacket));
         if net_verbose > 10 then  AddMsgToList(recv_log,
                Format(#9'[~T]. P64 #%d, data_rest = %d',
                        [pmsg.hdr.packet_no, nSize - ofst]) + ' content = ' + String(pmsg.msg) );
         // прозрачная обрабработка перезапроса порции
         if pmsg.msg = NMSG_RECVPT_FAILED then
           ResendPortion(pmsg.hdr.usr_value)
         else
                 if pmsg.msg = NMSG_RECVOK then
                    out_data.PtList.CompletePackage(pmsg.hdr.usr_value)
                 else
                    in_data.Write(pmsg, sizeof(TMsgPacket));;


         if (err_flags = $100) then
            ODS('[~T]. #NETDBG: Message after error = ' + pmsg.msg +
                                ', pk_no = ' + IntToStr(pmsg.hdr.packet_no) );


         err_flags := 0;   
        end;


       else
        begin
         fp := -1;
         for i := ofst to nSize - 2 do
           case PWord(@psrc[i])^ of
             PACKET32_SIGNATURE,
             PACKET64_SIGNATURE:
              if CheckPacket(@psrc[i]) then
                begin
                 fp := i;
                 break;
                end;
           end;
         err_flags := $100;
         s := 'Сбой последовательности потока данных, при парсинге пакетов. '#13#10' dtype = %d,';
         s := s + ' ofst = %d, DataRest = %d, int.recv = %d, found_pk = %d, prv_rest = %d, total_recv = %d';
         s := s + ' last_recv_ctx: '#13#10 + FSocket.recv_ctx.Text + #13#10;
         s := s + ' recv_log: '#13#10 + recv_log.Text;
         s := s + ' Received buffer dump:'#13#10;
         for i := Max(0, ofst - 32) to Min(nSize - 1, ofst + 52) do
           s := s + ' ' + Byte2Hex (psrc [i]);
         s := Format (s, [dt, ofst, nSize - ofst, rb, fp, prv_rest, total_recv]);
         PrintError(s);
         if fp >= 0 then
            ofst := fp
         else
           begin
            ofst := nSize;
            break;
           end;
        end;
     end;
    end;
 finally
  // завершение приемки данных, и снятие блокировки
  if data_recv then
     in_data.ptList.Unlock;

 end;
 // расчет размера остатка
 result := Max(0, nSize - ofst);
 if result > 0 then
  ODS(Format('[~T].~C0B #NETDBG: ParsePackets returned with rest =~C0D %d~C0B,' +
             ' dt =~C0D %d~C07, nSize=~C0D %d~C07', [result, dt, nSize]));
end; // ParsePackets

function TClientCon.TransferRecv: Integer;
var rb, cb: Integer;
    sobj: TTCPSocket;
    rest: Integer;
    pdata: PByteArray;
    waits: Integer;
begin
 sobj := FSocket;
 result := 0;
 if (sobj = nil) then exit;
 trans_ctx := 'Recv';
 rest := 0;
 waits := 0;
 repeat
  pdata := @cache[rest];
  rb := sobj.ready_recv;
  if (rb = 0) and (rest = 0) then break;
  
  if ((rb = 0) and (rest > 0)) or
     ((rb < 32) and (rest = 0)) then
      begin
       sobj.WaitFor(5);
       Inc (waits);
       if waits < 30 then continue;
      end;

  // данные читаются из сокета блоками
  cb := Min (NET_CACHE_SIZE - rest, rb); //
  if (cb > 0) then
    begin
     if (cb < NET_CACHE_SIZE) then sobj.reset_event;
     rb := sobj.recv (pdata, cb);
     if net_verbose > 10 then
        AddMsgToList(sobj.recv_ctx,
                Format(#9'[~T]. TransferRecv, bytes = %d from %d ', [rb, cb] ));
     if rb <= 0 then
       begin
        OnError;
        // if (not Ready) then break;
        ODS ('[~T].~C0C #PROF: Delaying recv~C07');
       end;

     if rb > 0 then
       begin
        Inc (result, rb);
        Inc (total_recv, rb);
        cb := rb + rest;
        if (cb < 32) or (rest > 16) then
         asm
          nop
         end;
        // остаток выбирается после парсинга
        rest := ParsePackets(@cache[0], cb, rest);
        // если образуется остаток, он смещается в начало кэш-буфера
        if (rest > 0) then;
            SafeMove(cache[cb - rest], cache[0], rest);
       end;
    end;
  until FALSE or (not Ready);
  //  if result > 0 then ODS('[~T]. #NETDBG: TransferRecv result =~C0D ' + IntToStr(result) + '~C07', 2);
  ASSERT (rest <= 0, 'TransferRecv: ofst > 0 at exit');
  trans_ctx := 'Free';
end;

function TClientCon.TransferSend: Integer;
var cb, wb: Integer;
    pdata: Pointer;
    sobj: TTCPSocket;
begin
  sobj := FSocket;
  result := -1;
  if (sobj = nil) then exit;
  trans_ctx := 'Send';
  cb := Min (NET_CACHE_SIZE, out_buff.GetCount);
  pdata := @cache[0];
  if cb > 0 then
      begin
       out_buff.Read(pdata, cb, TRUE);
       if cb < 32 then
         ODS(Format('[~T].~C0C #WARN: Sending small portion, bytes = %d ~C07', [cb]));
       wb := sobj.send(pdata, cb);
       if wb > 0 then
         begin
          if cb > wb then
            begin
             ODS(Format( '[~T].~C0E #NETDBG: Sended~C0D %d~C0E bytes from~C0D %d~C07', [wb, cb]));
             sobj.WaitFor(1);
            end;
          out_buff.Read(pdata, wb); // really read
          result := wb;
         end
       else
         begin
          result := 0;               // cannot push data to socket
          OnError;
          if not Ready then result := -5;

         end;
      end;
 trans_ctx := 'Free';     
end;

procedure TClientCon.UnlockInBuff;
begin
 in_data.UnlockWrite;
end;

// TransferSend


function TClientCon.CheckPacket(p32: PPacket32): Boolean;
var pmsg: PMsgPacket absolute p32;
    crc32: DWORD;
begin
 result := FALSE;
 if (p32.signature = PACKET32_SIGNATURE) then
     result := CalcCRC32 (p32, sizeof(TPacket32) - 4) = p32.pk_crc32;
 if (p32.signature = PACKET64_SIGNATURE) then
   begin
    crc32 := CalcCRC32 (@pmsg.hdr, sizeof(pmsg.hdr) - 4);
    crc32 := CalcCRC32 (@pmsg.msg, sizeof (pmsg.msg), crc32);
    result := (crc32 = pmsg.hdr.pk_crc32); 
   end;
end;


procedure TClientCon.InitPacket;
begin
 FillChar(p32^, sizeof (TPacket32), 0);
 p32.tag_ident := tag;
 p32.usr_value := usrv;
 p32.signature := PACKET32_SIGNATURE;
end;

procedure TClientCon.LockInBuff(const ctx_msg: String);
begin
 in_data.LockWrite('LockInBuff:' + ctx_msg);
end;

function TClientCon.CheckPacketOrder(p32: PPacket32; bSave: Boolean): Boolean;
var dta: Integer;
begin
 dta := p32.packet_no - last_recv_no;
 result := (dta <= 1);
 {if not result then
    ODS(Format('[~T].~C0C #WARN: packet outorder, last = %d, new = %d~C07', [last_recv_no, p32.packet_no]));{}
 if (bSave) then last_recv_no := p32.packet_no;
end;

procedure TClientCon.ResendPortion;
var
    pinf: PPortionInfo;
begin
 pinf := out_data.ptList.FindPortion( np );
 if pinf = nil then
  begin
   PrintError('Requested failed portion, not exist in collector.');
   exit;
  end;
 pinf.status := -1;
 SendPortions(1, np);
end; // ResendPortion



function TClientCon.MsgInBuff: Boolean;
begin
 result := (IncomingBytes >= sizeof (TMsgPacket)) and ( GetDataType and PKTAG_MESSAGE <> 0 );
end; // MsgInBuff


function TClientCon.DataInBuff: Boolean;
begin
 result := (IncomingBytes >= sizeof (TPacket32)) and ( GetDataType and PKTAG_DATA <> 0 );
end; // DataInBuff

function TClientCon.WaitPacketRecv(timeOut: DWORD): DWORD;
begin
 result := in_data.WaitDataArrive(32, timeOut);
end; // WaitPacketRecv

function TClientCon.WaitPortionRecv(timeOut: DWORD; npt: Integer): DWORD;
var
   tstart: Int64;
   elapsed: Integer;
   pl: TPortionList;
begin
 pl := in_data.ptList;
 repeat
  tstart := GetTickCount; // simple-timing
  result := WaitForSingleObject(pl.evt_pt_recv.Handle, timeOut);
  if (result = WAIT_OBJECT_0) then pl.evt_pt_recv.ResetEvent;
  if (npt < 0) then break;
  // Цикл ожидания прекратить, только при нахождении порции
  if (pl.FindPortion(npt) <> nil) then break;
  elapsed := GetTickCount - tstart;
  if timeOut < DWORD(elapsed) then
    begin
     result := WAIT_TIMEOUT;
     break;
    end
  else
     Dec (timeOut, elapsed);
 until FALSE;
end; // WaitPortionRecv

function TClientCon.WaitPackageRecv(n_pack, pack_sz: Integer; timeOut: DWORD): DWORD;
begin
 result := 0;
 with in_data.PtList do
  begin
   if PackageSize (n_pack) = pack_sz then exit;
   wpk_size := pack_sz;
   wait_package := n_pack;
   result := WaitForSingleObject(evt_pk_recv.Handle, timeOut);
   if result = 0 then evt_pk_recv.ResetEvent;
   wait_package := -1;
   wpk_size := 0;
  end;
end; // WaitPackage


{ TNetServerThread }

function TNetServerThread.ConCount: Integer;
begin
 result := FConList.Count;
end;

procedure TNetServerThread.CloseClients;
var n: Integer;
begin
 if Assigned (FConList) then
 try
   for n := FConList.Count - 1 downto 0 do
    if Assigned (FConList [n]) then
    with TClientCon(FConList [n]) do
        if Ready then
          begin
           need_disconnect := TRUE;
           WakeUp;
          end;

 except
  on E: Exception do
    PrintError ('Exception in TNetServerThread.CloseClients: ' + E.Message);
 end;
end; // CloseClients

constructor TNetServerThread.Create(CreateSuspended: Boolean;
  const name: String);
begin
 FConList := TObjectList.Create (TRUE);
 FAsgList := TList.Create;
 FClosedList := TList.Create;

 FPool := TWorkThreadPool.Create(MakeNetThread);
 wait_func := NoWait;
 inherited Create ( CreateSuspended, Name );
end;

destructor TNetServerThread.Destroy;
begin
 WaitStop (1500);
 FConList.Clear;
 FAsgList.Free;
 FAsgList := nil;
 FClosedList.Free;
 FClosedList := nil;
 FConList.Free;      // TODO: close all gracefully
 FConList := nil;
 FPool.Free;
 FPool := nil;
 inherited;
end;

function TNetServerThread.MakeNetThread (const sName: String): TWorkerThread;
begin
 result := TNetThread.Create (FALSE, 'nt@' + sName);
 TNetThread (result).FOwner := self;
end;

function TNetServerThread.GetClientCon(nIndex: Integer): TClientCon;
begin
 result := TClientCon (FConList [nIndex]);
end;

function TNetServerThread.IsOwned(clcon: TClientCon): Boolean;
begin
 result := FConList.IndexOf (clcon) >= 0;
end;

procedure TNetServerThread.OleExec;
var n: Integer;
begin
 // Priority := tpHighest;
 inherited;
 CloseClients;
 for n := FPool.Count - 1 downto 0 do FPool.Items[n].StopThread;
 for n := FPool.Count - 1 downto 0 do FPool.Items[n].WaitStop;
 FSocket.Free;
 FSocket := nil;
end;

procedure TNetServerThread.OnClientDisconnect(Sender: TObject);
var i: Integer;
begin
 with TClientCon (Sender) do
  begin
   ODS ('[~T]. Подключение ~C0F' + Name + '~C07 (login =~C0A ' + Login + '~C07), было завершено или прервано.');
   if Assigned(OnDisconnect) then
      OnDisconnect(Sender);
   FLastState := SST_CLOSED;
   i := FConList.IndexOf(Sender);
   if Socket <> nil then
     begin
      FClosedList.Add (Ptr (Socket.socket));
      need_disconnect := TRUE;
     end;
   if (i >= 0) and OwnedByServer then
     begin
      ODS(#9#9' Объектов подключений сейчас: ' + IntToStr(ConCount));
      DestroyTimer := 20; // several seconds
     end;
  end;
end; // OnClientDisconnect

procedure TNetServerThread.OnClientReleased(clcon: TClientCon);
var n: Integer;
begin
 if not clcon.OwnedByServer then exit;
 n := FConList.IndexOf(clcon);
 if Assigned (FClosedList) then
    FClosedList.Remove (Ptr (clcon.LastSocket)); 
 if n < 0 then exit;
 FConList[n] := nil;
 FConList.Delete (n);
end; // OnClientReleased

function TNetServerThread.Ready: Boolean;
begin
 result := Assigned (FSocket) and (not Terminated);
 if result then result := (FSocket.state = SST_LISTENED);
end;

function TNetServerThread.StartServer(const Host: String; port: Integer): Boolean;
begin
 if Assigned(FSocket) then FSocket.Free;
 FStrError := 'OK';
 FSocket := TTCPSocket.Create;
 result := FALSE;
 if not FSocket.bind (Host, port) then
  begin
   FStrError := 'Cannot bind host: ' + Err2Str (FSocket.Error);
   exit;
  end;
 if not FSocket.listen then
  begin
   FStrError := 'Cannot listen: ' + Err2Str (FSocket.Error);
   exit;
  end;
 FSocket.StartAsyncMsg (hMsgWnd, WM_WSAMSG, FD_ACCEPT or FD_READ or FD_CLOSE);
 if ( FSocket.AsyncMode <> WSA_ASYNC_MSG ) then
  begin
   FStrError := 'Cannot start async: ' + Err2Str (FSocket.Error);
   exit;
  end;
 result := TRUE;
end; // StartServer


procedure TNetServerThread.AcceptCon;
var
   newSocket: TTCPSocket;
   clcon: TClientCon;
begin
 newSocket := FSocket.accept;
 if not Assigned (newSocket) then exit;
 clcon := TClientCon.Create('client con#' + IntToStr(FConList.Count) );
 clcon.OwnedByServer := TRUE;
 clcon.AssignSocket(newSocket);
 newSocket.setopt_i(SOL_SOCKET, SO_KEEPALIVE, 1);
 AddCon (clcon);
 clcon.FLastState := SST_CONNECTED;
 if Assigned(OnClientConnect) then
     OnClientConnect (clcon);
end; // AcceptCon

procedure TNetServerThread.ProcessEvents(evts: Integer);

begin
 if Ready and (evts and FD_ACCEPT <> 0) then AcceptCon;
end;

procedure TNetServerThread.WorkProc;
var n: Integer;
begin
 if (pass_cnt = 0) then
   SetTimer (hMsgWnd, 1000, 500, nil);

 HandleMessages (TRUE);  // ждать сообщения
 HandleRequests (False);

 // check disconnect
 if pass_cnt and $F = 0 then
 for n := ConCount - 1 downto 0 do
 with ClientCons[n] do
   begin
    if Ready and (Owner = nil) then
      if (Socket.ready_recv > 0) or (OutcomingBytes > 0) then WakeUp;

    if (DestroyTimer > 0) then Dec (FDestroyTimer);
    if (DestroyTimer = 0) then
       begin
        FConList.Delete (n);
       end;
   end;
 Inc (pass_cnt);  
 // теоретически могло произойти событие FD_ACCEPT
end; // WorkProc




function TNetServerThread.ProcessMessage(uMsg: DWORD; wParam,  lParam: Integer): Integer;
begin
 result := 0;
 if (uMsg = WM_WSAMSG) or (uMsg = WM_WSAMSG + 1) then
  begin

   if net_verbose > 3 then
     ODS('[~T].~C0B #WM_WSAMSG+' + IntToStr(uMsg - WM_WSAMSG) +
         ': wParam = ~C0D' + IntToStr(wParam) + '~C0B, lParam =~C0D $' + IntToHex(lParam, 8) + '~C07' );
   if wParam = FSocket.socket then
       begin
        ProcessEvents (LoWord (lParam));
        exit;
       end;

   exit;
   // if HiWord (lParam) = 0 then
  end;
 result := inherited ProcessMessage (uMsg, wParam, lParam);
end;

function TNetServerThread.FindCon(s: TSocket): TClientCon;
var n: Integer;
begin
 result := nil;
 for n := 0 to ConCount - 1 do
   if ( FConList[n] <> nil ) and
        Assigned (ClientCons[n].Socket) and
        ( ClientCons[n].Socket.socket = s ) then
     result := ClientCons[n];
end;

function TNetServerThread.AllocateWorker: TNetThread;
begin
 result := TNetThread ( FPool.AllocateWorker );
end;

procedure TNetServerThread.AddCon(clcon: TClientCon);
var wt: TNetThread;
    n: Integer;
begin
 if IsOwned (clcon) then
   begin
    ODS('[~T]. #WARN: Подключение ~C0A' + clcon.Name + '~C07 уже зарегистрированно в серверной нити.');
    exit;
   end;
 clcon.FSrvThread := self;
 clcon.OwnedByServer := TRUE;
 FConList.Add(clcon);
 clcon.FOwner := nil;
 wt := nil;
 for n := 0 to FPool.Count - 1 do
   if TNetThread(FPool[n]).ConCount < 100 then
     begin
      wt := TNetThread (FPool [n]);
      break;
     end;

 if wt = nil then
    wt := TNetThread ( FPool.AllocateWorker('#nt_worker_' + IntToStr(FPool.Count)) );

 wt.AddCon(clcon);
end;

procedure TNetServerThread.OnThreadReleased(clcon: TClientCon; nt: TNetThread);
begin
 FAsgList.Remove(clcon);
end;

procedure TNetServerThread.OnTimer(idTimer: Integer);
begin
  inherited;
end;

{ TNetThread }

function TNetThread.ConCount: Integer;
begin
 result := FConnections.Count;
end;

constructor TNetThread.Create(CreateSuspended: Boolean; const name: String);
begin
 inherited Create ( CreateSuspended, Name, TRUE ) ; //
 wait_func := NoWait;
 bWaitMessages := TRUE;
 FConnections := TObjectList.Create (FALSE);
end;

destructor TNetThread.Destroy;
begin
 FConnections.Free;
 inherited;
end;

function TNetThread.FindCon(nSocket: Integer): TClientCon;
var n: Integer;
    lcon: TClientCon;
    sobj: TTCPSocket;
begin
 result := nil;
 with FConnections do
   for n := Count - 1 downto 0 do
    begin
     lcon := Connections[n];
     sobj := lcon.Socket;
     if (sobj <> nil) and (sobj.socket = nSocket) then
        begin
         result := lcon;
         break;
        end;
    end;
end; // FindCon

function TNetThread.GetConnection(nIndex: Integer): TClientCon;
begin
 result := TClientCon(FConnections[nIndex]);
end;

function TNetThread.PeekMessages(uMsg, flags: DWORD): Integer;
var
   msg: tagMSG;
begin
 result := 0;
 while PeekMessage(msg, hMsgWnd, uMsg, uMsg, flags) do
       Inc (result);
end; // PeekMessages

function TNetThread.ProcessMessage(uMsg: DWORD; wParam, lParam: Integer): Integer;
const MAX_MSG_CNT = 16; // предельное количество различающихся сообщений
var i, rr, evts: Integer;
    bUpd: Boolean;
    lcon: TClientCon;
    msg: tagMSG;
    pmi: PMsgItem;
    umsg_list: array [0..MAX_MSG_CNT] of TMsgItem; // unique message list
    umsg_count: Integer;
begin
 result := 0;
 if ThreadId <> GetCurrentThreadId then exit;
 umsg_count := 0;

 if ((uMsg = WM_WSAMSG) or (uMsg = WM_WSAMSG + 1)) then
   begin
    pmi := @umsg_list[0];
    Inc (umsg_count);
    SetMsgItem(pmi, uMsg, wParam, lParam);

     while PeekMessage(msg, hMsgWnd, uMsg, uMsg, PM_REMOVE) do
      begin
       bUpd := FALSE;
       for i := 0 to umsg_count - 1 do
         if (umsg_list[i].socket = wParam) then
            begin
             Inc (umsg_list[i].msg_cnt);
             bUpd := TRUE;
            end;

       if not bUpd then
         begin
          if umsg_count >= MAX_MSG_CNT then break; // break while
          pmi := @umsg_list[umsg_count];
          SetMsgItem(pmi, uMsg, wParam, lParam);
          Inc(umsg_count);
         end;
      end;
    end;

 // обработка набора однотипных сообщений
 for i := 0 to umsg_count - 1 do
  begin
   pmi := @umsg_list[i];
   lcon := FindCon (pmi.socket);
   if lcon = nil then exit;
   lcon.WUSendCount := 0;
   rr := 0;
   if lcon.Ready then rr := lcon.socket.ready_recv;
   wp_exec_cnt := 0;
   if net_verbose > 3 then
           ODS (Format('[~T].~C0B #NETDBG[' + ThreadName +
                       '/WM_WSAMSG + %d]~C07: socket = %d, lParam = %d, msg_cnt = %d, ready_recv = %d' ,
                                         [uMsg - WM_WSAMSG, wParam, pmi.lParam, pmi.msg_cnt, rr]));
   result := pmi.error;
   evts := pmi.evts;
   if (lcon = nil) or (lcon.DestroyTimer < 50) then exit;
   // обработка завершения подключения
   if evts and FD_CLOSE <> 0 then
    begin
     lcon.Disconnect;
     if result = WSAECONNABORTED then result := 0;
    end;

   if (uMsg = WM_WSAMSG + 1) then evts := evts or FD_WRITE;
   if lcon.Ready then
     begin
      rr := lcon.TransferData (evts);
      if (rr > 0) and (net_verbose >= 3) then
         ODS('[~T]. #NETDBG: TNetThread.ProcessMessage, transferred data (bytes) = ' + IntToStr(rr));
      // if lcon.socket.ready_recv = 0 then PeekMessages(WM_WSAMSG, PM_REMOVE);
     end;
   exit;
  end;

 result := inherited ProcessMessage (uMsg, wParam, lParam);
end;

function TNetThread.ProcessRequest(const rqs: String;  rqobj: TObject): Integer;
var lcon: TClientCon;
    ns: Integer;
begin
 result := 0;
 if (rqs = 'ADDCON') then
    begin
     if FConnections.Count = 0 then
        SetTimer(hMsgWnd, 519, 111, nil);
     if FConnections.IndexOf(rqobj) < 0 then
        FConnections.Add(rqobj);
     if TClientCon(rqobj).Owner <> nil then
      begin
       PrintError(
          Format('Попытка назначения нити соединению уже связанному с другой нитью.' +
                 ' ThreadId = %d, Owner.ThreadId = %d ', [ThreadId, TClientCon(rqobj).Owner.ThreadId ]));
        exit;
      end;
     lcon:= TClientCon (rqobj);
     lcon.FOwner := self;
     lcon.LockInBuff('WorkerThread-Assigned');
     lcon.in_data.write_owner := ThreadId;
     lcon.Socket.StartAsyncMsg(hMsgWnd, WM_WSAMSG, FD_READ or FD_CLOSE);
     ODS(Format('[~T] #NETDBG: Зарегистрировано подключение socket = %d в нити %d',
                [lcon.Socket.socket, ThreadId]));
     exit;
    end;


 if rqs = 'INIT' then
    SetTimer(hMsgWnd, 225, 500, nil); // for regular work-proc executing

 lcon := TClientCon (rqobj);
 if Assigned (lcon) and (FConnections.IndexOf(lcon) >= 0) then
  begin
   if (rqs = 'FLUSH') or (rqs = 'WAKEUP') then
      begin
       if net_verbose > 3 then ODS('[~T].~C09 #NETDBG: Processing ' + rqs + ' request...~C07');
       if rqs = 'FLUSH' then lcon.TransferData (FD_WRITE or FD_READ);
      end;
   if (rqs = 'WAKEUP') and (lcon.WUSendCount > 0) then Dec (lcon.WUSendCount);
   if (rqs = 'STOPTHREAD') then
       begin
        FConnections.Clear;
        Release;
       end;
   if (rqs = 'RMCON') then
      begin
       lcon.FOwner := nil;
       FConnections.Remove(lcon);
       ns := 0;
       if Assigned(lcon.Socket) then
          ns := lcon.Socket.socket;
       ODS(Format('[~T]. #NETDBG: Удалено подключение %s (socket = %d) из нити %d',
                  [lcon.Name, ns, ThreadId]));
      end;
  end;
 inherited ProcessRequest (rqs, rqobj);
end;

procedure TNetThread.RemoveCon(clcon: TClientCon);
begin
 if ThreadID = GetCurrentThreadId then
   ProcessRequest('RMCON', clcon)
 else
   AddRequest('RMCON', clcon);
end;

procedure TNetThread.AddCon(clcon: TClientCon);
begin
 if clcon.Ready then else exit;
 FProfTimer.Start;
 RState := 0;
 clcon.max_transfer := 0;
 clcon.FNextOwner := self;
 AddRequest('ADDCON', clcon);
 while (rq_list.Count > 0) do Sleep(0);
 PostMessage(WM_WSAMSG, clcon.Socket.socket, FD_CONNECT);
 // Priority := tpHigher;
end;


procedure TNetThread.WorkProc;
var
   n, arrived: Integer;
   lcon: TClientCon;
begin
 inherited WorkProc;

 with FConnections do
   for n := Count - 1 downto 0 do
    begin
     lcon := Connections[n];
     if not Assigned(lcon) then
        begin
         Delete(n);
         continue;
        end;
     if lcon.need_disconnect then lcon.BreakCon;
     arrived := 0;
     if Assigned(lcon.Socket) then
        arrived := lcon.socket.ready_recv;

     if (lcon.Ready) and ((lcon.OutcomingBytes > 0) or (arrived > 0)) then
        begin
         if net_verbose >= 5 then
                 ODS('[~T].~C0E #NETDBG: Forced data-transfer - OutcomingBytes =~C0D ' +
                                IntToStr(lcon.OutcomingBytes)+ '~C0E, arrived =~C0D ' +
                                        IntToStr(arrived) + '~C07' );
         lcon.TransferData;
        end;
    end;
end; // WorkProc

{ TPortionList }

function TPortionList.AddPackageInfo(npk: Integer): PPackageInfo;
begin
 result := FindPackageInfo (MAXINT);
 if result = nil then
  begin
   result := FHeap.AllocMem (sizeof (TPackageInfo));
   result.package_no := npk;
   pkinf_list.Add(result);
  end;
 result.package_no := npk;
 result.recv_sz := 0;
end; // AddPackageInfo

function TPortionList.AddPortion: PPortionInfo;
begin
 // сначала поискать свободную порцию
 result := FindPortion(MAXINT);
 if result <> nil then exit;
 // #WARN: любое изменение размера опорных данных приведет к сбою!
 result := FHeap.AllocMem (sizeof(TPortionInfo) + FPtSize);
 Inc(FMemUsage, sizeof(TPortionInfo) + FPtSize);
 // result := PPortionInfo(AddRow);
 FillChar(result^, sizeof(TPortionInfo), 0);
 Add(result);
end; // AddPortion


function      CheckPortion(pinf: PPortionInfo): Boolean;
var pdta: Pointer;
begin
 result := FALSE;
 if (pinf.data_size <= 0) or (pinf.portion_no = MAXINT) then
  begin
   pinf.status := 0;
   exit;
  end;
 pdta := GetPtData(pinf);
 if DWORD(pdta) - DWORD(pinf) <> sizeof(TPortionInfo) then
   asm
    int 3
   end;
 // проверка на повреждения
 if CalcCRC32 (pdta, pinf.data_size) = pinf.crc32 then
  begin
   pinf.status := 1;
   result := TRUE;
  end
 else
  begin
   pinf.status := -1;
  end;
end; // CheckPortion


function TPortionList.CheckData: Integer;
var n: Integer;
    pinf: PPortionInfo;
begin
 result := 0;
 good_count := 0;
 for n := 0 to Count - 1 do
  begin
   pinf := Items[n];
   if CheckPortion (pinf) then
    begin
     Inc(good_count);
     Inc(result, pinf.data_size);
    end;
  end;
end; // CheckData

procedure TPortionList.Clear;
var n: Integer;
begin
 for n := Count - 1 downto 0 do
     RemovePortion (Items[n]);
 inherited Clear;    
end;

procedure TPortionList.CompletePackage(n_pack: Integer);
var n: Integer;
    pki: PPackageInfo;
begin
 //ODS(Format('[~T].~C09 #PERF: Releasing data for package #%d ~C07', [n_pack]));
 for n := 0 to Count - 1 do
  with Items [n]^ do
   if package_no = n_pack then
    begin
     portion_no := MAXINT;
     package_no := MAXINT;
     status := 100;
    end;
 // убиение информации о порции
 pki := FindPackageInfo (n_pack);
 if pki <> nil then pki.package_no := MAXINT;
end;

constructor TPortionList.Create(nPortionSize: Integer);
begin
 FPtSize := nPortionSize;
 sc_share := TCritSection.Create(ClassName + '.sc_share');
 evt_pt_recv := TEvent.Create(nil, TRUE, FALSE, '');
 evt_pk_recv := TEvent.Create(nil, TRUE, FALSE, '');
 pkinf_list := TList.Create;
 wait_package := -1;
 FHeap := TWindowsHeap.Create('TPortionList.FHeap');
 inherited Create ();
end;

destructor TPortionList.Destroy;
var n: Integer;
begin
 sc_share.Lock('.Destroy');
 evt_pt_recv.Free;
 evt_pk_recv.Free;
 for n := Count - 1 downto 0 do
    RemovePortion(Items[n]);
 pkinf_list.Free;
 Clear;
 FHeap.Free;
 inherited;
 sc_share.Free;
end;

function TPortionList.FindPackageInfo(npk: Integer): PPackageInfo;
var n: Integer;
begin
 for n := 0 to pkinf_list.Count - 1 do
   begin
    result := pkinf_list[n];
    if result.package_no = npk then exit;
   end;
 result := nil;  
end;

function TPortionList.FindPortion(pt_no: Integer; pIndex: PInteger): PPortionInfo;
var n: Integer;
    pinf: PPortionInfo;
begin
 result := nil;
 if pIndex <> nil then pIndex^ := -1;
 // if (packet_no < min_pk_no) or (packet_no > max_pk_no) then exit;
 Lock('FindPortion');
 try
   for n := 0 to Count - 1 do
    begin
     pinf := Items[n];
     if pinf.portion_no = pt_no then
      begin
       result := pinf;
       if pIndex <> nil then pIndex^ := n;
       break;
      end;
    end;
 finally
  Unlock;
 end;
end; // FindPortion

function TPortionList.GetIndex(pt_no, i: Integer): Integer;
begin
 result := -1;
 if (i < Count - 1) and (pt_no = Items[i + 1].portion_no) then
  result := i + 1
 else
  FindPortion(pt_no, @result);
end;

function TPortionList.GetItem(nIndex: Integer): PPortionInfo;
begin
 result := PPortionInfo(TList(self).Items[nIndex]);
end;

function TPortionList.JoinData(dst: TStream; n_pack, need_sz: Integer): Integer;
var n, n_curr: Integer;
    minp: Integer;
    pinf: PPortionInfo;
    pdta: Pointer;
    fsize, avail: Integer;
    err: Boolean;
begin
 result := 0;
 fsize := 0;
 avail := dst.Size - dst.Position;
 n_curr := MAXINT;
 minp := MAXINT;
 Lock('JoinData');
 try
   // подсчет, сколько всего данных в байтах распределенно в порциях,
   // а так-же выделение первой подходящей порции
   err := FALSE;
   for n := 0 to Count - 1 do
     with Items [n]^ do
     if (package_no = n_pack) then
        begin
         if (status < 0) then
          begin
           PrintError('[~T]. #ERROR: TPortionList.JoinData - недозагружена проция #' + IntToStr(portion_no));
           err := TRUE;
          end
         else
          begin

           Inc (fsize, data_size);
           if (minp > portion_no) then
             begin
              n_curr := n;
              minp := portion_no;
             end;
          end;
        end;

   if err then
    begin
     result := 0;
     dst.Size := 0;
     exit;
    end;

   // расширение буфера, под добавление данных
   if (avail < fsize) then
       dst.Size := dst.Size + (fsize - avail);

   if (n_curr <= count - 1) then
    repeat
     pinf := Items[n_curr];
     pdta := GetPtData (pinf);
     if (pinf.status <= 0) or (pinf.data_size <= 0) or
        (pinf.package_no <> n_pack) then break;
     dst.Write(pdta^, pinf.data_size);
     Inc (result, pinf.data_size);
     n_curr := GetIndex (pinf.portion_no + 1, n_curr);
     // маркировка порций свободными
     pinf.portion_no := MAXINT;
     pinf.package_no := MAXINT;
    until (n_curr < 0);
 finally
  Unlock;
 end;
end; // JoinData

procedure TPortionList.Lock;
begin
 sc_share.Lock( '.' + ctx, 50);
end;

procedure TPortionList.Optimize();
var 
    pinf: PPortionInfo;
    prva: Integer;
begin
 prva := MemUsage;
 if sc_share.TryLock('Optimize', 50) then
 try
  if Count > 1 then
  repeat
   pinf := FindPortion(MAXINT);
   if pinf = nil then break;
   RemovePortion(pinf);
  until Count <= 1;
 finally
  sc_share.Unlock;
  if prva > MemUsage then
   ODS(Format('[~T]. #PERF: PortionList mem-usage optimized from %d to %d bytes', [prva, MemUsage]));
 end;
end; // Optimize


function TPortionList.PackageSize(n_pack: Integer): Integer;
var pki: PPackageInfo;
begin
 result := 0;
 pki := FindPackageInfo (n_pack);
 if (pki <> nil) then
     result :=  pki.recv_sz; 
end; // PackageSize

function TPortionList.RecvPortion;
var
   np, cb, rb, wofst: Integer;
   need_recv: Integer;
   pinf: PPortionInfo;
   pki: PPackageInfo;
   psrc, pdta: PByteArray;
   sobj: TTCPSocket;
   owt: TNetThread;
begin
 result := 0;
 np := phdr.portion_no;
 if np = MAXINT then
  begin
   PrintError('Wrong portion number for receive');
   exit;
  end;
 // циклическая загрузка данных для единичной посылки-порции
 cb := phdr.body_size;
 if cb <= 0 then exit;
 Lock('RecvPortion');
 try
   pinf := FindPortion(np);
   if (pinf = nil) then
      pinf := AddPortion;
   pinf.data_size := 0;
   pinf.portion_no := np;
   pinf.package_no := phdr.package_no;
   if (pinf.status < 0) then
      ODS('[~T]. #DEBUG:~C0E Принята исправленная порция~C0D #' + IntToStr(np) + '~C07');
   pinf.status := 0;
   pdta := GetPtData(pinf);
   psrc := RelativePtr(phdr, sizeof(TPacket32));
   if rcv_bytes > 0 then  // Если кол-во уже принятых данных существенное
      begin
       rb := Min(rcv_bytes, cb);
       SafeMove(psrc[0], pdta[0], rb);
       Inc (result, rb);
      end;
   // Прямой прием данных из сокета, механизм по идее вызываться должен из нити FCon.Owner    
   if (rcv_bytes < cb) then
    begin
     need_recv := cb - rcv_bytes;
     wofst := rcv_bytes;
     sobj := FCon.Socket;
     owt := FCon.Owner;
     if sobj <> nil then
     repeat
      if FCon.Socket = nil then
         begin
          PrintError('Portion receiving breaked, by connection socket closed');
          exit;
         end;
      if sobj.ready_recv < need_recv then sobj.WaitFor(20);
      rb := sobj.recv(@pdta[wofst], need_recv);
      if net_verbose > 10 then
        AddMsgToList ( sobj.recv_ctx,
        Format(#9'[~T]. RecvPortion, bytes = %d from %d, hdr_packet = #%d, package = #%d, portion = #%d ',
                        [rb, need_recv, phdr.packet_no, phdr.package_no, phdr.portion_no]));


      if (rb <= 0) then
         begin
          // #NOT_RECV handling
          ODS('[~T]. #NETDBG: RecvPortion / Wait-data #0 ', 2);
          if FCon.Ready then
            begin
             if owt <> nil then
                owt.PeekMessages(WM_WSAMSG, PM_REMOVE);
             FCon.Socket.WaitFor(500);
             if sobj.ready_recv = 0 then Sleep(5);
            end
           else
          break;
         end
      else
         begin
          Inc (result, rb); // сколько реально было получено от сокета
          Dec (need_recv, rb);
          Inc (wofst, rb);
         end;
     until (need_recv <= 0); // if sobj <> nil
     if (owt <> nil) and (sobj <> nil) and (sobj.ready_recv = 0) then owt.PeekMessages(WM_WSAMSG, PM_REMOVE);
    end;



   pinf.crc32 := phdr.body_crc32;
   pinf.data_size := phdr.body_size;
   Inc (recv_count);
   // проверка целостности данных порции, и сигнализация в случае завершения приемки
   if CheckPortion (pinf) then
      begin
       pki := FindPackageInfo (pinf.package_no);
       if pki = nil then
          pki := AddPackageInfo (pinf.package_no);
       Inc(pki.recv_sz, pinf.data_size);
       if (wait_package = pki.package_no) and (pki.recv_sz >= wpk_size) then
           begin
            evt_pk_recv.SetEvent;
            wait_package := -1;
           end;
      end
   else
      begin
       if FCon.Ready then FCon.SendMsgPacket(NMSG_RECVPT_FAILED, np);
       ODS('[~T].~C0C #WARN: Порция #' + IntToStr(np) + ' повреждена. Перезапрос.~C07');
      end;
    {else
       ODS('[~T]. #NETDBG: Received portion #' + IntToStr(np) + ', package #' +
          IntToStr(pinf.package_no));}


 finally
  Unlock;
 end;
end; // TPortionList.RecvPortion


procedure TPortionList.RemovePortion(pinf: PPortionInfo);
begin
 Remove(pinf);
 FHeap.ReleaseMem(pinf);
 Dec(FMemUsage, sizeof(TPortionInfo) + FPtSize);
end;

function TPortionList.SerialCheck: Integer;
var n, ptno, n_curr: Integer;
    pinf: PPortionInfo;
begin
 result := -1;
 ptno := last_good_no;
 if (ptno <= 0) then
     ptno := MAXINT;
   n_curr := -1;
 // поиск наименьшей порции
 for n := 0 to Count - 1 do
  with Items[n]^ do
  if ptno >= portion_no then
    begin
     ptno := portion_no;
     n_curr := n;
     result := n;
    end;
 if (n_curr < 0) then exit;   // no portions?
 repeat
  pinf := Items[n_curr];
  // если порция испорчена, продолжение не следует
  if pinf.status > 0 then result := n_curr else break;
  n_curr := GetIndex(pinf.portion_no + 1, n_curr);
 until (n_curr < 0);
 with Items[result]^ do
 if last_good_no < portion_no then
       evt_pt_recv.SetEvent;
end; // SerialCheck

procedure TPortionList.SplitData(src: TStream; nSize, n_pack: Integer);
// разбивка части буфера, начиная с текущей позиции
var
   pinf: PPortionInfo;
   pdta: Pointer;
   rest: Integer;
begin
 nSize := Min(src.Size - src.Position, nSize);
 rest := nSize;

 repeat
  pinf := AddPortion;
  pinf.data_size := Min (rest, FPtSize);
  // данные следуют сразу за заголовком порции
  pdta := GetPtData (pinf);
  src.Read(pdta^, pinf.data_size);
  pinf.crc32 := CalcCRC32 (pdta, pinf.data_size);
  pinf.status := 0;
  pinf.portion_no := InterlockedIncrement(last_pt_no);
  pinf.package_no := n_pack;
  Dec(rest, pinf.data_size);
 until (rest <= 0);
end;  // SplitData


procedure TPortionList.Unlock;
begin
 sc_share.Unlock;
end;

{ TDataCollector }



constructor TDataCollector.Create;
begin
 FPtList := TPortionList.Create;
 FCon := AOwner;
 ptList.FCon := AOwner;
 inherited Create();
end;

destructor TDataCollector.Destroy;
begin
 ptList.Free;
 FPtList := nil;
 inherited;
end;


end.
