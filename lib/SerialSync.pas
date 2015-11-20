unit SerialSync;

interface
uses Windows, SysUtils, Math, Misc;

{
   Глобальные данные синхронизатора:
    *  массив ассоциаций поток-событие, для устранения многократного выделения ресурсов
         В условиях частого создания потоков, этот массив может разрастаться, поэтому
       можно иногда выполнять его оптимизацию - удаляя ассоциации для мертвых потоков,
       освобождая евенты для новых.
       Для этого генерируется копия массива, оптимизируется и указатель на нее замещает исходный
       с помощью InterlockedExchange.

}

type
   {$A4}        // 32-bit aligning use
   TICEParam = Pointer;

   PThreadSlot = ^TThreadSlot;
   TThreadSlot = record
    blink: PThreadSlot;
    slink: PThreadSlot;                      // указатель на себя
    flink: PThreadSlot;                      // указатель на следующий поток
    dwThreadId: DWORD;
    n_slot: Integer;
    hWakeUpEvt: THandle;                     // событие для пробуждения потока, при передаче ресурса
    dtTimeout: TDateTime;                    // временная точка, после которой захват не требуется
   end; // TThreadSlot



   TSyncSection = record
    nWaiters: Integer;
    nHead: Integer;
    nMaxSlots: Integer;
    tid_owner: DWORD;
    mod_lock: DWORD;
    slots: array [0..7] of TThreadSlot; // динамически можно выделить больше 8 слотов
   end; // TThreadSlot

   PSyncSection = ^TSyncSection;

   {
   TThreadSyncInfo = packed record
    dwThreadId: DWORD;
    hEvent: THandle;
   end;   // TThreadSyncInfo {}


procedure init_ss (pss: PSyncSection);
procedure release_ss(pss: PSyncSection; bFree: Boolean = FALSE);

function try_enter_ss(pss: PSyncSection; nTimeOut: Integer; nSpinLockCount: Integer = 500): Boolean;
procedure leave_ss(pss: PSyncSection);

implementation

{
type // ограничение на 128 тредов!
    TGlobalThreadsInfo = array [0..127] of TThreadSyncInfo;
    PGlobalThreadsInfo = ^TGlobalThreadsInfo;

var
   gThreadSyncInfo: PGlobalThreadsInfo;
   gTSICount: Integer = 0;
 {}


function InterlockedCompareExchange(var Destination: DWORD;
                        Exchange: DWORD; Comperand: DWORD): DWORD stdcall; external kernel32 name 'InterlockedCompareExchange';





procedure init_ss (pss: PSyncSection);
var n: Integer;
begin
 FillChar(pss^, sizeof(TSyncSection), 0);
 pss.nMaxSlots := 8;
 for n := 0 to pss.nMaxSlots - 1 do
    pss.slots[n].hWakeUpEvt := CreateEvent(nil, TRUE, FALSE, nil);
end; // init_ss

procedure release_ss(pss: PSyncSection; bFree: Boolean = FALSE);
var n: Integer;
begin
 for n := 0 to pss.nMaxSlots - 1 do
    CloseHandle(pss.slots[n].hWakeUpEvt);
end; // release_ss


function try_enter_ss(pss: PSyncSection; nTimeOut: Integer; nSpinLockCount: Integer): Boolean;
var
   self_slot: PThreadSlot;
   tid: DWORD;
   n, i, cur_tail: Integer;
begin
 InterlockedIncrement (pss.nWaiters);
 tid := GetCurrentThreadId;
 cur_tail := pss.nHead;
 self_slot := nil;
 // поиск свободных слотов, относительно начала очереди
 while TRUE do
  begin
   if cur_tail >= pss.nMaxSlots then cur_tail := 0;
   self_slot := @pss.slots[cur_tail];
   n := InterlockedCompareExchange ( self_slot.dwThreadId, tid, 0);
   if n = 0 then break;
   Inc (cur_tail);
  end;


 ASSERT (self_slot.dwThreadId = tid, 'Slot already ownered');
 ResetEvent(self_slot.hWakeUpEvt);
 result := FALSE;


 // быстрый захват секции - по условию достижения собственного слота как первичного
 for i := 0 to Min(0, nSpinLockCount - 1) do
    if (pss.nHead = cur_tail) or ( pss.tid_owner = tid ) then
      begin
       pss.tid_owner := tid;
       result := TRUE;
       exit;
      end;


 if not result then
    // теперь выжидание по событию
   result := ( WAIT_OBJECT_0 = WaitForSingleObject(self_slot.hWakeUpEvt, nTimeOut) );

 result := result and ( pss.tid_owner = tid );


 if not result then
   begin
    self_slot.dwThreadId := 0;
    InterlockedDecrement (pss.nWaiters);
    ColorPrint('~C0C #ERROR: try_enter_ss - timeout for thread ' + IntToStr(tid) + ' ~C07');
   end;
end; // try_enter_ss

procedure leave_ss(pss: PSyncSection);
var
   tid: DWORD;
   pSlot: PThreadSlot;
   cnt_check, n: Integer;
begin
 tid := GetCurrentThreadId;
 // здесь ресурс передается следующему потоку в очереди
 pSlot := @pss.slots[pss.nHead];
 if pSlot.dwThreadId <> tid then
   ColorPrint('~C0C #ERROR: leave_ss - other thread (' + IntToStr(pSlot.dwThreadId) + ') slot ~C07');
 pSlot.dwThreadId := 0;        // теперь слот свободен для других желающих
 pss.tid_owner := 0;
 cnt_check := 0;
 InterlockedDecrement (pss.nWaiters);
 n := pss.nHead;
 // циклический поиск наследников ресурса, поскольку в кольце могут быть дыры!
 while (pss.nWaiters > 0) do
  begin
   Inc (n);
   if n >= pss.nMaxSlots then n := 0;
   Inc (cnt_check);
   if cnt_check and pss.nMaxSlots = pss.nMaxSlots then
     begin
      Sleep(0);
     end;

   pSlot := @pss.slots[ n ];
   if pSlot.dwThreadId = 0 then continue;
   pss.nHead := n;
   pss.tid_owner := pSlot.dwThreadId;
   ColorPrint('#DEBUG: activating thread ' + IntToStr(pss.tid_owner));

   SetEvent(pSlot.hWakeUpEvt);
   break;
  end;
 if cnt_check > 1 then
   WriteLn ('#debug: count of checks ' + IntToStr(cnt_check) + ', waiters ' + IntToStr(pss.nWaiters));
 pss.mod_lock := 0;
end; // leave_ss



initialization

finalization

end.
