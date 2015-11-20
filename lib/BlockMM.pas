unit BlockMM;

interface
uses Windows, BaseLib;

const
   MIB = 1048576;

procedure con_log(const msg: WideString; flags: DWORD); stdcall;



procedure ReleaseBlock(dwSize: DWORD);
procedure FreeBuffer;


var
   log_proc: Procedure (const msg: WideString; flags: Cardinal = 255); stdcall = con_log;
   alloc_sum: Int64 = 0;
   afake_sum: Int64 = 0;
   freed_sum: Int64 = 0;
   alloc_cnt: Integer = 0;


implementation

const
   BUFF_SIZE = 64 * MIB;
   PAGE_SIZE = 4096;  // boundary
   PAGE_MASK = PAGE_SIZE - 1;
   PAGE_COUNT = BUFF_SIZE div PAGE_SIZE;
   PAGE_RESV = 1;
   PAGE_COMT = 2;
   PAGE_USED = PAGE_RESV or PAGE_COMT;
   PAGE_LAST = 128;


   MID_BLOCK = 65536;
   BIG_BLOCK = MIB;


type

   TBytePage = array[0..PAGE_SIZE - 1] of BYTE;
   PBytePage = ^TBytePage;

   TByteVector = array [0..BUFF_SIZE + PAGE_SIZE - 1] of BYTE;
   PByteVector = ^TByteVector;





var
   // _buffer: TByteVector; // cycled buffer
   // buffer: PByteVector;

   small_blks: array [0..1023] of PByteVector;
   mid_blocks: array [0..63] of PByteVector;
   big_blocks: array [0..70] of PByteVector;

   _finalized: Boolean = FALSE;
   enable_con: Boolean = FALSE;
   // pages: array [0..PAGE_COUNT - 1] of TPageInfo;
   sc_lock: TRTLCriticalSection;

procedure con_log(const msg: WideString; flags: DWORD); stdcall;
begin
 if enable_con then WriteLn(msg);
 OutputDebugString (PChar (msg));
end;



procedure FailExit;
var
   s: String;
begin
 s := 'GetLastError = ' + itos(GetLastError);
 OutputDebugString (PChar(s));
 ExitProcess (0);
end;



function AllocBlock(dwSize: DWORD): Pointer;
begin
 Inc (afake_sum, dwSize);
 result := Windows.VirtualAlloc (nil, dwSize, MEM_COMMIT or MEM_RESERVE or MEM_TOP_DOWN, PAGE_READWRITE);
end;

procedure FreeBlock (var pb: PByteVector; sz, ft: DWORD);
var
   ret: Boolean;
begin
 if (pb = nil) or (sz = 0) then exit;
 // VirtualFree (pb, 0, MEM_RELEASE) and
 case ft of
  MEM_DECOMMIT: ret := VirtualFree (pb, sz, ft);
   MEM_RELEASE: ret := VirtualFree (pb, 0, ft);
  else ret := FALSE;
 end;

 if ret then
    Inc (freed_sum, sz)
 else
    log_proc ('[~T].~C0C #ERROR:~C07 Failed free block ' + PtrToStr(pb) + ' = ' + itos(GetLastError));
 pb := nil;
end;

procedure FreeBuffer;
var
   n, cnt: Integer;

begin
 cnt := High(big_blocks) - 70;
 for n := 0 to cnt - 1 do
  if big_blocks [n] <> nil then
   begin
    FreeBlock (big_blocks[n], BIG_BLOCK, MEM_DECOMMIT);
    break;
   end;
end;



procedure InitFakeMemLoad;
var
   n: Integer;
begin
 for n := 0 to High(small_blks) do
     small_blks[n] := AllocBlock (PAGE_SIZE);

 for n := 0 to High(mid_blocks) do
     mid_blocks[n] := AllocBlock (MID_BLOCK);

 for n := 0 to High(big_blocks) do
     big_blocks[n] := AllocBlock (BIG_BLOCK);
end;

function  FindBlock (const arr: array of PByteVector; last: Integer): Integer;
var
   n: Integer;
begin
 result := -1;
 for n := last downto 0 do
  if arr [n] <> nil then
   begin
    result := n;
    break;
   end;
end;


function ReleaseLoop(var arr: array of PByteVector; last: Integer; dwBlockSize, dwSize: DWORD): DWORD;
var
   n: Integer;
begin
 // циклический отъем
 while  ( dwSize >= dwBlockSize ) or ( (dwSize > 0) and ( dwBlockSize = PAGE_SIZE ) ) do
  begin
   n := FindBlock (arr, last);
   if n >= 0 then
    begin
     FreeBlock ( arr [n], dwBlockSize, MEM_RELEASE);

     if (dwSize > dwBlockSize) then
         Dec ( dwSize, dwBlockSize )
     else
         dwSize := 0;
    end
   else
     break;
  end;

 result := dwSize;
end;

procedure ReleaseBlock(dwSize: DWORD);
begin
 if _finalized then exit;
 EnterCriticalSection(sc_lock);
 try
  Inc (alloc_sum, dwSize);
  Inc (alloc_cnt);
  dwSize := ReleaseLoop (big_blocks, High(big_blocks), BIG_BLOCK, dwSize);
  dwSize := ReleaseLoop (mid_blocks, High(mid_blocks), MID_BLOCK, dwSize);
  ReleaseLoop (small_blks, High(small_blks), PAGE_SIZE, dwSize);
 finally
  LeaveCriticalSection(sc_lock);
 end;
end;




initialization

 if not IsConsole then
     AllocConsole;
 InitializeCriticalSection (sc_lock);
 InitFakeMemLoad;

 //  buffer := Windows.VirtualAlloc(nil, BUFF_SIZE, MEM_COMMIT or MEM_RESERVE, PAGE_READWRITE);

{ ptr_first := Cardinal (buffer);
 ptr_first := AlignOffset (ptr_first);
 buffer := PByteVector (ptr_first);
 ptr_last := ptr_first + sizeof(buffer);

 // InitPages; }


finalization
 log_proc := nil;
 _finalized := TRUE;
 DeleteCriticalSection (sc_lock);
end.
