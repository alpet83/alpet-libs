unit SMTypes;
{ ����� ������ �������� �������� SegMath ��� ��������� � ������ � DLL.
  08-09-2009 created by alpet
}
interface
uses Windows, SysUtils, StrUtils, Classes, ArrayTypes;

const
     MAX_INSTR_DATA = 16;

     NPARAM1 = 0;
     NPARAM2 = 1;
     NPARAM3 = 2;
     NPARAM4 = 3;
     NPARAM5 = 4;

     // ����� �������� ���������������� ����������� BuySell
     SIG_BUY_AUTO     = +$001;  // using general trade optimizations
     SIG_BUY_LIMIT    = +$002;
     SIG_BUY_MARKET   = +$005;
     SIG_CLOSE_SHORT  = +$00C;

     SIG_SELL_AUTO    = -$001;
     SIG_SELL_LIMIT   = -$002;
     SIG_SELL_MARKET  = -$005;
     SIG_CLOSE_LONG   = -$00C;






     SMLIB_VERSION = $00010001;
     SMF_HASPARAMS = $0001;
     SMJ_PARAM_COUNT  = 16;
     STD_ORIGIN       = 200;


     DSF_DATA_STREAM  = $0001;
     DSF_TIME_STREAM  = $0008 or DSF_DATA_STREAM;
     DSF_DATE_STREAM  = $0010;
     DSF_BARS_DATA    = $1000;

     DSF_DISPLAYED = $01000000;

     STGF_REALTIME     = $1000;
     STGF_SCALPER      = $4000;

     SDR_FLOAT_ARRAY   = $0001;
     SDR_INT_ARRAY     = $0002;
     SDR_DATRNG_ARRAY  = $0003;

type

     TSMAllocFunc = function ( dwBytes: DWORD; pp: Pointer ): Pointer; cdecl; // ���������

     TSMDataRec = packed record // ~28 bytes
      name: array [0..15] of AnsiChar;

      wType: WORD;                      // ����������� ��������� ������ �� ���������
      wItemSize: WORD;                  // ���� � ��������
      nCount: Integer;
      nOrigin: Integer;
      nMask: Integer;                   // ��� �������� ����������� ���������
      ValidRange: TIndexRange;          // for internal reset
      lporig: Pointer;

     case BYTE of
      0: (fdata: PSingleArray);
      1: (idata: PIntArray);

     end; // TSMDataRec

     PSMDataRec = ^TSMDataRec;

     TInstrDataPtl = packed record  // ������ ���������� �� ������ ����������� (���� ��� ����)
      cnt_items: Integer;
      price_open, price_high, price_close, price_low: PSMDataRec;
      time_open, time_close: PSMDataRec;
      volume: PSMDataRec;
      cnt_days: Integer;
      date_map: PIntradayRangeArray;
      instr_alias: array [0..23] of AnsiChar;
      resv: array [0..31] of BYTE;
     end; // TInstrDataPtl

     PInstrDataPtl = ^TInstrDataPtl;

     TInstrDataList = packed array [0..MAX_INSTR_DATA - 1] of PInstrDataPtl;
     TIndDataList =   packed array [0..SMJ_PARAM_COUNT - 1] of PSMDataRec;

     TOffsetListPCx32 = packed record
       _in: packed array [0..SMJ_PARAM_COUNT - 1] of SmallInt; // ���������� �������� ����������
      _out: packed array [0..SMJ_PARAM_COUNT - 1] of SmallInt; // ���������� �������� ����������
     end;

     POffsetListPCx32 = ^TOffsetListPCx32;


     /// ��������� ���� ���������� ����� �������, ����������� ������������ ���������.
     TSMFuncParams = packed record
      dwVersion: DWORD;
      dwSize: DWORD;
      iFirst, iLast: Integer;          // �������� ������� �� ������
      n_segment: Integer;              // ����� �������� ��� ������. ��� ������� ��������.
       in_data: TIndDataList;
      out_data: TIndDataList;
      instr_data: TInstrDataList;      // ������������� �� NULL. ������ ������ ���� ��������
      i_params: array [0..SMJ_PARAM_COUNT - 1] of Int64;
      f_params: array [0..SMJ_PARAM_COUNT - 1] of Double;
      max_param_count: Integer;
      pfcAlloc: TSMAllocFunc;
      pContext: Pointer;               // �������������� ������� �������, ������������ small_ctx
      ctx_size: DWORD;                 // ������ ���������� ������
      stg_flags: DWORD;                // ������ ���������
      AOwner: TObject;                 // ��������

      offsets: TOffsetListPCx32;
      applied: TOffsetListPCx32;

      small_ctx: packed array [0..3475] of BYTE; // ��� ������������ � ������� ��������
     end; // TIndFuncParams

    PSMFuncParams = ^TSMFuncParams;

    TSMFunction =  function ( fp: PSMFuncParams ): Integer; stdcall;

    TCallbackFunc = packed record
     func_ver: DWORD;
     func_ptr: Pointer;
     func_name: array [0..7] of AnsiChar;
     call_type: array [0..7] of AnsiChar;
    end; // TCallbackFunc

    TCallbackList = packed record
     items_count: Integer;
     func_list: packed array [0..1] of TCallbackFunc; // �� ����� ���� ������ 2 ����� ����
    end; // TCallbackList

    PCallbackList = ^TCallbackList;


    TIndFuncDesc = packed record
     func_ver: DWORD;
     func_ptr: TSMFunction;
     func_name: array [0..15] of AnsiChar;
    end;

    TExportList = packed record
     items_count: Integer;
     func_list: packed array [0..1] of TIndFuncDesc;
     procedure          AddFunc (const name: String; func: TSMFunction);
    end; // TExportList

    PExportList = ^TExportList;


var
   _LogODS: procedure (msg: PWideChar) stdcall = nil;
/// ---------------------------------------------------------------------------------------------------------- ///

function             CheckDataParams (const idl: TIndDataList; const prefix: String; args_min: Integer): Boolean;
procedure            LogODS (const msg: String);

implementation

procedure            LogODS (const msg: String);
begin
 if Assigned(_LogODS) then
    _LogODS ( PWideChar (msg) );
end; // LogODS


function             CheckDataParams (const idl: TIndDataList; const prefix: String; args_min: Integer): Boolean;
var n: Integer;
begin
 result := TRUE;
 for n := 0 to args_min - 1 do
 if idl [n] = nil then
  begin
   LogODS ('[~T].~C0C #ERROR: unassigned parameter ' + prefix + '[' + IntToStr(n) + ']~C07');
   result := FALSE;
   exit;
  end;

end; // CheckDataParams


{ TExportList }

procedure TExportList.AddFunc(const name: String; func: TSMFunction);
begin
 with func_list[items_count] do
  begin
   StrPCopy ( func_name, AnsiString(name));
   func_ptr := func;
  end;
 Inc (items_count);
end; // AddFunc

end.
