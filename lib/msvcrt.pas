unit Msvcrt;

interface
uses Windows, SysUtils, Classes;

const
     MSVCRT_LIB  = 'msvcr120.dll';
     FT_DIFF     = 116444736000000000;


type
     __time64_t = UInt64;

     __utimbuf64 = record
       actime: __time64_t;
      modtime: __time64_t;
     end;


// set file modification time
function _utime64(filename: LPSTR; const times: __utimbuf64): __time64_t; cdecl;
function _wutime64(filename: LPWSTR; const times: __utimbuf64): __time64_t; cdecl;
function _get_modtime64(filename: LPWSTR): __time64_t;
function _filetime_convert (const ft: FILETIME): __time64_t;

implementation


function _filetime_convert (const ft: FILETIME): __time64_t;
begin
 Move (ft, result, sizeof(result));
 result := (result - FT_DIFF) div 10000000;
end;

function _utime64(filename: LPSTR; const times: __utimbuf64): __time64_t;     cdecl; external MSVCRT_LIB name '_utime64';
function _wutime64(filename: LPWSTR; const times: __utimbuf64): __time64_t;  cdecl; external MSVCRT_LIB name '_wutime64';


function _get_modtime64(filename: LPWSTR): __time64_t;
var
     h: THandle;
    wt: FILETIME;

begin
 result := __time64_t(-1);
 if DirectoryExists(filename, FALSE) then   exit;
 result := 0;

 h := CreateFile (filename, GENERIC_READ,   FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE, nil, OPEN_ALWAYS, 0, 0);
 if h = INVALID_HANDLE_VALUE then exit;
 if GetFileTime(h, nil, nil, @wt) then
    result := _filetime_convert(wt);

 CloseHandle(h);
end;


end.
