unit msvcrt80;

interface
uses Windows;

type
    SIZE_T = DWORD;



function malloc(size: SIZE_T): Pointer; stdcall;
function realloc(p: Pointer; size: SIZE_T): Pointer; stdcall;
function free(p: Pointer): Pointer; stdcall;


implementation
const
   LibName = 'msvcr80.dll';


function malloc(size: SIZE_T): Pointer; stdcall; external LibName;
function realloc(p: Pointer; size: SIZE_T): Pointer; stdcall; external LibName;
function free(p: Pointer): Pointer; stdcall; external LibName;


end.
