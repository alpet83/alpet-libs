unit DebugTools;

interface


function IsDebuggerPresent: Boolean; stdcall;

implementation
uses misc;

function IsDebuggerPresent: Boolean; stdcall; external 'kernel32.dll';

initialization

 __nop;
end.
