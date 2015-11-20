function VirtualAlloc(lpvAddress: Pointer; dwSize, flAllocationType, flProtect: Cardinal): Pointer; stdcall;
begin
 result := BlockAlloc(lpvAddress, dwSize, flAllocationType);
 if result = nil then
    Result := Windows.VirtualAlloc(lpvAddress, dwSize, flAllocationType, flProtect);
end;

function VirtualFree(lpAddress: Pointer; dwSize, dwFreeType: Cardinal): LongBool; stdcall;
begin
 result := BlockFree (lpAddress, dwSize);
 if result then
    Result := Windows.VirtualFree (lpAddress, dwSize, dwFreeType);
end;

function VirtualQuery(lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: DWORD): DWORD; stdcall;
begin
 result := BlockInfo (lpAddress, lpBuffer, dwLength);
 if result = 0 then
    result := Windows.VirtualQuery (lpAddress, lpBuffer, dwLength);
end;


