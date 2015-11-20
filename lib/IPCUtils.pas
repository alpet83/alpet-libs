unit IPCUtils;

interface
uses Windows, SysUtils, Classes, Misc, WinAPI.AclAPI, WinAPI.AccCtrl, AccSec, Math;

const
    DEF_MAP_PROTECT = PAGE_READWRITE or SEC_COMMIT;


type
    TFileMapping = object
    private

    public
     hMapping: THandle;
        hFile: THandle;
     fProtect: DWORD;
     dwAccess: DWORD;     // desired access
        pView: Pointer;
        fSize: LARGE_INTEGER;
        nSize: DWORD;
       szFile: array [0..MAX_PATH - 1] of CHAR;
       szName: array [0..31] of CHAR;
     last_err: String;



     procedure          Close (bCloseFile: Boolean = FALSE);

     function           Create( const AName: String; OpenAccess: Boolean ): Boolean; overload;
     function           Create ( fHandle: THandle; const AName: String; OpenAccess: Boolean ): Boolean; overload;
     function           Create ( const AFileName, AName: String; OpenAccess: Boolean ): Boolean; overload;


     procedure          Init ( ASize: DWORD; AProtect: DWORD = DEF_MAP_PROTECT ); // constructor replace
     function           MapView (offset: UInt64 = 0): Pointer;


     function           Open ( const AName: String ): Boolean;
     procedure          UnmapView;

    end; // TFileMapping

    PFileMapping = ^TFileMapping;


implementation

{ TFileMapping }

procedure TFileMapping.Close;
begin
 if @self = nil then exit;
 UnmapView;
 if hMapping <> 0 then CloseHandle (hMapping);
 hMapping := 0;
 last_err := '';
 if bCloseFile and (hFile <> INVALID_HANDLE_VALUE) and (hFile <> 0) then
   begin
    CloseHandle (hFile);
    hFile := INVALID_HANDLE_VALUE;
   end;
end;

function TFileMapping.Create (const AName: String; OpenAccess: Boolean): Boolean;
begin
 result := Create ( INVALID_HANDLE_VALUE, AName, OpenAccess );
end;


function TFileMapping.Create(fHandle: THandle; const AName: String; OpenAccess: Boolean): Boolean;
const

     SECURITY_WORLD_RID          = 0;
     SECURITY_CREATOR_OWNER_RID  = 0;
var
    pName: PChar;
      acl: TACLObject;
      pSA: PSecurityAttributes;
       SD: TSecDescriptor;

begin
 ASSERT (fProtect <> 0, 'fProcect field not initialized');
 // ODS('[~T]. #DBG: Creating named FileMapping ' + AName );
 hFile := fHandle;
 fSize.LowPart := GetFileSize ( hFile, @fSize.HighPart );

 SetStrWZ ( szName, AName, 31 );
 if OpenAccess then
  begin
   acl := TACLObject.Create (2);
   if not acl.InitSID (0, 1, [SECURITY_WORLD_RID]) then // world authority aka AnyOne
          RaiseLastError ('Cannont allocate Anyone SID');
   if not acl.InitSID (1, 3, [SECURITY_CREATOR_OWNER_RID] ) then
          RaiseLastError ('Cannont allocate Owner SID'); // creator authority
   acl.EAList[0].Init ( FILE_MAP_READ or FILE_MAP_WRITE );
   acl.EAList[1].Init ( FILE_MAP_ALL_ACCESS );
   acl.SetTrustee (0);
   acl.SetTrustee (1);
   if acl.SetEntries then
          RaiseLastError ('SetEntriesInAcl failed');
   SD := TSecDescriptor.Create;
   if not SD.SetDacl (acl.ACL) then
          RaiseLastError ('SetSecurityDescriptorDacl failed ');
   pSA := SD.GetSA;
  end
 else
  begin
   acl := nil;
   pSA := nil;
   SD := nil;
  end;

 pName := nil;
 if AName <> '' then
    pName := PChar (AName);

 if ( fHandle <> INVALID_HANDLE_VALUE ) and ( nSize = 0 ) then
      nSize := GetFileSize ( fHandle, nil );

 last_err := '';

 if ( fHandle = INVALID_HANDLE_VALUE ) and ( pName = nil ) then
      Raise EArgumentException.Create('Must be set file handle and/or name for mapping object');


 hMapping := CreateFileMapping ( fHandle, pSA, fProtect, 0, nSize, pName );

 result := ( hMapping <> 0 );
 if not result then
    begin
     last_err := err2str;
     PrintError('CreateFileMapping failed ' + last_err);
    end;


 acl.Free;
 SD.Free;
end;


function TFileMapping.Create(const AFileName, AName: String; OpenAccess: Boolean): Boolean;

begin
 if not FileExists (AFileName) then
   begin
    result := FALSE;
    exit;
   end;

 hFile := CreateFile ( PChar (AFileName), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ, nil, OPEN_ALWAYS, 0, 0 );

 result := Create ( hFile, AName, OpenAccess );
end;

procedure TFileMapping.Init;
begin
 nSize := ASize;
 fProtect := AProtect;
 dwAccess := FILE_MAP_READ or FILE_MAP_WRITE;
end;

function TFileMapping.MapView ( offset: UInt64 ): Pointer;
var
   ofs: LARGE_INTEGER;
begin
 ofs.QuadPart := offset;
 nSize := Min (nSize, fSize.QuadPart - ofs.QuadPart );

 result := MapViewOfFile ( hMapping, dwAccess, ofs.HighPart, ofs.LowPart, nSize );
 pView := result;
end;

function TFileMapping.Open(const AName: String): Boolean;
begin
 SetStrWZ ( szName, AName, 31 );
 ODS('[~T]. #DBG: Opening named FileMapping ' + AName );
 hMapping := OpenFileMapping ( dwAccess, FALSE, PChar (AName) );
 result := ( hMapping <> 0 );
 // if not result then PrintError('OpenFileMapping failed ' + err2str);
end;

procedure TFileMapping.UnmapView;
begin
 if (@self = nil) or (pView = nil) then exit;
 UnmapViewOfFile ( pView );
 pView := nil;
end;

end.
