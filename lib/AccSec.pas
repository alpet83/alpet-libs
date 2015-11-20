unit AccSec; { Account security tools }

interface
uses Windows, SysUtils, Classes, Misc, WinApi.AclAPI, WinAPI.AccCtrl;


type
    TExplicitAccess = object
     EA: EXPLICIT_ACCESS;

     procedure  Init (perm: DWORD; amode: ACCESS_MODE = SET_ACCESS; inherit: DWORD = NO_INHERITANCE);
     procedure  SetTrustee ( pName: PChar; AForm: TRUSTEE_FORM = TRUSTEE_IS_SID; AType: TRUSTEE_TYPE = TRUSTEE_IS_WELL_KNOWN_GROUP );
    end;

    PExplicitAccess = ^TExplicitAccess;


    TSecIdentifier = class
    private
     FAuth: SID_IDENTIFIER_AUTHORITY;
      FSID: PSID;
    public
     { props }

     property     SID: PSID read FSID;

     { C & D }
     constructor   Create (AInit: BYTE);
     destructor    Destroy; override;

     { methods }

     function      Allocate ( const SubAuth: array of DWORD ): Boolean;

    end;

    TACLObject = class
    private
     FEAList: array of TExplicitAccess;
     FIDList: array of TSecIdentifier;
        FACL: PACL;

     function GetEA(index: Integer): PExplicitAccess; inline;


    public

     property    ACL: PACL read FACL;
     property    EAList[index: Integer]: PExplicitAccess read GetEA;

     { C & D }
     constructor  Create ( ACount: Integer );
     destructor   Destroy; override;

     { methods }

     function    InitSID ( Index: Integer; AInit: BYTE; const SubAuth: array of DWORD ): Boolean;

     function    SetEntries: Boolean;
     procedure   SetTrustee ( Index: Integer; AForm: TRUSTEE_FORM = TRUSTEE_IS_SID; AType: TRUSTEE_TYPE = TRUSTEE_IS_WELL_KNOWN_GROUP );

    end;


    TSecDescriptor = class
    protected
     FSD: PSecurityDescriptor;
     FSA: TSecurityAttributes;

    public
     { prop }

     property           Descriptor: PSecurityDescriptor read FSD;

     { C &  D }
     constructor        Create;
     destructor         Destroy; override;

     { methods }


     function           GetSA: PSecurityAttributes;
     function           SetDAcl ( acl: PACL ): Boolean;

    end;



implementation

{ TExplicitAccess }

procedure TExplicitAccess.Init(perm: DWORD; amode: ACCESS_MODE; inherit: DWORD);
begin
 FillChar (EA, sizeof(EA), 0);
 EA.grfAccessPermissions := perm;
 EA.grfAccessMode := amode;
 EA.grfInheritance := inherit;
end;

procedure TExplicitAccess.SetTrustee(pName: PChar; AForm: TRUSTEE_FORM; AType: TRUSTEE_TYPE);
begin
 EA.Trustee.ptstrName := pName;
 EA.Trustee.TrusteeForm := AForm;
 EA.Trustee.TrusteeType := AType;
end;

{ TSecDescriptor }

constructor TSecDescriptor.Create;
begin
 FSD := Ptr ( LocalAlloc(LPTR, SECURITY_DESCRIPTOR_MIN_LENGTH) );
 if not InitializeSecurityDescriptor(FSD, SECURITY_DESCRIPTOR_REVISION) then
           RaiseLastError ('InitializeSecurityDescriptor failed ');
end;

destructor TSecDescriptor.Destroy;
begin
 if FSD <> nil then
    LocalFree ( NativeUInt (FSD) );
 inherited;
end;

function TSecDescriptor.GetSA: PSecurityAttributes;
begin
 FSA.nLength := sizeof (FSA);
 FSA.lpSecurityDescriptor := FSD;
 FSA.bInheritHandle := FALSE;
 result := @FSA;
end;

function TSecDescriptor.SetDAcl(acl: PACL): Boolean;
begin
 result := SetSecurityDescriptorDacl ( FSD, TRUE, acl, FALSE )
end;

{ TACLObject }

constructor TACLObject.Create( ACount: Integer);
begin
 SetLength ( FEAList, ACount );
 SetLength ( FIDList, ACount );
end;

destructor TACLObject.Destroy;
begin
 if ACL <> nil then LocalFree ( NativeUInt (ACL) );

 FreeObjectList ( @FIDList[0], Length (FIDList) );
 SetLength (FEAlist, 0);
 SetLength (FIDList, 0);
 inherited;
end;

function TACLObject.GetEA(index: Integer): PExplicitAccess;
begin
 result := @FEAList [index];
end;

function  TACLObject.InitSID(Index: Integer; AInit: BYTE; const SubAuth: array of DWORD): Boolean;
begin
 FIDList [Index] := TSecIdentifier.Create ( AInit );
 result := FIDList [Index].Allocate ( SubAuth );
end;

function TACLObject.SetEntries: Boolean;
begin
 FACL := nil;
 result := ( ERROR_SUCCESS <> SetEntriesInAcl ( Length (FEAList), @FEAList [0].EA, nil, FACL ) );
end;

procedure TACLObject.SetTrustee(Index: Integer; AForm: TRUSTEE_FORM; AType: TRUSTEE_TYPE);
var
   SID: PSID;
begin
 SID := FIDList [index].SID;
 Assert ( Assigned(SID), 'SID = nil' );
 EAList [index].SetTrustee ( SID, AForm, AType );
end;

{ TSecIdentifier }

function TSecIdentifier.Allocate(const SubAuth: array of DWORD): Boolean;
var
   sa: array [0..7] of DWORD;
    n: Integer;
begin
 FillChar ( sa, sizeof(sa), 0 ); // Zinit
 for n := 0 to Length (SubAuth) do
     sa [n] := SubAuth [n]; // copy


 result := AllocateAndInitializeSid ( @FAuth, Length (SubAuth), sa [0], sa [1], sa [2], sa [3], sa [4], sa [5], sa [6], sa [7], FSID );
end;

constructor TSecIdentifier.Create(AInit: BYTE);
begin
 FillChar ( FAuth, sizeof (FAuth), 0 );
 FAuth.Value [5] := AInit;
end;

destructor TSecIdentifier.Destroy;
begin
 if FSID <> nil then
    FreeSID ( FSID );
 inherited;
end;

end.
