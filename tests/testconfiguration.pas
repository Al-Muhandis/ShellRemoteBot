unit testconfiguration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, configuration,
  LazFileUtils
  ;

type

  { TTestConfigUsers }

  TTestConfigUsers = class(TTestCase)
  private
    { Writes AContent into a fresh ini file and returns its full path }
    function WriteIni(const AContent: String): String;
    procedure DoTest(const AUsersSection: String; AUserID: Int64;
      AExpected: TUserStatus);
  published
    procedure TestAdminStatus;
    procedure TestBannedStatus;
    procedure TestSimpleStatus;
    procedure TestUnknownStatusIsSimple;
    procedure TestMissingUserIsSimple;
    procedure TestOnlyFirstCharacterCounts;
  end;

implementation

function TTestConfigUsers.WriteIni(const AContent: String): String;
var
  L: TStringList;
begin
  // user config dir of the current user, isolated in its own subfolder
  Result:=IncludeTrailingPathDelimiter(GetAppConfigDirUTF8(False, True))+
    'tgshdtests'+PathDelim+'users.ini';
  ForceDirectoriesUTF8(ExtractFileDir(Result));
  L:=TStringList.Create;
  try
    L.Text:=AContent;
    L.SaveToFile(Result);
  finally
    L.Free;
  end;
end;

procedure TTestConfigUsers.DoTest(const AUsersSection: String; AUserID: Int64;
  AExpected: TUserStatus);
var
  Cfg: TConfig;
  Path: String;
begin
  Path:=WriteIni(AUsersSection);
  try
    Cfg:=TConfig.Create(Path);
    try
      AssertTrue('Users['+IntToStr(AUserID)+'] status',
        Cfg.Users[AUserID]=AExpected);
    finally
      Cfg.Free;
    end;
  finally
    DeleteFileUTF8(Path);
  end;
end;

procedure TTestConfigUsers.TestAdminStatus;
begin
  DoTest('[users]'+LineEnding+'111=a'+LineEnding, 111, usAdmin);
end;

procedure TTestConfigUsers.TestBannedStatus;
begin
  DoTest('[users]'+LineEnding+'222=b'+LineEnding, 222, usBanned);
end;

procedure TTestConfigUsers.TestSimpleStatus;
begin
  DoTest('[users]'+LineEnding+'333=s'+LineEnding, 333, usSimple);
end;

procedure TTestConfigUsers.TestUnknownStatusIsSimple;
begin
  // an unrecognized status character must never grant admin rights
  DoTest('[users]'+LineEnding+'444=x'+LineEnding, 444, usSimple);
end;

procedure TTestConfigUsers.TestMissingUserIsSimple;
begin
  // users absent from the list are simple, not admins
  DoTest('[users]'+LineEnding+'111=a'+LineEnding, 999, usSimple);
end;

procedure TTestConfigUsers.TestOnlyFirstCharacterCounts;
begin
  // GetUsers decides by the first character of the value only
  DoTest('[users]'+LineEnding+'555=abc'+LineEnding, 555, usAdmin);
end;

initialization
  RegisterTest(TTestConfigUsers);

end.
