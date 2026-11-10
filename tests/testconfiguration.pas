unit testconfiguration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, configuration,
  LazFileUtils
  ;

type

  { TTestConfig }

  TTestConfig = class(TTestCase)
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
    procedure TestSilentForNonAdminsDefault;
    procedure TestSilentForNonAdminsEnabled;
    procedure TestSilentForNonAdminsNumericOne;
    procedure TestRichMessagesDefault;
    procedure TestRichMessagesEnabled;
    procedure TestRichMessagesOffSpelling;
  end;

implementation

function TTestConfig.WriteIni(const AContent: String): String;
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

procedure TTestConfig.DoTest(const AUsersSection: String; AUserID: Int64;
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

procedure TTestConfig.TestAdminStatus;
begin
  DoTest('[users]'+LineEnding+'111=a'+LineEnding, 111, usAdmin);
end;

procedure TTestConfig.TestBannedStatus;
begin
  DoTest('[users]'+LineEnding+'222=b'+LineEnding, 222, usBanned);
end;

procedure TTestConfig.TestSimpleStatus;
begin
  DoTest('[users]'+LineEnding+'333=s'+LineEnding, 333, usSimple);
end;

procedure TTestConfig.TestUnknownStatusIsSimple;
begin
  // an unrecognized status character must never grant admin rights
  DoTest('[users]'+LineEnding+'444=x'+LineEnding, 444, usSimple);
end;

procedure TTestConfig.TestMissingUserIsSimple;
begin
  // users absent from the list are simple, not admins
  DoTest('[users]'+LineEnding+'111=a'+LineEnding, 999, usSimple);
end;

procedure TTestConfig.TestOnlyFirstCharacterCounts;
begin
  // GetUsers decides by the first character of the value only
  DoTest('[users]'+LineEnding+'555=abc'+LineEnding, 555, usAdmin);
end;

procedure TTestConfig.TestSilentForNonAdminsDefault;
var
  Cfg: TConfig;
  Path: String;
begin
  // absent option keeps the historical behavior: answer non-admin users
  Path:=WriteIni('[users]'+LineEnding+'111=a'+LineEnding);
  try
    Cfg:=TConfig.Create(Path);
    try
      AssertFalse(Cfg.SilentForNonAdmins);
    finally
      Cfg.Free;
    end;
  finally
    DeleteFileUTF8(Path);
  end;
end;

procedure TTestConfig.TestSilentForNonAdminsEnabled;
var
  Cfg: TConfig;
  Path: String;
begin
  Path:=WriteIni('[users]'+LineEnding+'SilentForNonAdmins=true'+LineEnding);
  try
    Cfg:=TConfig.Create(Path);
    try
      AssertTrue(Cfg.SilentForNonAdmins);
    finally
      Cfg.Free;
    end;
  finally
    DeleteFileUTF8(Path);
  end;
end;

procedure TTestConfig.TestSilentForNonAdminsNumericOne;
var
  Cfg: TConfig;
  Path: String;
begin
  // '1' is the spelling FPC ReadBool itself accepts
  Path:=WriteIni('[users]'+LineEnding+'SilentForNonAdmins=1'+LineEnding);
  try
    Cfg:=TConfig.Create(Path);
    try
      AssertTrue(Cfg.SilentForNonAdmins);
    finally
      Cfg.Free;
    end;
  finally
    DeleteFileUTF8(Path);
  end;
end;

procedure TTestConfig.TestRichMessagesDefault;
var
  Cfg: TConfig;
  Path: String;
begin
  // absent option keeps the classic listings: rich messages are opt-in
  // because fresh Telegram clients only render them correctly
  Path:=WriteIni('[API]'+LineEnding+'Token=x'+LineEnding);
  try
    Cfg:=TConfig.Create(Path);
    try
      AssertFalse(Cfg.RichMessages);
    finally
      Cfg.Free;
    end;
  finally
    DeleteFileUTF8(Path);
  end;
end;

procedure TTestConfig.TestRichMessagesEnabled;
var
  Cfg: TConfig;
  Path: String;
begin
  Path:=WriteIni('[API]'+LineEnding+'RichMessages=true'+LineEnding);
  try
    Cfg:=TConfig.Create(Path);
    try
      AssertTrue(Cfg.RichMessages);
    finally
      Cfg.Free;
    end;
  finally
    DeleteFileUTF8(Path);
  end;
end;

procedure TTestConfig.TestRichMessagesOffSpelling;
var
  Cfg: TConfig;
  Path: String;
begin
  // 'false' and any unrecognized spelling keep the flag off
  Path:=WriteIni('[API]'+LineEnding+'RichMessages=maybe'+LineEnding);
  try
    Cfg:=TConfig.Create(Path);
    try
      AssertFalse(Cfg.RichMessages);
    finally
      Cfg.Free;
    end;
  finally
    DeleteFileUTF8(Path);
  end;
end;

initialization
  RegisterTest(TTestConfig);

end.
