unit configuration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  TUserStatus = (usAdmin, usSimple, usBanned); // in future it can increase enums

  { TConfig }

  TConfig = class
  private
    FIni: TMemIniFile;
    FUsers: TStringList;
    function GetAPIEndPoint: String;
    function GetAPITimeout: Integer;
    function GetBotTooken: String;
    function GetDefaultDir: String;
    function GetHTTPProxyHost: String;
    function GetHTTPProxyPort: Word;
    function GetHTTPProxyPswd: String;
    function GetHTTPProxyUser: String;
    function GetScriptsDirectory: String;
    function GetUsers(UserID: Int64): TUserStatus;
    function GetUserList: TStrings;
  public
    constructor Create(const AConfFile: String);
    destructor Destroy; override;
    property APIEndPoint: String read GetAPIEndPoint;
    property BotTooken: String read GetBotTooken;
    property Users[UserID: Int64]: TUserStatus read GetUsers;
    property APITimeout: Integer read GetAPITimeout; // longpolling timeout
    property HTTPProxyHost: String read GetHTTPProxyHost;
    property HTTPProxyPort: Word read GetHTTPProxyPort;
    property HTTPProxyUser: String read GetHTTPProxyUser;
    property HTTPProxyPswd: String read GetHTTPProxyPswd;
    property ScriptsDirectory: String read GetScriptsDirectory;
    property DefaultDir: String read GetDefaultDir;
  end;

var
  Cnfg: TConfig;

implementation

uses
  tgsendertypes
  ;

var
  CnfDir: String;

{ TConfig }

function TConfig.GetAPIEndPoint: String;
begin
  Result:=FIni.ReadString('API', 'Endpoint', TelegramAPI_URL);
end;

function TConfig.GetAPITimeout: Integer;
begin
  Result:=FIni.ReadInteger('API', 'Timeout', 20);   // 20 sec for longpolling request ??
end;

function TConfig.GetBotTooken: String;
begin
  Result:=FIni.ReadString('API', 'Token', EmptyStr)
end;

function TConfig.GetDefaultDir: String;
begin
  Result:=FIni.ReadString('File', 'DefaultDir', PathDelim);
end;

function TConfig.GetHTTPProxyHost: String;
begin
  Result:=FIni.ReadString('Proxy', 'Host', EmptyStr);
end;

function TConfig.GetHTTPProxyPort: Word;
begin
  Result:=FIni.ReadInteger('Proxy', 'Port', 3128);
end;

function TConfig.GetHTTPProxyPswd: String;
begin
  Result:=FIni.ReadString('Proxy', 'Password', EmptyStr);
end;

function TConfig.GetHTTPProxyUser: String;
begin
  Result:=FIni.ReadString('Proxy', 'Username', EmptyStr);
end;

function TConfig.GetScriptsDirectory: String;
begin
  Result:=FIni.ReadString('Scripts', 'Directory', CnfDir);
end;

function TConfig.GetUsers(UserID: Int64): TUserStatus;
var
  s: String;
begin
  s:=GetUserList.Values[IntToStr(UserID)];
  if s=EmptyStr then
    Exit(usSimple);
  case s[1] of
    'a': Result:=usAdmin;
    'b': Result:=usBanned;
    's': Result:=usSimple;
  else
    Result:=usSimple;
  end;
end;

function TConfig.GetUserList: TStrings;
begin
  if not Assigned(FUsers) then
  begin
    FUsers:=TStringList.Create;
    FIni.ReadSectionRaw('users', FUsers);
    FUsers.Sorted:=True;
  end;
  Result:=FUsers;
end;

constructor TConfig.Create(const AConfFile: String);
begin
  FIni:=TMemIniFile.Create(AConfFile);
end;

destructor TConfig.Destroy;
begin
  FUsers.Free;
  FIni.Free;
  inherited Destroy;
end;

initialization
  CnfDir:=IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));
  Cnfg:=TConfig.Create(CnfDir+ChangeFileExt(ExtractFileName(ParamStr(0)), '.ini'));

finalization
  FreeAndNil(Cnfg);

end.

