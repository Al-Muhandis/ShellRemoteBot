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
    function GetServiceUser: Int64;
    function GetRichMessages: Boolean;
    function GetSilentForNonAdmins: Boolean;
    function GetUsers(UserID: Int64): TUserStatus;
    function GetUserList: TStrings;
  public
    constructor Create(const AConfFile: String);
    destructor Destroy; override;
    property ServiceUser: Int64 read GetServiceUser;
    property APIEndPoint: String read GetAPIEndPoint;
    property BotTooken: String read GetBotTooken;
    property Users[UserID: Int64]: TUserStatus read GetUsers;
    { true: non-admin users are not answered at all, so the bot does not
      reveal its existence (banned users are always ignored) }
    property SilentForNonAdmins: Boolean read GetSilentForNonAdmins;
    property APITimeout: Integer read GetAPITimeout; // longpolling timeout
    property HTTPProxyHost: String read GetHTTPProxyHost;
    property HTTPProxyPort: Word read GetHTTPProxyPort;
    property HTTPProxyUser: String read GetHTTPProxyUser;
    property HTTPProxyPswd: String read GetHTTPProxyPswd;
    property ScriptsDirectory: String read GetScriptsDirectory;
    property DefaultDir: String read GetDefaultDir;
    { true: the /dir listing is sent as a rich message (Bot API 10.1+),
      falling back to the classic sendMessage on failure. Rich messages
      render properly only on fresh Telegram clients, so this is opt-in }
    property RichMessages: Boolean read GetRichMessages;
  end;

var
  Cnfg: TConfig;

implementation

uses
  tgsendertypes
  ;

var
  CnfDir: String;

{ FPC ReadBool understands '1' only (CharToBool), so parse the common
  spellings explicitly; anything else keeps ADefault }
function ReadFlexibleBool(AIni: TMemIniFile; const ASection, AKey: String;
  ADefault: Boolean): Boolean;
var
  s: String;
begin
  s:=Lowercase(AIni.ReadString(ASection, AKey, EmptyStr));
  if s=EmptyStr then
    Result:=ADefault
  else
    Result:=(s='1') or (s='true') or (s='yes') or (s='on');
end;

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
  Result:=IncludeTrailingPathDelimiter(FIni.ReadString('Scripts', 'Directory', CnfDir));
end;

function TConfig.GetServiceUser: Int64;
begin
  Result:=FIni.ReadInt64('Service', 'User', 0);
end;

function TConfig.GetRichMessages: Boolean;
begin
  // opt-in: rich messages render properly only on fresh Telegram clients
  Result:=ReadFlexibleBool(FIni, 'API', 'RichMessages', False);
end;

function TConfig.GetSilentForNonAdmins: Boolean;
begin
  // absent (or unrecognized) option keeps bots answering non-admins
  Result:=ReadFlexibleBool(FIni, 'Users', 'SilentForNonAdmins', False);
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

initialization{$ifdef portable}
  CnfDir:=IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));
  Cnfg:=TConfig.Create(CnfDir+ChangeFileExt(ExtractFileName(ParamStr(0)), '.ini'));{$else}
  CnfDir:=GetAppConfigDir(True);
  Cnfg:=TConfig.Create(CnfDir+ChangeFileExt(ExtractFileName(ParamStr(0)), '.ini'));{$endif}

finalization
  FreeAndNil(Cnfg);

end.

