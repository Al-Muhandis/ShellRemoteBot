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
    function GetUsers(UserID: Int64): TUserStatus;
    function GetUserList: TStrings;
  public
    constructor Create(const AConfFile: String);
    destructor Destroy; override;
    property APIEndPoint: String read GetAPIEndPoint;
    property BotTooken: String read GetBotTooken;
    property Users[UserID: Int64]: TUserStatus read GetUsers;
    property APITimeout: Integer read GetAPITimeout; // longpolling timeout
  end;

var
  Cnfg: TConfig;

implementation

uses
  tgsendertypes;

var
  CnfDir: String;

{ TConfig }

function TConfig.GetAPIEndPoint: String;
begin
  Result:=FIni.ReadString('API', 'Endpoint', TelegramAPI_URL);
end;

function TConfig.GetAPITimeout: Integer;
begin
  Result:=FIni.ReadInteger('API', 'Timeout', 50);   // 50 sec for longpolling request ??
end;

function TConfig.GetBotTooken: String;
begin
  Result:=FIni.ReadString('Bot', 'Token', EmptyStr)
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
  Cnfg:=TConfig.Create(CnfDir+'telegram.ini');

finalization
  FreeAndNil(Cnfg);

end.

