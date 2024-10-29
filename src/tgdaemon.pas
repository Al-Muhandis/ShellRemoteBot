(*
  Tg framework, FCL HTTP Daemon Broker

  Copyright (C) 2014 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit tgdaemon;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF MSWINDOWS}
  ServiceManager,
{$ENDIF}
  DaemonApp, Classes, SysUtils, IniFiles, tgsendertypes, tgtypes, process;

type

  { TTgShellDaemon }

  TTgShellDaemon = class(TDaemon)
  private
    FBot: TTelegramSender;
    FChatID: Int64;
    FConf: TMemIniFile;
    FProc: TProcess;
    FTerminated: Boolean;
    procedure BotReceiveUpdate({%H-}ASender: TObject; AnUpdate: TTelegramUpdateObj);
    procedure BotReceiveMessage({%H-}ASender: TObject; AMessage: TTelegramMessageObj);
    procedure BotLogMessage({%H-}ASender: TObject; EventType: TEventType; const Msg: String);
    procedure OutputStd;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Install: Boolean; override;
    function Execute: Boolean; override;
    function Stop: Boolean; override;
    function Uninstall: Boolean; override;
  end;

  { TTgShellMapper }

  TTgShellMapper = class(TDaemonMapper)
  private
  published
    constructor Create(AOwner: TComponent); override;
  end;

var
  TgShellDaemon: TTgShellDaemon;
  TgShellMapper: TTgShellMapper;

implementation

uses
  {$IFDEF MSWINDOWS}windows{$ENDIF}
  ;

var
  AppDir: String;

{ TTgShellMapper }

constructor TTgShellMapper.Create(AOwner: TComponent);
var
  DmnDef: TDaemonDef;
begin
  inherited CreateNew(AOwner);
  DmnDef:=DaemonDefs.Add as TDaemonDef;
  DmnDef.DaemonClassName:='TTgShellDaemon';
  DmnDef.DisplayName:='Telegram shell bot';
  DmnDef.Name:='TgShellDaemon';
end;

{ TTgShellDaemon }

procedure TTgShellDaemon.BotReceiveUpdate(ASender: TObject;
  AnUpdate: TTelegramUpdateObj);
begin
  Logger.Log(etDebug, 'Update received: '+AnUpdate.AsString);
end;

procedure TTgShellDaemon.BotReceiveMessage(ASender: TObject;
  AMessage: TTelegramMessageObj);
var
  ExitCode: integer;
  InputString: String;
begin
  InputString:=AMessage.Text;
  if InputString=EmptyStr then
    Exit;
  InputString+=LineEnding;
  if FProc.Running then
  begin
    OutputStd;
    FProc.Input.Write(InputString[1], Length(InputString));
    sleep(100);
    OutputStd;
  end;
end;

procedure TTgShellDaemon.BotLogMessage(ASender: TObject; EventType: TEventType;
  const Msg: String);
begin
  Logger.Log(EventType, Msg);
end;

procedure TTgShellDaemon.OutputStd;
var
  CharBuffer: array [0..511] of char;
  ReadCount: integer;
  OutputString: String;
begin
  OutputString:=EmptyStr;
  // read stdout and write to our stdout
  while FProc.Output.NumBytesAvailable > 0 do
  begin
    ReadCount := Min(512, FProc.Output.NumBytesAvailable); //Read up to buffer, not more
    FProc.Output.Read(CharBuffer{%H-}, ReadCount);
    OutputString+=Copy(CharBuffer, 0, ReadCount);
    Write(StdOut, OutputString);
  end;
  if OutputString<>EmptyStr then
    FBot.sendMessage(OutputString);
  // read stderr and write to our stderr
  { while FProc.Stderr.NumBytesAvailable > 0 do
  begin
    ReadCount := Min(512, FProc.Stderr.NumBytesAvailable); //Read up to buffer, not more
    FProc.Stderr.Read(CharBuffer, ReadCount);
    FBot.sendMessage(Copy(CharBuffer, 0, ReadCount));
//        Write(StdErr, Copy(CharBuffer, 0, ReadCount));
  end; }
end;

constructor TTgShellDaemon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConf:=TMemIniFile.Create(AppDir+'telegram.ini');
  TelegramAPI_URL:=FConf.ReadString('API', 'Endpoint', TelegramAPI_URL); // For Russian specific case
  FBot:=TTelegramSender.Create(FConf.ReadString('Bot', 'Token', EmptyStr));
  if FBot.Token=EmptyStr then
  begin
    Logger.Error('Please, specify bot token in telegram.ini!');
    Exit;
  end;
  FChatID:=FConf.ReadInt64('Chat', 'ID', 0);
  if FChatID=0 then
  begin
    Logger.Error('Please, specify chat ID in telegram.ini! See readme.md');
    Exit;
  end;
  FBot.OnReceiveUpdate:=@BotReceiveUpdate;
  FBot.OnReceiveMessage:=@BotReceiveMessage;
  FBot.OnLogMessage:=@BotLogMessage;
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);{$ENDIF}
  FProc:=TProcess.Create(nil);
  FProc.Options := [poUsePipes, poStderrToOutPut];
  FProc.Executable:={$IFDEF MSWINDOWS}'cmd'{$ELSE}'sh'{$ENDIF};
  FProc.Execute;
  sleep(100);
  OutputStd;
  FTerminated:=False;
end;

destructor TTgShellDaemon.Destroy;
begin
  FreeAndNil(FProc);
  FreeAndNil(FConf);
  FreeAndNil(FBot);
  inherited Destroy;
end;

function TTgShellDaemon.Install: Boolean;
{$IFDEF MSWINDOWS}
var
  VSM: TServiceManager;
{$ENDIF}
begin
  Result := inherited Install;
{$IFDEF MSWINDOWS}
  VSM := TServiceManager.Create(nil);
  try
    VSM.Connect;
    if VSM.Connected then
      VSM.StartService('TelegramShellBot', nil);
    VSM.Disconnect;
  finally
    VSM.Free;
  end;
{$ENDIF}
  WriteLn('Service installed.');
  WriteLn('Some help from terminal');
end;

function TTgShellDaemon.Uninstall: Boolean;
{$IFDEF MSWINDOWS}
var
  VSM: TServiceManager;
{$ENDIF}
begin
  Result := inherited Uninstall;
{$IFDEF MSWINDOWS}
  VSM := TServiceManager.Create(nil);
  try
    VSM.Connect;
    if VSM.Connected then
      VSM.StopService('TelegramShellBot', True);
    VSM.Disconnect;
  finally
    VSM.Free;
  end;
{$ENDIF}
  WriteLn('Service uninstalled.');
end;

function TTgShellDaemon.Execute: Boolean;
begin
  while not FTerminated do
    FBot.getUpdatesEx(0, 10);
  Result:=True;
end;

function TTgShellDaemon.Stop: Boolean;
begin
  Result:=inherited Stop;
  FTerminated:=True;
end;

initialization
  AppDir:=IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)));
  RegisterDaemonClass(TTgShellDaemon);
  RegisterDaemonMapper(TTgShellMapper);

end.
