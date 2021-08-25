unit shotsthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tgsendertypes, eventlog
  ;

type

  { TShotThread }

  TShotThread = class(TThread)
  private
    FLogger: TEventLog;
    FBot: TTelegramSender;
    FShotChat: Int64;
    FTerminated: Boolean;
    FTerminateEvent: pRTLEvent;   // for terminating while the thread is delayed
    function GetLogger: TEventLog;
    procedure SetLogger(AValue: TEventLog);
    property Logger: TEventLog read GetLogger write SetLogger;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    procedure TerminateThread;
  end;

implementation

uses
  configuration, screenshot
  ;

{ TShotThread }

function TShotThread.GetLogger: TEventLog;
begin
  if not Assigned(FLogger) then
  begin
    FLogger:=TEventLog.Create(nil);
    FLogger.Identification:='Shot thread';
    FLogger.LogType:=ltFile;
  end;
  Result:=FLogger;
end;

procedure TShotThread.SetLogger(AValue: TEventLog);
begin
  if FLogger=AValue then Exit;
  FLogger.Free;
  FLogger:=AValue;
end;

constructor TShotThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate:=True;
  FBot:=TTelegramSender.Create(Cnfg.BotToken);
  FBot.Logger:=Logger;
  if FBot.Token=EmptyStr then
  begin
    Logger.Error('Please, specify bot token in telegram.ini!');
    Exit;
  end;
  FBot.HTTPProxyHost:=Cnfg.HTTPProxyHost;
  FBot.HTTPProxyPort:=Cnfg.HTTPProxyPort;
  FShotChat:=Cnfg.ServiceUser;
  FTerminated:=False;

  FTerminateEvent:=RTLEventCreate;
end;

destructor TShotThread.Destroy;
begin
  RTLEventDestroy(FTerminateEvent);

  FreeAndNil(FBot);
  FreeAndNil(FLogger);
  inherited Destroy;
end;

procedure TShotThread.Execute;
var
  aStream: TMemoryStream;
begin
  aStream:=TMemoryStream.Create;
  while not Terminated do
  begin
    try
      RTLeventWaitFor(FTerminateEvent, MSecsPerSec*SecsPerMin*1);
      CreateScreenshot(aStream);
      FBot.sendPhotoStream(Cnfg.ServiceUser, 'Screenshot', aStream, 'Screenshot caption');
    except
      on E: Exception do
        Logger.Error(E.ClassName+': '+E.Message);
    end;
  end;
  aStream.Free;
end;

procedure TShotThread.TerminateThread;
begin          
  Terminate;
  RTLEventSetEvent(FTerminateEvent);
end;

end.

