unit shellthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tgtypes, tgsendertypes, process, eventlog, tgshbot
  {$IFDEF MSWINDOWS}, Windows{$ENDIF}
  ;

type

  { TShellThread }

  TShellThread=class(TThread)
  private
    FLogger: TEventLog;
    FBot: TTgShBot;
    FProc: TProcess;
    FTerminated: Boolean;
    FLPTimeout: Integer;
    procedure BotReceiveMessage({%H-}ASender: TObject; AMessage: TTelegramMessageObj);
    { Read output from shell terminal by command }
    procedure BotReceiveReadCommand({%H-}ASender: TObject; const {%H-}ACommand: String;
      {%H-}AMessage: TTelegramMessageObj);
    function CheckIsAdmin: Boolean;
    function GetLogger: TEventLog;
    procedure OutputStd(const NoOutput: String = '');
    procedure SetLogger(AValue: TEventLog);
    property Logger: TEventLog read GetLogger write SetLogger;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

uses
  configuration
  ;

{ TShellThread }

procedure TShellThread.SetLogger(AValue: TEventLog);
begin
  if FLogger=AValue then Exit;
  FLogger.Free;
  FLogger:=AValue;
end;

function TShellThread.GetLogger: TEventLog;
begin
  if not Assigned(FLogger) then
  begin
    FLogger:=TEventLog.Create(nil);
    FLogger.Identification:='Shell thread';
  end;
  Result:=FLogger;
end;

procedure TShellThread.BotReceiveMessage(ASender: TObject;
  AMessage: TTelegramMessageObj);
var
  InputString: String;
begin
  if not CheckIsAdmin then
    Exit;
  InputString:=AMessage.Text;
  if InputString=EmptyStr then
    Exit;
  InputString+=LineEnding;
  if FProc.Running then
  begin
    OutputStd;
    FProc.Input.Write(InputString[1], Length(InputString));
    OutputStd;
  end;
end;

procedure TShellThread.BotReceiveReadCommand(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
begin
  FBot.UpdateProcessed:=True;  // There is no point in further processing
  if not CheckIsAdmin then
    Exit;
  OutputStd('No new messages in the terminal');
end;

function TShellThread.CheckIsAdmin: Boolean;
begin
  Result:=not FBot.CurrentIsSimpleUser;
  if not Result then
    FBot.sendMessage('You cannot access to this bot!')
  else
    Result:=True;
end;

procedure TShellThread.OutputStd(const NoOutput: String);
var
  CharBuffer: array [0..511] of char;
  ReadCount: integer;
  OutputString: String;
const
  SleepForOut = 100;  // Pause to wait for output
begin
  Sleep(SleepForOut);
  OutputString:=EmptyStr;
  while FProc.Output.NumBytesAvailable > 0 do
  begin
    ReadCount := FProc.Output.NumBytesAvailable;
    if ReadCount>Length(CharBuffer) then
      ReadCount:=Length(CharBuffer);
    FProc.Output.Read(CharBuffer{%H-}, ReadCount);
    OutputString+=Copy(CharBuffer, 0, ReadCount);
//    Write(StdOut, OutputString); // You can uncomment for debug
  end;
  if (OutputString=EmptyStr) and (NoOutput<>EmptyStr) then
    FBot.sendMessageSafe(NoOutput)
  else
    if not FBot.sendMessageCode(OutputString) then
      Logger.Error('['+ClassName+'.OutpuStd] Cant send message to bot!');
  // read stderr and write to our stderr ... crashing :((
  { while FProc.Stderr.NumBytesAvailable > 0 do
  begin
    ReadCount := Min(512, FProc.Stderr.NumBytesAvailable);
    FProc.Stderr.Read(CharBuffer, ReadCount);
    FBot.sendMessage(Copy(CharBuffer, 0, ReadCount));
//        Write(StdErr, Copy(CharBuffer, 0, ReadCount));
  end; }
end;

constructor TShellThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate:=False;
  TelegramAPI_URL:=Cnfg.APIEndPoint; // For Russian specific case
  FBot:=TTgShBot.Create(Cnfg.BotTooken);
  FBot.Logger:=Logger;
  if FBot.Token=EmptyStr then
  begin
    Logger.Error('Please, specify bot token in telegram.ini!');
    Exit;
  end;
//  FBot.LogDebug:=True;
  FBot.OnReceiveMessage:=@BotReceiveMessage;
  { Read output of shell terminal called by user via /read command }
  FBot.CommandHandlers['/read']:=@BotReceiveReadCommand;
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);{$ENDIF}
  FProc:=TProcess.Create(nil);
  FProc.Options := [poUsePipes, poStderrToOutPut];
  FProc.Executable:={$IFDEF MSWINDOWS}'cmd'{$ELSE}'sh'{$ENDIF};
  FProc.Execute;
  FTerminated:=False;
  FLPTimeout:=Cnfg.APITimeout;
end;

destructor TShellThread.Destroy;
begin
  FreeAndNil(FProc);
  FreeAndNil(FBot);
  inherited Destroy;
end;

procedure TShellThread.Execute;
begin
  OutputStd;
  while not Terminated do
    FBot.getUpdatesEx(0, FLPTimeout);
end;

end.

