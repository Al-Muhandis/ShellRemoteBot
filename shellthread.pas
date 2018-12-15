unit shellthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tgtypes, tgsendertypes, process, eventlog
  {$IFDEF MSWINDOWS}, Windows{$ENDIF}
  ;

type

  { TShellThread }

  TShellThread=class(TThread)
  private
    FLogger: TEventLog;
    FBot: TTelegramSender;
    FProc: TProcess;
    FTerminated: Boolean;
    FLPTimeout: Integer;
    procedure BotReceiveMessage({%H-}ASender: TObject; AMessage: TTelegramMessageObj);
    procedure BotLogMessage({%H-}ASender: TObject; EventType: TEventType; const Msg: String);
    function GetLogger: TEventLog;
    procedure OutputStd;
    procedure SetLogger(AValue: TEventLog);
    property Logger: TEventLog read GetLogger write SetLogger;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

uses
  configuration, tgshbot
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
  if FBot.CurrentIsSimpleUser then
  begin
    FBot.sendMessage('You cannot access to this bot!');
    Exit;
  end;
  InputString:=AMessage.Text;
  if InputString=EmptyStr then
    Exit;
  InputString+=LineEnding;
  if FProc.Running then
  begin
    OutputStd;
    FProc.Input.Write(InputString[1], Length(InputString));
    sleep(50);
    OutputStd;
  end;
end;

procedure TShellThread.BotLogMessage(ASender: TObject; EventType: TEventType;
  const Msg: String);
begin
  Logger.Log(EventType, Msg);
end;

procedure TShellThread.OutputStd;
var
  CharBuffer: array [0..511] of char;
  ReadCount: integer;
  OutputString, Msg: String;
const
  MaxMsgLength = 4096;   // maximum message length to send
  MsgPartLength = 3000;
begin
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
  if OutputString<>EmptyStr then
  begin
    Msg:=IsolateShellOutput(OutputString);
    if Length(Msg)<MaxMsgLength then
      FBot.sendMessage(Msg, pmMarkdown)
    else begin
      while OutputString<>EmptyStr do
      begin
        Msg:=LeftStr(OutputString, MsgPartLength);
        OutputString:=RightStr(OutputString, Length(OutputString)-Length(Msg));
        FBot.sendMessage(IsolateShellOutput(Msg), pmMarkdown);
      end;
    end;
  end;
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
  if FBot.Token=EmptyStr then
  begin
    Logger.Error('Please, specify bot token in telegram.ini!');
    Exit;
  end;
  FBot.OnReceiveMessage:=@BotReceiveMessage;
  FBot.OnLogMessage:=@BotLogMessage;
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);{$ENDIF}
  FProc:=TProcess.Create(nil);
  FProc.Options := [poUsePipes, poStderrToOutPut];
  FProc.Executable:={$IFDEF MSWINDOWS}'cmd'{$ELSE}'sh'{$ENDIF};
  FProc.Execute;
  sleep(50);
  OutputStd;
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
  while not Terminated do
    FBot.getUpdatesEx(0, FLPTimeout);
end;

end.

