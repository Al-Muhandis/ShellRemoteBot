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
    procedure BotReceiveCallbackQuery({%H-}ASender: TObject; ACallback: TCallbackQueryObj);
    procedure BotReceiveMessage({%H-}ASender: TObject; AMessage: TTelegramMessageObj);
    { Read output from shell terminal by command }
    procedure BotReceiveReadCommand({%H-}ASender: TObject; const {%H-}ACommand: String;
      {%H-}AMessage: TTelegramMessageObj);
    procedure BotReceiveScriptsCommand({%H-}ASender: TObject; const {%H-}ACommand: String;
      {%H-}AMessage: TTelegramMessageObj);
    {$IFDEF UNIX}
    procedure BotReceiveSIGCommand({%H-}ASender: TObject; const {%H-}ACommand: String;
      {%H-}AMessage: TTelegramMessageObj);
    { procedure emulate SIGINT (Ctrl+C) }
    procedure BotReceiveSIGINTCommand({%H-}ASender: TObject; const {%H-}ACommand: String;
      {%H-}AMessage: TTelegramMessageObj);
    procedure BotReceiveSIGKILLCommand({%H-}ASender: TObject; const {%H-}ACommand: String;
      {%H-}AMessage: TTelegramMessageObj);
    { procedure emulate SIGQUIT (Ctrl+\) }
    procedure BotReceiveSIGQUITCommand({%H-}ASender: TObject; const {%H-}ACommand: String;
      {%H-}AMessage: TTelegramMessageObj);
    procedure BotReceiveSIGTERMCommand({%H-}ASender: TObject; const {%H-}ACommand: String;
      {%H-}AMessage: TTelegramMessageObj);
    {$ENDIF}
    function CommandStart: Boolean;
    function CheckIsAdmin: Boolean;
    function GetLogger: TEventLog;
    procedure OutputStd(const NoOutput: String = '');
    procedure SendToShellTerminal(const InputString: String);
    {$IFDEF UNIX}procedure SendSIG(SigNumber: Byte);{$ENDIF}
    procedure SetLogger(AValue: TEventLog);
    property Logger: TEventLog read GetLogger write SetLogger;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

uses
  configuration{$IFDEF UNIX}, strutils {$ENDIF}, LazFileUtils, FileUtil
  ;

const
{$IFDEF UNIX}
  { POSIX signals }
  {%H-}SIGABRT = 6;
  {%H-}SIGALRM = 14;
  {%H-}SIGHUP  = 1;
  SIGINT  = 2;
  SIGKILL = 9;
  SIGQUIT = 3;
  SIGTERM = 15;
  {%H-}SIGTRAP = 5;
{$ENDIF}
  _ScriptFileExt='.script';


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
  if not CommandStart then
    Exit;
  InputString:=AMessage.Text;
  if InputString=EmptyStr then
    Exit;
  InputString+=LineEnding;
  SendToShellTerminal(InputString);
end;

procedure TShellThread.BotReceiveCallbackQuery(ASender: TObject; ACallback: TCallbackQueryObj);
var
  aScript: TStringList;
  aFileName: String;
const
  aScriptStart='script ';
begin
  if not CommandStart then
    Exit;
  if ACallback.Data.StartsWith(aScriptStart) then
  begin
    aScript:=TStringList.Create;
    try
      aFileName:=RightStr(ACallback.Data, Length(ACallback.Data)-Length(aScriptStart));
      aScript.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFileDir(Cnfg.ScriptsDirectory))+aFileName+_ScriptFileExt);
      SendToShellTerminal(aScript.Text);
    finally
      aScript.Free;
    end;
  end;
end;

procedure TShellThread.BotReceiveReadCommand(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
begin
  if CommandStart then
    OutputStd('No new messages in the terminal');
end;

procedure TShellThread.BotReceiveScriptsCommand(ASender: TObject; const ACommand: String; AMessage: TTelegramMessageObj
  );
var
  aScriptFiles: TStringList;
  aReplyMarkup: TReplyMarkup;
  f, aFile: String;
begin
  if not CommandStart then
    Exit;
  aScriptFiles:=TStringList.Create;
  aReplyMarkup:=TReplyMarkup.Create;
  try
    FindAllFiles(aScriptFiles, Cnfg.ScriptsDirectory, '*'+_ScriptFileExt, False);
    aReplyMarkup.InlineKeyBoard:=TInlineKeyboard.Create;
    for f in aScriptFiles do
    begin
      aFile:=LazFileUtils.ExtractFileNameOnly(f);
      aReplyMarkup.InlineKeyBoard.AddButton(aFile, 'script '+aFile, 3);
    end;
    if aScriptFiles.Count>0 then
      FBot.sendMessage('Select script to run', pmDefault, True, aReplyMarkup)
    else
      FBot.sendMessage('Empty script file list! You can add a script file to the directory "'+
        Cnfg.ScriptsDirectory+'"', pmDefault, True, aReplyMarkup)
  finally
    aReplyMarkup.Free;
    aScriptFiles.Free;
  end;
end;

{$IFDEF UNIX}
procedure TShellThread.BotReceiveSIGCommand(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
var
  i: LongInt;
begin
  if not CommandStart then
    Exit;
  if TryStrToInt(ExtractDelimited(2, AMessage.Text, [' ']), i) then
    if i<=High(Byte) then
      SendSIG(i)
    else
      FBot.sendMessageSafe('SIG number limit by 255!')
  else
    FBot.sendMessageSafe('Please, specify SIG portable number. For example:'+LineEnding+
      '```sh'+LineEnding+'/sig 3```'+LineEnding+'where `3` - is SIGQUIT', pmMarkdown);
end;
procedure TShellThread.BotReceiveSIGINTCommand(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
begin
  if not CommandStart then
    Exit;
  SendSIG(SIGINT);
end;

procedure TShellThread.BotReceiveSIGKILLCommand(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
begin
  if not CommandStart then
    Exit;
  SendSIG(SIGKILL);
end;

procedure TShellThread.BotReceiveSIGQUITCommand(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
begin
  if not CommandStart then
    Exit;
  SendSIG(SIGQUIT);
end;

procedure TShellThread.BotReceiveSIGTERMCommand(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
begin
  if not CommandStart then
    Exit;
  SendSIG(SIGTERM);
end;

{$ENDIF}

function TShellThread.CommandStart: Boolean;
begin
  FBot.UpdateProcessed:=True;  // There is no point in further processing
  Result:=CheckIsAdmin and FProc.Running;
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

procedure TShellThread.SendToShellTerminal(const InputString: String);
begin
  OutputStd;
  FProc.Input.Write(InputString[1], Length(InputString));
  OutputStd;
end;

{$IFDEF UNIX}
procedure TShellThread.SendSIG(SigNumber: Byte);
var
  Sig: String;
begin
  Sig:=chr(SigNumber){+LineEnding};
  SendToShellTerminal(Sig);
end;
{$ENDIF}

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
  FBot.HTTPProxyHost:=Cnfg.HTTPProxyHost;
  FBot.HTTPProxyPort:=Cnfg.HTTPProxyPort;
//  FBot.LogDebug:=True;
  FBot.OnReceiveMessage:=@BotReceiveMessage;
  { Read output of shell terminal called by user via /read command }
  FBot.CommandHandlers['/read']:=@BotReceiveReadCommand;
  FBot.CommandHandlers['/scripts']:=@BotReceiveScriptsCommand;{$IFDEF UNIX}
  FBot.CommandHandlers['/sig']:=@BotReceiveSigCommand;
  FBot.CommandHandlers['/sigint']:=@BotReceiveSIGINTCommand;
  FBot.CommandHandlers['/sigkill']:=@BotReceiveSIGKILLCommand;
  FBot.CommandHandlers['/sigquit']:=@BotReceiveSIGQUITCommand;
  FBot.CommandHandlers['/sigterm']:=@BotReceiveSIGTERMCommand;
  FBot.OnReceiveCallbackQuery:=@BotReceiveCallbackQuery;
  {$ENDIF}{$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);{$ENDIF}
  FProc:=TProcess.Create(nil);
  FProc.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
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
  begin
    FBot.getUpdatesEx(0, FLPTimeout);
    OutputStd;
  end;
end;

end.

