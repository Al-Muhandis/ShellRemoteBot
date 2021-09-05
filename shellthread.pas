unit shellthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tgtypes, tgsendertypes, process, eventlog, tgshbot, UTF8Process
  {$IFDEF MSWINDOWS}, Windows{$ENDIF}
  ;

type

  { TShellThread }

  TShellThread=class(TThread)
  private
    FIsCallBack: Boolean;
    FLogger: TEventLog;
    FBot: TTgShBot;
    FProc: TProcess;
    FTerminated: Boolean;
    FLPTimeout: Integer;
    procedure BotReceiveCallbackQuery({%H-}ASender: TObject; ACallback: TCallbackQueryObj);
    procedure BotReceiveMessage({%H-}ASender: TObject; AMessage: TTelegramMessageObj);     
    function BotReceiveDocument(aDocument: TTelegramDocument; const aPath: String): Boolean;
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
    procedure BotReceiveFileCommand({%H-}ASender: TObject; const {%H-}ACommand: String;
      {%H-}AMessage: TTelegramMessageObj); 
    procedure CallbackDir(const aMessage, aName: String);
    procedure CallbackFile(const aMessage, aName: String); 
    procedure CallbackInput(const aMessage: String);
    procedure CallbackScript(const aFileName: String);
    function CommandStart: Boolean;
    function CheckIsAdmin: Boolean;
    procedure DirHandler(const aPath: String);
    procedure FileHandler(const aPath: String);
    class function FormatPath(const aPath: String): String;
    function GetLogger: TEventLog;
    procedure OutputStd(const NoOutput: String = '');
    class function ParsePath(const aMessage: String): String;
    procedure SendToShellTerminal(const InputString: String);
    {$IFDEF UNIX}procedure SendSIG(SigNumber: Byte);{$ENDIF}
    procedure SetLogger(AValue: TEventLog);
    property Logger: TEventLog read GetLogger write SetLogger;
    property IsCallback: Boolean read FIsCallBack;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

uses
  configuration, strutils, LazFileUtils, FileUtil, tgutils, LazUTF8, fphttpclient
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

  emj_FileFolder='📁';

  dt_script='script';
  dt_dir='dir';
  dt_file='file'; 
  dt_input='input';


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
    FLogger.LogType:=ltSystem;
    FLogger.Active:=True;
    FLogger.AppendContent:=True;
  end;
  Result:=FLogger;
end;

procedure TShellThread.BotReceiveMessage(ASender: TObject;
  AMessage: TTelegramMessageObj);
var
  InputString, aPath: String;
begin
  if not CommandStart then
    Exit;
  if Assigned(AMessage.ReplyToMessage) then
  begin
    aPath:=ParsePath(AMessage.ReplyToMessage.Text);
    if aPath.IsEmpty then
      Exit;
    if Assigned(AMessage.Document) then
      BotReceiveDocument(AMessage.Document, aPath);
    Exit;
  end;
  InputString:=AMessage.Text;
  if InputString=EmptyStr then
    Exit;
  InputString+=LineEnding;
  SendToShellTerminal(InputString);
end;

function TShellThread.BotReceiveDocument(aDocument: TTelegramDocument; const aPath: String): Boolean;
var
  aFileID: String;
begin
  aFileID:=aDocument.FileID;
  if not FBot.getFile(aFileID) then
  begin
    Logger.Error('Failed to get file by FileID "'+AFileID+'"');
    Exit(False);
  end;
  if not Assigned(FBot.FileObj) then
  begin
    Logger.Error('FileObject not Assigned! FileID: "'+AFileID+'"');
    Exit(False);
  end; 
  try
    TFPHTTPClient.SimpleGet(FBot.FileObj.DownloadLink(FBot.Token), aPath+aDocument.FileName);
  except
    on E:Exception do
      Logger.Error(E.ClassName+': '+E.Message);
  end;
  Result:=True;
end;

procedure TShellThread.BotReceiveCallbackQuery(ASender: TObject; ACallback: TCallbackQueryObj);
var
  aName, aCmd: String;
begin
  if not CommandStart then
    Exit;
  FBot.answerCallbackQuery(ACallback.ID);
  FIsCallBack:=True;
  aCmd:=ExtractWord(1, ACallback.Data, [' ']);
  aName:=RightStr(ACallback.Data, Length(ACallback.Data)-Length(aCmd)-1);
  case aCmd of
    dt_script: CallbackScript(aName);
    dt_dir:    CallbackDir(ACallback.Message.Text, aName);
    dt_file:   CallbackFile(ACallback.Message.Text, aName);
    dt_input:  CallbackInput(ACallback.Message.Text);
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
      aReplyMarkup.InlineKeyBoard.AddButton(aFile, dt_script+' '+aFile, 3);
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

procedure TShellThread.BotReceiveFileCommand(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
var
  aPath: String;
begin
  if not CommandStart then
    Exit;
  aPath:=ExtractWord(2, AMessage.Text, [' ']);
  if aPath=EmptyStr then
    aPath:=Cnfg.DefaultDir;
  if aPath=EmptyStr then
    aPath:={$IFDEF MSWINDOWS} GetCurrentDir {$ENDIF} {$IFDEF UNIX} PathDelim {$ENDIF};
  DirHandler(aPath);
end;

procedure TShellThread.CallbackDir(const aMessage, aName: String);
var
  aPath: String;
begin
  if aName='..' then
    aPath:=ExtractFileDir(ExcludeTrailingPathDelimiter(ParsePath(aMessage)))
  else
    aPath:=ParsePath(aMessage)+aName;
  DirHandler(IncludeTrailingPathDelimiter(aPath));
end;

procedure TShellThread.CallbackFile(const aMessage, aName: String);
begin
  FileHandler(ParsePath(aMessage)+aName);
end;

procedure TShellThread.CallbackInput(const aMessage: String);
var
  aReplyMarkup: TReplyMarkup;
  aPath, aMsg: String;
begin
  aPath:=ParsePath(aMessage);
  aMsg:=FormatPath(aPath)+LineEnding;
  aMsg+='Upload file to the folder '+aPath;
  aReplyMarkup:=TReplyMarkup.Create;
  try
    aReplyMarkup.ForceReply:=True;
    FBot.sendMessage(aMsg, pmMarkdown, True, aReplyMarkup);
  finally
    aReplyMarkup.Free;
  end;
end;

procedure TShellThread.CallbackScript(const aFileName: String);
var
  aScript: TStringList;
begin
  aScript:=TStringList.Create;
  try
    aScript.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFileDir(Cnfg.ScriptsDirectory))+aFileName+_ScriptFileExt);
    SendToShellTerminal(aScript.Text);
  finally
    aScript.Free;
  end;
end;

function TShellThread.CommandStart: Boolean;
begin
  FBot.UpdateProcessed:=True;  // There is no point in further processing
  Result:=CheckIsAdmin and FProc.Running;
end;

function TShellThread.CheckIsAdmin: Boolean;
begin
  Result:=FBot.CurrentIsAdminUser;
  if not Result then
    FBot.sendMessage('You cannot access to this bot!')
  else
    Result:=True;
end;

procedure TShellThread.DirHandler(const aPath: String);
var
  s, aName: String;
  aList: TStringList;
  aReplyMarkup: TReplyMarkup;
begin
  aReplyMarkup:=TReplyMarkup.Create;
  aList:=TStringList.Create;
  try
    aReplyMarkup.InlineKeyBoard:=TInlineKeyboard.Create;
    FindAllDirectories(aList, aPath, False);
    if aPath<>PathDelim then
      aReplyMarkup.InlineKeyBoard.AddButton(emj_FileFolder+' '+'..', dt_dir+' '+'..');
    for s in aList do
    begin
      aName:=ExtractFileName(s);
      aReplyMarkup.InlineKeyBoard.AddButton(emj_FileFolder+' '+aName, dt_dir+' '+aName, 2);
    end;
    aList.Clear;
    FindAllFiles(aList, aPath, EmptyStr, False);
    for s in aList do
    begin
      aName:=ExtractFileName(s);
      aReplyMarkup.InlineKeyBoard.AddButton(aName, dt_file+' '+aName, 2);
    end;
    aReplyMarkup.InlineKeyBoard.Add.AddButton('Upload...', dt_input);
    if IsCallback then
      FBot.editMessageText(FormatPath(aPath), pmMarkdown, True, aReplyMarkup)
    else
      FBot.sendMessage(FormatPath(aPath), pmMarkdown, True, aReplyMarkup);
  finally
    aReplyMarkup.Free;
    aList.Free;
  end;
end;

procedure TShellThread.FileHandler(const aPath: String);
begin
  FBot.sendDocumentByFileName(FBot.CurrentChatId, aPath, ExtractFileDir(aPath));
end;

class function TShellThread.FormatPath(const aPath: String): String;
begin
  Result:=emj_FileFolder+' '+mdCode+IncludeTrailingPathDelimiter(aPath)+mdCode+' '+emj_FileFolder;
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
    if not FBot.sendMessageCode({$IFDEF MSWINDOWS}WinCPToUTF8(OutputString){$ENDIF}{$IFDEF UNIX}OutputString{$ENDIF}) then
      Logger.Error('['+ClassName+'.OutputStd] Can''t send message to bot!');
  // read stderr and write to our stderr ... crashing :((
  { while FProc.Stderr.NumBytesAvailable > 0 do
  begin
    ReadCount := Min(512, FProc.Stderr.NumBytesAvailable);
    FProc.Stderr.Read(CharBuffer, ReadCount);
    FBot.sendMessage(Copy(CharBuffer, 0, ReadCount));
//        Write(StdErr, Copy(CharBuffer, 0, ReadCount));
  end; }
end;

class function TShellThread.ParsePath(const aMessage: String): String;
var
  aStart, aFinish: Integer;
begin
  Result:=EmptyStr;
  aStart:=Pos(emj_FileFolder+' ', aMessage);
  if aStart=0 then Exit;
  Inc(aStart, Length(emj_FileFolder)+1);
  aFinish:=PosEx(' '+emj_FileFolder, aMessage, aStart);
  if aFinish=0 then Exit;
  Result:=IncludeTrailingPathDelimiter(Copy(aMessage, aStart, aFinish-aStart));
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
  FreeOnTerminate:=True;
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
  FBot.CommandHandlers['/sigterm']:=@BotReceiveSIGTERMCommand;{$ENDIF}
  FBot.CommandHandlers['/'+dt_dir]:=@BotReceiveFileCommand;
  FBot.OnReceiveCallbackQuery:=@BotReceiveCallbackQuery;{$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);{$ENDIF}
  FBot.ServiceUser:=Cnfg.ServiceUser;
  FProc:=TProcessUTF8.Create(nil);
  FProc.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
  FProc.Executable:={$IFDEF MSWINDOWS}'cmd'{$ELSE}'sh'{$ENDIF};
  FProc.Execute;
  FTerminated:=False;
  FLPTimeout:=Cnfg.APITimeout;

  Logger.Info('Log started');
end;

destructor TShellThread.Destroy;
begin 
  FreeAndNil(FProc);
  FreeAndNil(FBot);
  FreeAndNil(FLogger);
  inherited Destroy;
end;

procedure TShellThread.Execute;
begin
  try
    OutputStd;
    while not Terminated do
    begin
      try
        FBot.getUpdatesEx(0, FLPTimeout);
        OutputStd;
      except
        on E: Exception do
          Logger.Error(E.ClassName+': '+E.Message);
      end;
    end;
  except
    on E: Exception do
      Logger.Error(E.ClassName+': '+E.Message);
  end;
end;

end.

