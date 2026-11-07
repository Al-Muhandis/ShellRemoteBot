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
    FPaths: TStringList;   // callback token -> full path (dirs, files, scripts)
    FRefSeq: Integer;      // monotonically growing token counter
    FLPTimeout: Integer;
    procedure BotReceiveCallbackQuery({%H-}ASender: TObject; ACallback: TCallbackQueryObj);  
    procedure BotReceiveDeleteCommand({%H-}ASender: TObject; const {%H-}ACommand: String;
      AMessage: TTelegramMessageObj);
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
    function AddPathRef(const APath: String): String;
    procedure CallbackDir(const aMessage, aName: String);
    procedure CallbackFile(const {%H-}aMessage, aName: String);
    procedure CallbackInput(const aMessage: String);
    procedure CallbackScript(const AToken: String);
    function PathByToken(const AToken: String; out APath: String): Boolean;
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
  _MaxPathRefs = 1024;   // size of the callback token -> path ring map
  _MaxButtons = 98;      // Telegram inline keyboard hard limit is 100 buttons
  _StaleListMsg = 'Listing is outdated. Call /dir again';

  emj_FileFolder='📁';
  emj_Wrnng='⚠️';

  dt_script='script';
  dt_dir='dir';
  dt_file='file'; 
  dt_input='input'; 
  dt_dlt='delete';


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
  aFileID, aFileName: String;
begin
  aFileID:=aDocument.FileID;
  aFileName:=ExtractFileName(aDocument.FileName);
  if (aFileName=EmptyStr) or (aFileName='.') or (aFileName='..') then
  begin
    Logger.Error('Unsafe file name "'+aDocument.FileName+'"');
    Exit(False);
  end;
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
    TFPHTTPClient.SimpleGet(FBot.FileObj.DownloadLink(FBot.Token), aPath+aFileName);
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
  // aName is a token into FPaths ('dir 12', 'file 34', 'script 7') or '..'
  aName:=RightStr(ACallback.Data, Length(ACallback.Data)-Length(aCmd)-1);
  case aCmd of
    dt_script: CallbackScript(aName);
    dt_dir:    CallbackDir(ACallback.Message.Text, aName);
    dt_file:   CallbackFile(ACallback.Message.Text, aName);
    dt_input:  CallbackInput(ACallback.Message.Text);
  end;
end;

procedure TShellThread.BotReceiveDeleteCommand(ASender: TObject; const ACommand: String; AMessage: TTelegramMessageObj);
var
  aPath: String;
begin
  if not CommandStart then
    Exit;
  // everything after '/delete', markdown backticks stripped: paths with spaces survive
  aPath:=Trim(StringReplace(Copy(AMessage.Text, Length('/'+dt_dlt)+1, MaxInt),
    '`', EmptyStr, [rfReplaceAll]));
  if aPath.IsEmpty then
    FBot.sendMessage('Use command line format like /delete `/path-to-the-file/file-name.ext`', pmMarkdown)
  else
    if DeleteFileUTF8(aPath) then
      FBot.sendMessage(Format('File (%s) succesfully deleted', [aPath]))
    else
      FBot.sendMessage(emj_Wrnng+' '+Format('File (%s) deletion error', [aPath]))
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
      aReplyMarkup.InlineKeyBoard.AddButton(aFile, dt_script+' '+AddPathRef(f), 3);
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
    if not PathByToken(aName, aPath) then
    begin
      FBot.sendMessage(emj_Wrnng+' '+_StaleListMsg);
      Exit;
    end;
  DirHandler(IncludeTrailingPathDelimiter(aPath));
end;

procedure TShellThread.CallbackFile(const aMessage, aName: String);
var
  aPath: String;
begin
  if PathByToken(aName, aPath) then
    FileHandler(aPath)
  else
    FBot.sendMessage(emj_Wrnng+' '+_StaleListMsg);
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

function TShellThread.AddPathRef(const APath: String): String;
begin
  Result:=IntToStr(FRefSeq);
  Inc(FRefSeq);
  FPaths.Values[Result]:=APath;
  if FPaths.Count>_MaxPathRefs then
    FPaths.Delete(0); // the oldest token has the smallest number
end;

function TShellThread.PathByToken(const AToken: String; out APath: String): Boolean;
var
  i: Integer;
begin
  i:=FPaths.IndexOfName(AToken);
  if i>=0 then
  begin
    APath:=FPaths.ValueFromIndex[i];
    Result:=True;
  end
  else
  begin
    APath:=EmptyStr;
    Result:=False;
  end;
end;

procedure TShellThread.CallbackScript(const AToken: String);
var
  aPath: String;
  aScript: TStringList;
begin
  if not PathByToken(AToken, aPath) then
  begin
    FBot.sendMessage(emj_Wrnng+' '+_StaleListMsg);
    Exit;
  end;
  aScript:=TStringList.Create;
  try
    aScript.LoadFromFile(aPath);
    SendToShellTerminal(aScript.Text);
  finally
    aScript.Free;
  end;
end;

function TShellThread.CommandStart: Boolean;
begin
  FBot.UpdateProcessed:=True;  // There is no point in further processing
  Result:=CheckIsAdmin;
  if not Result then
    Exit;
  if not (Assigned(FProc) and FProc.Running) then
  begin
    FBot.sendMessage('Shell process is not running!');
    Result:=False;
  end;
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
  i, aAdded, aHidden: Integer;
begin
  aReplyMarkup:=TReplyMarkup.Create;
  aList:=TStringList.Create;
  try
    aReplyMarkup.InlineKeyBoard:=TInlineKeyboard.Create;
    aAdded:=0;
    aHidden:=0;
    if aPath<>PathDelim then
    begin
      aReplyMarkup.InlineKeyBoard.AddButton(emj_FileFolder+' '+'..', dt_dir+' '+'..');
      aAdded:=1;
    end;
    FindAllDirectories(aList, aPath, False);
    i:=0;
    while i<aList.Count do
    begin
      if aAdded>=_MaxButtons then
      begin
        aHidden:=aHidden+aList.Count-i;
        Break;
      end;
      aName:=ExtractFileName(aList[i]);
      aReplyMarkup.InlineKeyBoard.AddButton(emj_FileFolder+' '+aName,
        dt_dir+' '+AddPathRef(IncludeTrailingPathDelimiter(aList[i])), 2);
      Inc(aAdded);
      Inc(i);
    end;
    aList.Clear;
    FindAllFiles(aList, aPath, EmptyStr, False);
    i:=0;
    while i<aList.Count do
    begin
      if aAdded>=_MaxButtons then
      begin
        aHidden:=aHidden+aList.Count-i;
        Break;
      end;
      aName:=ExtractFileName(aList[i]);
      aReplyMarkup.InlineKeyBoard.AddButton(aName, dt_file+' '+AddPathRef(aList[i]), 2);
      Inc(aAdded);
      Inc(i);
    end;
    aReplyMarkup.InlineKeyBoard.Add.AddButton('Upload...', dt_input);
    if aHidden>0 then
      s:=FormatPath(aPath)+LineEnding+Format('(+%d more entries not shown)', [aHidden])
    else
      s:=FormatPath(aPath);
    if IsCallback then
      FBot.editMessageText(s, pmMarkdown, True, aReplyMarkup)
    else
      FBot.sendMessage(s, pmMarkdown, True, aReplyMarkup);
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
  FPaths:=TStringList.Create;   // callback token -> path map, must exist before handlers
  FRefSeq:=0;
  TelegramAPI_URL:=Cnfg.APIEndPoint; // For Russian specific case
  FBot:=TTgShBot.Create(Cnfg.BotTooken);
  FBot.Logger:=Logger;
  if FBot.Token=EmptyStr then
  begin
    Logger.Error('Please, specify bot token in tgshd.ini!');
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
  FBot.CommandHandlers['/'+dt_dlt]:=@BotReceiveDeleteCommand;
  FBot.OnReceiveCallbackQuery:=@BotReceiveCallbackQuery;{$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);{$ENDIF}
  FBot.ServiceUser:=Cnfg.ServiceUser;
  FProc:=TProcessUTF8.Create(nil);
  FProc.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
  FProc.Executable:={$IFDEF MSWINDOWS}'cmd'{$ELSE}'sh'{$ENDIF};
  FProc.Execute;
  FLPTimeout:=Cnfg.APITimeout;

  Logger.Info('Log started');
end;

destructor TShellThread.Destroy;
begin
  FreeAndNil(FProc);
  FreeAndNil(FBot);
  FreeAndNil(FPaths);
  FreeAndNil(FLogger);
  inherited Destroy;
end;

procedure TShellThread.Execute;
begin
  if (not Assigned(FBot)) or (not Assigned(FProc)) then
    Exit; // constructor exited early (e.g. empty bot token)
  try
    OutputStd;
    while not Terminated do
    begin
      try
        FBot.getUpdatesEx(0, FLPTimeout);
        OutputStd;
      except
        on E: Exception do
        begin
          Logger.Error(E.ClassName+': '+E.Message);
          Sleep(5000);    // sleep in case of an error to avoid sending a large number of requests to the telegram server
        end;
      end;
    end;
  except
    on E: Exception do
      Logger.Error(E.ClassName+': '+E.Message);
  end;
end;

end.

