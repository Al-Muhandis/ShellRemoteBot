unit shellthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tgtypes, tgsendertypes, process, eventlog, tgshbot,
  pathrefs, UTF8Process
  {$IFDEF MSWINDOWS}, Windows{$ENDIF}
  {$IFDEF UNIX}, BaseUnix{$ENDIF}
  ;

type

  { TShellThread }

  TShellThread=class(TThread)
  private
    FIsCallBack: Boolean;
    FLogger: TEventLog;
    FBot: TTgShBot;
    FProc: TProcess;
    FPaths: TPathRefs;     // callback token -> full path (dirs, files, scripts)
    FChatDirs: TStringList; // chat id -> current folder (also the pending upload folder)
    FLPTimeout: Integer;
    FShellOwnGroup: Boolean; // the shell leads its own process group (safe to killpg)
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
    procedure CallbackDir(const AToken: String);
    procedure CallbackFile(const AToken: String);
    procedure CallbackInput(const AToken: String);
    procedure CallbackScript(const AToken: String);
    function PathByToken(const AToken: String; out APath: String): Boolean;
    function CommandStart: Boolean;
    function CheckIsAdmin: Boolean;
    procedure DirHandler(const aPath: String; aOffset: Integer = 0);
    procedure SplitPageToken(const AValue: String; out APath: String; out AOffset: Integer);
    procedure FileHandler(const aPath: String);
    class function FormatPath(const aPath: String): String;
    function GetChatDir(AChatId: Int64; out ADir: String): Boolean;
    function GetLogger: TEventLog;
    procedure OutputStd(const NoOutput: String = '');
    procedure SetChatDir(AChatId: Int64; const ADir: String);
    procedure SendToShellTerminal(const InputString: String);
    {$IFDEF UNIX}procedure SendSIG(SigNumber: Byte);{$ENDIF}
    {$IFDEF UNIX}procedure ShellForkEvent(Sender: TObject);{$ENDIF}
    procedure StartShell;
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
  _MaxButtons = 90;      // entries per /dir page: '..' + prev/next + Upload
                         // must also fit into the 100-button Telegram hard limit
  _OfsSep = '|';         // page token separator: '<offset>|<path>'
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
    // the upload folder is kept per chat (set by the Upload... button),
    // no round-trip through the reply message text
    if Assigned(AMessage.Document) then
    begin
      if GetChatDir(FBot.CurrentChatId, aPath) then
        BotReceiveDocument(AMessage.Document, aPath)
      else
        FBot.sendMessage(emj_Wrnng+' No pending upload folder. Call /dir and press Upload...');
    end;
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
  p: Integer;
begin
  if not CommandStart then
    Exit;
  FBot.answerCallbackQuery(ACallback.ID);
  FIsCallBack:=True;
  aCmd:=ACallback.Data;
  aName:=EmptyStr;
  p:=Pos(' ', aCmd);
  if p>0 then
  begin
    aName:=Copy(aCmd, p+1, MaxInt);
    aCmd:=Copy(aCmd, 1, p-1);
  end;
  // aName is a token into FPaths ('dir 12', 'file 34', 'script 7', 'input 42')
  case aCmd of
    dt_script: CallbackScript(aName);
    dt_dir:    CallbackDir(aName);
    dt_file:   CallbackFile(aName);
    dt_input:  CallbackInput(aName);
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
    if (i>=0) and (i<=High(Byte)) then
      SendSIG(i)
    else
      FBot.sendMessageSafe('SIG number must be between 0 and 255!')
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

procedure TShellThread.CallbackDir(const AToken: String);
var
  aValue, aPath: String;
  aOffset: Integer;
begin
  if not PathByToken(AToken, aValue) then
  begin
    FBot.sendMessage(emj_Wrnng+' '+_StaleListMsg);
    Exit;
  end;
  SplitPageToken(aValue, aPath, aOffset);
  DirHandler(aPath, aOffset);
end;

procedure TShellThread.CallbackFile(const AToken: String);
var
  aPath: String;
begin
  if PathByToken(AToken, aPath) then
    FileHandler(aPath)
  else
    FBot.sendMessage(emj_Wrnng+' '+_StaleListMsg);
end;

procedure TShellThread.CallbackInput(const AToken: String);
var
  aReplyMarkup: TReplyMarkup;
  aPath, aMsg: String;
begin
  if not PathByToken(AToken, aPath) then
  begin
    FBot.sendMessage(emj_Wrnng+' '+_StaleListMsg);
    Exit;
  end;
  aPath:=IncludeTrailingPathDelimiter(aPath);
  // remember the pending upload folder instead of parsing it later
  // from the reply message text
  SetChatDir(FBot.CurrentChatId, aPath);
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
  Result:=FPaths.Add(APath);
end;

function TShellThread.PathByToken(const AToken: String; out APath: String): Boolean;
begin
  Result:=FPaths.Find(AToken, APath);
end;

procedure TShellThread.SetChatDir(AChatId: Int64; const ADir: String);
begin
  FChatDirs.Values[IntToStr(AChatId)]:=ADir;
end;

function TShellThread.GetChatDir(AChatId: Int64; out ADir: String): Boolean;
var
  i: Integer;
begin
  i:=FChatDirs.IndexOfName(IntToStr(AChatId));
  if i>=0 then
  begin
    ADir:=FChatDirs.ValueFromIndex[i];
    Result:=True;
  end
  else
  begin
    ADir:=EmptyStr;
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

{ Page tokens keep the offset in front of the path: '<offset>|<path>'. An
  absolute path never starts with '<digits>|', so the prefix is unambiguous }
procedure TShellThread.SplitPageToken(const AValue: String; out APath: String;
  out AOffset: Integer);
var
  p: Integer;
begin
  aPath:=AValue;
  aOffset:=0;
  p:=Pos(_OfsSep, AValue);
  if p>0 then
    if TryStrToInt(Copy(AValue, 1, p-1), aOffset) and (aOffset>=0) then
      aPath:=Copy(AValue, p+1, MaxInt)
    else
      aOffset:=0;
end;

procedure TShellThread.DirHandler(const aPath: String; aOffset: Integer = 0);
var
  s, aName, aDir, aParent: String;
  aDirs, aFiles: TStringList;
  aReplyMarkup: TReplyMarkup;
  aNavRow: TInlineKeyboardButtons;
  i, aTotal, aPageEnd, aShown, aPrev: Integer;
begin
  aDir:=IncludeTrailingPathDelimiter(aPath);
  // the current folder is tracked per chat: the '..' button and the upload
  // flow resolve it through tokens/per-chat state, never through message text
  SetChatDir(FBot.CurrentChatId, aDir);
  aReplyMarkup:=TReplyMarkup.Create;
  aDirs:=TStringList.Create;
  aFiles:=TStringList.Create;
  try
    aReplyMarkup.InlineKeyBoard:=TInlineKeyboard.Create;
    if aDir<>PathDelim then
    begin
      aParent:=IncludeTrailingPathDelimiter(ExtractFileDir(ExcludeTrailingPathDelimiter(aDir)));
      aReplyMarkup.InlineKeyBoard.AddButton(emj_FileFolder+' '+'..',
        dt_dir+' '+AddPathRef(aParent));
    end;
    FindAllDirectories(aDirs, aDir, False);
    FindAllFiles(aFiles, aDir, EmptyStr, False);
    aTotal:=aDirs.Count+aFiles.Count;
    aPageEnd:=aOffset+_MaxButtons;
    aShown:=0;
    for i:=0 to aDirs.Count-1 do
      if (i>=aOffset) and (i<aPageEnd) then
      begin
        aName:=ExtractFileName(aDirs[i]);
        aReplyMarkup.InlineKeyBoard.AddButton(emj_FileFolder+' '+aName,
          dt_dir+' '+AddPathRef(IncludeTrailingPathDelimiter(aDirs[i])), 2);
        Inc(aShown);
      end;
    for i:=0 to aFiles.Count-1 do
      if (aDirs.Count+i>=aOffset) and (aDirs.Count+i<aPageEnd) then
      begin
        aName:=ExtractFileName(aFiles[i]);
        aReplyMarkup.InlineKeyBoard.AddButton(aName, dt_file+' '+AddPathRef(aFiles[i]), 2);
        Inc(aShown);
      end;
    // prev/next page buttons share one row
    aNavRow:=nil;
    if aOffset>0 then
    begin
      aPrev:=aOffset-_MaxButtons;
      if aPrev<0 then
        aPrev:=0;
      aNavRow:=aReplyMarkup.InlineKeyBoard.Add;
      aNavRow.AddButton('◀️', dt_dir+' '+AddPathRef(IntToStr(aPrev)+_OfsSep+aDir));
    end;
    if aPageEnd<aTotal then
    begin
      if not Assigned(aNavRow) then
        aNavRow:=aReplyMarkup.InlineKeyBoard.Add;
      aNavRow.AddButton('▶️', dt_dir+' '+AddPathRef(IntToStr(aPageEnd)+_OfsSep+aDir));
    end;
    aReplyMarkup.InlineKeyBoard.Add.AddButton('Upload...', dt_input+' '+AddPathRef(aDir));
    s:=FormatPath(aDir);
    if (aOffset>0) or (aPageEnd<aTotal) then
      s+=LineEnding+Format('Entries %d-%d of %d', [aOffset+1, aOffset+aShown, aTotal]);
    if IsCallback then
      FBot.editMessageText(s, pmMarkdown, True, aReplyMarkup)
    else
      FBot.sendMessage(s, pmMarkdown, True, aReplyMarkup);
  finally
    aReplyMarkup.Free;
    aDirs.Free;
    aFiles.Free;
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

procedure TShellThread.SendToShellTerminal(const InputString: String);
begin
  OutputStd;
  FProc.Input.Write(InputString[1], Length(InputString));
  OutputStd;
end;

{$IFDEF UNIX}
{ Runs inside the forked shell child before exec: detach into a new session
  so the shell leads its own process group and a group signal from SendSIG
  can never reach the tgshd process itself }
procedure TShellThread.ShellForkEvent(Sender: TObject);
begin
  if FpSetsid<0 then
    FpExit(126);  // abort the start: the parent falls back to a plain shell
end;

procedure TShellThread.SendSIG(SigNumber: Byte);
var
  i: Integer;
begin
  if not (Assigned(FProc) and FProc.Running) then
    Exit;
  // Deliver the signal by kill(2) instead of terminal input bytes: without a
  // PTY, chr(N) written to the shell stdin is just input, never a signal.
  if FShellOwnGroup then
  begin
    // the whole process group: the shell and the command it currently runs
    if FpKill(-FProc.ProcessID, SigNumber)<>0 then
      FpKill(FProc.ProcessID, SigNumber);
  end
  else
    FpKill(FProc.ProcessID, SigNumber);  // no group isolation: the shell only
  OutputStd;    // flush what the interrupted command has printed
  i:=0;
  while (i<10) and FProc.Running do   // let the signal take effect
  begin
    Sleep(100);
    Inc(i);
  end;
  if not FProc.Running then
  begin
    OutputStd;
    StartShell;
    Logger.Info('Shell terminated by signal '+IntToStr(SigNumber)+', restarted');
    FBot.sendMessageSafe('Shell terminated by signal '+IntToStr(SigNumber)+', restarted');
  end;
end;
{$ENDIF}

procedure TShellThread.StartShell;
begin
  FreeAndNil(FProc);
  FShellOwnGroup:=False;
  FProc:=TProcessUTF8.Create(nil);
  FProc.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
  {$IFDEF UNIX}
  try
    FProc.OnForkEvent:=@ShellForkEvent;
    FProc.Executable:='sh';
    FProc.Execute;
    Sleep(50);   // let a child refused to detach (see ShellForkEvent) die fast
    if FProc.Running then
      FShellOwnGroup:=True
    else
      raise EProcess.Create('the shell did not start in its own session');
  except
    on E: Exception do
    begin
      Logger.Error('Failed to start the shell in its own session ('+E.Message+
        '), falling back to a shell without group isolation');
      FreeAndNil(FProc);
      FProc:=TProcessUTF8.Create(nil);
      FProc.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
      FProc.Executable:='sh';
      FProc.Execute;
    end;
  end;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  FProc.Executable:='cmd';
  FProc.Execute;
  {$ENDIF}
end;

constructor TShellThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate:=True;
  FPaths:=TPathRefs.Create;     // callback token -> path ring, must exist before handlers
  FChatDirs:=TStringList.Create; // chat id -> current folder map
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
  StartShell;
  FLPTimeout:=Cnfg.APITimeout;

  Logger.Info('Log started');
end;

destructor TShellThread.Destroy;
begin
  FreeAndNil(FProc);
  FreeAndNil(FBot);
  FreeAndNil(FPaths);
  FreeAndNil(FChatDirs);
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

