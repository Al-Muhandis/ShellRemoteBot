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

  { What the file card shows about a file. Found=False means the file could
    not be stat'ed, typically because it is already gone since the listing
    was built. ModeStr/OwnerStr are filled on UNIX only, where FpStat
    provides the rights and the owner }
  TFileStatInfo=record
    Found: Boolean;
    Size: Int64;
    ModTime: TDateTime;
    ModeStr: String;
    OwnerStr: String;
  end;

  { TShellThread }

  TShellThread=class(TThread)
  private
    FIsCallBack: Boolean;
    FLogger: TEventLog;
    FBot: TTgShBot;
    FProc: TProcess;
    FPaths: TPathRefs;     // callback token -> full path (dirs, files, scripts)
    FChatDirs: TStringList; // chat id -> current folder; pending edit when value contains _EditSep
    FLPTimeout: Integer;
    FShellOwnGroup: Boolean; // the shell leads its own process group (safe to killpg)
    FPanelToken: String;   // FPaths token of the live run panel; EmptyStr = none
    FPanelChatId: Int64;   // chat the live run panel message was sent to
    FPanelMsgId: Int64;    // message id the panel buttons edit; 0 = no live panel
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
    procedure CallbackCancel;
    procedure CallbackDelete(const AToken: String);
    procedure CallbackDeleteAsk(const AToken: String);
    procedure CallbackDir(const AToken: String);
    procedure CallbackDownload(const AToken: String);
    procedure CallbackEdit(const AToken: String);
    procedure CallbackFile(const AToken: String);
    procedure CallbackInput(const AToken: String);
    procedure CallbackRevert(const AToken: String);
    procedure CallbackRevertAsk(const AToken: String);
    procedure CallbackScript(const AToken: String);
    procedure CallbackPanelRead(const AToken: String);
    {$IFDEF UNIX}procedure CallbackPanelSignal(const AToken: String; ASignal: Byte);{$ENDIF}
    { Shows AMsg in place of the current message (optionally with a keyboard),
      falling back to a fresh message when the original is older than the
      Telegram edit limit }
    procedure ConfirmDone(const AMsg: String; AReplyMarkup: TReplyMarkup = nil;
      AMode: TParseMode = pmDefault);
    { Clears the live panel state without touching the panel message }
    procedure ForgetPanel;
    { Turns the live run panel into a plain Done note, retiring its buttons }
    procedure MarkPanelDone;
    function PathByToken(const AToken: String; out APath: String): Boolean;
    { Pending edit state helpers: edit state is stored in FChatDirs as a string
      containing _EditSep when an edit is in progress for the chat.
      GetEditState returns (FilePath, OriginalContent, BackupPath); the
      OriginalContent is empty when the state is not an edit state. }
    function GetEditState(const AValue: String; out AFilePath, AOriginalContent,
      ABackupPath: String): Boolean;
    function IsPendingEdit(AChatId: Int64): Boolean;
    procedure ClearPendingEdit(AChatId: Int64);
    { Saves ANewContent to AFilePath, creating a .bak backup first.
      Shows the result with a Revert button (dt_revertask). }
    procedure SaveEdit(const AFilePath, AOriginalContent, ANewContent: String);
    { Restores AFilePath from its .bak (Ask variant shows confirmation first).
      After a successful revert the .bak is deleted and the listing is reopened. }
    procedure DoRevert(const AFilePath, ABackupPath: String; AOffset: Integer);
    { Shows the revert confirmation: Ok carries dt_revert + offset|filepath|backup
      token, Cancel calls CallbackCancel which drops the pending state. }
    procedure AskRevertConfirm(const AToken: String);
    function CommandStart: Boolean;
    function CheckIsAdmin: Boolean;
    procedure DirHandler(const aPath: String; aOffset: Integer = 0);
    procedure SplitPageToken(const AValue: String; out APath: String; out AOffset: Integer);
    { Opens the file card in place of the listing; aOffset keeps the page of
      the listing the card was opened from }
    procedure FileCardHandler(const aPath: String; aOffset: Integer);
    { Reads size, modification time and, where the OS provides them, the
      rights and the owner of aPath }
    function GetFileStatInfo(const aPath: String): TFileStatInfo;
    { Shows the mandatory delete confirmation: Ok carries the delete verb with
      the path/page token, Cancel bails out. In a callback it replaces the
      current message (the file card), from /delete it posts a fresh one }
    procedure AskDeleteConfirm(const aPath: String; aOffset: Integer);
    class function FormatPath(const aPath: String): String;
    function GetChatDir(AChatId: Int64; out ADir: String): Boolean;
    function GetLogger: TEventLog;
    { Drains the shell output pipe after a short pause and returns the freshly
      printed text, converted to UTF-8 on Windows; empty when nothing came }
    function ReadShellOutput: String;
    { Reads the shell output and delivers it to the chat (or ANoOutput when
      nothing came); returns what was actually printed }
    function OutputStd(const NoOutput: String = ''): String;
    procedure SetChatDir(AChatId: Int64; const ADir: String);
    procedure SendRunPanel;
    procedure SendToShellTerminal(const InputString: String);
    {$IFDEF UNIX}function SendSIG(SigNumber: Byte): Boolean;{$ENDIF}
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

{ Fills AKeyboard with the /delete confirmation row: the Ok button carries the
  delete verb plus the path token, both buttons carry Bot API 10.3 styles }
procedure BuildDeleteConfirmKeyboard(const AToken: String; AKeyboard: TInlineKeyboard);

{ Fills AKeyboard with the file card actions: download, edit, delete and back to
  the parent listing; every button carries its callback token and a Bot API 10.3
  style }
procedure BuildFileCardKeyboard(const ADownToken, AEditToken, ADeleteToken,
  ABackToken: String; AKeyboard: TInlineKeyboard);

{ Fills AKeyboard with the /revert confirmation row: the Ok button carries the
  revert verb plus the token, both buttons carry Bot API 10.3 styles }
procedure BuildRevertConfirmKeyboard(const AToken: String; AKeyboard: TInlineKeyboard);

{ Fills AKeyboard with the run panel actions of a long-running command:
  SIGINT and SIGKILL on UNIX only (signals are not deliverable elsewhere) and
  Read everywhere; every button carries its callback token and a Bot API 10.3
  style }
procedure BuildRunPanelKeyboard(const AToken: String; AKeyboard: TInlineKeyboard);

{ Formats a byte count the human way: whole bytes under 1 KiB, then the largest
  fitting 1024-based unit with one decimal (K/M/G/T). Integer math only, so the
  output never depends on locale decimal separators }
function FormatFileSize(ASize: Int64): String;

{ Renders the 12 permission bits of a UNIX mode value (suid/sgid/sticky plus
  the rwx triplets) as the familiar 4-digit octal, e.g. 420 -> '0644' }
function ModeToOctal(AMode: LongWord): String;

{ Returns AOffset pulled back to the last page that still has entries when it
  went past the end: a delete can leave the stored page empty. ATotal<=0 (and
  a non-positive page size) always yields 0 }
function ClampPageOffset(ATotal, AOffset, APageSize: Integer): Integer;

{ Fills ACommands with the bot command menu: Telegram clients show it in the
  menu button, so every handler is discoverable without reading the docs.
  /sig is listed on UNIX only, where signals are actually deliverable }
procedure BuildBotCommands(ACommands: TBotCommandArray);

implementation

uses
  configuration, strutils, LazFileUtils, FileUtil, tgutils, LazUTF8, fphttpclient,
  fpjson
  {$IFDEF UNIX}, DateUtils{$ENDIF}
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
  _StaleDeleteMsg = 'Confirmation is outdated. Repeat /delete';
  _MaxButtons = 90;      // entries per /dir page: '..' + prev/next + Upload
                         // must also fit into the 100-button Telegram hard limit
  _OfsSep = '|';         // page token separator: '<offset>|<path>'
  _StaleListMsg = 'Listing is outdated. Call /dir again';
  _StalePanelMsg = 'The control panel is outdated (the command has finished or the bot was restarted)';
  _RunPanelMsg = '⏳ Command is running…';
  _PanelDoneMsg = '✔ Done';
  _MaxMsgLen = 4096;     // the Telegram hard limit for one message text
  _MaxSendDocSize = 50*1024*1024; // the Bot API hard limit for one document:
                                  // larger files must not even be attempted

  emj_FileFolder='📁';
  emj_Wrnng='⚠️';

  dt_script='script';
  dt_dir='dir';
  dt_file='file';
  dt_down='download';
  dt_edit='edit';
  dt_input='input';
  dt_dltask='askdelete';  // opens the confirmation; the deletion itself is
                          // only ever performed by dt_dlt after Ok
  dt_dlt='delete';
  dt_cancel='cancel';
  dt_revertask='revertask';  // opens the revert confirmation; revert itself
                             // only after Ok from dt_revert
  dt_revert='revert';
  {%H-}dt_sigint='sigint';    // run panel buttons: the panel of a command with no
  {%H-}dt_sigkill='sigkill';  // output yet; the signal ones exist on UNIX only
  dt_read='read';

  // Edit feature constants
  _EditSep = '|';           // separator for pending edit state in FChatDirs:
                            // '|<original_content>|<backup_path>'
  _MaxEditContent = 3500;   // send inline when content fits this many chars

  // Bot API 10.3 button styles (TInlineKeyboardButton.Style); 'link' is the
  // remaining style of the set, unused so far. Named so no raw style strings
  // leak into the markup code
  btnStyleDanger='danger';    // destructive confirmations
  btnStyleSuccess='success';  // positive actions
  btnStylePrimary='primary';  // neutral alternatives


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
  InputString, aPath, aEditFile, aOrigContent, aEditBackup: String;
  aTmpFile: String;
  aTmpStream: TFileStream;
  aDocSize: Int64;
  aNewContent: String;
begin
  if not CommandStart then
    Exit;
  if Assigned(AMessage.ReplyToMessage) and IsPendingEdit(FBot.CurrentChatId) and
     GetChatDir(FBot.CurrentChatId, aPath) and
     GetEditState(aPath, aEditFile, aOrigContent, aEditBackup) then
  begin
    // ----- user replied to the edit prompt -----
    if Assigned(AMessage.Document) then
    begin
      // document reply: download, validate UTF-8, save
      aTmpFile:=GetTempFileNameUTF8(GetTempDir, 'tgshd');
      aNewContent:=EmptyStr;
      try
        if not FBot.getFile(AMessage.Document.FileID) then
        begin
          FBot.sendMessage(emj_Wrnng+' Failed to get the file');
          Exit;
        end;
        aTmpStream:=TFileStream.Create(aTmpFile, fmCreate);
        try
          TFPHTTPClient.SimpleGet(FBot.FileObj.DownloadLink(FBot.Token), aTmpStream);
        finally
          aTmpStream.Free;
        end;
        // read the downloaded content into aNewContent
        with TFileStream.Create(aTmpFile, fmOpenRead) do
        try
          aDocSize:=Size;
          if aDocSize > _MaxEditSize then
          begin
            FBot.sendMessage(emj_Wrnng+' Downloaded file is '+
              FormatFileSize(aDocSize)+', larger than 1 MB limit for editing');
            Exit;
          end;
          SetLength(aNewContent, aDocSize);
          if aDocSize > 0 then
            Read(aNewContent[1], aDocSize);
        finally
          Free;
        end;
        if not IsTextFile(aTmpFile) then
        begin
          FBot.sendMessage(emj_Wrnng+' Downloaded file is not a valid UTF-8 text file');
          Exit;
        end;
        // save with the downloaded content (aOrigContent is the original for backup)
        SaveEdit(aEditFile, aOrigContent, aNewContent);
      finally
        DeleteFileUTF8(aTmpFile);
      end;
    end
    else if (AMessage.Text<>'/cancel') and (AMessage.Text<>EmptyStr) then
    begin
      // text reply: use as new content
      SaveEdit(aEditFile, aOrigContent, AMessage.Text);
    end
    else
    begin
      // /cancel or empty reply
      ClearPendingEdit(FBot.CurrentChatId);
      ConfirmDone('Edit cancelled');
      Exit;
    end;
    ClearPendingEdit(FBot.CurrentChatId);
    Exit;
  end;
  if Assigned(AMessage.ReplyToMessage) then
  begin
    // the upload folder is kept per chat (set by the Upload... button),
    // no round-trip through the reply message text
    if Assigned(AMessage.Document) then
    begin
      if GetChatDir(FBot.CurrentChatId, aPath) and (Pos(_EditSep, aPath)=0) then
        BotReceiveDocument(AMessage.Document, aPath)
      else
        FBot.sendMessage(emj_Wrnng+' No pending upload folder. Call /dir and press Upload...');
    end;
    Exit;
  end;
  InputString:=AMessage.Text;
  if InputString=EmptyStr then
    Exit;
  // /cancel from the user: clear any pending edit state
  if InputString='/cancel' then
  begin
    if IsPendingEdit(FBot.CurrentChatId) then
    begin
      ClearPendingEdit(FBot.CurrentChatId);
      ConfirmDone('Edit cancelled');
    end
    else
      FBot.sendMessage('Nothing to cancel');
    Exit;
  end;
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
  // aName is a token into FPaths ('dir 12', 'file 34', 'download 56',
  // 'script 7', 'input 42', 'askdelete 5', 'delete 3', 'read 9' of the run
  // panel, on UNIX also 'sigint 8'/'sigkill 4'); 'cancel' carries no token
  case aCmd of
    dt_script:  CallbackScript(aName);
    dt_dir:     CallbackDir(aName);
    dt_file:    CallbackFile(aName);
    dt_down:    CallbackDownload(aName);
    dt_edit:    CallbackEdit(aName);
    dt_input:   CallbackInput(aName);
    dt_dltask:  CallbackDeleteAsk(aName);
    dt_dlt:     CallbackDelete(aName);
    dt_revertask: CallbackRevertAsk(aName);
    dt_revert:  CallbackRevert(aName);
    {$IFDEF UNIX}
    dt_sigint:  CallbackPanelSignal(aName, SIGINT);
    dt_sigkill: CallbackPanelSignal(aName, SIGKILL);
    {$ENDIF}
    dt_read:    CallbackPanelRead(aName);
    dt_cancel:  CallbackCancel;
  end;
end;

procedure BuildDeleteConfirmKeyboard(const AToken: String; AKeyboard: TInlineKeyboard);
var
  aBtn: TInlineKeyboardButton;
begin
  // ready buttons instead of AddButton(text, data): the style travels with
  // the button it belongs to, no digging the button out of the last row
  aBtn:=TInlineKeyboardButton.Create('Ok');
  aBtn.callback_data:=dt_dlt+' '+AToken;
  aBtn.Style:=btnStyleDanger;
  AKeyboard.AddButton(aBtn, 2);
  aBtn:=TInlineKeyboardButton.Create('Cancel');
  aBtn.callback_data:=dt_cancel;
  aBtn.Style:=btnStylePrimary;
  AKeyboard.AddButton(aBtn, 2);
end;

procedure BuildFileCardKeyboard(const ADownToken, AEditToken, ADeleteToken,
  ABackToken: String; AKeyboard: TInlineKeyboard);
var
  aBtn: TInlineKeyboardButton;
begin
  // one row of four: download is the positive action, edit is the main
  // workflow, delete is destructive, back is the escape route
  aBtn:=TInlineKeyboardButton.Create('⬇ Download');
  aBtn.callback_data:=dt_down+' '+ADownToken;
  aBtn.Style:=btnStyleSuccess;
  AKeyboard.AddButton(aBtn, 4);
  aBtn:=TInlineKeyboardButton.Create('✏ Edit');
  aBtn.callback_data:=dt_edit+' '+AEditToken;
  aBtn.Style:=btnStylePrimary;
  AKeyboard.AddButton(aBtn, 4);
  aBtn:=TInlineKeyboardButton.Create('🗑 Delete');
  aBtn.callback_data:=dt_dltask+' '+ADeleteToken;
  aBtn.Style:=btnStyleDanger;
  AKeyboard.AddButton(aBtn, 4);
  aBtn:=TInlineKeyboardButton.Create('◀ Back');
  aBtn.callback_data:=dt_dir+' '+ABackToken;
  aBtn.Style:=btnStylePrimary;
  AKeyboard.AddButton(aBtn, 4);
end;

procedure BuildRevertConfirmKeyboard(const AToken: String; AKeyboard: TInlineKeyboard);
var
  aBtn: TInlineKeyboardButton;
begin
  aBtn:=TInlineKeyboardButton.Create('Ok');
  aBtn.callback_data:=dt_revert+' '+AToken;
  aBtn.Style:=btnStyleDanger;
  AKeyboard.AddButton(aBtn, 2);
  aBtn:=TInlineKeyboardButton.Create('Cancel');
  aBtn.callback_data:=dt_cancel;
  aBtn.Style:=btnStylePrimary;
  AKeyboard.AddButton(aBtn, 2);
end;

procedure BuildRunPanelKeyboard(const AToken: String; AKeyboard: TInlineKeyboard);
var
  aBtn: TInlineKeyboardButton;
begin
  {$IFDEF UNIX}
  // signals are delivered by killpg and exist on UNIX only: the panel is
  // assembled conditionally, exactly like the /sig* command handlers
  aBtn:=TInlineKeyboardButton.Create('⏹ SIGINT');
  aBtn.callback_data:=dt_sigint+' '+AToken;
  aBtn.Style:=btnStyleDanger;
  AKeyboard.AddButton(aBtn, 3);
  aBtn:=TInlineKeyboardButton.Create('✖ SIGKILL');
  aBtn.callback_data:=dt_sigkill+' '+AToken;
  aBtn.Style:=btnStyleDanger;
  AKeyboard.AddButton(aBtn, 3);
  {$ENDIF}
  aBtn:=TInlineKeyboardButton.Create('📖 Read');
  aBtn.callback_data:=dt_read+' '+AToken;
  aBtn.Style:=btnStylePrimary;
  AKeyboard.AddButton(aBtn, 3);
end;

function FormatFileSize(ASize: Int64): String;
var
  aIdx, aRem: Int64;
const
  _SizeUnit = 1024;
  _Units: array[0..4] of String = ('B', 'K', 'M', 'G', 'T');
begin
  aIdx:=0;
  aRem:=0;
  // scale down keeping the remainder for the decimal digit; the last unit
  // also takes everything larger, so no P/E ladder in the UI
  while (aIdx<High(_Units)) and (ASize>=_SizeUnit) do
  begin
    aRem:=ASize mod _SizeUnit;
    ASize:=ASize div _SizeUnit;
    Inc(aIdx);
  end;
  if aIdx=0 then
    Result:=IntToStr(ASize)+' B'
  else
    Result:=IntToStr(ASize)+'.'+IntToStr(aRem*10 div _SizeUnit)+' '+_Units[aIdx];
end;

function ModeToOctal(AMode: LongWord): String;
var
  i: Integer;
begin
  // 4 fixed octal digits keep the suid/sgid/sticky bits and leading zeros
  // visible, exactly how ls shows the permissions
  Result:=EmptyStr;
  for i:=3 downto 0 do
    Result+=IntToStr((AMode shr (i*3)) and 7);
end;

function ClampPageOffset(ATotal, AOffset, APageSize: Integer): Integer;
begin
  if (ATotal<=0) or (APageSize<=0) then
    Exit(0);
  Result:=((ATotal-1) div APageSize)*APageSize; // the last non-empty page
  if AOffset<Result then
    Result:=AOffset;
end;

procedure BuildBotCommands(ACommands: TBotCommandArray);
begin
  ACommands.AddCommand('/dir', 'file manager');
  ACommands.AddCommand('/scripts', 'run prepared scripts');
  ACommands.AddCommand('/read', 'read shell output');
  ACommands.AddCommand('/delete', 'delete a file');
  {$IFDEF UNIX}
  ACommands.AddCommand('/sig', 'send a signal to the shell process');
  {$ENDIF}
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
    if not FileExistsUTF8(aPath) then
      FBot.sendMessage(emj_Wrnng+' '+Format('File (%s) deletion error', [aPath]))
    else
      // confirm before touching the file: the Ok button carries a token into
      // FPaths, so the callback never parses the path out of message text.
      // Outside a callback ConfirmDone posts the confirmation as a fresh
      // message instead of an edit
      AskDeleteConfirm(aPath, 0);
end;

procedure TShellThread.BotReceiveReadCommand(ASender: TObject;
  const ACommand: String; AMessage: TTelegramMessageObj);
begin
  // the same read the run panel Read button performs: fresh output also
  // proves the command of a still open panel is over
  if CommandStart then
    if OutputStd('No new messages in the terminal')<>EmptyStr then
      MarkPanelDone;
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
  aValue, aPath: String;
  aOffset: Integer;
begin
  if not PathByToken(AToken, aValue) then
  begin
    FBot.sendMessage(emj_Wrnng+' '+_StaleListMsg);
    Exit;
  end;
  // the listing keeps the page offset with the path ('<offset>|<path>') so
  // the card can reopen exactly the page it was opened from
  SplitPageToken(aValue, aPath, aOffset);
  FileCardHandler(aPath, aOffset);
end;

procedure TShellThread.CallbackDeleteAsk(const AToken: String);
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
  AskDeleteConfirm(aPath, aOffset);
end;

procedure TShellThread.CallbackEdit(const AToken: String);
var
  aPath, aContent, aBackupPath: String;
  aReplyMarkup: TReplyMarkup;
  aMsg, aDisplayedContent: String;
  f: TFileStream;
  aSize: Int64;
  b: Byte;
begin
  if not PathByToken(AToken, aPath) then
  begin
    FBot.sendMessage(emj_Wrnng+' '+_StaleListMsg);
    Exit;
  end;
  // ----- size check -----
  aSize:=GetFileStatInfo(aPath).Size;
  if aSize > _MaxEditSize then
  begin
    ConfirmDone(emj_Wrnng+' '+Format('File is %s, larger than %d MB. Edit is limited to files up to 1 MB.',
      [FormatFileSize(aSize), _MaxEditSize div (1024*1024)]), nil, pmMarkdown);
    Exit;
  end;
  // ----- text / UTF-8 validity check -----
  if not IsTextFile(aPath) then
  begin
    ConfirmDone(emj_Wrnng+' File is not a valid UTF-8 text file (or contains #0 bytes). Only plain text files up to 1 MB can be edited.', nil, pmMarkdown);
    Exit;
  end;
  // ----- read and show content -----
  f:=TFileStream.Create(aPath, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(aContent, f.Size);
    if f.Size > 0 then
      f.Read(aContent[1], f.Size);
  finally
    f.Free;
  end;
  // store pending edit state in FChatDirs: '|<original_content>|<backuppath>'
  aBackupPath:=BackupPathFor(aPath);
  SetChatDir(FBot.CurrentChatId, aPath+_EditSep+aContent+_EditSep+aBackupPath);
  aReplyMarkup:=TReplyMarkup.Create;
  try
    aReplyMarkup.ForceReply:=True;
    aReplyMarkup.InputFieldPlaceholder:='New file content';
    if Length(aContent) <= _MaxEditContent then
    begin
      // show inline: fenced block without 'log' (plain text, not shell output)
      aDisplayedContent:='```'+LineEnding+
        StringReplace(aContent, '```', '``'+#$E2#$80#$8B+'`', [rfReplaceAll])+
        LineEnding+'```';
      FBot.sendMessage('✏ Editing: '+mdCode+ExtractFileName(aPath)+mdCode+LineEnding+
        'Reply with the new content, or /cancel', pmMarkdown, True, aReplyMarkup);
      // send the content as a follow-up (editing the same message would remove ForceReply)
      FBot.sendMessage(aDisplayedContent, pmMarkdown);
    end
    else
    begin
      // too large to show inline: send as document and ask user to edit and re-upload
      FBot.sendChatAction(FBot.CurrentChatId, caUploadDocument);
      FBot.sendDocumentByFileName(FBot.CurrentChatId, aPath, ExtractFileDir(aPath));
      FBot.sendMessage('✏ Editing: '+mdCode+ExtractFileName(aPath)+mdCode+LineEnding+
        'Download the file, edit it, and send it back as a document reply, or /cancel',
        pmMarkdown, True, aReplyMarkup);
    end;
  finally
    aReplyMarkup.Free;
  end;
end;

// ----- Pending edit state helpers -------------------------------------------

{ Parses AValue as a pending edit state ('filepath|original_content|backuppath').
  Returns True when AValue contains _EditSep and was successfully split; in that
  case AFilePath, AOriginalContent and ABackupPath are filled. Returns False
  when AValue is a plain path (e.g. from a pending upload folder), leaving all
  output parameters empty. }
function TShellThread.GetEditState(const AValue: String; out AFilePath,
  AOriginalContent, ABackupPath: String): Boolean;
var
  p1, p2: Integer;
begin
  Result:=False;
  AFilePath:=EmptyStr;
  AOriginalContent:=EmptyStr;
  ABackupPath:=EmptyStr;
  p1:=Pos(_EditSep, AValue);
  if p1<=0 then
    Exit;
  // format: 'filepath|<original_content>|<backuppath>'
  // filepath spans from 1 to p1-1
  // original_content from p1+1 to p2-1
  // backuppath from p2+1 to end
  AFilePath:=Copy(AValue, 1, p1-1);
  p2:=PosEx(_EditSep, AValue, p1+1);
  if p2<=p1 then
    Exit;
  AOriginalContent:=Copy(AValue, p1+1, p2-p1-1);
  ABackupPath:=Copy(AValue, p2+1, MaxInt);
  Result:=(AFilepath<>EmptyStr) and (ABackupPath<>EmptyStr);
end;

function TShellThread.IsPendingEdit(AChatId: Int64): Boolean;
var
  aValue, aFilePath, aContent, aBackup: String;
begin
  Result:=GetChatDir(AChatId, aValue) and
    GetEditState(aValue, aFilePath, aContent, aBackup);
end;

procedure TShellThread.ClearPendingEdit(AChatId: Int64);
begin
  SetChatDir(AChatId, EmptyStr);
end;

// ----- Save edit (with backup) ----------------------------------------------

procedure TShellThread.SaveEdit(const AFilePath, AOriginalContent,
  ANewContent: String);
var
  aBackupPath: String;
  aReplyMarkup: TReplyMarkup;
begin
  aBackupPath:=BackupPathFor(AFilePath);
  // 1. write .bak backup
  if not CopyFile(AFilePath, aBackupPath, False) then
  begin
    ConfirmDone(emj_Wrnng+' Failed to create backup '+aBackupPath, nil, pmMarkdown);
    Exit;
  end;
  // 2. write new content
  try
    with TFileStream.Create(AFilePath, fmCreate) do
    try
      if Length(ANewContent)>0 then
        Write(ANewContent[1], Length(ANewContent));
    finally
      Free;
    end;
  except
    on E: Exception do
    begin
      ConfirmDone(emj_Wrnng+' Failed to write file: '+E.Message, nil, pmMarkdown);
      Exit;
    end;
  end;
  // 3. show success with Revert button
  aReplyMarkup:=TReplyMarkup.Create;
  try
    aReplyMarkup.InlineKeyBoard:=TInlineKeyboard.Create;
    // token: '0|<filepath>|<backuppath>' — offset 0 (no listing context here)
    BuildRevertConfirmKeyboard(
      AddPathRef('0'+_OfsSep+AFilePath+_EditSep+aBackupPath),
      aReplyMarkup.InlineKeyBoard);
    ConfirmDone('✏ File saved: '+mdCode+ExtractFileName(AFilePath)+mdCode+
      ' — backup: '+mdCode+ExtractFileName(aBackupPath)+mdCode,
      aReplyMarkup, pmMarkdown);
  finally
    aReplyMarkup.Free;
  end;
end;

// ----- Revert flow ---------------------------------------------------------

procedure TShellThread.AskRevertConfirm(const AToken: String);
var
  aReplyMarkup: TReplyMarkup;
begin
  aReplyMarkup:=TReplyMarkup.Create;
  try
    aReplyMarkup.InlineKeyBoard:=TInlineKeyboard.Create;
    BuildRevertConfirmKeyboard(AToken, aReplyMarkup.InlineKeyBoard);
    ConfirmDone('↩ Revert to the backup and lose the changes?', aReplyMarkup);
  finally
    aReplyMarkup.Free;
  end;
end;

procedure TShellThread.CallbackRevertAsk(const AToken: String);
var
  aValue, aPath, aBackupPath: String;
  aOffset: Integer;
begin
  if not PathByToken(AToken, aValue) then
  begin
    FBot.sendMessage(emj_Wrnng+' '+_StaleListMsg);
    Exit;
  end;
  // token format: '0|<filepath>|<backuppath>'
  aOffset:=0;
  aPath:=EmptyStr;
  aBackupPath:=EmptyStr;
  // parse: the token is 'offset|filepath|backuppath' where offset is before first _OfsSep
  // and filepath|backuppath are separated by _EditSep
  if Pos(_OfsSep, aValue)>0 then
    SplitPageToken(aValue, aPath, aOffset);
  // aPath is now 'filepath|backuppath' — extract the two parts
  if not GetEditState(aPath, aPath,{%H-} aValue{unused}, aBackupPath) then
  begin
    // fallback: try BackupPathFor on aPath
    aBackupPath:=BackupPathFor(aPath);
  end;
  AskRevertConfirm(AddPathRef(IntToStr(aOffset)+_OfsSep+aPath+_EditSep+aBackupPath));
end;

procedure TShellThread.DoRevert(const AFilePath, ABackupPath: String;
  AOffset: Integer);
var
  aParent: String;
begin
  if not FileExistsUTF8(ABackupPath) then
  begin
    ConfirmDone(emj_Wrnng+' Backup file not found: '+ABackupPath, nil, pmMarkdown);
    Exit;
  end;
  if not CopyFile(ABackupPath, AFilePath, False) then
  begin
    ConfirmDone(emj_Wrnng+' Failed to restore from backup: '+ABackupPath, nil, pmMarkdown);
    Exit;
  end;
  DeleteFileUTF8(ABackupPath);  // clean up .bak after successful revert
  aParent:=ExtractFileDir(AFilePath);
  if aParent=EmptyStr then
    aParent:=PathDelim;
  DirHandler(aParent, AOffset);
end;

procedure TShellThread.CallbackRevert(const AToken: String);
var
  aValue, aPath, aBackupPath: String;
  aOffset: Integer;
begin
  if not PathByToken(AToken, aValue) then
  begin
    FBot.sendMessage(emj_Wrnng+' '+_StaleDeleteMsg);
    Exit;
  end;
  // token: 'offset|<filepath>|<backuppath>'
  aOffset:=0;
  if Pos(_OfsSep, aValue)>0 then
    SplitPageToken(aValue, aPath, aOffset);
  if not GetEditState(aPath, aPath,{%H-}aValue{unused}, aBackupPath) then
    aBackupPath:=BackupPathFor(aPath);
  DoRevert(aPath, aBackupPath, aOffset);
end;

procedure TShellThread.AskDeleteConfirm(const aPath: String; aOffset: Integer);
var
  aReplyMarkup: TReplyMarkup;
begin
  aReplyMarkup:=TReplyMarkup.Create;
  try
    // the Ok token keeps the page offset ('<offset>|<path>'): after the
    // delete the listing reopens on the same page; a plain path from
    // /delete has none and lands on page 0
    aReplyMarkup.InlineKeyBoard:=TInlineKeyboard.Create;
    BuildDeleteConfirmKeyboard(AddPathRef(IntToStr(aOffset)+_OfsSep+aPath),
      aReplyMarkup.InlineKeyBoard);
    ConfirmDone(Format('Delete %s?', [aPath]), aReplyMarkup);
  finally
    aReplyMarkup.Free;
  end;
end;

procedure TShellThread.CallbackDelete(const AToken: String);
var
  aValue, aPath, aParent: String;
  aOffset: Integer;
begin
  if not PathByToken(AToken, aValue) then
  begin
    FBot.sendMessage(emj_Wrnng+' '+_StaleDeleteMsg);
    Exit;
  end;
  // the Ok button of the confirmation carries the page offset with the path
  // ('<offset>|<path>'), a plain path from /delete has none and lands on
  // page 0
  SplitPageToken(aValue, aPath, aOffset);
  if DeleteFileUTF8(aPath) then
  begin
    // no separate 'deleted' note: the reopened listing proves it and the
    // user stays one tap from continuing to work with the folder
    aParent:=ExtractFileDir(aPath);
    if aParent=EmptyStr then
      aParent:=PathDelim;
    DirHandler(aParent, aOffset);
  end
  else
    ConfirmDone(emj_Wrnng+' '+Format('File (%s) deletion error', [aPath]));
end;

procedure TShellThread.CallbackCancel;
begin
  ConfirmDone('Deletion cancelled');
end;

procedure TShellThread.CallbackPanelRead(const AToken: String);
var
  aValue, aOut: String;
begin
  // a button of an already retired panel must not read the shell: only the
  // token of the live panel passes (an old message survives a bot restart
  // with its buttons, the tokens do not)
  if (AToken<>FPanelToken) or (not PathByToken(AToken, aValue)) then
  begin
    FBot.sendMessage(emj_Wrnng+' '+_StalePanelMsg);
    Exit;
  end;
  // the very read /read performs, only the fresh output is returned instead
  // of being sent right away
  aOut:=ReadShellOutput;
  if aOut=EmptyStr then
    // nothing yet: the panel stays, the command may still be running
    FBot.sendMessageSafe('No new messages in the terminal')
  else
  begin
    // output proves the command is over: the panel message becomes the
    // output itself (the edit drops the keyboard) when it fits one message
    if (Length(IsolateShellOutput(aOut))<_MaxMsgLen) and
       FBot.editMessageText(IsolateShellOutput(aOut), FPanelChatId, FPanelMsgId,
         pmMarkdown, True) then
      ForgetPanel
    else
    begin
      // too long to edit or the edit failed (e.g. the 48-hours limit): the
      // output goes out in parts and the panel only notes the completion
      FBot.sendMessageCode(aOut);
      MarkPanelDone;
    end;
  end;
end;

{$IFDEF UNIX}
procedure TShellThread.CallbackPanelSignal(const AToken: String; ASignal: Byte);
var
  aValue: String;
begin
  // a button of an already retired panel must never signal whatever the
  // shell runs now: only the token of the live panel passes
  if (AToken<>FPanelToken) or (not PathByToken(AToken, aValue)) then
  begin
    FBot.sendMessage(emj_Wrnng+' '+_StalePanelMsg);
    Exit;
  end;
  // the whole /sigint//sigkill logic lives in SendSIG: killpg on the shell
  // session, the output flush and the restart note
  if SendSIG(ASignal) then
    // the shell died by the signal: nothing is left to control
    MarkPanelDone;
end;
{$ENDIF}

procedure TShellThread.ConfirmDone(const AMsg: String; AReplyMarkup: TReplyMarkup;
  AMode: TParseMode);
begin
  // editing removes the previous keyboard; Telegram forbids editing
  // messages older than 48 hours, fall back to a fresh message then.
  // sendMessageSafe also survives a markup the message text breaks (400)
  // and rate limiting (429)
  if not FBot.editMessageText(AMsg, AMode, True, AReplyMarkup) then
    FBot.sendMessageSafe(AMsg, AMode, True, AReplyMarkup);
end;

procedure TShellThread.ForgetPanel;
begin
  FPanelToken:=EmptyStr;
  FPanelChatId:=0;
  FPanelMsgId:=0;
end;

procedure TShellThread.MarkPanelDone;
begin
  if FPanelMsgId=0 then
    Exit;
  // the edit drops the keyboard; the 48-hours edit limit falls back to a
  // fresh note per the ConfirmDone pattern
  if not FBot.editMessageText(_PanelDoneMsg, FPanelChatId, FPanelMsgId, pmDefault, True) then
    FBot.sendMessageSafe(_PanelDoneMsg);
  ForgetPanel;
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
    // banned users are always ignored by the sender; a simple user gets the
    // answer unless the config opts out of revealing the bot existence at all
    if not Cnfg.SilentForNonAdmins then
      FBot.sendMessage('You cannot access to this bot!');
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
  aUploadBtn: TInlineKeyboardButton;
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
    // a delete can leave the stored page empty: reopen on the last page
    // that still has entries instead
    aOffset:=ClampPageOffset(aTotal, aOffset, _MaxButtons);
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
        // the file token carries the page offset ('<offset>|<path>'): the
        // card it opens sends Back and delete-return to the same page
        aReplyMarkup.InlineKeyBoard.AddButton(aName,
          dt_file+' '+AddPathRef(IntToStr(aOffset)+_OfsSep+aFiles[i]), 2);
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
    // the upload action is the positive one: it gets its own row and the
    // Bot API 10.3 success style
    aUploadBtn:=TInlineKeyboardButton.Create('Upload...');
    aUploadBtn.callback_data:=dt_input+' '+AddPathRef(aDir);
    aUploadBtn.Style:=btnStyleSuccess;
    aReplyMarkup.InlineKeyBoard.Add.Add(aUploadBtn);
    s:=FormatPath(aDir);
    if (aOffset>0) or (aPageEnd<aTotal) then
      s+=LineEnding+Format('Entries %d-%d of %d', [aOffset+1, aOffset+aShown, aTotal]);
    if IsCallback then
      ConfirmDone(s, aReplyMarkup, pmMarkdown)
    else
      FBot.sendMessage(s, pmMarkdown, True, aReplyMarkup);
  finally
    aReplyMarkup.Free;
    aDirs.Free;
    aFiles.Free;
  end;
end;

procedure TShellThread.CallbackDownload(const AToken: String);
var
  aInfo: TFileStatInfo;
  aPath, aMsg: String;
begin
  if not PathByToken(AToken, aPath) then
  begin
    FBot.sendMessage(emj_Wrnng+' '+_StaleListMsg);
    Exit;
  end;
  aInfo:=GetFileStatInfo(aPath);
  if not aInfo.Found then
    ConfirmDone(emj_Wrnng+' '+Format('File (%s) is not available anymore', [aPath]))
  else
  if aInfo.Size>_MaxSendDocSize then
  begin
    // sending would be rejected by the Bot API anyway: save the user the
    // upload attempt and point at the shell tools that can shrink the file
    aMsg:=Format('File (%s) is %s, larger than %s the Bot API can send.',
      [aPath, FormatFileSize(aInfo.Size), FormatFileSize(_MaxSendDocSize)]);
    aMsg+=' Split or zip it via the shell, for example '+mdCode+'split -b 40M '+
      ExtractFileName(aPath)+mdCode;
    ConfirmDone(emj_Wrnng+' '+aMsg, nil, pmMarkdown);
  end
  else
  begin
    // uploading may take a while for big files: show the upload status instead
    // of a silent pause
    FBot.sendChatAction(FBot.CurrentChatId, caUploadDocument);
    FBot.sendDocumentByFileName(FBot.CurrentChatId, aPath, ExtractFileDir(aPath));
  end;
end;

procedure TShellThread.FileCardHandler(const aPath: String; aOffset: Integer);
var
  aInfo: TFileStatInfo;
  aMsg, aParent, aPageToken: String;
  aReplyMarkup: TReplyMarkup;
begin
  aInfo:=GetFileStatInfo(aPath);
  aMsg:=emj_FileFolder+' '+mdCode+ExtractFileName(aPath)+mdCode+LineEnding;
  if aInfo.Found then
  begin
    aMsg+='Size: '+FormatFileSize(aInfo.Size)+LineEnding;
    aMsg+='Modified: '+FormatDateTime('yyyy-mm-dd hh:nn:ss', aInfo.ModTime);
    if aInfo.ModeStr<>EmptyStr then
      aMsg+=LineEnding+'Mode: '+aInfo.ModeStr+', owner '+aInfo.OwnerStr;
  end
  else
    // the file may be gone since the listing was built: keep the card
    // readable instead of hiding it behind an error
    aMsg+='Size: n/a'+LineEnding+'Modified: n/a';
  aParent:=ExtractFileDir(aPath);
  if aParent=EmptyStr then
    aParent:=PathDelim;
  // the offset travels with the tokens: Back and a successful delete reopen
  // the very page of the parent listing the card was opened from
  aPageToken:=IntToStr(aOffset)+_OfsSep;
  aReplyMarkup:=TReplyMarkup.Create;
  try
    aReplyMarkup.InlineKeyBoard:=TInlineKeyboard.Create;
    BuildFileCardKeyboard(AddPathRef(aPath),
      AddPathRef(aPath), // edit: same file token the download action uses
      AddPathRef(aPageToken+aPath),
      AddPathRef(aPageToken+aParent),
      aReplyMarkup.InlineKeyBoard);
    ConfirmDone(aMsg, aReplyMarkup, pmMarkdown);
  finally
    aReplyMarkup.Free;
  end;
end;

{$IFDEF UNIX}
function TShellThread.GetFileStatInfo(const aPath: String): TFileStatInfo;
var
  aStat: TStat;
begin
  Result.Found:=False;
  Result.Size:=0;
  Result.ModTime:=0;
  Result.ModeStr:=EmptyStr;
  Result.OwnerStr:=EmptyStr;
  aStat:=Default(TStat); // FpStat is external: the compiler cannot see it
                         // filling the buffer
  if FpStat(pChar(aPath), aStat)<0 then
    Exit;  // typically the file is already gone: the card shows unknowns
  Result.Found:=True;
  Result.Size:=aStat.st_size;
  Result.ModTime:=UnixToDateTime(aStat.st_mtime);
  // numeric owner: resolving uid/gid to names would add the users package,
  // which is missing from some FPC installs this project builds with
  Result.ModeStr:=ModeToOctal(aStat.st_mode);
  Result.OwnerStr:=IntToStr(aStat.st_uid)+':'+IntToStr(aStat.st_gid);
end;
{$ENDIF}
{$IFDEF MSWINDOWS}
function TShellThread.GetFileStatInfo(const aPath: String): TFileStatInfo;
var
  sr: TSearchRec;
begin
  Result.Found:=False;
  Result.Size:=0;
  Result.ModTime:=0;
  Result.ModeStr:=EmptyStr;
  Result.OwnerStr:=EmptyStr;
  if FindFirstUTF8(aPath, faAnyFile, sr)=0 then
  try
    Result.Found:=True;
    Result.Size:=sr.Size;
    // DOS timestamps: 2-second granularity, plenty for a card display
    Result.ModTime:=FileDateToDateTime(sr.Time);
  finally
    FindCloseUTF8(sr);
  end;
end;
{$ENDIF}

class function TShellThread.FormatPath(const aPath: String): String;
begin
  Result:=emj_FileFolder+' '+mdCode+IncludeTrailingPathDelimiter(aPath)+mdCode+' '+emj_FileFolder;
end;

function TShellThread.ReadShellOutput: String;
var
  CharBuffer: array [0..511] of char;
  ReadCount: integer;
const
  SleepForOut = 100;  // Pause to wait for output
begin
  Sleep(SleepForOut);
  Result:=EmptyStr;
  while FProc.Output.NumBytesAvailable > 0 do
  begin
    ReadCount := FProc.Output.NumBytesAvailable;
    if ReadCount>Length(CharBuffer) then
      ReadCount:=Length(CharBuffer);
    FProc.Output.Read(CharBuffer{%H-}, ReadCount);
    Result+=Copy(CharBuffer, 0, ReadCount);
//    Write(StdOut, Result); // You can uncomment for debug
  end;
  {$IFDEF MSWINDOWS}Result:=WinCPToUTF8(Result);{$ENDIF}
end;

function TShellThread.OutputStd(const NoOutput: String): String;
begin
  Result:=ReadShellOutput;
  if (Result=EmptyStr) and (NoOutput<>EmptyStr) then
    FBot.sendMessageSafe(NoOutput)
  else
    if not FBot.sendMessageCode(Result) then
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

procedure TShellThread.SendRunPanel;
var
  aReplyMarkup: TReplyMarkup;
  aChatId, aMsgId: Int64;
  aToken: String;
begin
  // a new silent command replaces the previous panel: at most one live panel
  // per chat keeps the message flow readable
  MarkPanelDone;
  // the token is registered before the message exists, its FPaths value is
  // not used by the handlers: the live panel is identified by FPanelToken
  aToken:=AddPathRef('panel');
  aReplyMarkup:=TReplyMarkup.Create;
  try
    aReplyMarkup.InlineKeyBoard:=TInlineKeyboard.Create;
    BuildRunPanelKeyboard(aToken, aReplyMarkup.InlineKeyBoard);
    // the same chat resolution sendMessageSafe applies inside
    aChatId:=FBot.CurrentChatId;
    if aChatId=0 then
      aChatId:=FBot.ServiceUser;
    if FBot.sendMessageSafe(_RunPanelMsg, pmDefault, True, aReplyMarkup) then
    begin
      // the panel buttons must edit this very message later: its id comes
      // back inside the send response ('result.message_id')
      aMsgId:=0;
      if FBot.JSONResponse is TJSONObject then
        aMsgId:=TJSONObject(FBot.JSONResponse).Get('message_id', 0);
      if aMsgId>0 then
      begin
        FPanelToken:=aToken;
        FPanelChatId:=aChatId;
        FPanelMsgId:=aMsgId;
      end;
    end;
  finally
    aReplyMarkup.Free;
  end;
end;

procedure TShellThread.SendToShellTerminal(const InputString: String);
var
  aOut: String;
begin
  // both the interactive commands and the script runs end up here: show the
  // typing status while the command makes its way through the shell pipes
  FBot.sendChatAction(FBot.CurrentChatId, caTyping);
  if OutputStd<>EmptyStr then
    MarkPanelDone;  // the previous command just finished printing: its panel is over
  FProc.Input.Write(InputString[1], Length(InputString));
  aOut:=OutputStd;
  if aOut=EmptyStr then
    // nothing printed right away: the command may run long, hand out the
    // control panel instead of leaving the user to guess and type /sigint//read
    SendRunPanel
  else
    MarkPanelDone;  // output arrived, the command is over
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

{ Sends a signal to the shell session; returns True when the shell died by
  the signal and was restarted, so the command cannot come back }
function TShellThread.SendSIG(SigNumber: Byte): Boolean;
var
  i: Integer;
begin
  Result:=False;
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
    Result:=True;
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
var
  aCommands: TBotCommandArray;
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
  aCommands:=TBotCommandArray.Create;
  try
    BuildBotCommands(aCommands);
    // the menu is a convenience: a failed registration must not prevent the
    // bot from starting (network errors are already swallowed into False)
    if not FBot.setMyCommands(aCommands) then
      Logger.Error('Failed to register bot commands (error '+
        IntToStr(FBot.LastErrorCode)+')');
  finally
    aCommands.Free;
  end;
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

