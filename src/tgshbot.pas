unit tgshbot;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tgsendertypes
  ;

type

  { TTgShBot }

  TTgShBot = class (TTelegramSender)
  private
    FServiceUser: Int64;
  protected
    function IsAdminUser(ChatID: Int64): Boolean; override;
    function IsBanned(ChatID: Int64): Boolean; override;
  public
  {   Independently split too large messages and waits in case of error 429 }
    function sendMessageCode(const AMessage: String; ReplyMarkup: TReplyMarkup = nil): Boolean; overload;
    function sendMessageSafe(const AMessage: String; ParseMode: TParseMode = pmDefault;
      DisableWebPagePreview: Boolean=False; ReplyMarkup: TReplyMarkup = nil;
      ReplyToMessageID: Integer = 0): Boolean; overload;
    property ServiceUser: Int64 read FServiceUser write FServiceUser;
  end;

function IsolateShellOutput(const S: String): String;

// ----- Text / UTF-8 validation ---------------------------------------------

{ Maximum file size (1 MB) for text editing: the user must be able to hold the
  original content in memory and send the replacement, so the Telegram document
  limit of 50 MB is not the right ceiling here }
const
  _MaxEditSize = 1 * 1024 * 1024;

{ Scans the first ACheckBytes bytes of ABuffer for validity as UTF-8.
  Returns True when every byte is either a valid single-byte ASCII (< 128) or
  a well-formed multibyte UTF-8 sequence, and no #0 occurs within the checked
  prefix. ACheckBytes may be larger than Length(ABuffer) — the whole buffer is
  then scanned. Used for the byte-by-byte UTF-8 check that precedes text editing. }
function IsValidUtf8Buffer(const ABuffer; ABufferLen, ACheckBytes: PtrUInt): Boolean;

{ Returns True when the file at AFilePath is a text file suitable for editing:
  size is at most _MaxEditSize, it contains no #0 bytes in its first
  _MaxEditSize bytes, and that prefix is valid UTF-8.
  Raises no exceptions (assertions disabled inside for reliability). }
function IsTextFile(const AFilePath: String): Boolean;

{ Formats the backup file name for AFilePath: <name>.bak is appended to the
  directory part, e.g. /home/user/data.txt -> /home/user/data.txt.bak }
function BackupPathFor(const AFilePath: String): String;

{ Splits AMessage into parts of at most APartLength UTF-8 characters, never
  cutting inside a multibyte character: a byte cut would produce invalid
  UTF-8 and Telegram rejects such messages. The concatenation of the parts
  reproduces AMessage exactly; an empty message produces no parts.
  APartLength must be positive. }
procedure SplitUTF8Parts(const AMessage: String; APartLength: Integer; AParts: TStrings);

implementation

uses
  configuration, fpjson, LazUTF8
  ;

function IsolateShellOutput(const S: String): String;
begin
  // A stray ``` inside the shell output would close the markdown fence and the
  // whole message would be rejected by Telegram: break it with zero-width space
  Result:='```log'+LineEnding+
    StringReplace(S, '```', '``'+#$E2#$80#$8B+'`', [rfReplaceAll])+LineEnding+'```';
end;

function IsValidUtf8Buffer(const ABuffer; ABufferLen, ACheckBytes: PtrUInt): Boolean;
var
  I: PtrUInt;
  B: Byte;
  SeqLen: Integer;
const
  MaxCheck = 64 * 1024; // scan at most 64 KB for #0 and UTF-8 validity
begin
  if ACheckBytes = 0 then
    ACheckBytes := MaxCheck;
  if ACheckBytes > ABufferLen then
    ACheckBytes := ABufferLen;
  I := 0;
  while I < ACheckBytes do
  begin
    B := PByte(@ABuffer)[I];
    Inc(I);
    // #0 inside the checked prefix disqualifies the file as text
    if B = 0 then
      Exit(False);
    // ASCII byte: valid UTF-8, move to next
    if B < $80 then
      Continue;
    // Determine expected sequence length from the leading byte
    if (B and $E0) = $C0 then
      SeqLen := 2  // 110xxxxx: one continuation byte
    else if (B and $F0) = $E0 then
      SeqLen := 3  // 1110xxxx: two continuation bytes
    else if (B and $F8) = $F0 then
      SeqLen := 4  // 11110xxx: three continuation bytes
    else
      Exit(False); // invalid lead byte (10xxxxxx is orphan, or F5+ is invalid)
    // Verify that enough bytes remain for the complete sequence
    if I + PtrUInt(SeqLen - 1) > ACheckBytes then
      Exit(False);
    // Check continuation bytes
    case SeqLen of
      2: if (PByte(@ABuffer)[I] and $C0) <> $80 then Exit(False);
      3: begin
           if (PByte(@ABuffer)[I] and $C0) <> $80 then Exit(False);
           if (PByte(@ABuffer)[I + 1] and $C0) <> $80 then Exit(False);
         end;
      4: begin
           if (PByte(@ABuffer)[I] and $C0) <> $80 then Exit(False);
           if (PByte(@ABuffer)[I + 1] and $C0) <> $80 then Exit(False);
           if (PByte(@ABuffer)[I + 2] and $C0) <> $80 then Exit(False);
         end;
    end;
    Inc(I, PtrUInt(SeqLen - 1));
  end;
  Result := True;
end;

function IsTextFile(const AFilePath: String): Boolean;
var
  f: TFileStream;
  Buf: array[0..Pred(_MaxEditSize)] of Byte;
  BytesRead: PtrUInt;
begin
  Result := False;
  f := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyWrite);
  try
    if f.Size > _MaxEditSize then
      Exit;
    BytesRead := f.Read(Buf, SizeOf(Buf));
    Result := IsValidUtf8Buffer(Buf, BytesRead, BytesRead);
  finally
    f.Free;
  end;
end;

function BackupPathFor(const AFilePath: String): String;
begin
  Result := AFilePath + '.bak';
end;

procedure SplitUTF8Parts(const AMessage: String; APartLength: Integer;
  AParts: TStrings);
var
  MsgRest: String;
begin
  if APartLength<1 then
    raise EArgumentException.Create('Part length must be positive');
  MsgRest:=AMessage;
  while MsgRest<>EmptyStr do
  begin
    AParts.Add(UTF8Copy(MsgRest, 1, APartLength));
    MsgRest:=UTF8Copy(MsgRest, APartLength+1, MaxInt);
  end;
end;

{ TTgShBot }

function TTgShBot.IsAdminUser(ChatID: Int64): Boolean;
begin
  Result:=Cnfg.Users[ChatID]=usAdmin;
end;

function TTgShBot.IsBanned(ChatID: Int64): Boolean;
begin
  Result:=Cnfg.Users[ChatID]=usBanned;
end;

function TTgShBot.sendMessageCode(const AMessage: String;
  ReplyMarkup: TReplyMarkup): Boolean;
var
  Msg: String;
  Parts: TStringList;
  i: Integer;
const
  MaxMsgLength = 4096;   // Maximum message length to send
  MsgPartLength = 3500;  // Length of one message part for the splitting (UTF-8 characters)
begin
  Result:=True;
  if AMessage<>EmptyStr then
  begin
    Msg:=IsolateShellOutput(AMessage);
    if Length(Msg)<MaxMsgLength then
      Result:=sendMessageSafe(Msg, pmMarkdown, False, ReplyMarkup)
    else begin
      Parts:=TStringList.Create;
      try
        SplitUTF8Parts(AMessage, MsgPartLength, Parts);
        for i:=0 to Parts.Count-1 do
        begin
          Result:=sendMessageSafe(IsolateShellOutput(Parts[i]), pmMarkdown, False, ReplyMarkup);
          Sleep(300);
        end;
      finally
        Parts.Free;
      end;
    end;
  end;
end;

function TTgShBot.sendMessageSafe(const AMessage: String;
  ParseMode: TParseMode; DisableWebPagePreview: Boolean;
  ReplyMarkup: TReplyMarkup; ReplyToMessageID: Integer): Boolean;
var
  i, SleepTime: Integer;
  aChatID: Int64;
const
  MaxRetries = 5;        // consecutive 429 attempts
begin
  aChatID:=CurrentChatId;
  if CurrentChatId=0 then
    aChatID:=FServiceUser;
  Result:=sendMessage(aChatID, AMessage, ParseMode, DisableWebPagePreview, ReplyMarkup, ReplyToMessageID);
  i:=0;
  while (not Result) and (LastErrorCode=429) and (i<MaxRetries) do
  begin
    Inc(i);
    try
      SleepTime:=(JSONResponse as TJSONObject).Integers['retry_after']*MSecsPerSec;
    except
      SleepTime:=20*MSecsPerSec;
    end;
    sleep(SleepTime);
    Result:=sendMessage(aChatID, AMessage, ParseMode, DisableWebPagePreview, ReplyMarkup, ReplyToMessageID);
  end;
  // Telegram rejects some markup (e.g. unbalanced markdown entities) with 400:
  // repeating the very same text without a parse mode keeps the message from
  // being lost, the shell output is more valuable than its formatting
  if (not Result) and (LastErrorCode=400) and (ParseMode<>pmDefault) then
  begin
    InfoMessage('Send rejected with 400, retried the same text without parse mode');
    Result:=sendMessage(aChatID, AMessage, pmDefault, DisableWebPagePreview, ReplyMarkup, ReplyToMessageID);
  end;
end;

end.

