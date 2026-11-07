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
  Msg, MsgRest: String;
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
      MsgRest:=AMessage;
      while MsgRest<>EmptyStr do
      begin
        // split on character boundaries: a byte cut may break a multibyte
        // character and Telegram rejects invalid UTF-8
        Msg:=UTF8Copy(MsgRest, 1, MsgPartLength);
        MsgRest:=UTF8Copy(MsgRest, MsgPartLength+1, MaxInt);
        Result:=sendMessageSafe(IsolateShellOutput(Msg), pmMarkdown, False, ReplyMarkup);
        Sleep(300);
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
end;

end.

