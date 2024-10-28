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
  configuration, fpjson
  ;

function IsolateShellOutput(const S: String): String;
begin
  Result:='```log'+LineEnding+S+LineEnding+'```';
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
  MsgPartLength = 3500;  // Length of one message part for the splitting
begin
  Result:=False;
  if AMessage<>EmptyStr then
  begin
    Msg:=IsolateShellOutput(AMessage);
    if Length(Msg)<MaxMsgLength then
      Result:=sendMessageSafe(Msg, pmMarkdown, False, ReplyMarkup)
    else begin
      MsgRest:=AMessage;
      while MsgRest<>EmptyStr do
      begin
        Msg:=LeftStr(MsgRest, MsgPartLength);
        MsgRest:=RightStr(AMessage, Length(MsgRest)-Length(Msg));
        Result:=sendMessageSafe(IsolateShellOutput(Msg), pmMarkdown, False, ReplyMarkup);
        Sleep(300);
      end;
    end;
  end
  else
    Result:=True;
end;

function TTgShBot.sendMessageSafe(const AMessage: String;
  ParseMode: TParseMode; DisableWebPagePreview: Boolean;
  ReplyMarkup: TReplyMarkup; ReplyToMessageID: Integer): Boolean;
var
  SleepTime: Integer;
  aChatID: Int64;
begin
  aChatID:=CurrentChatId;
  if CurrentChatId=0 then
    aChatID:=FServiceUser;
  Result:=sendMessage(aChatID, AMessage, ParseMode, DisableWebPagePreview, ReplyMarkup, ReplyToMessageID);
  if not Result then
    if LastErrorCode=429 then       // Too many requests
    begin
      try
        SleepTime:=(JSONResponse as TJSONObject).Integers['retry_after']*MSecsPerSec;
      except
        SleepTime:=20*MSecsPerSec;
      end;
      sleep(SleepTime); { TODO : Provide protection from unlikely loops }
      Result:=sendMessageSafe(AMessage, ParseMode, DisableWebPagePreview, ReplyMarkup, ReplyToMessageID);
    end;
end;

end.

