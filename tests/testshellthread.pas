unit testshellthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, tgsendertypes, shellthread
  ;

type

  { TTestDeleteConfirmMarkup }

  TTestDeleteConfirmMarkup = class(TTestCase)
  published
    procedure TestOkButtonDangerWithDeleteCallback;
    procedure TestCancelButtonPrimaryWithCancelCallback;
    procedure TestButtonsJSONCarryStyleAndCallbackData;
  end;

  { TTestBotCommandList }

  TTestBotCommandList = class(TTestCase)
  private
    function IndexOfCommand(ACommands: TBotCommandArray;
      const AName: String): Integer;
  published
    procedure TestCoreCommandsRegistered;
    {$IFDEF UNIX}
    procedure TestSigRegisteredOnUnix;
    {$ELSE}
    procedure TestSigNotRegisteredOutsideUnix;
    {$ENDIF}
  end;

implementation

{ TTestDeleteConfirmMarkup }

procedure TTestDeleteConfirmMarkup.TestOkButtonDangerWithDeleteCallback;
var
  Keyboard: TInlineKeyboard;
  Row: TInlineKeyboardButtons;
begin
  Keyboard:=TInlineKeyboard.Create;
  try
    BuildDeleteConfirmKeyboard('42', Keyboard);
    AssertEquals('single confirmation row', 1, Keyboard.Count);
    Row:=TInlineKeyboardButtons(Keyboard.Items[0]);
    AssertEquals(2, Row.Count);
    // expected values are spelled out: the test pins the exact Bot API wire
    // values, not what the shellthread constants happen to expand to
    AssertEquals('Ok', TInlineKeyboardButton(Row.Items[0]).text);
    AssertEquals('danger', TInlineKeyboardButton(Row.Items[0]).Style);
    AssertEquals('delete 42', TInlineKeyboardButton(Row.Items[0]).callback_data);
  finally
    Keyboard.Free;
  end;
end;

procedure TTestDeleteConfirmMarkup.TestCancelButtonPrimaryWithCancelCallback;
var
  Keyboard: TInlineKeyboard;
  Row: TInlineKeyboardButtons;
begin
  Keyboard:=TInlineKeyboard.Create;
  try
    BuildDeleteConfirmKeyboard('42', Keyboard);
    Row:=TInlineKeyboardButtons(Keyboard.Items[0]);
    AssertEquals('Cancel', TInlineKeyboardButton(Row.Items[1]).text);
    AssertEquals('primary', TInlineKeyboardButton(Row.Items[1]).Style);
    // cancel carries no token: it never resolves anything in FPaths
    AssertEquals('cancel', TInlineKeyboardButton(Row.Items[1]).callback_data);
  finally
    Keyboard.Free;
  end;
end;

procedure TTestDeleteConfirmMarkup.TestButtonsJSONCarryStyleAndCallbackData;
var
  Keyboard: TInlineKeyboard;
  Markup: TJSONArray;
  OkBtn, CancelBtn: TJSONObject;
begin
  Keyboard:=TInlineKeyboard.Create;
  try
    BuildDeleteConfirmKeyboard('7', Keyboard);
    // serialize exactly as the sender puts it on the wire and read the raw
    // JSON back: the style key must survive serialization untouched
    Markup:=TJSONArray(GetJSON(Keyboard.AsJSON));
    try
      AssertEquals(1, Markup.Count);
      OkBtn:=Markup.Arrays[0].Objects[0];
      CancelBtn:=Markup.Arrays[0].Objects[1];
      AssertEquals('danger', OkBtn.Strings['style']);
      AssertEquals('delete 7', OkBtn.Strings['callback_data']);
      AssertEquals('primary', CancelBtn.Strings['style']);
      AssertEquals('cancel', CancelBtn.Strings['callback_data']);
    finally
      Markup.Free;
    end;
  finally
    Keyboard.Free;
  end;
end;

{ TTestBotCommandList }

function TTestBotCommandList.IndexOfCommand(ACommands: TBotCommandArray;
  const AName: String): Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to ACommands.Count-1 do
    if TBotCommand(ACommands.Items[i]).Command=AName then
      Exit(i);
end;

procedure TTestBotCommandList.TestCoreCommandsRegistered;
var
  Commands: TBotCommandArray;
begin
  Commands:=TBotCommandArray.Create;
  try
    BuildBotCommands(Commands);
    // /sig, when present, is appended last, so the core four lead the menu on
    // every platform; expected values are spelled out: the test pins the exact
    // menu the Telegram clients show, not what the constants expand to
    AssertTrue('core commands lead the menu', Commands.Count>=4);
    AssertEquals('/dir', TBotCommand(Commands.Items[0]).Command);
    AssertEquals('file manager', TBotCommand(Commands.Items[0]).Description);
    AssertEquals('/scripts', TBotCommand(Commands.Items[1]).Command);
    AssertEquals('run prepared scripts', TBotCommand(Commands.Items[1]).Description);
    AssertEquals('/read', TBotCommand(Commands.Items[2]).Command);
    AssertEquals('read shell output', TBotCommand(Commands.Items[2]).Description);
    AssertEquals('/delete', TBotCommand(Commands.Items[3]).Command);
    AssertEquals('delete a file', TBotCommand(Commands.Items[3]).Description);
  finally
    Commands.Free;
  end;
end;

{$IFDEF UNIX}
procedure TTestBotCommandList.TestSigRegisteredOnUnix;
var
  Commands: TBotCommandArray;
begin
  Commands:=TBotCommandArray.Create;
  try
    BuildBotCommands(Commands);
    AssertEquals(5, Commands.Count);
    AssertEquals('/sig', TBotCommand(Commands.Items[4]).Command);
    AssertEquals('send a signal to the shell process',
      TBotCommand(Commands.Items[4]).Description);
  finally
    Commands.Free;
  end;
end;
{$ELSE}
procedure TTestBotCommandList.TestSigNotRegisteredOutsideUnix;
var
  Commands: TBotCommandArray;
begin
  Commands:=TBotCommandArray.Create;
  try
    BuildBotCommands(Commands);
    // signals are delivered by FpKill and have no meaning off UNIX
    AssertEquals(4, Commands.Count);
    AssertTrue('no /sig off UNIX', IndexOfCommand(Commands, '/sig')<0);
  finally
    Commands.Free;
  end;
end;
{$ENDIF}

initialization
  RegisterTest(TTestDeleteConfirmMarkup);
  RegisterTest(TTestBotCommandList);

end.
