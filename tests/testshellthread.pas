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

  { TTestFileCardHelpers }

  TTestFileCardHelpers = class(TTestCase)
  published
    procedure TestFormatFileSizeWholeBytes;
    procedure TestFormatFileSizeScaled;
    procedure TestFormatFileSizeLargestUnitTakesAll;
    procedure TestModeToOctalPadsLeadingZero;
    procedure TestModeToOctalSpecialBits;
    procedure TestClampPageOffsetKeepsFittingPage;
    procedure TestClampPageOffsetReturnsToLastPage;
    procedure TestClampPageOffsetEmptyOrInvalid;
  end;

  { TTestFileCardKeyboard }

  TTestFileCardKeyboard = class(TTestCase)
  published
    procedure TestThreeButtonsInOneRow;
    procedure TestButtonsCarryVerbsAndStyles;
  end;

  { TTestRunPanelKeyboard }

  TTestRunPanelKeyboard = class(TTestCase)
  published
    {$IFDEF UNIX}
    procedure TestThreeButtonsInOneRow;
    procedure TestButtonsCarryVerbsAndStyles;
    {$ELSE}
    procedure TestOnlyReadButtonOutsideUnix;
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

{ TTestFileCardHelpers }

procedure TTestFileCardHelpers.TestFormatFileSizeWholeBytes;
begin
  // under 1 KiB the count stays in whole bytes, no '.0' noise
  AssertEquals('0 B', FormatFileSize(0));
  AssertEquals('1 B', FormatFileSize(1));
  AssertEquals('511 B', FormatFileSize(511));
  AssertEquals('1023 B', FormatFileSize(1023));
end;

procedure TTestFileCardHelpers.TestFormatFileSizeScaled;
begin
  // scaled units are 1024-based with exactly one decimal digit, floor-rounded
  AssertEquals('1.0 K', FormatFileSize(1024));
  AssertEquals('1.5 K', FormatFileSize(1536));
  AssertEquals('1023.9 K', FormatFileSize(1024*1024-1));
  AssertEquals('1.0 M', FormatFileSize(1024*1024));
  AssertEquals('1.5 M', FormatFileSize(1536*1024));
  AssertEquals('1.0 G', FormatFileSize(1024*1024*1024));
end;

procedure TTestFileCardHelpers.TestFormatFileSizeLargestUnitTakesAll;
begin
  // everything past T stays in T: servers do hold such files and the UI
  // ladder stops here on purpose
  AssertEquals('1.0 T', FormatFileSize(Int64(1024)*1024*1024*1024));
  AssertEquals('2.0 T', FormatFileSize(Int64(2048)*1024*1024*1024));
end;

procedure TTestFileCardHelpers.TestModeToOctalPadsLeadingZero;
begin
  // the leading zero is part of the display, 644 alone reads wrong
  AssertEquals('0000', ModeToOctal(0));
  AssertEquals('0644', ModeToOctal($1A4));
  AssertEquals('0755', ModeToOctal($1ED));
end;

procedure TTestFileCardHelpers.TestModeToOctalSpecialBits;
begin
  // suid/sgid/sticky live in the 4th octal digit: they must not be lost
  AssertEquals('4755', ModeToOctal($9ED));
  AssertEquals('1777', ModeToOctal($3FF));
end;

procedure TTestFileCardHelpers.TestClampPageOffsetKeepsFittingPage;
begin
  // a valid offset (a page that has entries) must survive untouched
  AssertEquals(0, ClampPageOffset(50, 0, 90));
  AssertEquals(90, ClampPageOffset(100, 90, 90));
  AssertEquals(90, ClampPageOffset(95, 90, 90));
end;

procedure TTestFileCardHelpers.TestClampPageOffsetReturnsToLastPage;
begin
  // an offset past the end (a delete emptied the last page) lands on the
  // last page that still has entries
  AssertEquals(0, ClampPageOffset(50, 90, 90));
  AssertEquals(90, ClampPageOffset(100, 180, 90));
  // a single remaining entry only fills page 0, however far the old
  // offset pointed
  AssertEquals(0, ClampPageOffset(1, 90, 90));
end;

procedure TTestFileCardHelpers.TestClampPageOffsetEmptyOrInvalid;
begin
  AssertEquals(0, ClampPageOffset(0, 90, 90));
  AssertEquals(0, ClampPageOffset(-1, 90, 90));
  AssertEquals(0, ClampPageOffset(50, 90, 0));
end;

{ TTestFileCardKeyboard }

procedure TTestFileCardKeyboard.TestThreeButtonsInOneRow;
var
  Keyboard: TInlineKeyboard;
  Row: TInlineKeyboardButtons;
begin
  Keyboard:=TInlineKeyboard.Create;
  try
    BuildFileCardKeyboard('11', '12', '13', Keyboard);
    AssertEquals('actions share one row', 1, Keyboard.Count);
    Row:=TInlineKeyboardButtons(Keyboard.Items[0]);
    AssertEquals(3, Row.Count);
    // expected values are spelled out: the test pins the exact Bot API wire
    // values, not what the shellthread constants happen to expand to
    AssertEquals('⬇ Download', TInlineKeyboardButton(Row.Items[0]).text);
    AssertEquals('🗑 Delete', TInlineKeyboardButton(Row.Items[1]).text);
    AssertEquals('◀ Back', TInlineKeyboardButton(Row.Items[2]).text);
  finally
    Keyboard.Free;
  end;
end;

procedure TTestFileCardKeyboard.TestButtonsCarryVerbsAndStyles;
var
  Keyboard: TInlineKeyboard;
  Row: TInlineKeyboardButtons;
begin
  Keyboard:=TInlineKeyboard.Create;
  try
    BuildFileCardKeyboard('11', '12', '13', Keyboard);
    Row:=TInlineKeyboardButtons(Keyboard.Items[0]);
    // download is the positive action, delete the destructive one, back the
    // neutral escape route; delete only ASKS (askdelete): the deletion itself
    // is performed by the Ok button of the confirmation, never by the card
    AssertEquals('download 11', TInlineKeyboardButton(Row.Items[0]).callback_data);
    AssertEquals('success', TInlineKeyboardButton(Row.Items[0]).Style);
    AssertEquals('askdelete 12', TInlineKeyboardButton(Row.Items[1]).callback_data);
    AssertEquals('danger', TInlineKeyboardButton(Row.Items[1]).Style);
    AssertEquals('dir 13', TInlineKeyboardButton(Row.Items[2]).callback_data);
    AssertEquals('primary', TInlineKeyboardButton(Row.Items[2]).Style);
  finally
    Keyboard.Free;
  end;
end;

{ TTestRunPanelKeyboard }

{$IFDEF UNIX}
procedure TTestRunPanelKeyboard.TestThreeButtonsInOneRow;
var
  Keyboard: TInlineKeyboard;
  Row: TInlineKeyboardButtons;
begin
  Keyboard:=TInlineKeyboard.Create;
  try
    BuildRunPanelKeyboard('42', Keyboard);
    AssertEquals('actions share one row', 1, Keyboard.Count);
    Row:=TInlineKeyboardButtons(Keyboard.Items[0]);
    AssertEquals(3, Row.Count);
    // expected values are spelled out: the test pins the exact Bot API wire
    // values, not what the shellthread constants happen to expand to
    AssertEquals('⏹ SIGINT', TInlineKeyboardButton(Row.Items[0]).text);
    AssertEquals('✖ SIGKILL', TInlineKeyboardButton(Row.Items[1]).text);
    AssertEquals('📖 Read', TInlineKeyboardButton(Row.Items[2]).text);
  finally
    Keyboard.Free;
  end;
end;

procedure TTestRunPanelKeyboard.TestButtonsCarryVerbsAndStyles;
var
  Keyboard: TInlineKeyboard;
  Row: TInlineKeyboardButtons;
begin
  Keyboard:=TInlineKeyboard.Create;
  try
    BuildRunPanelKeyboard('42', Keyboard);
    Row:=TInlineKeyboardButtons(Keyboard.Items[0]);
    // both signal buttons are destructive (they may restart the shell), read
    // is the neutral one
    AssertEquals('sigint 42', TInlineKeyboardButton(Row.Items[0]).callback_data);
    AssertEquals('danger', TInlineKeyboardButton(Row.Items[0]).Style);
    AssertEquals('sigkill 42', TInlineKeyboardButton(Row.Items[1]).callback_data);
    AssertEquals('danger', TInlineKeyboardButton(Row.Items[1]).Style);
    AssertEquals('read 42', TInlineKeyboardButton(Row.Items[2]).callback_data);
    AssertEquals('primary', TInlineKeyboardButton(Row.Items[2]).Style);
  finally
    Keyboard.Free;
  end;
end;
{$ELSE}
procedure TTestRunPanelKeyboard.TestOnlyReadButtonOutsideUnix;
var
  Keyboard: TInlineKeyboard;
  Row: TInlineKeyboardButtons;
begin
  Keyboard:=TInlineKeyboard.Create;
  try
    BuildRunPanelKeyboard('42', Keyboard);
    // signals are delivered by killpg and have no meaning off UNIX: the panel
    // carries Read only
    AssertEquals('single Read row', 1, Keyboard.Count);
    Row:=TInlineKeyboardButtons(Keyboard.Items[0]);
    AssertEquals(1, Row.Count);
    AssertEquals('📖 Read', TInlineKeyboardButton(Row.Items[0]).text);
    AssertEquals('primary', TInlineKeyboardButton(Row.Items[0]).Style);
    AssertEquals('read 42', TInlineKeyboardButton(Row.Items[0]).callback_data);
  finally
    Keyboard.Free;
  end;
end;
{$ENDIF}

initialization
  RegisterTest(TTestDeleteConfirmMarkup);
  RegisterTest(TTestBotCommandList);
  RegisterTest(TTestFileCardHelpers);
  RegisterTest(TTestFileCardKeyboard);
  RegisterTest(TTestRunPanelKeyboard);

end.
