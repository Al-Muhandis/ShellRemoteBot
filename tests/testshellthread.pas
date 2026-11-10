unit testshellthread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, tgtypes, tgsendertypes,
  shellthread
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
    procedure TestFourButtonsInOneRow;
    procedure TestButtonsCarryVerbsAndStyles;
  end;

  { TTestRevertConfirmKeyboard }

  TTestRevertConfirmKeyboard = class(TTestCase)
  published
    procedure TestTwoButtonsInOneRow;
    procedure TestOkButtonDangerWithRevertCallback;
    procedure TestCancelButtonPrimaryWithCancelCallback;
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

  { TTestRichMessageInput }

  TTestRichMessageInput = class(TTestCase)
  private
    { Builds a TTelegramMessageObj from the update JSON as the Bot API
      delivers it (message_id, date and chat are read unconditionally by the
      fp-telegram constructor) }
    function MakeMessage(const AJSON: String): TTelegramMessageObj;
  published
    procedure TestPlainTextInput;
    procedure TestRichMessagePlainTextFallback;
    procedure TestRichMessageBlocksJoinedWithLineEndings;
    procedure TestNoTextNoRichYieldsEmpty;
    procedure TestPlainTextPreferredOverRich;
  end;

  { TTestRichListing }

  TTestRichListing = class(TTestCase)
  published
    procedure TestTextTravelsAsMarkdown;
    procedure TestKeyboardRowsMirroredToButtonsBlocks;
    procedure TestKeyboardStylesCopied;
    procedure TestNilKeyboardYieldsNoButtonsBlocks;
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

procedure TTestFileCardKeyboard.TestFourButtonsInOneRow;
var
  Keyboard: TInlineKeyboard;
  Row: TInlineKeyboardButtons;
begin
  Keyboard:=TInlineKeyboard.Create;
  try
    BuildFileCardKeyboard('11', '12', '13', '14', Keyboard);
    AssertEquals('actions share one row', 1, Keyboard.Count);
    Row:=TInlineKeyboardButtons(Keyboard.Items[0]);
    AssertEquals(4, Row.Count);
    // expected values are spelled out: the test pins the exact Bot API wire
    // values, not what the shellthread constants happen to expand to
    AssertEquals('⬇ Download', TInlineKeyboardButton(Row.Items[0]).text);
    AssertEquals('✏ Edit', TInlineKeyboardButton(Row.Items[1]).text);
    AssertEquals('🗑 Delete', TInlineKeyboardButton(Row.Items[2]).text);
    AssertEquals('◀ Back', TInlineKeyboardButton(Row.Items[3]).text);
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
    BuildFileCardKeyboard('11', '12', '13', '14', Keyboard);
    Row:=TInlineKeyboardButtons(Keyboard.Items[0]);
    // download is the positive action, edit is the main workflow (primary),
    // delete the destructive one, back the neutral escape route; delete only
    // ASKS (askdelete): the deletion itself is performed by the Ok button
    // of the confirmation, never by the card
    AssertEquals('download 11', TInlineKeyboardButton(Row.Items[0]).callback_data);
    AssertEquals('success', TInlineKeyboardButton(Row.Items[0]).Style);
    AssertEquals('edit 12', TInlineKeyboardButton(Row.Items[1]).callback_data);
    AssertEquals('primary', TInlineKeyboardButton(Row.Items[1]).Style);
    AssertEquals('askdelete 13', TInlineKeyboardButton(Row.Items[2]).callback_data);
    AssertEquals('danger', TInlineKeyboardButton(Row.Items[2]).Style);
    AssertEquals('dir 14', TInlineKeyboardButton(Row.Items[3]).callback_data);
    AssertEquals('primary', TInlineKeyboardButton(Row.Items[3]).Style);
  finally
    Keyboard.Free;
  end;
end;

{ TTestRevertConfirmKeyboard }

procedure TTestRevertConfirmKeyboard.TestTwoButtonsInOneRow;
var
  Keyboard: TInlineKeyboard;
  Row: TInlineKeyboardButtons;
begin
  Keyboard:=TInlineKeyboard.Create;
  try
    BuildRevertConfirmKeyboard('42', Keyboard);
    AssertEquals('single confirmation row', 1, Keyboard.Count);
    Row:=TInlineKeyboardButtons(Keyboard.Items[0]);
    AssertEquals(2, Row.Count);
    AssertEquals('Ok', TInlineKeyboardButton(Row.Items[0]).text);
    AssertEquals('Cancel', TInlineKeyboardButton(Row.Items[1]).text);
  finally
    Keyboard.Free;
  end;
end;

procedure TTestRevertConfirmKeyboard.TestOkButtonDangerWithRevertCallback;
var
  Keyboard: TInlineKeyboard;
  Row: TInlineKeyboardButtons;
begin
  Keyboard:=TInlineKeyboard.Create;
  try
    BuildRevertConfirmKeyboard('99', Keyboard);
    Row:=TInlineKeyboardButtons(Keyboard.Items[0]);
    AssertEquals('revert 99', TInlineKeyboardButton(Row.Items[0]).callback_data);
    AssertEquals('danger', TInlineKeyboardButton(Row.Items[0]).Style);
  finally
    Keyboard.Free;
  end;
end;

procedure TTestRevertConfirmKeyboard.TestCancelButtonPrimaryWithCancelCallback;
var
  Keyboard: TInlineKeyboard;
  Row: TInlineKeyboardButtons;
begin
  Keyboard:=TInlineKeyboard.Create;
  try
    BuildRevertConfirmKeyboard('7', Keyboard);
    Row:=TInlineKeyboardButtons(Keyboard.Items[0]);
    AssertEquals('cancel', TInlineKeyboardButton(Row.Items[1]).callback_data);
    AssertEquals('primary', TInlineKeyboardButton(Row.Items[1]).Style);
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

{ TTestRichMessageInput }

function TTestRichMessageInput.MakeMessage(const AJSON: String): TTelegramMessageObj;
var
  aJSONData: TJSONData;
begin
  // the fp-telegram constructor clones the JSON, so the parsed copy is
  // freed right away and only the message object survives
  aJSONData:=GetJSON(AJSON);
  try
    Result:=TTelegramMessageObj.Create(aJSONData as TJSONObject);
  finally
    aJSONData.Free;
  end;
end;

procedure TTestRichMessageInput.TestPlainTextInput;
var
  Msg: TTelegramMessageObj;
begin
  // a classic client message: the text passes through untouched
  Msg:=MakeMessage('{"message_id":1,"date":0,"chat":{"id":1},"text":"ls -la"}');
  try
    AssertEquals('ls -la', MessageInputText(Msg));
  finally
    Msg.Free;
  end;
end;

procedure TTestRichMessageInput.TestRichMessagePlainTextFallback;
var
  Msg: TTelegramMessageObj;
begin
  // a styled message of a fresh client (Bot API 10.1+): no text field, the
  // command lives in the rich message blocks
  Msg:=MakeMessage('{"message_id":2,"date":0,"chat":{"id":1},' +
    '"rich_message":{"blocks":[{"type":"text","text":"echo hi"}]}}');
  try
    AssertEquals('echo hi', MessageInputText(Msg));
  finally
    Msg.Free;
  end;
end;

procedure TTestRichMessageInput.TestRichMessageBlocksJoinedWithLineEndings;
var
  Msg: TTelegramMessageObj;
begin
  // several text blocks read as one input, line-joined like the PlainText
  // of the fp-telegram rich message does
  Msg:=MakeMessage('{"message_id":3,"date":0,"chat":{"id":1},' +
    '"rich_message":{"blocks":[{"type":"text","text":"echo"},' +
    '{"type":"text","text":"hi"}]}}');
  try
    AssertEquals('echo'+LineEnding+'hi', MessageInputText(Msg));
  finally
    Msg.Free;
  end;
end;

procedure TTestRichMessageInput.TestNoTextNoRichYieldsEmpty;
var
  Msg: TTelegramMessageObj;
begin
  // neither text nor rich message (a sticker, a service message): the
  // caller must see the empty string it used to see
  Msg:=MakeMessage('{"message_id":4,"date":0,"chat":{"id":1}}');
  try
    AssertEquals('', MessageInputText(Msg));
  finally
    Msg.Free;
  end;
end;

procedure TTestRichMessageInput.TestPlainTextPreferredOverRich;
var
  Msg: TTelegramMessageObj;
begin
  // the plain text wins when both are present: it is the authoritative
  // content, the rich blocks are only the fallback
  Msg:=MakeMessage('{"message_id":5,"date":0,"chat":{"id":1},"text":"plain",' +
    '"rich_message":{"blocks":[{"type":"text","text":"rich"}]}}');
  try
    AssertEquals('plain', MessageInputText(Msg));
  finally
    Msg.Free;
  end;
end;

{ TTestRichListing }

procedure TTestRichListing.TestTextTravelsAsMarkdown;
var
  Rich: TInputRichMessage;
begin
  Rich:=CreateRichListing('📁 /tmp/ 📁', nil);
  try
    AssertEquals('📁 /tmp/ 📁', Rich.Markdown);
  finally
    Rich.Free;
  end;
end;

procedure TTestRichListing.TestKeyboardRowsMirroredToButtonsBlocks;
var
  Keyboard: TInlineKeyboard;
  Rich: TInputRichMessage;
  Blocks: TJSONArray;
  aBlock: TInputRichBlockButtons;
begin
  Keyboard:=TInlineKeyboard.Create;
  try
    // the confirmation row: Ok + Cancel, one keyboard row
    BuildDeleteConfirmKeyboard('42', Keyboard);
    Rich:=CreateRichListing('listing', Keyboard);
    try
      // the markdown text must survive the blocks being added
      AssertEquals('listing', Rich.Markdown);
      Blocks:=Rich.Blocks;
      AssertTrue('buttons blocks mirror the keyboard rows', Assigned(Blocks));
      AssertEquals(Keyboard.Count, Blocks.Count);
      aBlock:=TInputRichBlockButtons(Blocks.Items[0]);
      AssertEquals('buttons', aBlock.Strings['type']);
      AssertEquals(2, aBlock.Buttons.Count);
      AssertEquals('Ok', TRichMessageButton(aBlock.Buttons.Items[0]).text);
      AssertEquals('delete 42', TRichMessageButton(aBlock.Buttons.Items[0]).callback_data);
      AssertEquals('Cancel', TRichMessageButton(aBlock.Buttons.Items[1]).text);
      AssertEquals('cancel', TRichMessageButton(aBlock.Buttons.Items[1]).callback_data);
    finally
      Rich.Free;
    end;
  finally
    Keyboard.Free;
  end;
end;

procedure TTestRichListing.TestKeyboardStylesCopied;
var
  Keyboard: TInlineKeyboard;
  Rich: TInputRichMessage;
  aBlock: TInputRichBlockButtons;
begin
  Keyboard:=TInlineKeyboard.Create;
  try
    // the file card row carries all three styles (Bot API 10.3)
    BuildFileCardKeyboard('11', '12', '13', '14', Keyboard);
    Rich:=CreateRichListing('card', Keyboard);
    try
      aBlock:=TInputRichBlockButtons(Rich.Blocks.Items[0]);
      AssertEquals(4, aBlock.Buttons.Count);
      AssertEquals('success', TRichMessageButton(aBlock.Buttons.Items[0]).Style);
      AssertEquals('primary', TRichMessageButton(aBlock.Buttons.Items[1]).Style);
      AssertEquals('danger', TRichMessageButton(aBlock.Buttons.Items[2]).Style);
      AssertEquals('primary', TRichMessageButton(aBlock.Buttons.Items[3]).Style);
    finally
      Rich.Free;
    end;
  finally
    Keyboard.Free;
  end;
end;

procedure TTestRichListing.TestNilKeyboardYieldsNoButtonsBlocks;
var
  Rich: TInputRichMessage;
begin
  Rich:=CreateRichListing('no keyboard', nil);
  try
    // no keyboard, no blocks: the rich message keeps only the markdown, the
    // API accepts exactly one of the content forms being extended
    AssertTrue('no buttons block without a keyboard', not Assigned(Rich.Blocks));
  finally
    Rich.Free;
  end;
end;

initialization
  RegisterTest(TTestDeleteConfirmMarkup);
  RegisterTest(TTestBotCommandList);
  RegisterTest(TTestFileCardHelpers);
  RegisterTest(TTestFileCardKeyboard);
  RegisterTest(TTestRevertConfirmKeyboard);
  RegisterTest(TTestRunPanelKeyboard);
  RegisterTest(TTestRichMessageInput);
  RegisterTest(TTestRichListing);

end.
