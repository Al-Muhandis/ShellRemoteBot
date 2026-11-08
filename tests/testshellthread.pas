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

initialization
  RegisterTest(TTestDeleteConfirmMarkup);

end.
