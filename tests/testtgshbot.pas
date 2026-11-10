unit testtgshbot;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, tgshbot, LazUTF8, LazFileUtils
  ;

const
  // zero-width space (U+200B) used to break stray markdown fences
  ZWSP = #$E2#$80#$8B;

type

  { TTestIsolateShellOutput }

  TTestIsolateShellOutput = class(TTestCase)
  private
    function CountOccurrences(const ASub, AText: String): Integer;
  published
    procedure TestWrapsInFence;
    procedure TestEscapesTripleBacktick;
    procedure TestEscapesEveryOccurrence;
    procedure TestEmptyOutput;
  end;

  { TTestSplitUTF8Parts }

  TTestSplitUTF8Parts = class(TTestCase)
  private
    function RepeatStr(const S: String; ACount: Integer): String;
    function JoinParts(AParts: TStrings): String;
  published
    procedure TestShortMessageSinglePart;
    procedure TestEmptyMessageNoParts;
    procedure TestSplitsByLength;
    procedure TestPartLengthLimit;
    procedure TestKeepsUTF8CharactersWhole;
    procedure TestNonPositivePartLengthRaises;
  end;

  { TTestBackupPathFor }

  TTestBackupPathFor = class(TTestCase)
  published
    procedure TestAppendsBakToSimpleName;
    procedure TestAppendsBakToPathWithDir;
  end;

  { TTestTextFileValidation }

  TTestTextFileValidation = class(TTestCase)
  private
    function MakeTestFile(const AContent: String): String;
  published
    procedure TestPlainAsciiTextIsValid;
    procedure TestUtf8TextWithMultibyteCharsIsValid;
    procedure TestEmptyFileIsValid;
    procedure TestFileWithZeroByteIsInvalid;
    procedure TestBinarySequencesAreInvalid;
    procedure TestFileLargerThanLimitIsInvalid;
  end;

implementation

{ TTestIsolateShellOutput }

function TTestIsolateShellOutput.CountOccurrences(const ASub, AText: String): Integer;
var
  i: Integer;
begin
  Result:=0;
  for i:=1 to Length(AText)-Length(ASub)+1 do
    if Copy(AText, i, Length(ASub))=ASub then
      Inc(Result);
end;

procedure TTestIsolateShellOutput.TestWrapsInFence;
begin
  AssertEquals('```log'+LineEnding+'plain output'+LineEnding+'```',
    IsolateShellOutput('plain output'));
end;

procedure TTestIsolateShellOutput.TestEscapesTripleBacktick;
var
  S: String;
begin
  // a stray ``` would close the markdown fence and the message would be
  // rejected by Telegram: it must be broken with a zero-width space
  S:=IsolateShellOutput('a```b');
  AssertEquals('```log'+LineEnding+'a``'+ZWSP+'`b'+LineEnding+'```', S);
  // the escaped output carries exactly one zero-width space, i.e. the single
  // stray ``` of the input was broken
  AssertEquals(1, CountOccurrences(ZWSP, S));
end;

procedure TTestIsolateShellOutput.TestEscapesEveryOccurrence;
var
  S: String;
begin
  S:=IsolateShellOutput('x```y```z```');
  AssertEquals(3, CountOccurrences(ZWSP, S));
end;

procedure TTestIsolateShellOutput.TestEmptyOutput;
begin
  AssertEquals('```log'+LineEnding+LineEnding+'```',
    IsolateShellOutput(EmptyStr));
end;

{ TTestSplitUTF8Parts }

function TTestSplitUTF8Parts.RepeatStr(const S: String; ACount: Integer): String;
var
  i: Integer;
begin
  Result:=EmptyStr;
  for i:=1 to ACount do
    Result+=S;
end;

function TTestSplitUTF8Parts.JoinParts(AParts: TStrings): String;
var
  i: Integer;
begin
  Result:=EmptyStr;
  for i:=0 to AParts.Count-1 do
    Result+=AParts[i];
end;

procedure TTestSplitUTF8Parts.TestShortMessageSinglePart;
var
  Parts: TStringList;
begin
  Parts:=TStringList.Create;
  try
    SplitUTF8Parts('hello world', 3500, Parts);
    AssertEquals(1, Parts.Count);
    AssertEquals('hello world', Parts[0]);
  finally
    Parts.Free;
  end;
end;

procedure TTestSplitUTF8Parts.TestEmptyMessageNoParts;
var
  Parts: TStringList;
begin
  Parts:=TStringList.Create;
  try
    SplitUTF8Parts(EmptyStr, 3500, Parts);
    AssertEquals(0, Parts.Count);
  finally
    Parts.Free;
  end;
end;

procedure TTestSplitUTF8Parts.TestSplitsByLength;
var
  Msg: String;
  Parts: TStringList;
begin
  Msg:=RepeatStr('x', 3501);
  Parts:=TStringList.Create;
  try
    SplitUTF8Parts(Msg, 3500, Parts);
    AssertEquals(2, Parts.Count);
    AssertEquals(3500, Length(Parts[0]));
    AssertEquals(1, Length(Parts[1]));
    AssertEquals(Msg, JoinParts(Parts));
  finally
    Parts.Free;
  end;
end;

procedure TTestSplitUTF8Parts.TestPartLengthLimit;
var
  Msg: String;
  Parts: TStringList;
  i, Total: Integer;
begin
  // a mixed message of exactly 100 UTF-8 characters
  Msg:=RepeatStr('a', 40)+RepeatStr('ж', 30)+RepeatStr('⊕', 20)+RepeatStr('😀', 10);
  Parts:=TStringList.Create;
  try
    SplitUTF8Parts(Msg, 7, Parts);
    AssertTrue(Parts.Count>1);
    Total:=0;
    for i:=0 to Parts.Count-1 do
    begin
      AssertTrue(UTF8Length(Parts[i])<=7);
      Total+=UTF8Length(Parts[i]);
    end;
    AssertEquals(100, Total);
    AssertEquals(UTF8Length(Msg), Total);
    AssertEquals(Msg, JoinParts(Parts));
  finally
    Parts.Free;
  end;
end;

procedure TTestSplitUTF8Parts.TestKeepsUTF8CharactersWhole;
var
  Msg: String;
  Parts: TStringList;
  i: Integer;
const
  // 2-byte, 3-byte and 4-byte UTF-8 characters
  TwoByte='ж';
  ThreeByte='⊕';
  FourByte='😀';
begin
  Msg:=RepeatStr(TwoByte, 4)+RepeatStr(ThreeByte, 4)+RepeatStr(FourByte, 4);
  Parts:=TStringList.Create;
  try
    // a split length of 3 crosses a multibyte boundary everywhere: each part
    // must still be a sequence of whole characters (a byte cut would produce
    // invalid UTF-8 and Telegram rejects such messages)
    SplitUTF8Parts(Msg, 3, Parts);
    AssertTrue(Parts.Count>1);
    for i:=0 to Parts.Count-1 do
      AssertTrue(UTF8Length(Parts[i])<=3);
    AssertEquals(Msg, JoinParts(Parts));
  finally
    Parts.Free;
  end;
end;

procedure TTestSplitUTF8Parts.TestNonPositivePartLengthRaises;
var
  Parts: TStringList;
  Raised: Boolean;
begin
  // zero or negative part length would make the splitting loop endless
  Parts:=TStringList.Create;
  try
    Raised:=False;
    try
      SplitUTF8Parts('abc', 0, Parts);
    except
      on E: EArgumentException do
        Raised:=True;
    end;
    AssertTrue('EArgumentException expected for zero part length', Raised);
  finally
    Parts.Free;
  end;
end;

{ TTestBackupPathFor }

procedure TTestBackupPathFor.TestAppendsBakToSimpleName;
begin
  AssertEquals('/some/path/file.txt.bak', BackupPathFor('/some/path/file.txt'));
end;

procedure TTestBackupPathFor.TestAppendsBakToPathWithDir;
begin
  AssertEquals('myfile.bak', BackupPathFor('myfile'));
end;

{ TTestTextFileValidation }

function TTestTextFileValidation.MakeTestFile(const AContent: String): String;
begin
  Result:=GetTempFileNameUTF8(GetTempDir, 'tgshd');
  with TFileStream.Create(Result, fmCreate) do
  try
    if Length(AContent)>0 then
      Write(AContent[1], Length(AContent));
  finally
    Free;
  end;
end;

procedure TTestTextFileValidation.TestPlainAsciiTextIsValid;
var
  F: String;
begin
  F:=MakeTestFile('Hello, world!'+LineEnding+'Goodbye.');
  try
    AssertTrue('plain ASCII text should be valid', IsTextFile(F));
  finally
    DeleteFileUTF8(F);
  end;
end;

procedure TTestTextFileValidation.TestUtf8TextWithMultibyteCharsIsValid;
var
  F: String;
begin
  F:=MakeTestFile('Hello, '+#209#128#209#129+' '+#$E2#$98#$83); // Cyrillic + snowman
  try
    AssertTrue('UTF-8 text should be valid', IsTextFile(F));
  finally
    DeleteFileUTF8(F);
  end;
end;

procedure TTestTextFileValidation.TestEmptyFileIsValid;
var
  F: String;
begin
  F:=MakeTestFile(EmptyStr);
  try
    AssertTrue('empty file should be valid', IsTextFile(F));
  finally
    DeleteFileUTF8(F);
  end;
end;

procedure TTestTextFileValidation.TestFileWithZeroByteIsInvalid;
var
  F: String;
begin
  F:=MakeTestFile('Hello'+#0+'world');
  try
    AssertFalse('file with #0 byte should be invalid', IsTextFile(F));
  finally
    DeleteFileUTF8(F);
  end;
end;

procedure TTestTextFileValidation.TestBinarySequencesAreInvalid;
var
  F: String;
begin
  // 0x80 is a bare continuation byte with no leading byte — invalid UTF-8
  F:=MakeTestFile('Hello'+#128+'world');
  try
    AssertFalse('bare continuation byte should be invalid', IsTextFile(F));
  finally
    DeleteFileUTF8(F);
  end;
end;

procedure TTestTextFileValidation.TestFileLargerThanLimitIsInvalid;
var
  F: String;
  Data: array[0.._MaxEditSize] of Byte; // _MaxEditSize + 1 bytes (over limit)
  i: Integer;
begin
  // fill with 0x80 bytes (bare continuation byte — invalid UTF-8, and over size)
  for i:=0 to High(Data) do
    Data[i]:=$80;
  F:=GetTempFileNameUTF8(GetTempDir, 'tgshd');
  with TFileStream.Create(F, fmCreate) do
  try
    Write(Data, SizeOf(Data));
  finally
    Free;
  end;
  AssertFalse('file larger than 1 MB should be rejected', IsTextFile(F));
  DeleteFileUTF8(F);
end;

initialization
  RegisterTest(TTestIsolateShellOutput);
  RegisterTest(TTestSplitUTF8Parts);
  RegisterTest(TTestBackupPathFor);
  RegisterTest(TTestTextFileValidation);

end.
