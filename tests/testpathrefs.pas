unit testpathrefs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, pathrefs
  ;

type

  { TTestPathRefs }

  TTestPathRefs = class(TTestCase)
  published
    procedure TestTokensAreMonotonic;
    procedure TestFindReturnsAddedPath;
    procedure TestUnknownTokenIsNotFound;
    procedure TestRingEvictsOldestAtCapacity;
  end;

implementation

procedure TTestPathRefs.TestTokensAreMonotonic;
var
  Refs: TPathRefs;
  Token, Prev: String;
  i: Integer;
begin
  Refs:=TPathRefs.Create;
  try
    Prev:=EmptyStr;
    for i:=1 to 10 do
    begin
      Token:=Refs.Add('/some/path');
      // tokens are strictly increasing numbers: a reused or non-monotonic
      // token could resolve an outdated button to a fresh path
      AssertTrue(Token<>Prev);
      AssertTrue(StrToIntDef(Token, -1)>StrToIntDef(Prev, -1));
      Prev:=Token;
    end;
  finally
    Refs.Free;
  end;
end;

procedure TTestPathRefs.TestFindReturnsAddedPath;
var
  Refs: TPathRefs;
  Token1, Token2, Path: String;
begin
  Refs:=TPathRefs.Create;
  try
    Token1:=Refs.Add('/etc/tgshd/tgshd.ini');
    Token2:=Refs.Add('/var/log');
    AssertTrue(Refs.Find(Token1, Path));
    AssertEquals('/etc/tgshd/tgshd.ini', Path);
    AssertTrue(Refs.Find(Token2, Path));
    AssertEquals('/var/log', Path);
  finally
    Refs.Free;
  end;
end;

procedure TTestPathRefs.TestUnknownTokenIsNotFound;
var
  Refs: TPathRefs;
  Path: String;
begin
  Refs:=TPathRefs.Create;
  try
    Refs.Add('/var/log');
    // an evicted or made-up token must never resolve to a path
    AssertFalse(Refs.Find('999999', Path));
    AssertEquals(EmptyStr, Path);
    AssertFalse(Refs.Find('abc', Path));
    AssertFalse(Refs.Find('1', Path));
  finally
    Refs.Free;
  end;
end;

procedure TTestPathRefs.TestRingEvictsOldestAtCapacity;
var
  Refs: TPathRefs;
  FirstToken, Token: String;
  Path: String;
  i: Integer;
begin
  Refs:=TPathRefs.Create;
  try
    FirstToken:=Refs.Add('/path/0');
    for i:=1 to MaxPathRefs-1 do
      Refs.Add('/path/'+IntToStr(i));
    // the ring is full: the oldest token is still resolvable
    AssertTrue(Refs.Find(FirstToken, Path));
    AssertEquals('/path/0', Path);
    AssertEquals(MaxPathRefs, Refs.Count);
    // one more entry evicts the oldest token instead of growing further
    Token:=Refs.Add('/path/'+IntToStr(MaxPathRefs));
    AssertFalse(Refs.Find(FirstToken, Path));
    AssertEquals(EmptyStr, Path);
    AssertTrue(Refs.Find(Token, Path));
    AssertEquals('/path/'+IntToStr(MaxPathRefs), Path);
    AssertEquals(MaxPathRefs, Refs.Count);
  finally
    Refs.Free;
  end;
end;

initialization
  RegisterTest(TTestPathRefs);

end.
