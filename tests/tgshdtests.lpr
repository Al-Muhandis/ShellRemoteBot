program tgshdtests;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, consoletestrunner, testpathrefs, testtgshbot,
  testconfiguration, testshellthread
  ;

type

  { TTgShdTestsRunner }

  TTgShdTestsRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TTgShdTestsRunner;

begin
  Application:=TTgShdTestsRunner.Create(nil);
  Application.Initialize;
  Application.Title:='tgshdtests';
  Application.Run;
  Application.Free;
end.
