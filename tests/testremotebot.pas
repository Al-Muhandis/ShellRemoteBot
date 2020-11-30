unit testremotebot;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, configuration
  ;

type

  { TTestRemoteBot }

  TTestRemoteBot= class(TTestCase)
  published
    procedure TestScreenShot;
  end;

implementation

uses
  screenshot, tgsendertypes
  ;

procedure TTestRemoteBot.TestScreenShot;
var
  aStream: TStream;
  aBot: TTelegramSender;
begin
  aStream:=TMemoryStream.Create;
  aBot:=TTelegramSender.Create(Cnfg.BotToken);
  try
    CreateScreenshot(aStream);
    aBot.sendPhotoStream(Cnfg.ServiceUser, 'Screenshot', aStream, 'Screenshot image');
  finally
    aBot.Free;
    aStream.Free;
  end;
end;



initialization

  RegisterTest(TTestRemoteBot);
end.

