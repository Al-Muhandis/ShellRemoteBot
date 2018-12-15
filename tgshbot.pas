unit tgshbot;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tgsendertypes;

type

  { TTgShBot }

  TTgShBot = class (TTelegramSender)
  protected
    function IsSimpleUser(ChatID: Int64): Boolean; override;
    function IsBanned(ChatID: Int64): Boolean; override;
  end;

function IsolateShellOutput(const S: String): String;

implementation

uses
  configuration
  ;

function IsolateShellOutput(const S: String): String;
begin
  Result:='```bash'+LineEnding+S+LineEnding+'```';
end;

{ TTgShBot }

function TTgShBot.IsSimpleUser(ChatID: Int64): Boolean;
begin
  Result:=Cnfg.Users[ChatID]=usSimple;
end;

function TTgShBot.IsBanned(ChatID: Int64): Boolean;
begin
  Result:=Cnfg.Users[ChatID]=usBanned;
end;

end.

