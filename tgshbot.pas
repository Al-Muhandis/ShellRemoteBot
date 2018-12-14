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

implementation

uses
  configuration
  ;

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

