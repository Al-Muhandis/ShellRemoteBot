(*
  Tg framework, FCL HTTP Daemon Broker

  Copyright (C) 2014 Silvio Clecio

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

unit tgshelldmn;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF MSWINDOWS}
  ServiceManager,
{$ENDIF}
  DaemonApp, Classes, SysUtils,
  tgsendertypes, shellthread
  ;

type

  { TTgShellDaemon }

  TTgShellDaemon = class(TCustomDaemon)
  private
    FThread: TThread;
  public
    function Install: Boolean; override;
    function Uninstall: Boolean; override;
    function Start: Boolean; override;
    function Stop: Boolean; override;
  end;

  { TTgShellDaemonMapper }

  TTgShellDaemonMapper = class(TCustomDaemonMapper)
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TTgShellDaemon }

function TTgShellDaemon.Start: Boolean;
begin
  Result := inherited Start;
  FThread := TShellThread.Create;
  FThread.Start;
end;

function TTgShellDaemon.Stop: Boolean;
begin
  Result := inherited Stop;
  FThread.Terminate;
  FThread.Free;
end;

function TTgShellDaemon.Install: Boolean;
{$IFDEF MSWINDOWS}
var
  VSM: TServiceManager;
{$ENDIF}
begin
  Result := inherited Install;
{$IFDEF MSWINDOWS}
  VSM := TServiceManager.Create(nil);
  try
    VSM.Connect;
    if VSM.Connected then
      VSM.StartService('TgShBotD', nil);
    VSM.Disconnect;
  finally
    VSM.Free;
  end;
{$ENDIF}
  WriteLn('Service installed.');
  WriteLn('Show text in terminal');
end;

function TTgShellDaemon.Uninstall: Boolean;
{$IFDEF MSWINDOWS}
var
  VSM: TServiceManager;
{$ENDIF}
begin
  Result := inherited Uninstall;
{$IFDEF MSWINDOWS}
  VSM := TServiceManager.Create(nil);
  try
    VSM.Connect;
    if VSM.Connected then
      VSM.StopService('TgShBotD', True);
    VSM.Disconnect;
  finally
    VSM.Free;
  end;
{$ENDIF}
  WriteLn('Service uninstalled.');
end;

{ TTgShellDaemonMapper }

constructor TTgShellDaemonMapper.Create(AOwner: TComponent);
var
  VDaemonDef: TDaemonDef;
begin
  inherited Create(AOwner);
  VDaemonDef := DaemonDefs.Add as TDaemonDef;
  VDaemonDef.Description := 'Telegram bot of shell emulator';
  VDaemonDef.DisplayName := 'Telegram Bot Shell emulator';
  VDaemonDef.Name := 'TgShBot';
  VDaemonDef.DaemonClassName := 'TTgShellDaemon';
  VDaemonDef.WinBindings.ServiceType := stWin32;
end;

initialization
  RegisterDaemonClass(TTgShellDaemon);
  RegisterDaemonMapper(TTgShellDaemonMapper);

end.
