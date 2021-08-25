unit threadscenter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  tgsendertypes, shellthread, shotsthread
  ;

type

  { TThreadPult }

  TThreadPult = class
  private
    FThread: TThread;
    FShotThread: TShotThread;
  public
    procedure Start;
    procedure Stop;
  end;

implementation

{ TThreadPult }

procedure TThreadPult.Start;
begin
  FThread := TShellThread.Create;
  FThread.Start;
  FShotThread:=TShotThread.Create;
  FShotThread.Start;
end;

procedure TThreadPult.Stop;
begin
  FThread.Terminate;
  FShotThread.TerminateThread;
end;

end.
