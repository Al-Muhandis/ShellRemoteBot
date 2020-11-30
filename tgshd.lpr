Program tgshd;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  configuration, DaemonApp, tgshelldmn, shellthread, tgshbot, shotsthread, screenshot
{ You must use tgsynapsehttpbroker if You want to use HTTP proxy }
//  ,tgsynapsehttpclientbroker
  ;

{$R *.res}

begin
  Application.Title:='Telegram shell daemon';
  Application.Initialize;
  Application.Run;
end.
