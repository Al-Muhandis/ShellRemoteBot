Program tgshd;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  configuration, DaemonApp, tgshelldmn, shellthread, tgshbot
  ;

begin
  Application.Title:='Telegram shell daemon';
  Application.Initialize;
  Application.Run;
end.
