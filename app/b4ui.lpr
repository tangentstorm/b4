program b4ui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Console
  { you can add units after this };

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TConsoleForm, ConsoleForm);
  Application.Run;
end.

