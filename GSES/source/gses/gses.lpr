program gses;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, unit2, Unit3
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='GSES - Super-Encypherment Scrambler';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  application.createform(TSessionForm, SessionForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.Run;
end.

