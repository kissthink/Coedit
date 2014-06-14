program coedit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms, lazcontrols, runtimetypeinfocontrols, ce_main, ce_widget,
  ce_common, ce_messages, ce_editor, ce_project, ce_synmemo, ce_dmdwrap,
  ce_projconf, ce_projconfframe, ce_projconfall;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TCEMainForm, mainForm);
  Application.Run;
end.

