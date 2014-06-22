program coedit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms, lazcontrols, runtimetypeinfocontrols, ce_widget,
  ce_dmdwrap, ce_common, ce_synmemo, ce_main, ce_messages, ce_editor,
  ce_projinspect, ce_projconf, jsonparser, ce_project, 
  ce_widgettypes, ce_staticexplorer;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TCEMainForm, mainForm);
  Application.Run;
end.

