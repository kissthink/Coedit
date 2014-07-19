program coedit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms, lazcontrols, runtimetypeinfocontrols, anchordockpkg,
  AnchorDocking, AnchorDockStorage, AnchorDockOptionsDlg,
  ce_widget, ce_dmdwrap, ce_common, ce_synmemo, ce_main, ce_messages, ce_editor,
  ce_projinspect, ce_projconf, jsonparser, ce_project, ce_widgettypes,
  ce_staticexplorer, ce_search, ce_dlang, ce_dlangutils, ce_miniexplorer;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TCEMainForm, CEMainForm);
  Application.Run;
end.

