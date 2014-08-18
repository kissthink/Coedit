program coedit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms, lazcontrols, runtimetypeinfocontrols,
  ce_dcd, ce_observer, ce_main;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TCEMainForm, CEMainForm);
  Application.Run;
end.

