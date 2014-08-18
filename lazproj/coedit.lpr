program coedit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms, lazcontrols, runtimetypeinfocontrols,
  ce_main, ce_dcd, ce_observer;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TCEMainForm, CEMainForm);
  Application.Run;
end.

