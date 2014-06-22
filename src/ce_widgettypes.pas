unit ce_widgettypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, actnList, ce_project;

type

  (**
   * An implementer is informed when a new document is added, focused or closed.
   *)
  ICEMultiDocMonitor = interface
    procedure docChange(const aNewIndex: integer);
    procedure docClose(const aNewIndex: integer);
  end;

  (**
   * An implementer adds some menu actions when its context is valid.
   *)
  ICEContextualActions = interface
    function contextName: string;
    function contextActionCount: integer;
    function contextAction(index: integer): TAction;
  end;

  (**
   * An implementer is informed when a project changes.
   *)
  ICEProjectMonitor = interface
    procedure projNew(const aProject: TCEProject);
    procedure projChange(const aProject: TCEProject);
    procedure projClose(const aProject: TCEProject);
  end;

implementation
end.

