unit ce_widgettypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, actnList, ce_synmemo, ce_project;

type

  // TODO-cinterface: document content access/modification

  (**
   * An implementer can save and load some stuffs on application start/quit
   *)
  ICEWidgetPersist = interface
    // Coedit options are about to be saved.
    procedure beforeSave(sender: TObject);
    // some custom properties can be declared to aFiler.
    procedure declareProperties(aFiler: TFiler);
    // Coedit options has just been reloaded.
    procedure afterLoad(sender: TObject);
  end;

  (**
   * An implementer declares some actions on demand.
   *)
  ICEContextualActions = interface
    // declares a context name for the actions
    function contextName: string;
    // action count, called before contextAction()
    function contextActionCount: integer;
    // declares actions, called in loop, from 0 to contextActionCount-1
    function contextAction(index: integer): TAction;
  end;

  (**
   * An implementer is informed when a new document is added, focused or closed.
   *)
  ICEMultiDocMonitor = interface
    // the new document aDoc has been created (empty, runnable, project source, ...).
    procedure docNew(const aDoc: TCESynMemo);
    // aDoc is the document being edited.
    procedure docFocused(const aDoc: TCESynMemo); // rename to: docSelected or docActivated
    // aDoc content has just been modified (edited, saved).
    procedure docChanged(const aDoc: TCESynMemo);
    // aDoc is about to be closed.
    procedure docClose(const aDoc: TCESynMemo);
  end;

  (**
   * An implementer is informed when a project changes.
   *)
  ICEProjectMonitor = interface
    // the new project aProject has been created/opened
    procedure projNew(const aProject: TCEProject);
    // aProject has been modified: switches, source name, ...
    procedure projChange(const aProject: TCEProject);
    // aProject is about to be closed.
    procedure projClose(const aProject: TCEProject);
    // aProject is about to be compiled.
    procedure projCompile(const aProject: TCEProject);
    // aProject is about to be executed.
    procedure projRun(const aProject: TCEProject);
    // not used yet: the active project is now aProject
    procedure projFocused(const aProject: TCEProject); // rename: projSelected or projActivated
  end;

implementation
end.

