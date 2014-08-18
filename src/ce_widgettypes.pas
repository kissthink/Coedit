unit ce_widgettypes;

{$MODE OBJFPC}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, actnList, menus, ce_synmemo, ce_project, ce_observer;

type

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
   * TODO-cfeature: improve the interface so that a widget can declare a complete main menu category.
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
   * An implementer is informed about the current file(s).
   *)
  ICEMultiDocObserver = interface(ICEObserver)
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
   * An implementer informs some ICEMultiDocObserver about the current file(s)
   *)
  TCEMultiDocSubject = class(TCECustomSubject, ICEMultiDocObserver)
  protected
    function acceptObserver(aObject: TObject): boolean; override;
  public
    procedure docNew(const aDoc: TCESynMemo);
    procedure docFocused(const aDoc: TCESynMemo);
    procedure docChanged(const aDoc: TCESynMemo);
    procedure docClose(const aDoc: TCESynMemo);
  end;

  (**
   * An implementer is informed about the current project(s).
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

  (**
   * An implementer can add a mainmenu entry.
   *)
  ICEMainMenuProvider = interface(ICEObserver)
    // item must contain the full items tree to be added
    procedure menuDeclare(out item: TMenuItem);
  end;

implementation

function TCEMultiDocSubject.acceptObserver(aObject: TObject): boolean;
begin
  result := (aObject as ICEMultiDocObserver) <> nil;
end;

procedure TCEMultiDocSubject.docNew(const aDoc: TCESynMemo);
var
  i: Integer;
begin
  for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEMultiDocObserver).docNew(aDoc);
end;

procedure TCEMultiDocSubject.docFocused(const aDoc: TCESynMemo);
var
  i: Integer;
begin
  for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEMultiDocObserver).docFocused(aDoc);
end;

procedure TCEMultiDocSubject.docChanged(const aDoc: TCESynMemo);
var
  i: Integer;
begin
  for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEMultiDocObserver).docChanged(aDoc);
end;

procedure TCEMultiDocSubject.docClose(const aDoc: TCESynMemo);
var
  i: Integer;
begin
  for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEMultiDocObserver).docClose(aDoc);
end;

end.
