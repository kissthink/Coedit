unit ce_interfaces;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, actnList, menus, ce_synmemo, ce_project, ce_observer;

type

  (**
   * An implementer can save and load some stuffs on application start/quit
   *)
  ICESessionOptionsObserver = interface
  ['ICEWidgetPersist']
    // persistent things are about to be saved.
    procedure sesoptBeforeSave;
    // persistent things can be declared to aFiler.
    procedure sesoptDeclareProperties(aFiler: TFiler);
    // persistent things have just been reloaded.
    procedure sesoptAfterLoad;
  end;
  (**
   * An implementer gets and gives back some things
   *)
  TCESessionOptionsSubject = class(TCECustomSubject)
  protected
    function acceptObserver(aObject: TObject): boolean; override;
  end;



  (**
   * An implementer declares some actions on demand.
   *)
  ICEContextualActions = interface
  ['ICEContextualActions']
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
  ICEMultiDocObserver = interface
  ['ICEMultiDocObserver']
    // aDoc has been created (empty, runnable, project source, ...).
    procedure docNew(const aDoc: TCESynMemo);
    // aDoc is the document being edited.
    procedure docFocused(const aDoc: TCESynMemo);
    // aDoc content has just been modified (edited, saved).
    procedure docChanged(const aDoc: TCESynMemo);
    // aDoc is about to be closed.
    procedure docClosing(const aDoc: TCESynMemo);
  end;
  (**
   * An implementer informs some ICEMultiDocObserver about the current file(s)
   *)
  TCEMultiDocSubject = class(TCECustomSubject)
  protected
    function acceptObserver(aObject: TObject): boolean; override;
  end;



  (**
   * An implementer is informed about the current project(s).
   *)
  ICEProjectObserver = interface
  ['ICEProjectObserver']
    // aProject has been created/opened
    procedure projNew(const aProject: TCEProject);
    // aProject has been modified: switches, source name, ...
    procedure projChanged(const aProject: TCEProject);
    // aProject is about to be closed.
    procedure projClosing(const aProject: TCEProject);
    // not used yet: the active project is now aProject
    procedure projFocused(const aProject: TCEProject);
  end;
  (**
   * An implementer informs some ICEProjectObserver about the current project(s)
   *)
  TCEProjectSubject = class(TCECustomSubject)
  protected
    function acceptObserver(aObject: TObject): boolean; override;
  end;



  (**
   * An implementer can add a mainmenu entry.
   *)
  ICEMainMenuProvider = interface
  ['ICEMainMenuProvider']
    // item must contain the full items tree to be added
    procedure menuDeclare(out item: TMenuItem);
    // the implementer can update the actions used in the menu declared previously.
    procedure menuActionsUpdate;
  end;
  (**
   * An implementer agregates its observers menus.
   *)
  TCEMainMenuSubject = class(TCECustomSubject)
  protected
    function acceptObserver(aObject: TObject): boolean; override;
  end;



  (**
   * An implementer can expose some customizable shortcuts
   *)
  ICEEditableShortCut = interface
  ['ICEEditableShortCut']
    // a TCEEditableShortCutSubject queries the editable shortcuts count.
    procedure scGetCount(out aValue: Integer);
    // a TCEEditableShortCutSubject queries the shortcut category name.
    procedure scGetCategory(out aValue: string);
    // a TCEEditableShortCutSubject queries the state of the index-th shortcut.
    procedure scGetItem(index: Integer; out aName: string; out aShortcut: Word);
    // a TCEEditableShortCutSubject sends the possibly modified assignation of the index-th shortcut.
    procedure scSetItem(index: Integer; const aCategory, aName: string; aShortcut: Word);
  end;
  (**
   * An implementer manages its observers shortcuts.
   *)
  TCEEditableShortCutSubject = class(TCECustomSubject)
  protected
    function acceptObserver(aObject: TObject): boolean; override;
  end;


{
  subject Primitives:

  A subject cannot necessarly provides all the informations the observers expect.
  It can compose using the following "primitives".
}

  (**
   * TCEMultiDocSubject primitives.
   *)
   procedure subjDocNew(aSubject: TCEMultiDocSubject; aDoc: TCESynMemo);      {$IFDEF RELEASE}inline;{$ENDIF}
   procedure subjDocClosing(aSubject: TCEMultiDocSubject; aDoc: TCESynMemo);  {$IFDEF RELEASE}inline;{$ENDIF}
   procedure subjDocFocused(aSubject: TCEMultiDocSubject; aDoc: TCESynMemo);  {$IFDEF RELEASE}inline;{$ENDIF}
   procedure subjDocChanged(aSubject: TCEMultiDocSubject; aDoc: TCESynMemo);  {$IFDEF RELEASE}inline;{$ENDIF}


  (**
   * TCEProjectSubject primitives.
   *)
   procedure subjProjNew(aSubject: TCEProjectSubject; aProj: TCEProject);     {$IFDEF RELEASE}inline;{$ENDIF}
   procedure subjProjClosing(aSubject: TCEProjectSubject; aProj: TCEProject); {$IFDEF RELEASE}inline;{$ENDIF}
   procedure subjProjFocused(aSubject: TCEProjectSubject; aProj: TCEProject); {$IFDEF RELEASE}inline;{$ENDIF}
   procedure subjProjChanged(aSubject: TCEProjectSubject; aProj: TCEProject); {$IFDEF RELEASE}inline;{$ENDIF}

  (**
   * TCESessionOptionsSubject primitives.
   *)
   procedure subjSesOptsBeforeSave(aSubject: TCESessionOptionsSubject);                       {$IFDEF RELEASE}inline;{$ENDIF}
   procedure subjSesOptsDeclareProperties(aSubject: TCESessionOptionsSubject; aFiler: TFiler);{$IFDEF RELEASE}inline;{$ENDIF}
   procedure subjSesOptsAfterLoad(aSubject: TCESessionOptionsSubject);                        {$IFDEF RELEASE}inline;{$ENDIF}


implementation

{$REGION TCEMultiDocSubject-----------------------------------------------------}
function TCEMultiDocSubject.acceptObserver(aObject: TObject): boolean;
begin
  exit(aObject is ICEMultiDocObserver);
end;

procedure subjDocNew(aSubject: TCEMultiDocSubject; aDoc: TCESynMemo);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEMultiDocObserver).docNew(aDoc);
end;

procedure subjDocClosing(aSubject: TCEMultiDocSubject; aDoc: TCESynMemo);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEMultiDocObserver).docClosing(aDoc);
end;

procedure subjDocFocused(aSubject: TCEMultiDocSubject; aDoc: TCESynMemo);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEMultiDocObserver).docFocused(aDoc);
end;

procedure subjDocChanged(aSubject: TCEMultiDocSubject; aDoc: TCESynMemo);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEMultiDocObserver).docChanged(aDoc);
end;
{$ENDREGION}

{$REGION TCEProjectSubject------------------------------------------------------}
function TCEProjectSubject.acceptObserver(aObject: TObject): boolean;
begin
  exit(aObject is ICEProjectObserver);
end;

procedure subjProjNew(aSubject: TCEProjectSubject; aProj: TCEProject);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEProjectObserver).ProjNew(aProj);
end;

procedure subjProjClosing(aSubject: TCEProjectSubject; aProj: TCEProject);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEProjectObserver).projClosing(aProj);
end;

procedure subjProjFocused(aSubject: TCEProjectSubject; aProj: TCEProject);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEProjectObserver).projFocused(aProj);
end;

procedure subjProjChanged(aSubject: TCEProjectSubject; aProj: TCEProject);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICEProjectObserver).projChanged(aProj);
end;
{$ENDREGION}

{$REGION TCESessionOptionsSubject-----------------------------------------------}
function TCESessionOptionsSubject.acceptObserver(aObject: TObject): boolean;
begin
  exit(aObject is ICESessionOptionsObserver);
end;

procedure subjSesOptsBeforeSave(aSubject: TCESessionOptionsSubject);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICESessionOptionsObserver).sesoptBeforeSave;
end;

procedure subjSesOptsDeclareProperties(aSubject: TCESessionOptionsSubject; aFiler: TFiler);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICESessionOptionsObserver).sesoptDeclareProperties(aFiler);
end;

procedure subjSesOptsAfterLoad(aSubject: TCESessionOptionsSubject);
var
  i: Integer;
begin
  with aSubject do for i:= 0 to fObservers.Count-1 do
    (fObservers.Items[i] as ICESessionOptionsObserver).sesoptAfterLoad;
end;
{$ENDREGION}

{$REGION TCEEditableShortCutSubject}
function TCEMainMenuSubject.acceptObserver(aObject: TObject): boolean;
begin
  exit(aObject is ICEMainMenuProvider);
end;
{$ENDREGION}

{$REGION TCEEditableShortCutSubject}
function TCEEditableShortCutSubject.acceptObserver(aObject: TObject): boolean;
begin
  exit(aObject is ICEEditableShortCut);
end;
{$ENDREGION}
end.
