unit ce_observer;

{$MODE OBJFPC}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, Contnrs;

type

  (**
   * Manages the connections between the observers and their subjects in the whole program.
   *)
  TCEEntitiesConnector = class
  private
    fObservers: TObjectList;
    fSubjects: TObjectList;
    fUpdating: boolean;
    procedure updateEntities;
  public
    constructor create;
    destructor destroy; override;
    //
    procedure beginUpdate;
    procedure endUpdate;
    procedure addObserver(anObserver: TObject);
    procedure addSubject(aSubject: TObject);
    procedure removeObserver(anObserver: TObject);
    procedure removeSubject(aSubject: TObject);
  end;

  (**
   * Interface for a Coedit subject. Basically designed to hold a list of observer
   *)
  ICESubject = interface
  ['ICESubject']
    // an observer is proposed. anObserver is not necessarly compatible.
    procedure addObserver(anObserver: TObject);
    // anObserver must be removed.
    procedure removeObserver(anObserver: TObject);
    // optionally implemented to trigger all the methods of the observer interface.
    procedure updateObservers;
  end;

  (**
   *  Standard implementation of an ICESubject
   *)
  TCECustomSubject = class(ICESubject)
  protected
    fObservers: TObjectList;
    // test for a specific interface when adding an observer.
    function acceptObserver(aObject: TObject): boolean; virtual;
  public
    constructor create; virtual;
    destructor destroy; override;
    //
    procedure addObserver(anObserver: TObject);
    procedure removeObserver(anObserver: TObject);
    procedure updateObservers; virtual;
  end;

var
  EntitiesConnector: TCEEntitiesConnector = nil;

implementation

{$REGION TCEEntitiesConnector --------------------------------------------------}
constructor TCEEntitiesConnector.create;
begin
  fObservers  := TObjectList.create(false);
  fSubjects   := TObjectList.create(false);
end;

destructor TCEEntitiesConnector.destroy;
begin
  fObservers.Free;
  fSubjects.Free;
  inherited;
end;

procedure TCEEntitiesConnector.updateEntities;
var
  i,j: Integer;
begin
  fUpdating := false;
  for i := 0 to fSubjects.Count-1 do
  begin
    if not (fSubjects[i] is ICESubject) then
      continue;
    for j := 0 to fObservers.Count-1 do
    begin
      if fSubjects[i] <> fObservers[j] then
        (fSubjects[i] as ICESubject).addObserver(fObservers[j]);
    end;
  end;
end;

procedure TCEEntitiesConnector.beginUpdate;
begin
  fUpdating := true;
end;

procedure TCEEntitiesConnector.endUpdate;
begin
  updateEntities;
end;

procedure TCEEntitiesConnector.addObserver(anObserver: TObject);
begin
  if fObservers.IndexOf(anObserver) <> -1 then
    exit;
  fUpdating := true;
  fObservers.Add(anObserver);
end;

procedure TCEEntitiesConnector.addSubject(aSubject: TObject);
begin
  if (aSubject as ICESubject) = nil then
    exit;
  if fSubjects.IndexOf(aSubject) <> -1 then
    exit;
  fUpdating := true;
  fSubjects.Add(aSubject);
end;

procedure TCEEntitiesConnector.removeObserver(anObserver: TObject);
var
  i: Integer;
begin
  fUpdating := true;
  fObservers.Remove(anObserver);
  for i := 0 to fSubjects.Count-1 do
    if fSubjects[i] <> nil then
      (fSubjects[i] as ICESubject).removeObserver(anObserver);
end;

procedure TCEEntitiesConnector.removeSubject(aSubject: TObject);
begin
  fUpdating := true;
  fSubjects.Remove(aSubject);
end;
{$ENDREGION}

{$REGION TCECustomSubject --------------------------------------------------}
constructor TCECustomSubject.create;
begin
  fObservers := TObjectList.create(false);
  EntitiesConnector.addSubject(Self);
  EntitiesConnector.endUpdate;
end;

destructor TCECustomSubject.destroy;
begin
  EntitiesConnector.removeSubject(Self);
  EntitiesConnector.endUpdate;
  fObservers.Free;
  Inherited;
end;

function TCECustomSubject.acceptObserver(aObject: TObject): boolean;
begin
  exit(false);
end;

procedure TCECustomSubject.addObserver(anObserver: TObject);
begin
  if not acceptObserver(anObserver) then
    exit;
  if fObservers.IndexOf(anObserver) <> -1 then
    exit;
  fObservers.Add(anObserver);
end;

procedure TCECustomSubject.removeObserver(anObserver: TObject);
begin
  fObservers.Remove(anObserver);
end;

procedure TCECustomSubject.updateObservers;
begin
end;
{$ENDREGION}

initialization
  EntitiesConnector := TCEEntitiesConnector.create;
finalization
  EntitiesConnector.Free;
  EntitiesConnector := nil;
end.

