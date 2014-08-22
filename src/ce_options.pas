unit ce_options;

{$I ce_defines.inc}

interface

uses
  classes, sysutils, ce_common, ce_writableComponent, ce_observer;

type

  TCEOptions = class(TWritableComponent)
  private
    fSubjPersObservers: TCECustomSubject;
  protected
    procedure defineProperties(Filer: TFiler); override;
    procedure beforeSave; override;
    procedure afterLoad; override;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

implementation

uses
  ce_interfaces;

constructor TCEOptions.create(aOwner: TComponent);
begin
  inherited;
  fSubjPersObservers := TCESessionOptionsSubject.create;
  //
  EntitiesConnector.addSubject(fSubjPersObservers);
  EntitiesConnector.endUpdate;
end;

destructor TCEOptions.destroy;
begin
  EntitiesConnector.removeSubject(fSubjPersObservers);
  EntitiesConnector.endUpdate;
  //
  fSubjPersObservers.Free;
  inherited;
end;

procedure TCEOptions.defineProperties(Filer: TFiler);
begin
  subjSesOptsDeclareProperties(TCESessionOptionsSubject(fSubjPersObservers), Filer);
end;

procedure TCEOptions.beforeSave;
begin
  subjSesOptsBeforeSave(TCESessionOptionsSubject(fSubjPersObservers));
end;

procedure TCEOptions.afterLoad;
begin
  subjSesOptsAfterLoad(TCESessionOptionsSubject(fSubjPersObservers));
end;

end.
