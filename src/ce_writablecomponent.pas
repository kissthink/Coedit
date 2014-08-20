unit ce_writableComponent;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, ce_common;

type

  (**
   * The ancestor of classes which can be saved or reloaded to/from
   * a text file. It's used each time some options or data have to
   * persist from a cession to another.
   *)
  TWritableComponent = class(TComponent)
  protected
    fFilename: string;
    fHasLoaded: boolean;
    fHasSaved: boolean;
    procedure setFilename(const aValue: string); virtual;
    procedure beforeLoad; virtual;
    procedure beforeSave; virtual;
    procedure afterLoad; virtual;
    procedure afterSave; virtual;
    procedure readerPropNoFound(Reader: TReader; Instance: TPersistent;
      var PropName: string; IsPath: boolean; var Handled, Skip: Boolean); virtual;
    procedure readerError(Reader: TReader; const Message: string;
      var Handled: Boolean); virtual;
  public
    procedure saveToFile(const aFilename: string); virtual;
    procedure loadFromFile(const aFilename: string); virtual;
    //
    property Filename: string read fFilename write setFilename;
    property hasLoaded: boolean read fHasLoaded;
    property hasSaved: boolean read fHasSaved;
  end;

implementation

procedure TWritableComponent.beforeSave;
begin
end;

procedure TWritableComponent.beforeLoad;
begin
end;

procedure TWritableComponent.afterLoad;
begin
end;

procedure TWritableComponent.afterSave;
begin
end;

procedure TWritableComponent.setFilename(const aValue: string);
begin
  fFilename := aValue;
end;

procedure TWritableComponent.readerPropNoFound(Reader: TReader; Instance: TPersistent;
      var PropName: string; IsPath: boolean; var Handled, Skip: Boolean);
begin
  Handled := true;
  Skip := true;
end;

procedure TWritableComponent.readerError(Reader: TReader; const Message: string;
      var Handled: Boolean);
begin
  Handled := true;
  fHasLoaded := false;
end;

procedure TWritableComponent.saveToFile(const aFilename: string);
begin
  fHasSaved := true;
  beforeSave;
  try
    saveCompToTxtFile(self, aFilename);
  except
    fHasSaved := false;
  end;
  setFilename(aFilename);
  afterSave;
end;

procedure TWritableComponent.loadFromFile(const aFilename: string);
begin
  fHasLoaded := true;
  beforeLoad;
  setFilename(aFilename);
  loadCompFromTxtFile(self, aFilename, @readerPropNoFound, @readerError);
  afterLoad;
end;

initialization
  registerClasses([TWritableComponent]);
end.
