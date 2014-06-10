unit ce_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ce_dmdwrap;

type

  (**
   * An implementer is informed when a new document is added, focused or closed.
   *)
  ICEMultiDocMonitor = interface
    procedure docChange(const aNewIndex: integer);
    procedure docClose(const aNewIndex: integer);
  end;

  (**
   * An implementer informs when a new document is added, focused or closed.
   *)
  ICEMultiDocEmitter = interface
    procedure docChange(const aNewIndex: integer);
    procedure docClose(const aNewIndex: integer);
  end;

  (*****************************************************************************
   * Writable project.
   *)
  TCEProject = class(TComponent)
  private
    fModified: boolean;
    fFilename: string;
    fOptsColl: TCollection;
    fSrcs: TStringList; // an editor can be associated to a file using the Object[] property
    fConfIx: Integer;
    procedure subMemberChanged(sender : TObject);
    procedure setOptsColl(const aValue: TCollection);
    procedure setFname(const aValue: string);
    procedure setSrcs(const aValue: TStringList);
    procedure setConfIx(aValue: Integer);
    function getConfig(const ix: integer): TCompilerConfiguration;
  published
    property OptionsCollection: TCollection read fOptsColl write setOptsColl;
    property Sources: TStringList read fSrcs write setSrcs;
    property ConfigurationIndex: Integer read fConfIx write setConfIx;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    function addConfiguration: TCompilerConfiguration;
    property configuration[ix: integer]: TCompilerConfiguration read getConfig;
    property fileName: string read fFilename write setFname;
  end;

implementation

(*****************************************************************************
 * TProject
 *)
constructor TCEProject.create(aOwner: TComponent);
var
  defConf: TCompilerConfiguration;
begin
  inherited create(aOwner);
  fSrcs := TStringList.Create;
  fSrcs.OnChange := @subMemberChanged;
  fOptsColl := TCollection.create(TCompilerConfiguration);

  defConf := addConfiguration;
  defConf.name := 'default';
end;

destructor TCEProject.destroy;
begin
  fSrcs.free;
  fOptsColl.free;
  inherited;
end;

function TCEProject.addConfiguration: TCompilerConfiguration;
begin
  result := TCompilerConfiguration(fOptsColl.Add);
  result.onChanged := @subMemberChanged;
end;

procedure TCEProject.setOptsColl(const aValue: TCollection);
begin
  fOptsColl.Assign(aValue);
end;

procedure TCEProject.setFname(const aValue: string);
begin
  if fFilename = aValue then exit;
  fFilename := aValue;
  subMemberChanged(nil);
end;

procedure TCEProject.setSrcs(const aValue: TStringList);
begin
  fSrcs.Assign(aValue);
  subMemberChanged(nil);
end;

procedure TCEProject.setConfIx(aValue: Integer);
begin
  if fConfIx = aValue then exit;
  if aValue < 0 then aValue := 0;
  if aValue > fOptsColl.Count-1 then aValue := fOptsColl.Count-1;
  fConfIx := aValue;
  subMemberChanged(nil);
end;

procedure TCEProject.subMemberChanged(sender : TObject);
begin
  fModified := true;
end;

function TCEProject.getConfig(const ix: integer): TCompilerConfiguration;
begin
  result := TCompilerConfiguration(fOptsColl.Items[ix]);
end;

end.

