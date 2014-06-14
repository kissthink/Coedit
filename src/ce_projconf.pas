unit ce_projconf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, ce_widget, ce_common, ce_projconfall, ce_dmdwrap;

type

  { TCEProjectConfigurationWidget }
  TCEProjectConfigurationWidget = class(TCEWidget)
    btnAddConf: TButton;
    btnDelConf: TButton;
    selConf: TComboBox;
    frameEditAll: TCEProjConfAll;
    Panel1: TPanel;
    Tree: TTreeView;
    procedure btnAddConfClick(Sender: TObject);
    procedure btnDelConfClick(Sender: TObject);
    procedure selConfChange(Sender: TObject);
  private
    fProj: TCEProject;
  protected
    procedure updaterProc2; //override;
  public
    procedure projChange(const aProject: TCEProject); override;
    property project: TCEProject read fProj;
  end;

implementation
{$R *.lfm}

procedure TCEProjectConfigurationWidget.projChange(const aProject: TCEProject);
begin
  fProj := aProject;
  updaterProc2;
end;

procedure TCEProjectConfigurationWidget.selConfChange(Sender: TObject);
begin
  if fUpdating then exit;
  if fProj = nil then exit;
  if selConf.ItemIndex = -1 then exit;
  //
  fProj.ConfigurationIndex := selConf.ItemIndex;
  updaterProc2;
end;

procedure TCEProjectConfigurationWidget.btnAddConfClick(Sender: TObject);
var
  nme: string;
  cfg: TCompilerConfiguration;
begin
  if fProj = nil then exit;
  //
  cfg := fProj.addConfiguration;
  nme := '';
  if InputQuery('Configuration name', '', nme) then cfg.name := nme;
  fProj.ConfigurationIndex := cfg.Index;
end;

procedure TCEProjectConfigurationWidget.btnDelConfClick(Sender: TObject);
begin
  if fProj = nil then exit;
  if fProj.OptionsCollection.Count = 1 then exit;
  //
  frameEditAll.Grid.TIObject := nil;
  frameEditAll.Grid.Clear;
  frameEditAll.Invalidate;
  fProj.OptionsCollection.Delete(selConf.ItemIndex);
  fProj.ConfigurationIndex := 0;
  updaterProc2;
end;

procedure TCEProjectConfigurationWidget.updaterProc2;
var
  i: NativeInt;
  obj: TPersistent;
begin

  selConf.Clear;

  if (fProj = nil) then
  begin
    frameEditAll.Grid.Selection.Clear;
    frameEditAll.Grid.Clear;
    // AV if the previous TIObject is already destroyed
    frameEditAll.Grid.TIObject := nil;
    frameEditAll.Invalidate;
    exit;
  end;

  for i:= 0 to fProj.OptionsCollection.Count-1 do
    selConf.Items.Add(fProj.configuration[i].name);
  selConf.ItemIndex := fProj.ConfigurationIndex;

  obj := fProj.configuration[fProj.ConfigurationIndex];
  if frameEditAll.Grid.TIObject <> obj then
    frameEditAll.Grid.TIObject := obj;

end;

end.

