unit ce_projconf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, Menus, Buttons, ce_widget, ce_common, ce_projconfall,
  ce_dmdwrap;

type

  { TCEProjectConfigurationWidget }
  TCEProjectConfigurationWidget = class(TCEWidget)
    imgList: TImageList;
    selConf: TComboBox;
    frameEditAll: TCEProjConfAll;
    Panel1: TPanel;
    btnAddConf: TSpeedButton;
    btnDelConf: TSpeedButton;
    btnCloneConf: TSpeedButton;
    Tree: TTreeView;
    procedure btnAddConfClick(Sender: TObject);
    procedure btnDelConfClick(Sender: TObject);
    procedure btnCloneCurrClick(Sender: TObject);
    procedure selConfChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    fProj: TCEProject;
  protected
    procedure manualWidgetUpdate; override;
  public
    procedure projNew(const aProject: TCEProject); override;
    procedure projChange(const aProject: TCEProject); override;
    procedure projClose(const aProject: TCEProject); override;
    property project: TCEProject read fProj;
  end;

implementation
{$R *.lfm}

procedure TCEProjectConfigurationWidget.projNew(const aProject: TCEProject);
begin
  beginManualWidgetUpdate;
  fProj := aProject;
  endManualWidgetUpdate;
end;

procedure TCEProjectConfigurationWidget.projChange(const aProject: TCEProject);
begin
  beginManualWidgetUpdate;
  fProj := aProject;
  endManualWidgetUpdate;
end;

procedure TCEProjectConfigurationWidget.projClose(const aProject: TCEProject);
begin
  frameEditAll.Grid.TIObject := nil;
  frameEditAll.Grid.ItemIndex :=-1;
  fProj := nil;
end;

procedure TCEProjectConfigurationWidget.selConfChange(Sender: TObject);
begin
  if fProj = nil then exit;
  if isManualUpdating then exit;
  if selConf.ItemIndex = -1 then exit;
  //
  beginManualWidgetUpdate;
  fProj.ConfigurationIndex := selConf.ItemIndex;
  endManualWidgetUpdate;
end;

procedure TCEProjectConfigurationWidget.SpeedButton1Click(Sender: TObject);
begin

end;

procedure TCEProjectConfigurationWidget.btnAddConfClick(Sender: TObject);
var
  nme: string;
  cfg: TCompilerConfiguration;
begin
  if fProj = nil then exit;
  //
  nme := '';
  beginManualWidgetUpdate;
  cfg := fProj.addConfiguration;
  if InputQuery('Configuration name', '', nme) then cfg.name := nme;
  fProj.ConfigurationIndex := cfg.Index;
  endManualWidgetUpdate;
end;

procedure TCEProjectConfigurationWidget.btnDelConfClick(Sender: TObject);
begin
  if fProj = nil then exit;
  if fProj.OptionsCollection.Count = 1 then exit;
  //
  beginManualWidgetUpdate;
  frameEditAll.Grid.TIObject := nil;
  frameEditAll.Grid.Clear;
  frameEditAll.Invalidate;
  fProj.OptionsCollection.Delete(selConf.ItemIndex);
  fProj.ConfigurationIndex := 0;
  endManualWidgetUpdate;
end;

procedure TCEProjectConfigurationWidget.btnCloneCurrClick(Sender: TObject);
var
  nme: string;
  trg,src: TCompilerConfiguration;
begin
  if fProj = nil then exit;
  //
  nme := '';
  beginManualWidgetUpdate;
  src := fProj.currentConfiguration;
  trg := fProj.addConfiguration;
  trg.assign(src);
  if InputQuery('Configuration name', '', nme) then trg.name := nme;
  fProj.ConfigurationIndex := trg.Index;
  endManualWidgetUpdate;
end;

procedure TCEProjectConfigurationWidget.manualWidgetUpdate;
var
  i: NativeInt;
begin
  selConf.ItemIndex:= -1;
  selConf.Clear;
  for i:= 0 to fProj.OptionsCollection.Count-1 do
    selConf.Items.Add(fProj.configuration[i].name);
  selConf.ItemIndex := fProj.ConfigurationIndex;

  frameEditAll.Grid.TIObject :=
    fProj.configuration[fProj.ConfigurationIndex];
end;

end.

