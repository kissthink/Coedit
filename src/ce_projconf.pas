unit ce_projconf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, StdCtrls, Menus, Buttons, PropEdits, ObjectInspector,
  ce_dmdwrap, ce_project, ce_widget;

type

  { TCEProjectConfigurationWidget }
  TCEProjectConfigurationWidget = class(TCEWidget)
    imgList: TImageList;
    selConf: TComboBox;
    Panel1: TPanel;
    btnAddConf: TSpeedButton;
    btnDelConf: TSpeedButton;
    btnCloneConf: TSpeedButton;
    Splitter1: TSplitter;
    Grid: TTIPropertyGrid;
    Tree: TTreeView;
    procedure btnAddConfClick(Sender: TObject);
    procedure btnDelConfClick(Sender: TObject);
    procedure btnCloneCurrClick(Sender: TObject);
    procedure GridEditorFilter(Sender: TObject; aEditor: TPropertyEditor;var aShow: boolean);
    procedure selConfChange(Sender: TObject);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
  private
    fProj: TCEProject;
    function getGridTarget: TPersistent;
  protected
    procedure UpdateByEvent; override;
  public
    constructor create(aOwner: TComponent); override;
    procedure projNew(const aProject: TCEProject); override;
    procedure projChange(const aProject: TCEProject); override;
    procedure projClose(const aProject: TCEProject); override;
    property project: TCEProject read fProj;
  end;

implementation
{$R *.lfm}

constructor TCEProjectConfigurationWidget.create(aOwner: TComponent);
begin
  inherited;
  Tree.Selected := Tree.Items.GetLastNode;
end;

procedure TCEProjectConfigurationWidget.projNew(const aProject: TCEProject);
begin
  beginUpdateByEvent;
  fProj := aProject;
  endUpdateByEvent;
end;

procedure TCEProjectConfigurationWidget.projChange(const aProject: TCEProject);
begin
  beginUpdateByEvent;
  fProj := aProject;
  endUpdateByEvent;
end;

procedure TCEProjectConfigurationWidget.projClose(const aProject: TCEProject);
begin
  Grid.TIObject := nil;
  Grid.ItemIndex :=-1;
  fProj := nil;
end;

procedure TCEProjectConfigurationWidget.selConfChange(Sender: TObject);
begin
  if fProj = nil then exit;
  if Updating then exit;
  if selConf.ItemIndex = -1 then exit;
  //
  beginUpdateByEvent;
  fProj.ConfigurationIndex := selConf.ItemIndex;
  endUpdateByEvent;
end;

procedure TCEProjectConfigurationWidget.TreeChange(Sender: TObject;
  Node: TTreeNode);
begin
  Grid.TIObject := getGridTarget;
end;

procedure TCEProjectConfigurationWidget.GridEditorFilter(Sender: TObject;
  aEditor: TPropertyEditor; var aShow: boolean);
begin
  if aEditor.ClassType = TCollectionPropertyEditor then aShow := false;
end;

procedure TCEProjectConfigurationWidget.btnAddConfClick(Sender: TObject);
var
  nme: string;
  cfg: TCompilerConfiguration;
begin
  if fProj = nil then exit;
  //
  nme := '';
  beginUpdateByEvent;
  cfg := fProj.addConfiguration;
  if InputQuery('Configuration name', '', nme) then cfg.name := nme;
  fProj.ConfigurationIndex := cfg.Index;
  endUpdateByEvent;
end;

procedure TCEProjectConfigurationWidget.btnDelConfClick(Sender: TObject);
begin
  if fProj = nil then exit;
  if fProj.OptionsCollection.Count = 1 then exit;
  //
  beginUpdateByEvent;
  Grid.TIObject := nil;
  Grid.Clear;
  Invalidate;
  fProj.OptionsCollection.Delete(selConf.ItemIndex);
  fProj.ConfigurationIndex := 0;
  endUpdateByEvent;
end;

procedure TCEProjectConfigurationWidget.btnCloneCurrClick(Sender: TObject);
var
  nme: string;
  trg,src: TCompilerConfiguration;
begin
  if fProj = nil then exit;
  //
  nme := '';
  beginUpdateByEvent;
  src := fProj.currentConfiguration;
  trg := fProj.addConfiguration;
  trg.assign(src);
  if InputQuery('Configuration name', '', nme) then trg.name := nme;
  fProj.ConfigurationIndex := trg.Index;
  endUpdateByEvent;
end;

function TCEProjectConfigurationWidget.getGridTarget: TPersistent;
begin
  if fProj = nil then exit(nil);
  if fProj.ConfigurationIndex = -1 then exit(nil);
  if Tree.Selected = nil then exit(nil);
  // Warning: TTreeNode.StateIndex is usually made for the images...it's not a tag
  case Tree.Selected.StateIndex of
    1: exit( fProj );
    2: exit( fProj.currentConfiguration.messagesOptions );
    3: exit( fProj.currentConfiguration.debugingOptions );
    4: exit( fProj.currentConfiguration.documentationOptions );
    5: exit( fProj.currentConfiguration.outputOptions );
    6: exit( fProj.currentConfiguration.otherOptions );
    7: exit( fProj.currentConfiguration.pathsOptions );
    8: exit( fProj.currentConfiguration );
    else result := nil;
  end;
end;

procedure TCEProjectConfigurationWidget.UpdateByEvent;
var
  i: NativeInt;
begin
  selConf.ItemIndex:= -1;
  selConf.Clear;
  for i:= 0 to fProj.OptionsCollection.Count-1 do
    selConf.Items.Add(fProj.configuration[i].name);
  selConf.ItemIndex := fProj.ConfigurationIndex;

  Grid.TIObject := getGridTarget;
end;

end.

