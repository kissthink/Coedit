unit ce_projinspect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, Graphics,
  actnlist, Dialogs, ExtCtrls, ComCtrls, Menus, Buttons, ce_project,
  ce_common, ce_widget, AnchorDocking;

type
  { TCEProjectInspectWidget }
  TCEProjectInspectWidget = class(TCEWidget)
    imgList: TImageList;
    Panel1: TPanel;
    btnAddFile: TSpeedButton;
    btnProjOpts: TSpeedButton;
    btnAddFold: TSpeedButton;
    btnRemFile: TSpeedButton;
    Tree: TTreeView;
    TreeFilterEdit1: TTreeFilterEdit;
    procedure btnAddFileClick(Sender: TObject);
    procedure btnAddFoldClick(Sender: TObject);
    procedure btnRemFileClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeSelectionChanged(Sender: TObject);
  protected
    procedure UpdateByEvent; override;
  private
    fActOpenFile: TAction;
    fActSelConf: TAction;
    fProject: TCEProject;
    fFileNode, fConfNode: TTreeNode;
    procedure actUpdate(sender: TObject);
    procedure TreeDblClick(sender: TObject);
    procedure actOpenFileExecute(sender: TObject);
  public
    constructor create(aOwner: TComponent); override;
    //
    procedure projNew(const aProject: TCEProject); override;
    procedure projChange(const aProject: TCEProject); override;
    procedure projClose(const aProject: TCEProject); override;
    //
    function contextName: string; override;
    function contextActionCount: integer; override;
    function contextAction(index: integer): TAction; override;
  end;

implementation
{$R *.lfm}

uses
  ce_main;

constructor TCEProjectInspectWidget.create(aOwner: TComponent);
begin
  fActOpenFile := TAction.Create(self);
  fActOpenFile.Caption := 'Open file in editor';
  fActOpenFile.OnExecute := @actOpenFileExecute;
  fActSelConf := TAction.Create(self);
  fActSelConf.Caption := 'Select configuration';
  fActSelConf.OnExecute := @actOpenFileExecute;
  fActSelConf.OnUpdate := @actUpdate;
  //
  inherited;
  Tree.OnDblClick := @TreeDblClick;
  fFileNode := Tree.Items[0];
  fConfNode := Tree.Items[1];
  //
  Tree.PopupMenu := contextMenu;
end;

function TCEProjectInspectWidget.contextName: string;
begin
  exit('Inspector');
end;

function TCEProjectInspectWidget.contextActionCount: integer;
begin
  exit(2);
end;

function TCEProjectInspectWidget.contextAction(index: integer): TAction;
begin
  case index of
    0: exit(fActOpenFile);
    1: exit(fActSelConf);
    else exit(nil);
  end;
end;

procedure TCEProjectInspectWidget.projNew(const aProject: TCEProject);
begin
  fProject := aProject;
  UpdateByEvent;
end;

procedure TCEProjectInspectWidget.projChange(const aProject: TCEProject);
begin
  fProject := aProject;
  UpdateByEvent;
end;

procedure TCEProjectInspectWidget.projClose(const aProject: TCEProject);
begin
  fProject := nil;
  UpdateByEvent;
end;

procedure TCEProjectInspectWidget.TreeKeyDown(Sender: TObject; var Key: Word;Shift: TShiftState);
begin
  if Key = 13 then TreeDblClick(nil);
end;

procedure TCEProjectInspectWidget.TreeSelectionChanged(Sender: TObject);
begin
  actUpdate(sender);
end;

procedure TCEProjectInspectWidget.TreeDblClick(sender: TObject);
var
  fname: string;
  i: NativeInt;
begin
  if fProject = nil then exit;
  if Tree.Selected = nil then exit;
  //
  if Tree.Selected.Parent = fFileNode then
  begin
    fname := Tree.Selected.Text;
    i := fProject.Sources.IndexOf(fname);
    if i > -1 then
    begin
      fname := fProject.getAbsoluteSourceName(i);
      if fileExists(fname) then
        CEMainForm.openFile(fname);
    end;
  end
  else if Tree.Selected.Parent = fConfNode then
  begin
    i := Tree.Selected.Index;
    fProject.ConfigurationIndex := i;
    UpdateByEvent;
  end;
end;

procedure TCEProjectInspectWidget.actOpenFileExecute(sender: TObject);
begin
  TreeDblClick(sender);
end;

procedure TCEProjectInspectWidget.actUpdate(sender: TObject);
begin
  fActSelConf.Enabled := false;
  fActOpenFile.Enabled := false;
  if Tree.Selected = nil then exit;
  fActSelConf.Enabled := Tree.Selected.Parent = fConfNode;
  fActOpenFile.Enabled := Tree.Selected.Parent = fFileNode;
end;

procedure TCEProjectInspectWidget.btnAddFileClick(Sender: TObject);
begin
  if fProject = nil then exit;
  //
  with TOpenDialog.Create(nil) do
  try
    filter := DdiagFilter;
    if execute then
      fProject.addSource(filename);
  finally
    free;
  end;
end;

procedure TCEProjectInspectWidget.btnAddFoldClick(Sender: TObject);
var
  dir, fname, ext: string;
  lst: TStringList;
  i: NativeInt;
begin
  if fProject = nil then exit;
  //
  if fileExists(fProject.fileName) then
    dir := extractFilePath(fProject.fileName)
  else dir := '';
  if selectDirectory('sources', dir, dir, true, 0) then
  begin
    lst := TStringList.Create;
    try
      listFiles(lst, dir, true);
      for i := 0 to lst.Count-1 do
      begin
        fname := lst.Strings[i];
        ext := extractFileExt(fname);
        if (ext = '.d') or (ext = '.di') then
          fProject.addSource(fname);
      end;
    finally
      lst.Free;
    end;
  end;
end;

procedure TCEProjectInspectWidget.btnRemFileClick(Sender: TObject);
var
  fname: string;
  i: NativeInt;
begin
  if fProject = nil then exit;
  if Tree.Selected = nil then exit;
  //
  if Tree.Selected.Parent = fFileNode then
  begin
    fname := Tree.Selected.Text;
    i := fProject.Sources.IndexOf(fname);
    if i > -1 then fProject.Sources.Delete(i);
    UpdateByEvent;
  end;
end;

procedure TCEProjectInspectWidget.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  fname: string;
begin
  CEMainForm.FormDropFiles(Sender, Filenames);
  if fProject = nil then exit;
  for fname in Filenames do fProject.addSource(fname);
end;

procedure TCEProjectInspectWidget.UpdateByEvent;
var
  src, conf: string;
  itm: TTreeNode;
  i: NativeInt;
begin
  fConfNode.DeleteChildren;
  fFileNode.DeleteChildren;
  if fProject = nil then exit;
  //
  for src in fProject.Sources do
   begin
     itm := Tree.Items.AddChild(fFileNode, src);
     itm.ImageIndex := 2;
     itm.SelectedIndex := 2;
   end;
  for i := 0 to fProject.OptionsCollection.Count-1 do
  begin
    conf := fProject.configuration[i].name;
    if i = fProject.ConfigurationIndex then conf += ' (active)';
    itm := Tree.Items.AddChild(fConfNode, conf);
    itm.ImageIndex := 3;
    itm.SelectedIndex:= 3;
  end;
end;

end.
