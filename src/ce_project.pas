unit ce_project;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, ComCtrls, Menus, Buttons, ce_common, ce_widget;

type
  { TCEProjectWidget }
  TCEProjectWidget = class(TCEWidget)
    imgList: TImageList;
    Panel1: TPanel;
    btnAddFile: TSpeedButton;
    btnProjOpts: TSpeedButton;
    btnAddFold: TSpeedButton;
    Tree: TTreeView;
    TreeFilterEdit1: TTreeFilterEdit;
    procedure btnAddFileClick(Sender: TObject);
    procedure btnAddFoldClick(Sender: TObject);
  protected
    procedure manualWidgetUpdate; override;
  private
    fProject: TCEProject;
    fFileNode, fConfNode: TTreeNode;
    procedure TreeDblClick(sender: TObject);
  public
    constructor create(aOwner: TComponent); override;
    //
    procedure projNew(const aProject: TCEProject); override;
    procedure projChange(const aProject: TCEProject); override;
    procedure projClose(const aProject: TCEProject); override;
  end;

implementation
{$R *.lfm}

uses
  ce_main;

constructor TCEProjectWidget.create(aOwner: TComponent);
begin
  inherited;
  fID := 'ID_PROJ';
  Tree.OnDblClick := @TreeDblClick;
  fFileNode := Tree.Items[0];
  fConfNode := Tree.Items[1];
end;

procedure TCEProjectWidget.projNew(const aProject: TCEProject);
begin
  fProject := aProject;
  manualWidgetUpdate;
end;

procedure TCEProjectWidget.projChange(const aProject: TCEProject);
begin
  fProject := aProject;
  manualWidgetUpdate;
end;

procedure TCEProjectWidget.projClose(const aProject: TCEProject);
begin
  fProject := nil;
  manualWidgetUpdate;
end;

procedure TCEProjectWidget.TreeDblClick(sender: TObject);
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
        mainForm.openFile(fname);
    end;
  end
  else if Tree.Selected.Parent = fConfNode then
  begin
    i := Tree.Selected.Index;
    fProject.ConfigurationIndex := i;
    manualWidgetUpdate;
  end;
end;

procedure TCEProjectWidget.btnAddFileClick(Sender: TObject);
begin
  if fProject = nil then exit;
  //
  with TOpenDialog.Create(nil) do
  try
    filter := 'd source|*.d|d interface|*.di|all files|*.*';
    if execute then
      fProject.addSource(filename);
  finally
    free;
  end;
end;

procedure TCEProjectWidget.btnAddFoldClick(Sender: TObject);
var
  dir, ext, fname: string;
  sr: TSearchRec;
  lst: TStringList;
begin
  if fProject = nil then exit;
  //
  if fileexists(fProject.fileName) then
    dir := extractfilePath(fProject.fileName)
  else dir := '';
  if selectDirectory(dir, [], 0) then
  begin
    if FindFirst(dir + DirectorySeparator + '*.*', faAnyFile, sr ) = 0 then
    try
      lst := TStringList.Create;
      ext := ExtractFileExt(sr.Name);
      if (ext = '.d') or (ext = '.di') then
        lst.Add(dir + DirectorySeparator + sr.Name);
      while FindNext(sr) = 0 do
      begin
        ext := ExtractFileExt(sr.Name);
        if (ext = '.d') or (ext = '.di') then
          lst.Add(dir + DirectorySeparator + sr.Name);
      end;
      for fname in lst do
        fProject.addSource(fname);
    finally
      lst.Free;
    end;
  end;
end;

procedure TCEProjectWidget.manualWidgetUpdate;
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

