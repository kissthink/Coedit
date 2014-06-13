unit ce_project;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, ce_common, ce_widget;

type
  { TCEProjectWidget }
  TCEProjectWidget = class(TCEWidget)
    imgList: TImageList;
    Tree: TTreeView;
  private
    fProject: TCEProject;
    fFileNode, fConfNode: TTreeNode;
    procedure updateView;
    procedure TreeDblClick(sender: TObject);
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    procedure projChange(const aProject: TCEProject); override;
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

destructor TCEProjectWidget.destroy;
begin
  inherited;
end;

procedure TCEProjectWidget.projChange(const aProject: TCEProject);
begin
  fProject := aProject;
  updateView;
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
      mainForm.openFile(fname);
    end;
  end
  else if Tree.Selected.Parent = fConfNode then
  begin
    i := Tree.Selected.Index;
    fProject.ConfigurationIndex:= i;
  end;
end;

procedure TCEProjectWidget.updateView;
var
  src, conf: string;
  itm: TTreeNode;
  i: NativeInt;
begin
  if fProject = nil then exit;
  //
  fConfNode.DeleteChildren;
  fFileNode.DeleteChildren;
  for src in fProject.Sources do
   begin
     itm := Tree.Items.AddChild(fFileNode, src);
     itm.ImageIndex := 2;
     itm.SelectedIndex := 2;
   end;
  for i := 0 to fProject.OptionsCollection.Count-1 do
  begin
    conf := fProject.configuration[i].name;
    itm := Tree.Items.AddChild(fConfNode, conf);
    itm.ImageIndex := 3;
    itm.SelectedIndex:= 3;
  end;
end;

end.

