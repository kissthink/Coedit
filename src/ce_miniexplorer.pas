unit ce_miniexplorer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, ComCtrls, Buttons, ce_widget, lcltype;

type

  { TCEMiniExplorerWidget }
  TCEMiniExplorerWidget = class(TCEWidget)
    Bevel1: TBevel;
    Bevel2: TBevel;
    btnAddFav: TBitBtn;
    btnEdit: TBitBtn;
    btnShellOpen: TBitBtn;
    btnRemFav: TBitBtn;
    imgList: TImageList;
    lstFiles: TListView;
    lstFav: TListView;
    Panel1: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Tree: TTreeView;
    procedure btnEditClick(Sender: TObject);
    procedure btnShellOpenClick(Sender: TObject);
    procedure btnAddFavClick(Sender: TObject);
    procedure btnRemFavClick(Sender: TObject);
    procedure lstFilesDblClick(Sender: TObject);
  private
    fFavorites: TStringList;
    fLastFold: string;
    procedure optset_LastFold(aReader: TReader);
    procedure optget_LastFold(aWriter: TWriter);
    procedure optset_Favs(aReader: TReader);
    procedure optget_Favs(aWriter: TWriter);
    procedure updateFavorites;
    procedure treeSetRoots;
    procedure lstFilesFromTree;
    procedure treeScanSubFolders(aRoot: TTreeNode);
    procedure treeClick(sender: TObject);
    procedure treeChanged(Sender: TObject; Node: TTreeNode);
    procedure treeCollapsed(Sender: TObject; Node: TTreeNode);
    procedure treeExpanded(Sender: TObject; Node: TTreeNode);
    procedure treeCollapsing(Sender: TObject; Node: TTreeNode; var allow: boolean);
    procedure treeExpanding(Sender: TObject; Node: TTreeNode; var allow: boolean);
    procedure treeDeletion(Sender: TObject; Item: TTreeNode);
    procedure treeSelectionChanged(sender: TObject);
    procedure favStringsChange(sender: TObject);
    procedure fillLstFiles(const aList: TStrings);
    procedure lstDeletion(Sender: TObject; Item: TListItem);
    procedure lstFavSelect(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure shellOpenSelected;
  public
    constructor create(aIwner: TComponent); override;
    destructor destroy; override;
    //
    procedure declareProperties(aFiler: TFiler); override;
  end;

implementation
{$R *.lfm}

uses
  ce_main, ce_common;

//TODO-cbugfix: click on the expander glyph, sometime the subdirs are not scanned but the fake sub item is still displayed

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEMiniExplorerWidget.create(aIwner: TComponent);
begin
  inherited;
  fFavorites := TStringList.Create;
  fFavorites.onChange := @favStringsChange;
  lstFiles.OnDeletion := @lstDeletion;
  lstFav.OnDeletion := @lstDeletion;
  lstFav.OnSelectItem := @lstFavSelect;

  Tree.OnClick := @treeClick;
  Tree.OnChange := @treeChanged;
  Tree.OnDeletion := @treeDeletion;
  Tree.OnSelectionChanged := @treeSelectionChanged;
  Tree.OnExpanded := @treeExpanded;
  Tree.OnCollapsed := @treeCollapsed;
  Tree.OnCollapsing := @treeCollapsing;
  Tree.OnExpanding := @treeExpanding;
  //
  treeSetRoots;
end;

destructor TCEMiniExplorerWidget.destroy;
begin
  fFavorites.Free;
  inherited;
end;

procedure TCEMiniExplorerWidget.lstDeletion(Sender: TObject; Item: TListItem);
begin
  if Item.Data <> nil then
    DisposeStr(PString(Item.Data));
end;
{$ENDREGION}

{$REGION ICEWidgetPersist ------------------------------------------------------}
procedure TCEMiniExplorerWidget.declareProperties(aFiler: TFiler);
begin
  inherited;
  aFiler.DefineProperty(Name + '_LastFolder', @optset_LastFold, @optget_LastFold, true);
  aFiler.DefineProperty(Name + '_FavoritesFolders', @optset_Favs, @optget_Favs, true);
end;

procedure TCEMiniExplorerWidget.optset_LastFold(aReader: TReader);
var
  lst: TstringList;
begin
  fLastFold := aReader.ReadString;
  if not directoryExists(fLastFold) then exit;
  //
  lst := TStringList.Create;
  try
    listFiles(lst, fLastFold);
    fillLstFiles(lst);
  finally
    lst.Free;
  end;
end;

procedure TCEMiniExplorerWidget.optget_LastFold(aWriter: TWriter);
begin
  aWriter.WriteString(fLastFold);
end;

procedure TCEMiniExplorerWidget.optset_Favs(aReader: TReader);
begin
  fFavorites.DelimitedText := aReader.ReadString;
  if fLastFold <> '' then
    if fFavorites.IndexOf(fLastFold) = -1 then
      fFavorites.Add(fLastFold);
  updateFavorites;
end;

procedure TCEMiniExplorerWidget.optget_Favs(aWriter: TWriter);
begin
  aWriter.WriteString(fFavorites.DelimitedText);
end;
{$ENDREGION}

{$REGION Favorites -------------------------------------------------------------}
procedure TCEMiniExplorerWidget.favStringsChange(sender: TObject);
begin
  updateFavorites;
end;

procedure TCEMiniExplorerWidget.updateFavorites;
var
  itm: TListItem;
  fold: string;
  dat: PString;
begin
  lstFav.Clear;
  for fold in fFavorites do
  begin
    itm := lstFav.Items.Add;
    itm.Caption := shortenPath(fold);
    dat := NewStr(fold);
    itm.Data := dat;
    itm.ImageIndex := 2;
  end;
end;

procedure TCEMiniExplorerWidget.lstFavSelect(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  lst: TStringList;
begin
  if not Selected then exit;
  //
  fLastFold := PString(Item.Data)^;
  lstFiles.Clear;
  lst := TStringList.Create;
  try
    listFiles(lst, fLastFold);
    fillLstFiles(lst);
  finally
  end;
end;

procedure TCEMiniExplorerWidget.btnRemFavClick(Sender: TObject);
var
  i: Integer;
begin
  if lstFav.Selected = nil then exit;
  i := fFavorites.IndexOf(PString(lstFav.Selected.Data)^);
  if i <> -1 then fFavorites.Delete(i);
  lstFiles.Clear;
end;

procedure TCEMiniExplorerWidget.lstFilesDblClick(Sender: TObject);
begin
  shellOpenSelected;
end;

procedure TCEMiniExplorerWidget.btnAddFavClick(Sender: TObject);
begin
  if Tree.Selected = nil then exit;
  fFavorites.Add(PString(Tree.Selected.Data)^);
end;
{$ENDREGION}

{$REGION Files -----------------------------------------------------------------}
procedure TCEMiniExplorerWidget.fillLstFiles(const aList: TStrings);
var
  itm: TListItem;
  fname: string;
  dat: PString;
begin
  lstFiles.Clear;
  for fname in aList do
  begin
    itm := lstFiles.Items.Add;
    itm.Caption := extractFileName(fname);
    dat := NewStr(fname);
    itm.Data := dat;
    itm.ImageIndex := 0;
  end;
end;

procedure TCEMiniExplorerWidget.btnShellOpenClick(Sender: TObject);
begin
  shellOpenSelected;
end;

procedure TCEMiniExplorerWidget.btnEditClick(Sender: TObject);
var
  fname: string;
begin
  if lstFiles.Selected = nil then exit;
  if lstFiles.Selected.Data = nil then exit;
  fname := PString(lstFiles.Selected.Data)^;
  if not fileExists(fname) then exit;
  CEMainForm.openFile(fname);
end;

procedure TCEMiniExplorerWidget.shellOpenSelected;
var
  fname: string;
begin
  if lstFiles.Selected = nil then exit;
  if lstFiles.Selected.Data = nil then exit;
  fname := PString(lstFiles.Selected.Data)^;
  if not fileExists(fname) then exit;
  if not shellOpen(fname) then CEMainForm.MessageWidget.addCeErr
    (format('the shell failed to open "%s"',[shortenPath(fname,25)]));
end;

{$ENDREGION}

{$REGION Tree ------------------------------------------------------------------}
procedure TCEMiniExplorerWidget.treeDeletion(Sender: TObject; Item: TTreeNode);
begin
  if Item.Data <> nil then
    DisposeStr(PString(Item.Data));
end;

procedure TCEMiniExplorerWidget.treeSetRoots;
var
  drv: string;
  itm: TTreeNode;
  lst: TStringList;
begin
  Tree.Items.Clear;
  lst := TStringList.Create;
  try
    listDrives(lst);
    for drv in lst do
    begin
      itm := Tree.Items.Add(nil, drv);
      itm.Data := NewStr(drv[1..length(drv)-1]);
      treeScanSubFolders(itm);
    end;
  finally
    lst.Free;
  end;
end;

procedure TCEMiniExplorerWidget.lstFilesFromTree;
var
  lst: TStringList;
  pth: string;
begin
  if Tree.Selected = nil then exit;
  //
  lst := TStringList.Create;
  try
    pth := PString(Tree.Selected.Data)^;
    listFiles(lst, pth);
    fillLstFiles(lst);
  finally
    lst.Free;
  end;
end;

procedure TCEMiniExplorerWidget.treeScanSubFolders(aRoot: TTreeNode);
var
  lst: TStringList;
  fold: string;
  itm: TTreeNode;
begin
  aRoot.DeleteChildren; // delete the fake item...
  lst := TStringList.Create;
  try
    listFolders(lst, PString(aRoot.Data)^ + directorySeparator);
    for fold in lst do
    begin
      itm := Tree.Items.AddChild(aRoot, extractFileName(fold));
      itm.Data := NewStr(fold);
      itm.ImageIndex := 1;
      itm.SelectedIndex := 1;
      //
      if hasFolder(fold) then
        Tree.Items.AddChild(itm, ''); //...created here to show the expander glyph
    end;
  finally
    lst.Free;
  end;
end;

procedure TCEMiniExplorerWidget.treeCollapsing(Sender: TObject; Node: TTreeNode; var allow: boolean);
begin
end;

procedure TCEMiniExplorerWidget.treeExpanding(Sender: TObject; Node: TTreeNode; var allow: boolean);
begin
end;

procedure TCEMiniExplorerWidget.treeCollapsed(Sender: TObject; Node: TTreeNode);
begin
end;

procedure TCEMiniExplorerWidget.treeExpanded(Sender: TObject; Node: TTreeNode);
begin
end;

procedure TCEMiniExplorerWidget.treeChanged(Sender: TObject; Node: TTreeNode);
begin
  if Node = nil then exit;
  Node.DeleteChildren;
  treeScanSubFolders(Node);
  lstFilesFromTree;
end;

procedure TCEMiniExplorerWidget.treeSelectionChanged(sender: TObject);
begin
  lstFilesFromTree;
end;

procedure TCEMiniExplorerWidget.treeClick(sender: TObject);
begin
  if Tree.Selected = nil then exit;
  if Tree.Selected.Expanded then exit;
  treeScanSubFolders(Tree.Selected);
end;
{$ENDREGION}

end.

