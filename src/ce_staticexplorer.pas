unit ce_staticexplorer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Menus, ComCtrls, ce_widget, jsonparser, fpjson,
  ce_synmemo, process, actnlist, ce_common, ce_project, AnchorDocking;

type

  { TCEStaticExplorerWidget }
  TCEStaticExplorerWidget = class(TCEWidget)
    imgList: TImageList;
    Panel1: TPanel;
    Tree: TTreeView;
    TreeFilterEdit1: TTreeFilterEdit;
    procedure TreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure TreeKeyPress(Sender: TObject; var Key: char);
  private
    fActRefresh: TAction;
    fActAutoRefresh: TAction;
    fActSelectInSource: TAction;
    fDoc: TCESynMemo;
    fProj: TCEProject;
    fAutoRefresh: boolean;
    ndAlias, ndClass, ndEnum, ndFunc: TTreeNode;
    ndImp, ndIntf, ndMix, ndStruct, ndTmp, ndVar: TTreeNode;
    procedure Rescan;
    procedure TreeDblClick(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actAutoRefreshExecute(Sender: TObject);
  protected
    procedure UpdateByDelay; override;
  published
    property autoRefresh: boolean read fAutoRefresh write fAutoRefresh;
  public
    constructor create(aOwner: TComponent); override;
    //
    procedure docFocused(const aDoc: TCESynMemo); override;
    procedure docChanged(const aDoc: TCESynMemo); override;
    procedure docClose(const aDoc: TCESynMemo); override;
    //
    function contextName: string; override;
    function contextActionCount: integer; override;
    function contextAction(index: integer): TAction; override;
    //
    procedure projNew(const aProject: TCEProject); override;
    procedure projChange(const aProject: TCEProject); override;
    procedure projClose(const aProject: TCEProject); override;
  end;

implementation
{$R *.lfm}

constructor TCEStaticExplorerWidget.create(aOwner: TComponent);
begin
  fAutoRefresh := true;
  fActRefresh := TAction.Create(self);
  fActRefresh.OnExecute := @actRefreshExecute;
  fActRefresh.Caption := 'Refresh';
  fActAutoRefresh := TAction.Create(self);
  fActAutoRefresh.OnExecute := @actAutoRefreshExecute;
  fActAutoRefresh.Caption := 'Auto-refresh';
  fActAutoRefresh.AutoCheck := true;
  fActAutoRefresh.Checked := fAutoRefresh;
  fActSelectInSource := TAction.Create(self);
  fActSelectInSource.OnExecute := @TreeDblClick;
  fActSelectInSource.Caption := 'Select in source';
  //
  inherited;
  fID := 'ID_SEXPL';
  //
  ndAlias   := Tree.Items[0];
  ndClass   := Tree.Items[1];
  ndEnum    := Tree.Items[2];
  ndFunc    := Tree.Items[3];
  ndImp     := Tree.Items[4];
  ndIntf    := Tree.Items[5];
  ndMix     := Tree.Items[6];
  ndStruct  := Tree.Items[7];
  ndTmp     := Tree.Items[8];
  ndVar     := Tree.Items[9];
  //
  Tree.OnDblClick := @TreeDblClick;
  Tree.PopupMenu := contextMenu;
  DockMaster.GetAnchorSite(Self).Name := ID;
end;

function TCEStaticExplorerWidget.contextName: string;
begin
  result := 'Static explorer';
end;

function TCEStaticExplorerWidget.contextActionCount: integer;
begin
  result := 3;
end;

function TCEStaticExplorerWidget.contextAction(index: integer): TAction;
begin
  case index of
    0: result := fActSelectInSource;
    1: result := fActRefresh;
    2: result := fActAutoRefresh;
    else result := nil;
  end;
end;

procedure TCEStaticExplorerWidget.actRefreshExecute(Sender: TObject);
begin
  if Updating then exit;
  Rescan;
end;

procedure TCEStaticExplorerWidget.actAutoRefreshExecute(Sender: TObject);
begin
  AutoRefresh := not AutoRefresh;
end;

procedure TCEStaticExplorerWidget.projNew(const aProject: TCEProject);
begin
  fProj := aProject;
end;

procedure TCEStaticExplorerWidget.projChange(const aProject: TCEProject);
begin
  fProj := aProject;
end;

procedure TCEStaticExplorerWidget.projClose(const aProject: TCEProject);
begin
  fProj := nil;
end;

procedure TCEStaticExplorerWidget.docFocused(const aDoc: TCESynMemo);
begin
  fDoc := aDoc;
  beginUpdateByDelay;
end;

procedure TCEStaticExplorerWidget.docChanged(const aDoc: TCESynMemo);
begin
  if fDoc <> aDoc then exit;
  beginUpdateByDelay;
end;

procedure TCEStaticExplorerWidget.docClose(const aDoc: TCESynMemo);
begin
  if fDoc <> aDoc then exit;
  fDoc := nil;
  beginUpdateByDelay;
end;

procedure TCEStaticExplorerWidget.UpdateByDelay;
begin
  if not fAutoRefresh then exit;
  Rescan;
end;

procedure TCEStaticExplorerWidget.TreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  if (node.Data <> nil) then
    Dispose(PInt64(node.Data));
end;

procedure TCEStaticExplorerWidget.TreeKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then TreeDblClick(nil);
end;

procedure TCEStaticExplorerWidget.TreeDblClick(Sender: TObject);
var
  line: Int64;
begin
  if fDoc = nil then exit;
  if Tree.Selected = nil then exit;
  if Tree.Selected.Data = nil then exit;
  //
  line := PInt64(Tree.Selected.Data)^;
  fDoc.CaretY := line;
  fDoc.SelectLine;
end;

procedure TCEStaticExplorerWidget.Rescan;
var
  dmdproc: TProcess;
  lines: TStringList;
  str: TMemoryStream;
  prs: TJsonParser;
  dat: TJsonData;
  memb: TJsonData;
  jsf, scf: string;
  ndCat: TTreeNode;
  ln: PInt64;
  nme: string;
  i: NativeInt;

  // recursivively display members, without master categories.
  procedure digMembers(const srcDt: TJsonData; const srcNd: TTreeNode);
  var
    _memb: TJsonData;
    _ln: PInt64;
    _nme: string;
    _i: NativeInt;
    _nd: TTreeNode;
  begin
    _memb := srcDt.FindPath('members');
    if _memb <> nil then for _i := 0 to _memb.Count-1 do
    begin
      _ln   := new(PInt64);
      _ln^  := _memb.Items[_i].GetPath('line').AsInt64;
      _nme  := _memb.Items[_i].GetPath('name').AsString;
      _nd   := Tree.Items.AddChildObject(srcNd, _nme, _ln);
      digMembers(_memb.Items[_i], _nd);
    end;
  end;

begin
  if ndAlias = nil then exit;

  ndAlias.DeleteChildren;
  ndClass.DeleteChildren;
  ndEnum.DeleteChildren;
  ndFunc.DeleteChildren;
  ndImp.DeleteChildren;
  ndIntf.DeleteChildren;
  ndMix.DeleteChildren;
  ndStruct.DeleteChildren;
  ndTmp.DeleteChildren;
  ndVar.DeleteChildren;

  ndAlias.Visible := false;
  ndClass.Visible := false;
  ndEnum.Visible := false;
  ndFunc.Visible := false;
  ndImp.Visible := false;
  ndIntf.Visible := false;
  ndMix.Visible := false;
  ndStruct.Visible := false;
  ndTmp.Visible := false;
  ndVar.Visible := false;

  if fDoc = nil then exit;
  if fDoc.Lines.Count = 0 then exit;
  memb := nil;

  // generate json
  // note: only if
  //  - the imports can be found (either using the project infos or sc.conf)
  //  - the source is error-less
  dmdproc := TProcess.Create(nil);
  lines := TStringList.Create;
  try
    jsf := GetTempDir(false);
    jsf += uniqueObjStr(dmdProc) + '.json';
    scf := GetTempDir(false);
    scf += uniqueObjStr(dmdProc) + '.d';
    //
    lines.Assign(fDoc.Lines);
    lines.SaveToFile(scf);
    //
    dmdProc.ShowWindow := swoHIDE;
    dmdproc.Options := [];
    dmdproc.Executable := 'dmd';
    dmdproc.Parameters.Add(scf);
    dmdproc.Parameters.Add('-c');
    dmdproc.Parameters.Add('-o-');
    dmdproc.Parameters.Add('-X');
    dmdproc.Parameters.Add('-Xf' + jsf);
    if fProj <> nil then
    begin
      dmdProc.CurrentDirectory := extractFilePath(fProj.fileName);
      if fProj <> nil then for i := 0 to fProj.Sources.Count-1 do
        dmdproc.Parameters.Add('-I' + fProj.getAbsoluteSourceName(i));
      for nme in fProj.Sources do
        dmdproc.Parameters.Add('-I' + extractFilePath(nme));
      for nme in fProj.currentConfiguration.pathsOptions.Includes do
        dmdproc.Parameters.Add('-I' + nme);
    end;
    dmdproc.Execute;
    while dmdproc.Running do;
  finally
    i := dmdproc.ExitStatus;
    dmdproc.Free;
    lines.Free;
    DeleteFile(scf);
  end;

  if i <> 0 then
    exit;

  // load json
  str := TMemoryStream.Create;
  try
    str.LoadFromFile(jsf);
    str.Position := 0;
    prs := TJsonParser.Create(str);
    try
      dat := prs.Parse;
    finally
      prs.Free;
    end;
  finally
    str.Free;
    DeleteFile(jsf);
  end;

  // update tree
  try
    memb := dat.items[0].FindPath('members');
    if memb <> nil then for i := 0 to memb.Count-1 do
    begin

      // categories
      ln  := new(PInt64);
      ln^ := memb.Items[i].GetPath('line').AsInt64;
      nme := memb.Items[i].GetPath('name').AsString;

      case memb.Items[i].GetPath('kind').AsString of
        'alias'     :ndCat := Tree.Items.AddChildObject(ndAlias, nme, ln);
        'class'     :ndCat := Tree.Items.AddChildObject(ndClass, nme, ln);
        'enum'      :ndCat := Tree.Items.AddChildObject(ndEnum, nme, ln);
        'function'  :ndCat := Tree.Items.AddChildObject(ndFunc, nme, ln);
        'import'    :ndCat := Tree.Items.AddChildObject(ndImp, nme, ln);
        'interface' :ndCat := Tree.Items.AddChildObject(ndIntf, nme, ln);
        'mixin'     :ndCat := Tree.Items.AddChildObject(ndMix, nme, ln);
        'struct'    :ndCat := Tree.Items.AddChildObject(ndStruct, nme, ln);
        'template'  :ndCat := Tree.Items.AddChildObject(ndTmp, nme, ln);
        'variable'  :ndCat := Tree.Items.AddChildObject(ndVar, nme, ln);
      end;

      ndCat.Parent.Visible := true;

      //recursive
      digMembers(memb.Items[i], ndCat);

    end;
  finally
    if dat <> nil then
    begin
      dat.Clear;
      dat.Free;
    end;
  end;
end;

end.

