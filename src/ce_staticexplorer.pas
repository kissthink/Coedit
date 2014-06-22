unit ce_staticexplorer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Menus, ComCtrls, ce_widget, jsonparser, fpjson,
  ce_synmemo, process;

type

  { TCEStaticExplorerWidget }
  TCEStaticExplorerWidget = class(TCEWidget)
    imgList: TImageList;
    Panel1: TPanel;
    Tree: TTreeView;
    TreeFilterEdit1: TTreeFilterEdit;
    procedure TreeDeletion(Sender: TObject; Node: TTreeNode);
  private
    fDoc: TCESynMemo;
    ndAlias, ndClass, ndEnum, ndFunc: TTreeNode;
    ndImp, ndMix, ndStruct, ndTmp, ndVar: TTreeNode;
    procedure Rescan;
    procedure TreeDblClick(Sender: TObject);
  protected
    procedure UpdateByDelay; override;
  public
    constructor create(aOwner: TComponent); override;
    //
    procedure docNew(const aDoc: TCESynMemo); override;
    procedure docFocused(const aDoc: TCESynMemo); override;
    procedure docChanged(const aDoc: TCESynMemo); override;
    procedure docClose(const aDoc: TCESynMemo); override;
  end;

implementation
{$R *.lfm}

constructor TCEStaticExplorerWidget.create(aOwner: TComponent);
begin
  inherited;
  fID := 'ID_SEXPL';
  //
  ndAlias   := Tree.Items[0];
  ndClass   := Tree.Items[1];
  ndEnum    := Tree.Items[2];
  ndFunc    := Tree.Items[3];
  ndImp     := Tree.Items[4];
  ndMix     := Tree.Items[5];
  ndStruct  := Tree.Items[6];
  ndTmp     := Tree.Items[7];
  ndVar     := Tree.Items[8];
  //
  Tree.OnDblClick := @TreeDblClick;
end;

procedure TCEStaticExplorerWidget.docNew(const aDoc: TCESynMemo);
begin
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
  Rescan;
end;

procedure TCEStaticExplorerWidget.TreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  if (node.Data <> nil) then
    FreeMem(node.Data)
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
  fDoc.SelectLine();
end;

procedure TCEStaticExplorerWidget.Rescan;
var
  dmdproc: TProcess;
  lines: TStringList;
  str: TMemoryStream;
  prs: TJsonParser;
  dat: TJsonData;
  memb: TJsonData;
  submemb: TJsonData;
  jsf, scf: string;
  ndCat: TTreeNode;
  ln: PInt64;
  nme: string;
  i, j: NativeInt;
begin
  if ndAlias = nil then exit;

  ndAlias.DeleteChildren;
  ndClass.DeleteChildren;
  ndEnum.DeleteChildren;
  ndFunc.DeleteChildren;
  ndImp.DeleteChildren;
  ndMix.DeleteChildren;
  ndStruct.DeleteChildren;
  ndTmp.DeleteChildren;
  ndVar.DeleteChildren;

  if fDoc = nil then exit;
  if fDoc.Lines.Count = 0 then exit;
  subMemb := nil;
  memb := nil;

  // generate json
  dmdproc := TProcess.Create(nil);
  lines := TStringList.Create;
  try
    jsf := GetTempDir(false);
    jsf += format('%.8X.json',[NativeUint(@dmdproc)]);
    scf := GetTempDir(false);
    scf += format('%.8X.d',[NativeUint(@dmdproc)]);
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
    dmdproc.Parameters.Add('-Xf' + jsf );
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
  memb := dat.items[0].FindPath('members');
  if memb <> nil then for i := 0 to memb.Count-1 do
  begin

    // category
    ln  := new(PInt64);
    ln^ := memb.Items[i].GetPath('line').AsInt64;
    nme := memb.Items[i].GetPath('name').AsString;

    case memb.Items[i].GetPath('kind').AsString of
      'alias'   :ndCat := Tree.Items.AddChildObject(ndAlias, nme, ln);
      'class'   :ndCat := Tree.Items.AddChildObject(ndClass, nme, ln);
      'enum'    :ndCat := Tree.Items.AddChildObject(ndEnum, nme, ln);
      'function':ndCat := Tree.Items.AddChildObject(ndFunc, nme, ln);
      'import'  :ndCat := Tree.Items.AddChildObject(ndImp, nme, ln);
      'mixin'   :ndCat := Tree.Items.AddChildObject(ndMix, nme, ln);
      'struct'  :ndCat := Tree.Items.AddChildObject(ndStruct, nme, ln);
      'template':ndCat := Tree.Items.AddChildObject(ndTmp, nme, ln);
      'variable':ndCat := Tree.Items.AddChildObject(ndVar, nme, ln);
    end;

    // optional item members
    submemb := memb.Items[i].FindPath('members');
    if subMemb <> nil then for j := 0 to submemb.Count-1 do
    begin
      ln  := new(PInt64);
      ln^ := submemb.Items[j].GetPath('line').AsInt64;
      nme := submemb.Items[j].GetPath('name').AsString;
      Tree.Items.AddChildObject(ndCat, nme, ln);
    end;

  end;


  if dat <> nil then
  begin
    dat.Clear;
    dat.Free;
  end;
end;

end.

