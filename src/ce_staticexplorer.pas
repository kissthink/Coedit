unit ce_staticexplorer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Menus, ComCtrls, ce_widget, jsonparser, fpjson;

type

  { TCEStaticExplorerWidget }
  TCEStaticExplorerWidget = class(TCEWidget)
    Panel1: TPanel;
    Tree: TTreeView;
    TreeFilterEdit1: TTreeFilterEdit;
  private
  public
    constructor create(aOwner: TComponent); override;
  end;

implementation
{$R *.lfm}

uses
  ce_jsoninfos;

constructor TCEStaticExplorerWidget.create(aOwner: TComponent);
var
  rt, nd, mb: TTreeNode;
  infs: TDSourceInfos;
  mods: TDSourceInfos;
  dt: TJsonData;
  memberKind: TInfKind;
  i,j,k: Integer;
  membinfs: TDSourceInfos;
begin
  inherited;
  fID := 'ID_SEXPL';
  //
  for i:= 0 to high(JSonInfos.fData) do
  begin

    mods := JSonInfos.getFileModule(i);
    for j := 0 to high(mods) do
    begin

      rt := Tree.Items.Add(nil, mods[j].infs);

      for memberKind in InfKindNonModule do
      begin
        membinfs := JSonInfos.getMembers(i, j, memberKind);
        nd := Tree.Items.AddChild(rt, InfKindStr[memberKind]);
        for k := 0 to high(membinfs) do
        begin
          mb := Tree.Items.AddChild(nd, membinfs[k].infs);
          mb.Data := @membinfs[k];
        end;
      end;

    end;



  end;
end;

end.

