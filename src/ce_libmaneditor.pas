unit ce_libmaneditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, ComCtrls, Buttons, ce_widget;

type

  { TCELibManEditorWidget }

  TCELibManEditorWidget = class(TCEWidget)
    btnSelFile: TBitBtn;
    btnAddLib: TBitBtn;
    btnRemLib: TBitBtn;
    btnEditAlias: TBitBtn;
    btnSelRoot: TBitBtn;
    List: TListView;
    Panel1: TPanel;
    procedure btnAddLibClick(Sender: TObject);
    procedure btnEditAliasClick(Sender: TObject);
    procedure btnRemLibClick(Sender: TObject);
    procedure btnSelFileClick(Sender: TObject);
    procedure btnSelRootClick(Sender: TObject);
    procedure ListEdited(Sender: TObject; Item: TListItem; var AValue: string);
  private
    procedure dataToGrid;
    procedure gridToData;
  protected
    procedure DoShow; override;
  end;

implementation
{$R *.lfm}

uses
  ce_main, ce_libman;

procedure TCELibManEditorWidget.ListEdited(Sender: TObject; Item: TListItem;
  var AValue: string);
begin
  GridToData;
end;

procedure TCELibManEditorWidget.btnAddLibClick(Sender: TObject);
var
  itm: TListItem;
const
  notav: string = '< n/a >';
begin
  itm := List.Items.Add;
  itm.Caption := notav;
  itm.SubItems.Add(notav);
  itm.SubItems.Add(notav);
end;

procedure TCELibManEditorWidget.btnEditAliasClick(Sender: TObject);
var
  al: string;
begin
  if List.Selected = nil then exit;
  al := List.Selected.Caption;
  if inputQuery('library alias', '', al) then
    List.Selected.Caption := al;
  GridToData;
end;

procedure TCELibManEditorWidget.btnRemLibClick(Sender: TObject);
begin
  if List.Selected = nil then exit;
  List.Items.Delete( List.Selected.Index );
  gridToData;
end;

procedure TCELibManEditorWidget.btnSelFileClick(Sender: TObject);
var
  ini: string;
begin
  if List.Selected = nil then exit;
  if List.Selected.SubItems.Count > 0 then
    ini := List.Selected.SubItems[0]
  else
  begin
    ini := '';
    List.Selected.SubItems.Add(ini);
  end;
  with TOpenDialog.Create(nil) do
  try
    filename := ini;
    if execute then
      List.Selected.SubItems[0] := filename;
  finally
    Free;
  end;
  GridToData;
end;

procedure TCELibManEditorWidget.btnSelRootClick(Sender: TObject);
var
  dir, outdir: string;
begin
  if List.Selected = nil then exit;
  if List.Selected.SubItems.Count > 1 then
    dir := List.Selected.SubItems[1]
  else
  begin
    dir := '';
    while List.Selected.SubItems.Count < 2 do
      List.Selected.SubItems.Add(dir);
  end;
  if selectDirectory('sources root', dir, outdir, true, 0) then
    List.Selected.SubItems[1] := outdir;
  GridToData;
end;

procedure TCELibManEditorWidget.DoShow;
begin
  inherited;
  dataToGrid;
end;

procedure TCELibManEditorWidget.dataToGrid;
var
  itm: TLibraryItem;
  row: TListItem;
  i: NativeInt;
begin
  List.Clear;
  with CEMainForm do
  begin
    if LibraryManager = nil then exit;
    for i:= 0 to LibraryManager.libraries.Count-1 do
    begin
      itm := TLibraryItem( LibraryManager.libraries.Items[i]);
      row := List.Items.Add;
      row.Caption := itm.libAlias;
      row.SubItems.Add(itm.libFile);
      row.SubItems.Add(itm.libSourcePath);
    end;
  end;
end;

procedure TCELibManEditorWidget.gridToData;
var
  itmt: TLibraryItem;
  row: TListItem;
begin
  with CEMainForm do
  begin
    if LibraryManager = nil then exit;
    LibraryManager.libraries.Clear;
    for row in List.Items do
    begin
      itmt := TLibraryItem(LibraryManager.libraries.Add);
      itmt.libAlias := row.Caption;
      itmt.libFile := row.SubItems.Strings[0];
      itmt.libSourcePath := row.SubItems.Strings[1];
    end;
  end;
end;

end.

