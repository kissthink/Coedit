unit ce_editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ExtendedNotebook, Forms, Controls, Graphics,
  SynEditKeyCmds, ComCtrls, SynEditHighlighter, SynEditHighlighterFoldBase, SynMacroRecorder,
  SynPluginSyncroEdit, SynEdit, Dialogs, ExtCtrls, ce_frame, ce_d2syn, ce_synmemo;

type
  { TCEWidgetEditor }

  { TCEEditorWidget }

  TCEEditorWidget = class(TCEWidget)
    PageControl: TExtendedNotebook;
    macRecorder: TSynMacroRecorder;
    procedure PageControlChange(Sender: TObject);
  private
    function getCurrentEditor: TCESynMemo;
    function getEditor(index: NativeInt): TCESynMemo;
    function getEditorCount: NativeInt;
    function getEditorIndex: NativeInt;
    procedure identifierToD2Syn(const aMemo: TCESynMemo);
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure addEditor;
    //
    procedure memoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure memoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    //
    property currentEditor: TCESynMemo read getCurrentEditor;
    property editor[index: NativeInt]: TCESynMemo read getEditor;
    property editorCount: NativeInt read getEditorCount;
    property editorIndex: NativeInt read getEditorIndex;
  end;

implementation
{$R *.lfm}

constructor TCEEditorWidget.create(aOwner: TComponent);
begin
  inherited;
  fID := 'ID_EDIT';
end;

destructor TCEEditorWidget.destroy;
begin
  inherited;
end;

function TCEEditorWidget.getEditorCount: NativeInt;
begin
  result := pageControl.PageCount;
end;

function TCEEditorWidget.getEditorIndex: NativeInt;
begin
  result := pageControl.PageIndex;
end;

function TCEEditorWidget.getCurrentEditor: TCESynMemo;
begin
  if pageControl.PageCount = 0 then result := nil
  else result := TCESynMemo(pageControl.ActivePage.Controls[0]);
end;

function TCEEditorWidget.getEditor(index: NativeInt): TCESynMemo;
begin
  result := TCESynMemo(pageControl.Pages[index].Controls[0]);
end;

procedure TCEEditorWidget.PageControlChange(Sender: TObject);
begin
  //http://bugs.freepascal.org/view.php?id=26320
  macRecorder.Editor := getCurrentEditor;
end;

procedure TCEEditorWidget.addEditor;
var
  sheet: TTabSheet;
  memo: TCESynMemo;
begin
  sheet := pageControl.AddTabSheet;
  memo  := TCESynMemo.Create(sheet);
  //
  memo.Align:=alClient;
  memo.Parent := sheet;
  //
  memo.OnKeyDown := @memoKeyDown;
  memo.OnKeyUp := @memoKeyDown;
  memo.OnMouseDown := @memoMouseDown;
  //
  macRecorder.Editor := memo; //http://bugs.freepascal.org/view.php?id=26320
end;

procedure TCEEditorWidget.identifierToD2Syn(const aMemo: TCESynMemo);
begin
  D2Syn.CurrentIdentifier := aMemo.GetWordAtRowCol(aMemo.LogicalCaretXY);
end;

procedure TCEEditorWidget.memoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (sender is TCESynMemo) then
    identifierToD2Syn(TCESynMemo(Sender));
end;

procedure TCEEditorWidget.memoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (sender is TCESynMemo) then
    identifierToD2Syn(TCESynMemo(Sender));
end;


end.

