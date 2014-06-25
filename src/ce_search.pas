unit ce_search;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, actnList, Buttons, SynEdit, SynEditSearch, SynEditTypes, ce_common,
  ce_widget, ce_synmemo;

type

  { TCESearchWidget }

  TCESearchWidget = class(TCEWidget)
    btnFind: TBitBtn;
    btnReplace: TBitBtn;
    btnReplaceAll: TBitBtn;
    cbToFind: TComboBox;
    cbReplaceWth: TComboBox;
    chkPrompt: TCheckBox;
    chkWWord: TCheckBox;
    chkBack: TCheckBox;
    chkFromCur: TCheckBox;
    chkCaseSens: TCheckBox;
    grpOpts: TGroupBox;
    imgList: TImageList;
    procedure cbReplaceWthChange(Sender: TObject);
    procedure cbToFindChange(Sender: TObject);
  private
    fEditor: TCESynMemo;
    fToFind: string;
    fReplaceWth: string;
    fActFindNext, fActReplaceNext: TAction;
    fActReplaceAll: TAction;
    fSearchMru, fReplaceMru: TMruList;
    function getOptions: TSynSearchOptions;
    procedure actFindNextExecute(sender: TObject);
    procedure actReplaceAllExecute(sender: TObject);
    procedure actReplaceNextExecute(sender: TObject);
    procedure replaceEvent(Sender: TObject; const ASearch, AReplace:
      string; Line, Column: integer; var ReplaceAction: TSynReplaceAction);
  protected
    procedure UpdateByEvent; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    //
    procedure docFocused(const aDoc: TCESynMemo); override;
    procedure docClose(const aDoc: TCESynMemo); override;
  end;

implementation
{$R *.lfm}

constructor TCESearchWidget.Create(aOwner: TComponent);
begin
  inherited;
  fID := 'ID_FIND';
  //
  fSearchMru := TMruList.Create;
  fReplaceMru:= TMruList.Create;
  //
  fActFindNext := TAction.Create(self);
  fActFindNext.Caption := 'Find';
  fActFindNext.OnExecute := @actFindNextExecute;
  btnFind.Action := fActFindNext;
  fActReplaceNext := TAction.Create(self);
  fActReplaceNext.Caption := 'Replace';
  fActReplaceNext.OnExecute := @actReplaceNextExecute;
  btnReplace.Action := fActReplaceNext;
  fActReplaceAll := TAction.Create(self);
  fActReplaceAll.Caption := 'Replace all';
  fActReplaceAll.OnExecute := @actReplaceAllExecute;
  btnReplaceAll.Action := fActReplaceAll;
end;

destructor TCESearchWidget.Destroy;
begin
  fSearchMru.Free;
  fReplaceMru.Free;
  inherited;
end;

procedure TCESearchWidget.docFocused(const aDoc: TCESynMemo);
begin
  fEditor := aDoc;
  UpdateByEvent;
end;

procedure TCESearchWidget.docClose(const aDoc: TCESynMemo);
begin
  if fEditor = aDoc then fEditor := nil;
  UpdateByEvent;
end;

procedure TCESearchWidget.cbToFindChange(Sender: TObject);
begin
  if Updating then exit;
  fToFind := cbToFind.Text;
end;

procedure TCESearchWidget.cbReplaceWthChange(Sender: TObject);
begin
  if Updating then exit;
  fReplaceWth := cbReplaceWth.Text;
end;

function TCESearchWidget.getOptions: TSynSearchOptions;
begin
  result := [];
  if chkWWord.Checked then result += [ssoWholeWord];
  if chkBack.Checked then result += [ssoBackwards];
  if chkCaseSens.Checked then result += [ssoMatchCase];
  if chkPrompt.Checked then result += [ssoPrompt];
end;

procedure TCESearchWidget.actFindNextExecute(sender: TObject);
begin
  if fEditor = nil then exit;
  //
  fSearchMru.Insert(0,fToFind);
  if not chkFromCur.Checked then if chkBack.Checked then
    fEditor.CaretXY := Point(high(Integer), high(Integer)) else
      fEditor.CaretXY := Point(0,0);
  fEditor.SearchReplace(fToFind, '', getOptions);
  UpdateByEvent;
end;

procedure TCESearchWidget.actReplaceNextExecute(sender: TObject);
begin
  if fEditor = nil then exit;
  //
  fSearchMru.Insert(0, fToFind);
  fReplaceMru.Insert(0, fReplaceWth);
  if chkPrompt.Checked then
    fEditor.OnReplaceText := @replaceEvent;
  if not chkFromCur.Checked then if chkBack.Checked then
    fEditor.CaretXY := Point(high(Integer), high(Integer)) else
      fEditor.CaretXY := Point(0,0);
  fEditor.SearchReplace(fToFind, fReplaceWth, getOptions + [ssoReplace]);
  fEditor.OnReplaceText := nil;
  UpdateByEvent;
end;

procedure TCESearchWidget.actReplaceAllExecute(sender: TObject);
var
  opts: TSynSearchOptions;
begin
  if fEditor = nil then exit;
  opts := getOptions + [ssoReplace];
  opts -= [ssoBackwards];
  //
  fSearchMru.Insert(0, fToFind);
  fReplaceMru.Insert(0, fReplaceWth);
  if chkPrompt.Checked then fEditor.OnReplaceText := @replaceEvent;
  fEditor.CaretXY := Point(0,0);
  while(true) do
  begin
    if fEditor.SearchReplace(fToFind, fReplaceWth, opts) = 0
      then break;
  end;
  fEditor.OnReplaceText := nil;
  UpdateByEvent;
end;

procedure TCESearchWidget.replaceEvent(Sender: TObject; const ASearch, AReplace:
      string; Line, Column: integer; var ReplaceAction: TSynReplaceAction);
begin
  ReplaceAction := raSkip;
  if dlgOkCancel('Replace this match ?') = mrOK then
    ReplaceAction := raReplace;
end;

procedure TCESearchWidget.UpdateByEvent;
begin
  fActFindNext.Enabled := fEditor <> nil;
  fActReplaceNext.Enabled := fEditor <> nil;
  //
  cbToFind.Items.Assign(fSearchMru);
  cbReplaceWth.Items.Assign(fReplaceMru);
end;

end.

