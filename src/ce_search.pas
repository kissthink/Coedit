unit ce_search;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, actnList, Buttons, SynEdit, SynEditSearch, SynEditTypes, ce_common,
  ce_widget, ce_synmemo, AnchorDocking;

type

  { TCESearchWidget }
  TCESearchWidget = class(TCEWidget)
    btnFind: TBitBtn;
    btnReplace: TBitBtn;
    btnReplaceAll: TBitBtn;
    cbToFind: TComboBox;
    cbReplaceWth: TComboBox;
    chkEnableRep: TCheckBox;
    chkPrompt: TCheckBox;
    chkWWord: TCheckBox;
    chkBack: TCheckBox;
    chkFromCur: TCheckBox;
    chkCaseSens: TCheckBox;
    grpOpts: TGroupBox;
    imgList: TImageList;
    Panel1: TPanel;
    procedure cbReplaceWthChange(Sender: TObject);
    procedure cbToFindChange(Sender: TObject);
    procedure chkEnableRepChange(Sender: TObject);
  private
    fEditor: TCESynMemo;
    fToFind: string;
    fReplaceWth: string;
    fActFindNext, fActReplaceNext: TAction;
    fActReplaceAll: TAction;
    fSearchMru, fReplaceMru: TMruList;
    fCancelAll: boolean;
    fHasSearched: boolean;
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
    //
    function contextName: string; override;
    function contextActionCount: integer; override;
    function contextAction(index: integer): TAction; override;
  end;

implementation
{$R *.lfm}

constructor TCESearchWidget.Create(aOwner: TComponent);
begin
  fActFindNext := TAction.Create(self);
  fActFindNext.Caption := 'Find';
  fActFindNext.OnExecute := @actFindNextExecute;
  fActReplaceNext := TAction.Create(self);
  fActReplaceNext.Caption := 'Replace';
  fActReplaceNext.OnExecute := @actReplaceNextExecute;
  fActReplaceAll := TAction.Create(self);
  fActReplaceAll.Caption := 'Replace all';
  fActReplaceAll.OnExecute := @actReplaceAllExecute;
  inherited;
  fID := 'ID_FIND';
  //
  btnFind.Action := fActFindNext;
  btnReplace.Action := fActReplaceNext;
  btnReplaceAll.Action := fActReplaceAll;
  //
  fSearchMru := TMruList.Create;
  fReplaceMru:= TMruList.Create;
  DockMaster.GetAnchorSite(Self).Name := ID;
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

function TCESearchWidget.contextName: string;
begin
  exit('Search');
end;

function TCESearchWidget.contextActionCount: integer;
begin
  exit(3);
end;

function TCESearchWidget.contextAction(index: integer): TAction;
begin
  case index of
    0: exit(fActFindNext);
    1: exit(fActReplaceNext);
    2: exit(fActReplaceAll);
    else exit(nil);
  end;
end;

procedure TCESearchWidget.cbToFindChange(Sender: TObject);
begin
  if Updating then exit;
  fToFind := cbToFind.Text;
  fHasSearched := false;
end;

procedure TCESearchWidget.chkEnableRepChange(Sender: TObject);
begin
  if Updating then exit;
  UpdateByEvent;
end;

procedure TCESearchWidget.cbReplaceWthChange(Sender: TObject);
begin
  if Updating then exit;
  fReplaceWth := cbReplaceWth.Text;
  fHasSearched := false;
end;

function TCESearchWidget.getOptions: TSynSearchOptions;
begin
  result := [ssoRegExpr];
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
  if not chkFromCur.Checked then
  begin
    if chkBack.Checked then
      fEditor.CaretXY := Point(high(Integer), high(Integer))
    else
      fEditor.CaretXY := Point(0,0);
  end
  else if fHasSearched then
  begin
    if chkBack.Checked then
      fEditor.CaretX := fEditor.CaretX - 1
    else
      fEditor.CaretX := fEditor.CaretX + length(fToFind);
  end;
  if fEditor.SearchReplace(fToFind, '', getOptions) = 0 then
    dlgOkInfo('the expression cannot be found')
  else fHasSearched := true;
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
  if not chkFromCur.Checked then
  begin
    if chkBack.Checked then
      fEditor.CaretXY := Point(high(Integer), high(Integer))
    else
      fEditor.CaretXY := Point(0,0);
  end
  else if fHasSearched then
  begin
    if chkBack.Checked then
      fEditor.CaretX := fEditor.CaretX - 1
    else
      fEditor.CaretX := fEditor.CaretX + length(fToFind);
  end;
  if fEditor.SearchReplace(fToFind, fReplaceWth, getOptions + [ssoReplace]) <> 0 then
    fHasSearched := true;
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
    if fCancelAll then
    begin
      fCancelAll := false;
      break;
    end;
  end;
  fEditor.OnReplaceText := nil;
  UpdateByEvent;
end;

function dlgReplaceAll: TModalResult;
const
  Btns = [mbYes, mbNo, mbYesToAll, mbNoToAll];
begin
  exit( MessageDlg('Coedit', 'Replace this match ?', mtConfirmation, Btns, ''));
end;

procedure TCESearchWidget.replaceEvent(Sender: TObject; const ASearch, AReplace:
      string; Line, Column: integer; var ReplaceAction: TSynReplaceAction);
begin
  case dlgReplaceAll of
    mrYes: ReplaceAction := raReplace;
    mrNo: ReplaceAction := raSkip;
    mrYesToAll: ReplaceAction := raReplaceAll;
    mrCancel, mrClose, mrNoToAll:
      begin
        ReplaceAction := raCancel;
        fCancelAll := true;
      end;
  end;
end;

procedure TCESearchWidget.UpdateByEvent;
begin
  fActFindNext.Enabled := fEditor <> nil;
  fActReplaceNext.Enabled := (fEditor <> nil) and (chkEnableRep.Checked);
  fActReplaceAll.Enabled := (fEditor <> nil) and (chkEnableRep.Checked);
  cbReplaceWth.Enabled := (fEditor <> nil) and (chkEnableRep.Checked);
  cbToFind.Enabled := fEditor <> nil;
  //
  cbToFind.Items.Assign(fSearchMru);
  cbReplaceWth.Items.Assign(fReplaceMru);
end;

end.

