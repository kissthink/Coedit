unit ce_search;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, actnList, Buttons, SynEdit, SynEditSearch, SynEditTypes, ce_common,
  ce_widget, ce_synmemo, ce_interfaces, ce_observer;

type
  TCESearchWidget = class(TCEWidget, ICEMultiDocObserver)
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
    fHasRestarted: boolean;
    procedure optset_SearchMru(aReader: TReader);
    procedure optget_SearchMru(aWriter: TWriter);
    procedure optset_ReplaceMru(aReader: TReader);
    procedure optget_ReplaceMru(aWriter: TWriter);
    function getOptions: TSynSearchOptions;
    procedure actReplaceAllExecute(sender: TObject);
    procedure replaceEvent(Sender: TObject; const ASearch, AReplace:
      string; Line, Column: integer; var ReplaceAction: TSynReplaceAction);
  protected
    procedure UpdateByEvent; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    //
    procedure docNew(const aDoc: TCESynMemo);
    procedure docClosing(const aDoc: TCESynMemo);
    procedure docFocused(const aDoc: TCESynMemo);
    procedure docChanged(const aDoc: TCESynMemo);
    //
    function contextName: string; override;
    function contextActionCount: integer; override;
    function contextAction(index: integer): TAction; override;
    //
    procedure sesoptDeclareProperties(aFiler: TFiler); override;
    //
    procedure actFindNextExecute(sender: TObject);
    procedure actReplaceNextExecute(sender: TObject);
  end;

implementation
{$R *.lfm}

{$REGION Standard Comp/Obj------------------------------------------------------}
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
  //
  btnFind.Action := fActFindNext;
  btnReplace.Action := fActReplaceNext;
  btnReplaceAll.Action := fActReplaceAll;
  //
  fSearchMru := TMruList.Create;
  fReplaceMru:= TMruList.Create;
  //
  EntitiesConnector.addObserver(self);
end;

destructor TCESearchWidget.Destroy;
begin
  EntitiesConnector.removeObserver(self);
  fSearchMru.Free;
  fReplaceMru.Free;
  inherited;
end;
{$ENDREGION}

{$REGION ICESessionOptionsObserver ------------------------------------------------------}
procedure TCESearchWidget.sesoptDeclareProperties(aFiler: TFiler);
begin
  inherited;
  aFiler.DefineProperty(Name + '_FindMRU', @optset_SearchMru, @optget_SearchMru, true);
  aFiler.DefineProperty(Name + '_ReplaceMRU', @optset_ReplaceMru, @optget_ReplaceMru, true);
end;

procedure TCESearchWidget.optset_SearchMru(aReader: TReader);
begin
  fSearchMru.DelimitedText := aReader.ReadString;
  cbToFind.Items.DelimitedText := fSearchMru.DelimitedText;
end;

procedure TCESearchWidget.optget_SearchMru(aWriter: TWriter);
begin
  aWriter.WriteString(fSearchMru.DelimitedText);
end;

procedure TCESearchWidget.optset_ReplaceMru(aReader: TReader);
begin
  fReplaceMru.DelimitedText := aReader.ReadString;
  cbReplaceWth.Items.DelimitedText := fReplaceMru.DelimitedText ;
end;
procedure TCESearchWidget.optget_ReplaceMru(aWriter: TWriter);
begin
  aWriter.WriteString(fReplaceMru.DelimitedText);
end;
{$ENDREGION}

{$REGION ICEContextualActions---------------------------------------------------}
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

function TCESearchWidget.getOptions: TSynSearchOptions;
begin
  result := [ssoRegExpr];
  if chkWWord.Checked then result += [ssoWholeWord];
  if chkBack.Checked then result += [ssoBackwards];
  if chkCaseSens.Checked then result += [ssoMatchCase];
  if chkPrompt.Checked then result += [ssoPrompt];
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
    begin
      if not fHasRestarted then
        fEditor.CaretXY := Point(0,0);
      fHasRestarted := true;
    end;
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
  else
  begin
    fHasSearched := true;
    fHasRestarted := false;
    chkFromCur.Checked := true;
  end;
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
{$ENDREGION}

{$REGION ICEMultiDocObserver ---------------------------------------------------}
procedure TCESearchWidget.docNew(const aDoc: TCESynMemo);
begin
  fEditor := aDoc;
  UpdateByEvent;
end;

procedure TCESearchWidget.docClosing(const aDoc: TCESynMemo);
begin
  if fEditor = aDoc then fEditor := nil;
  UpdateByEvent;
end;

procedure TCESearchWidget.docFocused(const aDoc: TCESynMemo);
begin
  fEditor := aDoc;
  UpdateByEvent;
end;

procedure TCESearchWidget.docChanged(const aDoc: TCESynMemo);
begin
end;
{$ENDREGION}

{$REGION Misc. -----------------------------------------------------------------}
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
{$ENDREGION}

end.
