unit ce_messages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  lcltype, ce_widget, ActnList, Menus, clipbrd, AnchorDocking, ce_project,
  ce_synmemo;

type

  TMessageContext = (msUnknown, msProject, msEditor);

  PMessageItemData = ^TMessageItemData;
  TMessageItemData = record
    ctxt: TMessageContext;
    data: Pointer;
  end;

  { TCEMessagesWidget }
  TCEMessagesWidget = class(TCEWidget)
    imgList: TImageList;
    List: TTreeView;
    procedure ListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    fActClearAll: TAction;
    fActClearEdi: TAction;
    fActSaveMsg: TAction;
    fActCopyMsg: TAction;
    fActSelAll: TAction;
    fProject: TCEProject;
    fMaxMessCnt: Integer;
    fDoc: TCESynMemo;
    procedure filterMessages;
    procedure clearOutOfRangeMessg;
    procedure actClearEdiExecute(Sender: TObject);
    procedure actClearAllExecute(Sender: TObject);
    procedure actSaveMsgExecute(Sender: TObject);
    procedure actCopyMsgExecute(Sender: TObject);
    procedure actSelAllExecute(Sender: TObject);
    procedure setMaxMessageCount(aValue: Integer);
    procedure listDeletion(Sender: TObject; Node: TTreeNode);
    function newMessageItemData(aCtxt: TMessageContext): PMessageItemData;
  published
    property maxMessageCount: Integer read fMaxMessCnt write setMaxMessageCount default 125;
  public
    constructor create(aOwner: TComponent); override;
    //
    procedure scrollToBack;
    procedure addMessage(const aMsg: string; aCtxt: TMessageContext = msUnknown);
    procedure addCeInf(const aMsg: string; aCtxt: TMessageContext = msUnknown);
    procedure addCeErr(const aMsg: string; aCtxt: TMessageContext = msUnknown);
    procedure addCeWarn(const aMsg: string; aCtxt: TMessageContext = msUnknown);
    //
    function contextName: string; override;
    function contextActionCount: integer; override;
    function contextAction(index: integer): TAction; override;
    //
    procedure projNew(const aProject: TCEProject); override;
    procedure projClose(const aProject: TCEProject); override;
    //
    procedure docFocused(const aDoc: TCESynMemo); override;
    procedure docClose(const aDoc: TCESynMemo); override;
    //
    procedure ClearAllMessages;
    procedure ClearMessages(aCtxt: TMessageContext);
  end;

  PTCEMessageItem = ^TCEMessageItem;
  TCEMessageItem = class(TListItem)
  end;

  TMessageKind = (msgkUnknown, msgkInfo, msgkHint, msgkWarn, msgkError);

  function semanticMsgAna(const aMessg: string): TMessageKind;

implementation
{$R *.lfm}

uses
  ce_main;

constructor TCEMessagesWidget.create(aOwner: TComponent);
begin
  fMaxMessCnt := 125;
  //
  fActClearAll := TAction.Create(self);
  fActClearAll.OnExecute := @actClearAllExecute;
  fActClearAll.caption := 'Clear all messages';
  fActClearEdi := TAction.Create(self);
  fActClearEdi.OnExecute := @actClearEdiExecute;
  fActClearEdi.caption := 'Clear editor messages';
  fActCopyMsg := TAction.Create(self);
  fActCopyMsg.OnExecute := @actCopyMsgExecute;
  fActCopyMsg.Caption := 'Copy message(s)';
  fActSelAll := TAction.Create(self);
  fActSelAll.OnExecute := @actSelAllExecute;
  fActSelAll.Caption := 'Select all';
  fActSaveMsg := TAction.Create(self);
  fActSaveMsg.OnExecute := @actSaveMsgExecute;
  fActSaveMsg.caption := 'Save selected message(s) to...';
  //
  inherited;
  //
  List.PopupMenu := contextMenu;
  List.OnDeletion := @ListDeletion;
end;

procedure TCEMessagesWidget.clearOutOfRangeMessg;
begin
  while List.Items.Count > fMaxMessCnt do
    List.Items.Delete(List.Items.GetFirstNode);
end;

procedure TCEMessagesWidget.setMaxMessageCount(aValue: Integer);
begin
  if aValue < 10 then aValue := 10;
  if aValue > 1023 then aValue := 1023;
  if fMaxMessCnt = aValue then exit;
  fMaxMessCnt := aValue;
  clearOutOfRangeMessg;
end;

function TCEMessagesWidget.newMessageItemData(aCtxt: TMessageContext): PMessageItemData;
begin
  result := new(PMessageItemData);
  result^.ctxt := aCtxt;
  case aCtxt of
    msUnknown: result^.data := nil;
    msProject: result^.data := Pointer(fProject);
    msEditor: result^.data := Pointer(fDoc);
  end;
end;

procedure TCEMessagesWidget.listDeletion(Sender: TObject; Node: TTreeNode);
begin
  if node.Data <> nil then
    Dispose( PMessageItemData(Node.Data));
end;

procedure TCEMessagesWidget.scrollToBack;
begin
  if not Visible then exit;
  if List.BottomItem <> nil then
    List.BottomItem.MakeVisible;
end;

procedure TCEMessagesWidget.ListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: NativeInt;
begin
  if Key in [VK_BACK, VK_DELETE] then
  begin
    if List.SelectionCount > 0 then
    begin
    for i := List.Items.Count-1 downto 0 do
      if List.Items[i].MultiSelected then
        List.Items.Delete(List.Items[i]);
    end
    else ClearAllMessages;
  end;
end;

procedure TCEMessagesWidget.filterMessages;
var
  itm: TTreeNode;
  dat: PMessageItemData;
  i: NativeInt;
begin
  for i := 0 to List.Items.Count-1 do
  begin
    itm := List.Items[i];
    dat := PMessageItemData(itm.Data);
    case dat^.ctxt of
      msProject: itm.Visible := Pointer(fProject) = (dat^.data);
      msEditor: itm.Visible := Pointer(fDoc) = (dat^.data);
      else itm.Visible := true;
    end;
  end;
end;

procedure TCEMessagesWidget.ClearAllMessages;
begin
  List.Items.Clear;
end;

procedure TCEMessagesWidget.ClearMessages(aCtxt: TMessageContext);
var
  i: NativeInt;
begin
  for i := List.Items.Count-1 downto 0 do
  begin
    if PMessageItemData(List.Items[i].Data)^.ctxt = aCtxt then
      List.Items.Delete(List.Items[i]);
  end;
end;

procedure TCEMessagesWidget.addCeInf(const aMsg: string; aCtxt: TMessageContext = msUnknown);
var
  item: TTreeNode;
begin
  item := List.Items.Add(nil, 'Coedit information: ' + aMsg);
  item.Data := newMessageItemData(aCtxt);
  item.ImageIndex := 1;
  item.SelectedIndex := 1;
  clearOutOfRangeMessg;
  scrollToBack;
end;

procedure TCEMessagesWidget.addCeWarn(const aMsg: string; aCtxt: TMessageContext = msUnknown);
var
  item: TTreeNode;
begin
  item := List.Items.Add(nil, 'Coedit warning: ' + aMsg);
  item.Data := newMessageItemData(aCtxt);
  item.ImageIndex := 3;
  item.SelectedIndex := 3;
  clearOutOfRangeMessg;
  scrollToBack;
end;

procedure TCEMessagesWidget.addCeErr(const aMsg: string; aCtxt: TMessageContext = msUnknown);
var
  item: TTreeNode;
begin
  item := List.Items.Add(nil, 'Coedit error: ' + aMsg);
  item.Data := newMessageItemData(aCtxt);
  item.ImageIndex := 4;
  item.SelectedIndex := 4;
  clearOutOfRangeMessg;
  scrollToBack;
end;

procedure TCEMessagesWidget.addMessage(const aMsg: string; aCtxt: TMessageContext = msUnknown);
var
  item: TTreeNode;
begin
  item := List.Items.Add(nil, aMsg);
  item.Data := newMessageItemData(aCtxt);
  item.ImageIndex := Integer( semanticMsgAna(aMsg) );
  item.SelectedIndex := Integer( semanticMsgAna(aMsg) );
  clearOutOfRangeMessg;
end;

function TCEMessagesWidget.contextName: string;
begin
  result := 'Messages';
end;

function TCEMessagesWidget.contextActionCount: integer;
begin
  result := 5;
end;

function TCEMessagesWidget.contextAction(index: integer): TAction;
begin
  case index of
    0: result := fActClearAll;
    1: result := fActClearEdi;
    2: result := fActCopyMsg;
    3: result := fActSelAll;
    4: result := fActSaveMsg;
    else result := nil;
  end;
end;

procedure TCEMessagesWidget.projNew(const aProject: TCEProject);
begin
  fProject := aProject;
  filterMessages;
end;

procedure TCEMessagesWidget.projClose(const aProject: TCEProject);
begin
  if fProject = aProject then ClearMessages(msProject);
  fProject := nil;
  filterMessages;
end;

procedure TCEMessagesWidget.docFocused(const aDoc: TCESynMemo);
begin
  fDoc := aDoc;
  filterMessages;
end;

procedure TCEMessagesWidget.docClose(const aDoc: TCESynMemo);
begin
  fDoc := nil;
  filterMessages;
end;

procedure TCEMessagesWidget.actClearAllExecute(Sender: TObject);
begin
  ClearAllMessages;
end;

procedure TCEMessagesWidget.actClearEdiExecute(Sender: TObject);
begin
  ClearMessages(msEditor);
end;

procedure TCEMessagesWidget.actCopyMsgExecute(Sender: TObject);
var
  i: NativeInt;
  str: string;
begin
  str := '';
  for i := 0 to List.Items.Count-1 do if List.Items[i].MultiSelected then
    str += List.Items[i].Text + LineEnding;
  Clipboard.AsText := str;
end;

procedure TCEMessagesWidget.actSelAllExecute(Sender: TObject);
var
  i: NativeInt;
begin
  for i := 0 to List.Items.Count-1 do
      List.Items[i].MultiSelected := true;
end;

procedure TCEMessagesWidget.actSaveMsgExecute(Sender: TObject);
var
  lst: TStringList;
  itm: TtreeNode;
begin
  with TSaveDialog.Create(nil) do
  try
    if execute then
    begin
      lst := TStringList.Create;
      try
        for itm in List.Items do
          lst.Add(itm.Text);
        lst.SaveToFile(filename);
      finally
        lst.Free;
      end;
    end;
  finally
    free;
  end;
end;

// TODO: link to editor line when possible.
function semanticMsgAna(const aMessg: string): TMessageKind;
var
  pos: Nativeint;
  idt: string;
function checkIdent: TMessageKind;
begin
  case idt of
    'ERROR', 'error', 'Error', 'Invalid', 'invalid',
    'exception', 'Exception', 'illegal', 'Illegal',
    'fatal', 'Fatal', 'Critical', 'critical':
      exit(msgkError);
    'Warning', 'warning', 'caution', 'Caution':
      exit(msgkWarn);
    'Hint', 'hint', 'Tip', 'tip', 'advice', 'Advice',
    'suggestion', 'Suggestion':
      exit(msgkHint);
    'Information', 'information':
      exit(msgkInfo);
    else
      exit(msgkUnknown);
  end;
end;
begin
  idt := '';
  pos := 1;
  result := msgkUnknown;
  while(true) do
  begin
    if pos > length(aMessg) then exit;
    if aMessg[pos] in [#0..#32] then
    begin
      Inc(pos);
      result := checkIdent;
      if result <> msgkUnknown then exit;
      idt := '';
      continue;
    end;
    if not (aMessg[pos] in ['a'..'z', 'A'..'Z']) then
    begin
      Inc(pos);
      result := checkIdent;
      if result <> msgkUnknown then exit;
      idt := '';
      continue;
    end;
    idt += aMessg[pos];
    Inc(pos);
  end;
end;

end.
