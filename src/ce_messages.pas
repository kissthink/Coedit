unit ce_messages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  lcltype, ce_widget, ActnList, Menus, clipbrd, AnchorDocking, ce_project,
  ce_synmemo, ce_dlangutils;

type

  TMessageContext = (mcUnknown, mcProject, mcEditor, mcApplication);

  PMessageItemData = ^TMessageItemData;
  TMessageItemData = record
    ctxt: TMessageContext;
    editor: TCESynMemo;
    project: TCEProject;
    position: TPoint;
  end;

  TCEMessagesWidget = class(TCEWidget)
    imgList: TImageList;
    List: TTreeView;
    procedure ListDblClick(Sender: TObject);
    procedure ListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    fActClearAll: TAction;
    fActClearEdi: TAction;
    fActSaveMsg: TAction;
    fActCopyMsg: TAction;
    fActSelAll: TAction;
    fProj: TCEProject;
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
    //
    procedure optset_MaxMessageCount(aReader: TReader);
    procedure optget_MaxMessageCount(awriter: TWriter);
  published
    property maxMessageCount: Integer read fMaxMessCnt write setMaxMessageCount default 125;
  public
    constructor create(aOwner: TComponent); override;
    //
    procedure scrollToBack;
    procedure addMessage(const aMsg: string; aCtxt: TMessageContext = mcUnknown);
    procedure addMessage(const aMsg: string; const aData: PMessageItemData);
    procedure addCeInf(const aMsg: string; aCtxt: TMessageContext = mcUnknown);
    procedure addCeErr(const aMsg: string; aCtxt: TMessageContext = mcUnknown);
    procedure addCeWarn(const aMsg: string; aCtxt: TMessageContext = mcUnknown);
    //
    procedure declareProperties(aFiler: TFiler); override;
    //
    function contextName: string; override;
    function contextActionCount: integer; override;
    function contextAction(index: integer): TAction; override;
    //
    procedure projNew(const aProject: TCEProject); override;
    procedure projClose(const aProject: TCEProject); override;
    procedure projFocused(const aProject: TCEProject); override;
    //
    procedure docFocused(const aDoc: TCESynMemo); override;
    procedure docClose(const aDoc: TCESynMemo); override;
    //
    procedure ClearAllMessages;
    procedure ClearMessages(aCtxt: TMessageContext);
  end;

  TMessageKind = (msgkUnknown, msgkInfo, msgkHint, msgkWarn, msgkError);

  function semanticMsgAna(const aMessg: string): TMessageKind;
  function getLineFromDmdMessage(const aMessage: string): TPoint;
  function getFileFromDmdMessage(const aMessage: string): TCESynMemo;
  function newMessageData: PMessageItemData;

implementation
{$R *.lfm}

uses
  ce_main;

{$REGION Standard Comp/Obj------------------------------------------------------}
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

procedure TCEMessagesWidget.listDeletion(Sender: TObject; Node: TTreeNode);
begin
  if node.Data <> nil then
    Dispose( PMessageItemData(Node.Data));
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
{$ENDREGION}

{$REGION ICEWidgetPersist ------------------------------------------------------}
procedure TCEMessagesWidget.setMaxMessageCount(aValue: Integer);
begin
  if aValue < 10 then aValue := 10;
  if aValue > 1023 then aValue := 1023;
  if fMaxMessCnt = aValue then exit;
  fMaxMessCnt := aValue;
  clearOutOfRangeMessg;
end;

procedure TCEMessagesWidget.optset_MaxMessageCount(aReader: TReader);
begin
  maxMessageCount := aReader.ReadInteger;
end;

procedure TCEMessagesWidget.optget_MaxMessageCount(aWriter: TWriter);
begin
  aWriter.WriteInteger(fMaxMessCnt);
end;

procedure TCEMessagesWidget.declareProperties(aFiler: TFiler);
begin
  inherited;
  aFiler.DefineProperty(Name + '_MaxMessageCount', @optset_MaxMessageCount, @optget_MaxMessageCount, true);
end;
{$ENDREGION}

{$REGION ICEContextualActions---------------------------------------------------}
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

procedure TCEMessagesWidget.actClearAllExecute(Sender: TObject);
begin
  ClearAllMessages;
end;

procedure TCEMessagesWidget.actClearEdiExecute(Sender: TObject);
begin
  ClearMessages(mcEditor);
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
{$ENDREGION}

{$REGION ICEProjectMonitor -----------------------------------------------------}
procedure TCEMessagesWidget.projNew(const aProject: TCEProject);
begin
  fProj := aProject;
  filterMessages;
end;

procedure TCEMessagesWidget.projClose(const aProject: TCEProject);
begin
  if fProj = aProject then
    ClearMessages(mcProject);
  fProj := nil;
  filterMessages;
end;

procedure TCEMessagesWidget.projFocused(const aProject: TCEProject);
begin
  fProj := aProject;
  filterMessages;
end;
{$ENDREGION}

{$REGION ICEMultiDocMonitor ----------------------------------------------------}
procedure TCEMessagesWidget.docFocused(const aDoc: TCESynMemo);
begin
  fDoc := aDoc;
  filterMessages;
end;

procedure TCEMessagesWidget.docClose(const aDoc: TCESynMemo);
begin
  if aDoc <> fDoc then exit;
  ClearMessages(mcEditor);
  fDoc := nil;
  filterMessages;
end;
{$ENDREGION}

{$REGION Messages --------------------------------------------------------------}
procedure TCEMessagesWidget.clearOutOfRangeMessg;
begin
  while List.Items.Count > fMaxMessCnt do
    List.Items.Delete(List.Items.GetFirstNode);
end;

function newMessageData: PMessageItemData;
begin
  result := new(PMessageItemData);
  result^.ctxt := mcUnknown;
  result^.project := nil;
  result^.editor := nil;
  result^.position := point(0,0);
end;

function TCEMessagesWidget.newMessageItemData(aCtxt: TMessageContext): PMessageItemData;
begin
  result := new(PMessageItemData);
  result^.ctxt := aCtxt;
  result^.project := fProj;
  result^.editor := fDoc;
  result^.position := point(0,0);
end;

procedure TCEMessagesWidget.scrollToBack;
begin
  if not Visible then exit;
  if List.BottomItem <> nil then
    List.BottomItem.MakeVisible;
end;

procedure TCEMessagesWidget.ListDblClick(Sender: TObject);
var
  dat: PMessageItemData;
begin
  if List.Selected = nil then exit;
  if List.Selected.Data = nil then exit;
  //
  dat := PMessageItemData(List.Selected.Data);
  if dat^.editor = nil then exit;
  CEMainForm.openFile(dat^.editor.fileName);
  dat^.editor.CaretXY := dat^.position;
  dat^.editor.SelectLine;
end;

procedure TCEMessagesWidget.filterMessages;
var
  itm: TTreeNode;
  dat: PMessageItemData;
  i: NativeInt;
begin
  if updating then exit;
  for i := 0 to List.Items.Count-1 do
  begin
    itm := List.Items[i];
    dat := PMessageItemData(itm.Data);
    case dat^.ctxt of
      mcProject: itm.Visible := fProj = dat^.project;
      mcEditor: itm.Visible := fDoc = dat^.editor;
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
  dt: TMessageItemData;
begin
  for i := List.Items.Count-1 downto 0 do
  begin
    dt := PMessageItemData(List.Items[i].Data)^;
    if dt.ctxt = aCtxt then case aCtxt of
      mcEditor: if dt.editor = fDoc then List.Items.Delete(List.Items[i]);
      mcProject: if dt.project = fProj then List.Items.Delete(List.Items[i]);
      else List.Items.Delete(List.Items[i]);
    end;
  end;
end;

procedure TCEMessagesWidget.addCeInf(const aMsg: string; aCtxt: TMessageContext = mcUnknown);
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

procedure TCEMessagesWidget.addCeWarn(const aMsg: string; aCtxt: TMessageContext = mcUnknown);
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

procedure TCEMessagesWidget.addCeErr(const aMsg: string; aCtxt: TMessageContext = mcUnknown);
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

procedure TCEMessagesWidget.addMessage(const aMsg: string; const aData: PMessageItemData);
var
  item: TTreeNode;
  imgIx: Integer;
begin
  item := List.Items.Add(nil, aMsg);
  item.Data := aData;
  imgIx := Integer(semanticMsgAna(aMsg));
  item.ImageIndex := imgIx;
  item.SelectedIndex := imgIx;
  clearOutOfRangeMessg;
end;

procedure TCEMessagesWidget.addMessage(const aMsg: string; aCtxt: TMessageContext = mcUnknown);
var
  item: TTreeNode;
  imgIx: Integer;
begin
  item := List.Items.Add(nil, aMsg);
  item.Data := newMessageItemData(aCtxt);
  imgIx := Integer(semanticMsgAna(aMsg));
  item.ImageIndex := imgIx;
  item.SelectedIndex := imgIx;
  clearOutOfRangeMessg;
end;

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

function getLineFromDmdMessage(const aMessage: string): TPoint;
var
  i: NativeInt;
  ident: string;
begin
  result.x := 0;
  result.y := 0;
  ident := '';
  i := 1;
  while (true) do
  begin
    if i > length(aMessage) then exit;
    if aMessage[i] = '.' then
    begin
      inc(i);
      if i > length(aMessage) then exit;
      if aMessage[i] = 'd' then
      begin
        inc(i);
        if i > length(aMessage) then exit;
        if aMessage[i] = '(' then
        begin
          inc(i);
          if i > length(aMessage) then exit;
          while( isNumber(aMessage[i]) ) do
          begin
            ident += aMessage[i];
            inc(i);
            if i > length(aMessage) then exit;
          end;
          if aMessage[i] = ')' then
          begin
            result.y := strToIntDef(ident, -1);
            exit;
          end;
        end;
      end;
    end;
    inc(i);
  end;
end;

function getFileFromDmdMessage(const aMessage: string): TCESynMemo;
var
  i: NativeInt;
  ident: string;
  ext: string;
begin
  ident := '';
  i := 0;
  result := nil;
  while(true) do
  begin
    inc(i);
    if i > length(aMessage) then exit;
    if aMessage[i] = '(' then
    begin
      if not fileExists(ident) then exit;
      ext := extractFileExt(ident);
      if not (ext = '.d') or (ext = '.di') then exit;
      CEMainForm.openFile(ident);
      result := CEMainForm.EditWidget.currentEditor;
    end;
    ident += aMessage[i];
  end;
end;
{$ENDREGION}

end.
