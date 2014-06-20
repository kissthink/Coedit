unit ce_messages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, ce_widget, ActnList, Menus, clipbrd;

type

  { TCEMessagesWidget }
  TCEMessagesWidget = class(TCEWidget)
    imgList: TImageList;
    List: TListView;
  private
    fActClear: TAction;
    fActSaveMsg: TAction;
    fActCopyMsg: TAction;
    fActSelAll: TAction;
    procedure actClearExecute(Sender: TObject);
    procedure actSaveMsgExecute(Sender: TObject);
    procedure actCopyMsgExecute(Sender: TObject);
    procedure actSelAllExecute(Sender: TObject);
  public
    constructor create(aOwner: TComponent); override;
    //
    procedure scrollToBack;
    procedure addMessage(const aMsg: string);
    procedure addCeInf(const aMsg: string);
    procedure addCeErr(const aMsg: string);
    procedure addCeWarn(const aMsg: string);
    //
    function contextName: string; override;
    function contextActionCount: integer; override;
    function contextAction(index: integer): TAction; override;
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
var
  itm: TMenuItem;
begin
  inherited;
  fID := 'ID_MSGS';
  //
  fActClear := TAction.Create(self);
  fActClear.OnExecute := @actClearExecute;
  fActClear.caption := 'Clear messages';
  fActCopyMsg := TAction.Create(self);
  fActCopyMsg.OnExecute := @actCopyMsgExecute;
  fActCopyMsg.Caption := 'Copy message(s)';
  fActSelAll := TAction.Create(self);
  fActSelAll.OnExecute := @actSelAllExecute;
  fActSelAll.Caption := 'Select all';
  fActSaveMsg := TAction.Create(self);
  fActSaveMsg.OnExecute := @actSaveMsgExecute;
  fActSaveMsg.caption := 'Save messages to...';
  //
  List.PopupMenu := contextMenu;
  itm := TMenuItem.Create(self);
  itm.Action := fActClear;
  contextMenu.Items.Add(itm);
  itm := TMenuItem.Create(self);
  itm.Action := fActCopyMsg;
  contextMenu.Items.Add(itm);
  itm := TMenuItem.Create(self);
  itm.Action := fActSelAll;
  contextMenu.Items.Add(itm);
  itm := TMenuItem.Create(self);
  itm.Action := fActSaveMsg;
  contextMenu.Items.Add(itm);
end;

procedure TCEMessagesWidget.scrollToBack;
begin
  if not Visible then exit;
  List.ViewOrigin := Point(0,List.Items.Count * 25);
end;

procedure TCEMessagesWidget.addCeInf(const aMsg: string);
var
  item: TCEMessageItem;
begin
  item := TCEMessageItem.Create(List.Items);
  item.Caption := 'Coedit information: ' + aMsg;
  item.ImageIndex := 1;
  List.Items.AddItem(item);
  scrollToBack;
end;

procedure TCEMessagesWidget.addCeWarn(const aMsg: string);
var
  item: TCEMessageItem;
begin
  item := TCEMessageItem.Create(List.Items);
  item.Caption := 'Coedit warning: ' + aMsg;
  item.ImageIndex := 3;
  List.Items.AddItem(item);
  scrollToBack;
end;

procedure TCEMessagesWidget.addCeErr(const aMsg: string);
var
  item: TCEMessageItem;
begin
  item := TCEMessageItem.Create(List.Items);
  item.Caption := 'Coedit error: ' + aMsg;
  item.ImageIndex := 4;
  List.Items.AddItem(item);
  scrollToBack;
end;

procedure TCEMessagesWidget.addMessage(const aMsg: string);
var
  item: TCEMessageItem;
begin
  item := TCEMessageItem.Create(List.Items);
  item.Caption := aMsg;
  item.Data := mainForm.EditWidget.currentEditor;
  item.ImageIndex := Integer( semanticMsgAna(aMsg) );
  List.Items.AddItem(item);
end;

function TCEMessagesWidget.contextName: string;
begin
  result := 'Messages';
end;

function TCEMessagesWidget.contextActionCount: integer;
begin
  result := 4;
end;

function TCEMessagesWidget.contextAction(index: integer): TAction;
begin
  case index of
    0: result := fActClear;
    1: result := fActCopyMsg;
    2: result := fActSelAll;
    3: result := fActSaveMsg;
    else result := nil;
  end;
end;

procedure TCEMessagesWidget.actClearExecute(Sender: TObject);
begin
  List.Clear;
end;

procedure TCEMessagesWidget.actCopyMsgExecute(Sender: TObject);
var
  i: NativeInt;
  str: string;
begin
  str := '';
  for i := 0 to List.Items.Count-1 do if List.Items[i].Selected then
    str += List.Items[i].Caption + LineEnding;
  Clipboard.AsText := str;
end;

procedure TCEMessagesWidget.actSelAllExecute(Sender: TObject);
begin
  List.SelectAll;
end;

procedure TCEMessagesWidget.actSaveMsgExecute(Sender: TObject);
var
  lst: TStringList;
  itm: TListItem;
begin
  with TSaveDialog.Create(nil) do
  try
    if execute then
    begin
      lst := TStringList.Create;
      try
        for itm in List.Items do
          lst.Add(itm.Caption);
        lst.SaveToFile(filename);
      finally
        lst.Free;
      end;
    end;
  finally
    free;
  end;
end;

function semanticMsgAna(const aMessg: string): TMessageKind;
var
  pos: Nativeint;
  idt: string;
function checkIdent: TMessageKind;
begin
  case idt of
    'ERROR', 'error', 'Error', 'Invalid', 'invalid',
    'illegal', 'Illegal', 'fatal', 'Fatal', 'Critical', 'critical':
      exit(msgkError);
    'Warning', 'warning':
      exit(msgkWarn);
    'Hint', 'hint', 'Tip', 'tip':
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
