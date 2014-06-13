unit ce_messages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, ce_widget, ActnList;

type

  { TCEMessagesWidget }
  TCEMessagesWidget = class(TCEWidget)
    imgList: TImageList;
    List: TListView;
  private
    fActClear: TAction;
    fActSaveMsg: TAction;
    procedure actClearExecute(Sender: TObject);
    procedure actSaveMsgExecute(Sender: TObject);
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    procedure addMessage(const aMsg: string);
    //
    function contextName: string; override;
    function contextActionCount: integer; override;
    function contextAction(index: integer): TAction; override;
  end;

  PTCEMessageItem = ^TCEMessageItem;
  TCEMessageItem = class(TListItem)
  end;

implementation
{$R *.lfm}

uses
  ce_main;

constructor TCEMessagesWidget.create(aOwner: TComponent);
begin
  inherited;
  fID := 'ID_MSGS';
  //
  fActClear := TAction.Create(self);
  fActClear.OnExecute := @actClearExecute;
  fActClear.caption := 'Clear messages';
  fActSaveMsg := TAction.Create(self);
  fActSaveMsg.OnExecute := @actSaveMsgExecute;
  fActSaveMsg.caption := 'Save messages to...';
end;

destructor TCEMessagesWidget.destroy;
begin
  inherited;
end;

procedure TCEMessagesWidget.addMessage(const aMsg: string);
var
  item: TCEMessageItem;
begin
  item := TCEMessageItem.Create(List.Items);
  item.Caption := aMsg;
  item.Data := mainForm.EditWidget.currentEditor;
  List.Items.AddItem(item);
end;

function TCEMessagesWidget.contextName: string;
begin
  result := 'Messages';
end;

function TCEMessagesWidget.contextActionCount: integer;
begin
  result := 2;
end;

function TCEMessagesWidget.contextAction(index: integer): TAction;
begin
  case index of
    0: result := fActClear;
    1: result := fActSaveMsg;
  end;
end;

procedure TCEMessagesWidget.actClearExecute(Sender: TObject);
begin
  List.Clear;
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

end.

