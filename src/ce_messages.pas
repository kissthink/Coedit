unit ce_messages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls, ce_widget, ce_common;

type

  { TCEMessagesWidget }
  TCEMessagesWidget = class(TCEWidget,ICEMultiDocMonitor)
    List: TListView;
  private
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    procedure addMessage(const aMsg: string);
    //
    procedure docChange(const aNewIndex: integer);
    procedure docClose(const aNewIndex: integer);
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

procedure TCEMessagesWidget.docChange(const aNewIndex: integer);
begin
  // can grow the list...
  // can display matching msgs from a list...
end;

procedure TCEMessagesWidget.docClose(const aNewIndex: integer);
begin
  // can shrink the list...
end;

end.

