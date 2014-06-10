unit ce_messages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, ce_frame, ce_common;

type

  { TCEWidgetMessages }

  TCEMessagesWidget = class(TCEWidget,ICEMultiDocMonitor)
    List: TListView;
  private
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    procedure docChange(const aNewIndex: integer);
    procedure docClose(const aNewIndex: integer);
  end;

implementation
{$R *.lfm}

constructor TCEMessagesWidget.create(aOwner: TComponent);
begin
  inherited;
  fID := 'ID_MSGS';
end;

destructor TCEMessagesWidget.destroy;
begin
  inherited;
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

