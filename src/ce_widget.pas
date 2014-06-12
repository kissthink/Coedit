unit ce_widget;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, ExtCtrls,
  ce_common;

type

  (**
   * Base type for an UI module.
   *)
  PTCEWidget = ^TCEWidget;

  { TCEWidget }
  TCEWidget = class(TForm)
    Content: TScrollBox;
    Back: TPanel;
    Header: TPanel;
  protected
    fID: string;
  published
    property ID: string read fID write fID;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
  end;

  (**
   * Holds a list of TCEWidget.
   *)
  TCEWidgetList = class(TList)
  private
    function getWidget(index: integer): TCEWidget;
  public
    procedure addWidget(aValue: PTCEWidget);
    property frame[index: integer]: TCEWidget read getWidget;
  end;

implementation
{$R *.lfm}


constructor TCEWidget.create(aOwner: TComponent);
begin
  inherited;
  fID := 'ID_XXXX';
end;

destructor TCEWidget.destroy;
begin
  inherited;
end;

(*******************************************************************************
 * TFrameList
 *)
function TCEWidgetList.getWidget(index: integer): TCEWidget;
begin
  result := PTCEWidget(Items[index])^;
end;

procedure TCEWidgetList.addWidget(aValue: PTCEWidget);
begin
  add(Pointer(aValue));
end;

end.

