unit ce_widget;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DividerBevel, Forms, Controls, ExtCtrls,
  ce_common, ActnList;

type

  (**
   * Base type for an UI module.
   *)
  PTCEWidget = ^TCEWidget;

  { TCEWidget }
  TCEWidget = class(TForm, ICEContextualActions, ICEProjectMonitor)
    Content: TScrollBox;
    Back: TPanel;
    Header: TPanel;
    Updater: TTimer;
  private
    procedure updaterTimer(Sender: TObject);
  protected
    fID: string;
    fNeedUpdate: boolean;
    procedure UpdaterProc; virtual;
  published
    property ID: string read fID write fID;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    procedure projChange(const aProject: TCEProject); virtual;
    //
    function contextName: string; virtual;
    function contextActionCount: integer; virtual;
    function contextAction(index: integer): TAction; virtual;
  end;

  (**
   * Holds a list of TCEWidget.
   *)
  TCEWidgetList = class(TList)
  private
    function getWidget(index: integer): TCEWidget;
  public
    procedure addWidget(aValue: PTCEWidget);
    property widget[index: integer]: TCEWidget read getWidget;
  end;

implementation
{$R *.lfm}

(*******************************************************************************
 * TCEWidget
 *)
constructor TCEWidget.create(aOwner: TComponent);
begin
  inherited;
  fID := 'ID_XXXX';
  Updater.OnTimer := @updaterTimer;
end;

destructor TCEWidget.destroy;
begin
  inherited;
end;

procedure TCEWidget.updaterTimer(Sender: TObject);
begin
  if not fNeedUpdate then exit;
  fNeedUpdate := false;
  UpdaterProc;
end;

procedure TCEWidget.UpdaterProc;
begin
end;

procedure TCEWidget.projChange(const aProject: TCEProject);
begin
end;

function TCEWidget.contextName: string;
begin
  result := '';
end;

function TCEWidget.contextActionCount: integer;
begin
  result := 0;
end;

function TCEWidget.contextAction(index: integer): TAction;
begin
  result := nil;
end;

(*******************************************************************************
 * TCEWidgetList
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

