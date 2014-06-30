unit ce_widget;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls,
  AnchorDocking, AnchorDockStorage, ActnList, Menus,
  ce_synmemo, ce_widgettypes, ce_project;

type

  (**
   * Base type for an UI module.
   *)
  PTCEWidget = ^TCEWidget;

  { TCEWidget }
  TCEWidget = class(TForm, ICEContextualActions, ICEProjectMonitor, ICEMultiDocMonitor)
    Content: TPanel;
    Back: TPanel;
    contextMenu: TPopupMenu;
  private
    fUpdating: boolean;
    fDelayDur: Integer;
    fLoopInter: Integer;
    fUpdaterAuto: TTimer;
    fUpdaterDelay: TTimer;
    fWidgUpdateCount: NativeInt;
    procedure setDelayDur(aValue: Integer);
    procedure setLoopInt(aValue: Integer);
    procedure updaterAutoProc(Sender: TObject);
    procedure updaterLatchProc(Sender: TObject);
  protected
    fID: string;
    // a descendant overrides to implementi a periodic update.
    procedure UpdateByLoop; virtual;
    // a descendant overrides to implement an event driven update.
    procedure UpdateByEvent; virtual;
    // a descendant overrides to implement a delayed update event.
    procedure UpdateByDelay; virtual;
  // May be used for appplication options
  published
    property updaterByLoopInterval: Integer read fLoopInter write setLoopInt;
    property updaterByDelayDuration: Integer read fDelayDur write setDelayDur;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    // restarts the wait period to the delayed update event.
    // if not re-called during 'updaterByDelayDuration' ms then
    // 'UpdateByDelay' is called once.
    procedure beginUpdateByDelay;
    // increments the updates count.
    procedure beginUpdateByEvent;
    // decrements the update count and call 'UpdateByEvent' if the
    // counter value is null.
    procedure endUpdateByEvent;
    // immediate call 'UpdateByEvent'
    procedure forceUpdateByEvent;
    //
    procedure docNew(const aDoc: TCESynMemo); virtual;
    procedure docFocused(const aDoc: TCESynMemo); virtual;
    procedure docChanged(const aDoc: TCESynMemo); virtual;
    procedure docClose(const aDoc: TCESynMemo); virtual;
    //
    procedure projNew(const aProject: TCEProject); virtual;
    procedure projChange(const aProject: TCEProject); virtual;
    procedure projClose(const aProject: TCEProject); virtual;
    //
    function contextName: string; virtual;
    function contextActionCount: integer; virtual;
    function contextAction(index: integer): TAction; virtual;
    // returns true if one of the three updater is processing.
    property updating: boolean read fUpdating;
    property ID: string read fID write fID;
  end;

  (**
   * TCEWidget list.
   *)
  TCEWidgetList = class(TList)
  private
    function getWidget(index: integer): TCEWidget;
  public
    procedure addWidget(aValue: PTCEWidget);
    property widget[index: integer]: TCEWidget read getWidget;
  end;

  TWidgetEnumerator = class
    fList: TCEWidgetList;
    fIndex: Integer;
    function getCurrent: TCEWidget;
    Function moveNext: boolean;
    property current: TCEWidget read getCurrent;
  end;

  operator enumerator(aWidgetList: TCEWidgetList): TWidgetEnumerator;

implementation
{$R *.lfm}

uses
  ce_main;

(*******************************************************************************
 * TCEWidget
 *)
constructor TCEWidget.create(aOwner: TComponent);
var
  i: NativeInt;
  itm: TmenuItem;
begin
  inherited;
  fID := 'ID_XXXX';

  fUpdaterAuto := TTimer.Create(self);
  fUpdaterAuto.Interval := 70;
  fUpdaterAuto.OnTimer := @updaterAutoProc;
  fUpdaterDelay := TTimer.Create(self);

  updaterByLoopInterval := 50;
  updaterByDelayDuration := 1000;

  DockMaster.MakeDockable(Self, true, true, true);
  DockMaster.GetAnchorSite(Self).Header.HeaderPosition := adlhpTop;
  DockMaster.GetAnchorSite(Self).Name := ID;

  for i := 0 to contextActionCount-1 do
  begin
    itm := TMenuItem.Create(self);
    itm.Action := contextAction(i);
    contextMenu.Items.Add(itm);
  end;

  PopupMenu := contextMenu;
end;

destructor TCEWidget.destroy;
begin
  inherited;
end;

procedure TCEWidget.setDelayDur(aValue: Integer);
begin
  if aValue < 100 then aValue := 100;
  if fDelayDur = aValue then exit;
  fDelayDur := aValue;
  fUpdaterDelay.Interval := fDelayDur;
end;

procedure TCEWidget.setLoopInt(aValue: Integer);
begin
  if aValue < 30 then aValue := 30;
  if fLoopInter = aValue then exit;
  fLoopInter := aValue;
  fUpdaterAuto.Interval := fLoopInter;
end;

procedure TCEWidget.beginUpdateByEvent;
begin
  Inc(fWidgUpdateCount);
end;

procedure TCEWidget.endUpdateByEvent;
begin
  Dec(fWidgUpdateCount);
  if fWidgUpdateCount > 0 then exit;
  fUpdating := true;
  UpdateByEvent;
  fUpdating := false;
  fWidgUpdateCount := 0;
end;

procedure TCEWidget.forceUpdateByEvent;
begin
  fUpdating := true;
  UpdateByEvent;
  fUpdating := false;
  fWidgUpdateCount := 0;
end;

procedure TCEWidget.beginUpdateByDelay;
begin
  fUpdaterDelay.Enabled := false;
  fUpdaterDelay.Enabled := true;
  fUpdaterDelay.OnTimer := @updaterLatchProc;
end;

procedure TCEWidget.updaterAutoProc(Sender: TObject);
begin
  fUpdating := true;
  UpdateByLoop;
  fUpdating := false;
end;

procedure TCEWidget.updaterLatchProc(Sender: TObject);
begin
  fUpdating := true;
  UpdateByDelay;
  fUpdating := false;
  fUpdaterDelay.OnTimer := nil;
end;

procedure TCEWidget.UpdateByLoop;
begin
end;

procedure TCEWidget.UpdateByEvent;
begin
end;

procedure TCEWidget.UpdateByDelay;
begin
end;

procedure TCEWidget.projNew(const aProject: TCEProject);
begin
end;

procedure TCEWidget.projChange(const aProject: TCEProject);
begin
end;

procedure TCEWidget.projClose(const aProject: TCEProject);
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

procedure TCEWidget.docNew(const aDoc: TCESynMemo);
begin
end;

procedure TCEWidget.docFocused(const aDoc: TCESynMemo);
begin
end;

procedure TCEWidget.docChanged(const aDoc: TCESynMemo);
begin
end;

procedure TCEWidget.docClose(const aDoc: TCESynMemo);
begin
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

function TWidgetEnumerator.getCurrent:TCEWidget;
begin
  result := fList.widget[fIndex];
end;

function TWidgetEnumerator.moveNext: boolean;
begin
  Inc(fIndex);
  result := fIndex < fList.Count;
end;

operator enumerator(aWidgetList: TCEWidgetList): TWidgetEnumerator;
begin
  result := TWidgetEnumerator.Create;
  result.fList := aWidgetList;
  result.fIndex := -1;
end;

end.

