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
  TCEWidget = class(TForm, ICEContextualActions, ICEProjectMonitor, ICEMultiDocMonitor, ICEWidgetPersist)
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
    //
    procedure optget_LoopInterval(aWriter: TWriter);
    procedure optset_LoopInterval(aReader: TReader);
    procedure optget_UpdaterDelay(aWriter: TWriter);
    procedure optset_UpdaterDelay(aReader: TReader);
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
    // prevent any pending update.
    procedure stopUpdateByDelay;
    // immediate call any pending update.
    procedure endUpdatebyDelay;
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
    procedure projCompile(const aProject: TCEProject); virtual;
    procedure projRun(const aProject: TCEProject); virtual;
    procedure projFocused(const aProject: TCEProject); virtual;
    //
    function contextName: string; virtual;
    function contextActionCount: integer; virtual;
    function contextAction(index: integer): TAction; virtual;
    //
    procedure beforeSave(sender: TObject); virtual;
    procedure declareProperties(aFiler: TFiler); virtual;
    procedure afterLoad(sender: TObject); virtual;
    //
    // returns true if one of the three updater is processing.
    property updating: boolean read fUpdating;
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

(*******************************************************************************
 * TCEWidget
 *)

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEWidget.create(aOwner: TComponent);
var
  i: NativeInt;
  itm: TmenuItem;
begin
  inherited;
  fUpdaterAuto := TTimer.Create(self);
  fUpdaterAuto.Interval := 70;
  fUpdaterAuto.OnTimer := @updaterAutoProc;
  fUpdaterDelay := TTimer.Create(self);

  updaterByLoopInterval := 70;
  updaterByDelayDuration := 1250;

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
{$ENDREGION}

{$REGION ICEWidgetPersist ------------------------------------------------------}
procedure TCEWidget.beforeSave(sender: TObject);
begin
end;

procedure TCEWidget.declareProperties(aFiler: TFiler);
begin
  // override rules: inhertied must be called. No dots in the property name, property name prefixed with the widget Name
  aFiler.DefineProperty(Name + '_updaterByLoopInterval', @optset_LoopInterval, @optget_LoopInterval, true);
  aFiler.DefineProperty(Name + '_updaterByDelayDuration', @optset_UpdaterDelay, @optget_UpdaterDelay, true);
end;

procedure TCEWidget.afterLoad(sender: TObject);
begin
end;

procedure TCEWidget.optget_LoopInterval(aWriter: TWriter);
begin
  aWriter.WriteInteger(fLoopInter);
end;

procedure TCEWidget.optset_LoopInterval(aReader: TReader);
begin
  updaterByLoopInterval := aReader.ReadInteger;
end;

procedure TCEWidget.optget_UpdaterDelay(aWriter: TWriter);
begin
  aWriter.WriteInteger(fDelayDur);
end;

procedure TCEWidget.optset_UpdaterDelay(aReader: TReader);
begin
  updaterByDelayDuration := aReader.ReadInteger;
end;
{$ENDREGION}

{$REGION ICEContextualActions---------------------------------------------------}
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
{$ENDREGION}

{$REGION ICEMultiDocMonitor ----------------------------------------------------}
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
{$ENDREGION}

{$REGION ICEProjectMonitor -----------------------------------------------------}
procedure TCEWidget.projNew(const aProject: TCEProject);
begin
end;

procedure TCEWidget.projChange(const aProject: TCEProject);
begin
end;

procedure TCEWidget.projClose(const aProject: TCEProject);
begin
end;

procedure TCEWidget.projCompile(const aProject: TCEProject);
begin
end;

procedure TCEWidget.projRun(const aProject: TCEProject);
begin
end;

procedure TCEWidget.projFocused(const aProject: TCEProject);
begin
end;
{$ENDREGION}

{$REGION Updaters---------------------------------------------------------------}
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

procedure TCEWidget.stopUpdateByDelay;
begin
  fUpdaterDelay.OnTimer := nil;
end;

procedure TCEWidget.endUpdateByDelay;
begin
  updaterLatchProc(nil);
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
{$ENDREGION}

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

