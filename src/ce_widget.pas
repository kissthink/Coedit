unit ce_widget;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls,
  AnchorDocking, AnchorDockStorage, ActnList, Menus, syncobjs, ce_common;

type

  (**
   * Base type for an UI module.
   *)
  PTCEWidget = ^TCEWidget;

  { TCEWidget }
  TCEWidget = class(TForm, ICEContextualActions, ICEProjectMonitor)
    Content: TPanel;
    Back: TPanel;
    contextMenu: TPopupMenu;
  private
    fAutoUpdater: TTimer;
    fAutoUpdating: boolean;
    fManuUpdating: boolean;
    fWidgUpdateCount: NativeInt;
    procedure autoUpdaterEvent(Sender: TObject);
  protected
    fID: string;
    fNeedAutoUpdate: boolean;
    fLocker: TCriticalSection;
    procedure autoWidgetUpdate; virtual;
    procedure manualWidgetUpdate; virtual;
  published
    property ID: string read fID write fID;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    procedure beginManualWidgetUpdate;
    procedure endManualWidgetUpdate;
    procedure forceManualWidgetUpdate;
    //
    procedure projNew(const aProject: TCEProject); virtual;
    procedure projChange(const aProject: TCEProject); virtual;
    procedure projClose(const aProject: TCEProject); virtual;
    //
    function contextName: string; virtual;
    function contextActionCount: integer; virtual;
    function contextAction(index: integer): TAction; virtual;
    //
    property isAutoUpdating: boolean read fAutoUpdating;
    property isManualUpdating: boolean read fManuUpdating;
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
constructor TCEWidget.create(aOwner: TComponent);
begin
  inherited;
  fID := 'ID_XXXX';
  fAutoUpdater := TTimer.Create(self);
  fAutoUpdater.Interval := 50;
  fAutoUpdater.OnTimer := @autoUpdaterEvent;
  fLocker := TCriticalSection.Create;
  DockMaster.MakeDockable(Self, true, true, true);
  DockMaster.GetAnchorSite(Self).Header.HeaderPosition := adlhpTop;
end;

destructor TCEWidget.destroy;
begin
  fLocker.Leave;
  fLocker.Free;
  inherited;
end;

procedure TCEWidget.beginManualWidgetUpdate;
begin
  Inc(fWidgUpdateCount);
end;

procedure TCEWidget.endManualWidgetUpdate;
begin
  Dec(fWidgUpdateCount);
  if fWidgUpdateCount > 0 then
  begin
    writeln('widget update count > 0');
    exit;
  end;

  fManuUpdating := true;
  //fLocker.Enter;
  manualWidgetUpdate;
  //fLocker.Leave;
  fManuUpdating := false;
  fWidgUpdateCount := 0;

end;

procedure TCEWidget.forceManualWidgetUpdate;
begin
  fManuUpdating := true;
  //fLocker.Enter;
  manualWidgetUpdate;
  //fLocker.Leave;
  fManuUpdating := false;
end;

procedure TCEWidget.autoUpdaterEvent(Sender: TObject);
begin
  if not fNeedAutoUpdate then exit;
  fAutoUpdating := true;
  try
    //fLocker.Enter;
    autoWidgetUpdate;
  finally
    //fLocker.Leave;
    fAutoUpdating := false;
    fNeedAutoUpdate := false;
  end;
end;

procedure TCEWidget.autoWidgetUpdate;
begin
end;

procedure TCEWidget.manualWidgetUpdate;
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

