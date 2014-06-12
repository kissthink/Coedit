unit ce_project;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, ce_common, ce_widget;

type

  { TCEWidgetProject }

  TCEProjectWidget = class(TCEWidget)
    Tree: TTreeView;
  private
    fProject: TCEProject;
    procedure updateView;
    procedure setProject(aValue: TCEProject);
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    property project: TCEProject read fProject write setProject;
  end;

implementation
{$R *.lfm}

constructor TCEProjectWidget.create(aOwner: TComponent);
begin
  inherited;
  fID := 'ID_PROJ';
end;

destructor TCEProjectWidget.destroy;
begin
  inherited;
end;

procedure TCEProjectWidget.setProject(aValue: TCEProject);
begin
  if fProject = aValue then exit;
  fProject := aValue;
  if aValue = nil then exit;
  updateView;
end;

procedure TCEProjectWidget.updateView;
begin

end;

end.

