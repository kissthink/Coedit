unit ce_customtools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, ce_common;

type

  TCEToolItem = class(TCollectionItem)
  private
    fExecutable: string;
    fWorkingDir: string;
    fShowWin: TShowWindowOptions;
    fProcess: TProcess;
    function getParameters: TStringList;
    procedure setParameters(const aValue: TStringList);
  published
    property executable: string read fExecutable write fExecutable;
    property workingDirectory: string read fWorkingDir write fWorkingDir;
    property parameters: TStringList read getParameters write setParameters;
    property showWindows: TShowWindowOptions read fShowWin write fShowWin;
  public
    constructor create(ACollection: TCollection); override;
    destructor destroy; override;
    //
    property process: TProcess read fProcess;
    procedure execute;
  end;

  TCETools = class(TCollection)
  private
    function getItem(index: Integer): TCEToolItem;
  public
    constructor create(itemClass: TCollectionItem) override;
    //
    property tool[index: integer]: TCEToolItem read getItem;
  end;

implementation

uses
  CEMainForm;

constructor TCEToolItem.create(ACollection: TCollection);
begin
  inherited;
  fProcess := TProcess.Create(nil);
end;

destructor TCEToolItem.destroy;
begin
  fProcess.Free;
  inherited;
end;

function TCEToolItem.getParameters: TStringList;
begin
  result := fProcess.Parameters;
end;

procedure TCEToolItem.setParameters(const aValue: TStringList);
begin
  fProcess.Parameters.Assign(aValue);
end;

procedure TCEToolItem.execute;
begin

end;

constructor TCETools.create(itemClass: TCollectionItem) override;
begin
  inherited create(itemClass);
end;

function TCETools.getItem(index: Integer): TCEToolItem;
begin
  exit(TCEToolItem(Items[index]));
end;

end.

