unit ce_projconfall;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, Forms, Controls, Graphics, Dialogs, 
    ce_projconfframe;

type

  { TCEProjConfAll }

  TCEProjConfAll = class(TCEProjConfFrame)
    Grid: TTIPropertyGrid;
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

end.

