unit ce_synmemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEdit, SynMemo, ce_d2syn,
  SynPluginSyncroEdit, SynEditKeyCmds, ce_project, ce_common;

type

  TCESynMemo = class(TSynMemo)
  private
    fFilename: string;
    fModified: boolean;
    fNoDateCheck: boolean;
    fFileDate: double;
    fAssocProject: TCEProject;
    function getIfDSource: Boolean;
    function getIfConfig: Boolean;
    procedure setFilename(const aValue: string);
  public
    constructor Create(aOwner: TComponent); override;
    procedure checkFileDate;
    //
    property fileName: string read fFilename write setFilename;
    property modified: boolean read fModified write fModified;
    property project: TCEProject read fAssocProject write fAssocProject;
    //
    property isDSource: boolean read getIfDSource;
    property isProjectSource: boolean read getIfConfig;
  end;

var
  D2Syn: TSynD2Syn;

implementation

uses
  graphics, ce_main, controls;

constructor TCESynMemo.Create(aOwner: TComponent);
begin
  inherited;
  Font.Quality := fqProof;
  TabWidth := 4;
  Options :=
    [ eoAutoIndent, eoBracketHighlight, eoGroupUndo, eoTabsToSpaces,
      eoTrimTrailingSpaces, eoDragDropEditing, eoShowCtrlMouseLinks,
      eoEnhanceHomeKey, eoTabIndent];
  Options2 := [eoEnhanceEndKey, eoFoldedCopyPaste, eoOverwriteBlock];
  //
  Gutter.LineNumberPart.ShowOnlyLineNumbersMultiplesOf := 5;
  Gutter.LineNumberPart.MarkupInfo.Foreground := clGray;
  Gutter.SeparatorPart.LineOffset := 1;
  Gutter.SeparatorPart.LineWidth := 1;
  Gutter.SeparatorPart.MarkupInfo.Foreground := clGray;
  Gutter.CodeFoldPart.MarkupInfo.Foreground := clGray;
  //
  Highlighter := D2Syn;
end;

procedure TCESynMemo.setFilename(const aValue: string);
begin
  if fFilename = aValue then exit;
  fNoDateCheck := false;
  fFilename := aValue;
  FileAge(fFilename, fFileDate);
end;

procedure TCESynMemo.checkFileDate;
var
  newDate: double;
begin
  if not FileAge(fFilename, newDate) then exit;
  if fFileDate = newDate then exit;
  if fFileDate <> 0.0 then
  begin
    if dlgOkCancel(format('"%s" has been modified by another program, load the new version ?',
      [shortenPath(fFilename)])) = mrOk then
    begin
      Lines.LoadFromFile(fFilename);
      fModified := false;
    end
    else fNoDateCheck := true;
  end;
  fFileDate := newDate;
end;

function TCESynMemo.getIfDSource: Boolean;
begin
  exit(Highlighter = D2Syn);
end;

function TCESynMemo.getIfConfig: Boolean;
begin
  exit(Highlighter = mainForm.LfmSyn);
end;

initialization
  D2Syn := TSynD2Syn.create(nil);
finalization
  D2Syn.free;
end.
