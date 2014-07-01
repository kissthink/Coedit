unit ce_synmemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEdit, SynMemo, ce_d2syn,
  SynPluginSyncroEdit, SynEditKeyCmds, ce_project;

type

  TCESynMemo = class(TSynMemo)
  private
    fFilename: string;
    fModified: boolean;
    fAssocProject: TCEProject;
    function getIfDSource: Boolean;
    function getIfConfig: Boolean;
  public
    constructor Create(aOwner: TComponent); override;
    //
    property fileName: string read fFilename write fFilename;
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
  graphics, ce_main;

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
