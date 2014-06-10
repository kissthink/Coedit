unit ce_synmemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEdit, SynMemo, ce_common, ce_d2syn,
  SynPluginSyncroEdit, SynEditKeyCmds;

type

  TCESynMemo = class(TSynMemo)
  private
    fFilename: string;
    fModified: boolean;
    fAssocProject: TCEProject;
  public
    constructor Create(aOwner: TComponent); override;
    //
    property fileName: string read fFilename write fFilename;
    property modified: boolean read fModified write fModified;
    property project: TCEProject read fAssocProject write fAssocProject;
  end;

var
  D2Syn: TSynD2Syn;

implementation

uses
  graphics;

constructor TCESynMemo.Create(aOwner: TComponent);
begin
  inherited;
  Font.Quality := fqProof;
  TabWidth := 4;
  Options :=
    [ eoAutoIndent, eoBracketHighlight, eoGroupUndo, eoTabsToSpaces,
      eoTrimTrailingSpaces, eoDragDropEditing, eoShowCtrlMouseLinks];
  Options2 := [eoEnhanceEndKey, eoFoldedCopyPaste, eoOverwriteBlock];
  //
  Gutter.LineNumberPart.ShowOnlyLineNumbersMultiplesOf := 5;
  Gutter.LineNumberPart.MarkupInfo.Foreground := clSilver;
  Gutter.SeparatorPart.LineOffset:=1;
  Gutter.SeparatorPart.LineWidth:=1;
  Gutter.SeparatorPart.MarkupInfo.Foreground := clSilver;
  Gutter.CodeFoldPart.MarkupInfo.Foreground := clSilver;
  //
  Highlighter := D2Syn;
end;

initialization
  D2Syn := TSynD2Syn.create(nil);
finalization
  D2Syn.free;
end.
