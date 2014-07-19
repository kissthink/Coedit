unit ce_synmemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEdit, SynMemo, ce_d2syn, SynEditHighlighter, controls,
  LazSynEditText, SynPluginSyncroEdit, SynEditKeyCmds, ce_project, ce_common;

type

  TCESynMemo = class(TSynMemo)
  private
    fFilename: string;
    fModified: boolean;
    fNoDateCheck: boolean;
    fFileDate: double;
    fAssocProject: TCEProject;
    fIsDSource: boolean;
    fIsConfig: boolean;
    fIdentifier: string;
    procedure changeNotify(Sender: TObject);
    procedure identifierToD2Syn;
  protected
    procedure SetHighlighter(const Value: TSynCustomHighlighter); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer); override;
  public
    constructor Create(aOwner: TComponent); override;
    procedure setFocus; override;
    procedure UpdateShowing; override;
    //
    procedure checkFileDate;
    procedure loadFromFile(const aFilename: string);
    procedure saveToFile(const aFilename: string);
    procedure save;
    //
    property Identifier: string read fIdentifier;
    property fileName: string read fFilename;
    property modified: boolean read fModified;
    property project: TCEProject read fAssocProject write fAssocProject;
    //
    property isDSource: boolean read fIsDSource;
    property isProjectSource: boolean read fIsConfig;
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
  //
  fFilename := '<new document>';
  fModified := false;

  TextBuffer.AddNotifyHandler(senrUndoRedoAdded, @changeNotify);
end;

procedure TCESynMemo.setFocus;
begin
  inherited;
  checkFileDate;
  identifierToD2Syn;
end;

procedure TCESynMemo.UpdateShowing;
begin
  inherited;
  if not Visible then exit;
  checkFileDate;
  identifierToD2Syn;
end;

procedure TCESynMemo.SetHighlighter(const Value: TSynCustomHighlighter);
begin
  inherited;
  fIsDSource := Highlighter = D2Syn;
  fIsConfig := Highlighter = CEMainForm.LfmSyn;
end;

procedure TCESynMemo.identifierToD2Syn;
begin
  fIdentifier := GetWordAtRowCol(LogicalCaretXY);
  if fIsDSource then D2Syn.CurrentIdentifier := fIdentifier;
end;

procedure TCESynMemo.changeNotify(Sender: TObject);
begin
  identifierToD2Syn;
  fModified := true;
end;

procedure TCESynMemo.loadFromFile(const aFilename: string);
var
  ext: string;
begin
  ext := extractFileExt(aFilename);
  if (ext <> '.d') and (ext <> '.di') then
    Highlighter := nil;
  Lines.LoadFromFile(aFilename);
  fFilename := aFilename;
  FileAge(fFilename, fFileDate);
  fNoDateCheck := false;
  fModified := false;
end;

procedure TCESynMemo.saveToFile(const aFilename: string);
begin
  Lines.SaveToFile(aFilename);
  fFilename := aFilename;
  FileAge(fFilename, fFileDate);
  fNoDateCheck := false;
  fModified := false;
end;

procedure TCESynMemo.save;
begin
  Lines.SaveToFile(fFilename);
  FileAge(fFilename, fFileDate);
  fNoDateCheck := false;
  fModified := false;
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
      [shortenPath(fFilename, 25)])) = mrOk then
    begin
      Lines.LoadFromFile(fFilename);
      fModified := false;
    end
    else fNoDateCheck := true;
  end;
  fFileDate := newDate;
end;

procedure TCESynMemo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  identifierToD2Syn;
end;

procedure TCESynMemo.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  identifierToD2Syn;
end;

procedure TCESynMemo.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:Integer);
begin
  inherited;
  identifierToD2Syn;
end;

initialization
  D2Syn := TSynD2Syn.create(nil);
finalization
  D2Syn.free;
end.
