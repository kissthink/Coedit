unit ce_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEditKeyCmds, Forms, Controls, Graphics,
  Dialogs, Menus, ActnList, ce_common, ce_widget, ce_messages, ce_editor,
  ce_project, ce_synmemo, process;

type

  { TCEMainForm }
  TCEMainForm = class(TForm)
    actCompAndRunFile: TAction;
    actCompileProj: TAction;
    ActCompileAndRunProj: TAction;
    ActCompAndRunFileWithArgs: TAction;
    actNewProj: TAction;
    actOpenProj: TAction;
    actSaveProjAs: TAction;
    actCut: TAction;
    actAddCurrToProj: TAction;
    actNewRunnable: TAction;
    actMacPlay: TAction;
    actMacStartStop: TAction;
    actRedo: TAction;
    actUndo: TAction;
    actPaste: TAction;
    actNewFile: TAction;
    actOpenFile: TAction;
    actSaveFileAs: TAction;
    actSaveFile: TAction;
    actCopy: TAction;
    actSaveProj: TAction;
    Action4: TAction;
    Actions: TActionList;
    imgList: TImageList;
    mainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    procedure actAddCurrToProjExecute(Sender: TObject);
    procedure actCompAndRunFileExecute(Sender: TObject);
    procedure ActCompAndRunFileWithArgsExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure ActionsUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure actMacPlayExecute(Sender: TObject);
    procedure actMacStartStopExecute(Sender: TObject);
    procedure actNewFileExecute(Sender: TObject);
    procedure actNewProjExecute(Sender: TObject);
    procedure actNewRunnableExecute(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
    procedure actOpenProjExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actRedoExecute(Sender: TObject);
    procedure actSaveFileAsExecute(Sender: TObject);
    procedure actSaveFileExecute(Sender: TObject);
    procedure actSaveProjAsExecute(Sender: TObject);
    procedure actSaveProjExecute(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
  private
    fProject: TCEProject;
    fWidgList: TCEWidgetList;
    fMesgWidg: TCEMessagesWidget;
    fEditWidg: TCEEditorWidget;
    fProjWidg: TCEProjectWidget;

    // widget interfaces subroutines
    procedure checkWidgetActions(const aWidget: TCEWidget);

    // run & exec sub routines
    procedure ProcessOutputToMsg(const aProcess: TProcess);
    procedure compileAndRunFile(const edIndex: NativeInt; const runArgs: string = '');

    // file sub routines
    procedure newFile;
    function findFile(const aFilename: string): NativeInt;
    procedure saveFile(const edIndex: NativeInt);
    procedure saveFileAs(const edIndex: NativeInt; const aFilename: string);

    // project sub routines
    procedure projChange(sender: TObject);
    procedure newProj;
    procedure saveProj;
    procedure saveProjAs(const aFilename: string);
    procedure openProj(const aFilename: string);
    procedure closeProj;
    procedure addSource(const aFilename: string);

  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    procedure openFile(const aFilename: string);
    //
    property WidgetList: TCEWidgetList read fWidgList;
    property MessageWidget: TCEMessagesWidget read fMesgWidg;
    property EditWidget: TCEEditorWidget read fEditWidg;
    property ProjectWidget: TCEProjectWidget read fProjWidg;
  end;

var
  mainForm: TCEMainForm;

implementation
{$R *.lfm}

uses
  SynMacroRecorder;

{$REGION std comp methods ******************************************************}
constructor TCEMainForm.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  //
  fWidgList := TCEWidgetList.Create;
  fMesgWidg := TCEMessagesWidget.create(nil);
  fEditWidg := TCEEditorWidget.create(nil);
  fProjWidg := TCEProjectWidget.create(nil);

  fWidgList.addWidget(@fMesgWidg);
  fWidgList.addWidget(@fEditWidg);
  fWidgList.addWidget(@fProjWidg);

  checkWidgetActions(fMesgWidg);

  fMesgWidg.Show;
  fEditWidg.Show;
  fProjWidg.Show;

  fProject := TCEProject.Create(self);
  fProject.onChange := @projChange;

end;

destructor TCEMainForm.destroy;
begin
  fWidgList.Free;
  fMesgWidg.Free;
  fEditWidg.Free;
  fProjWidg.Free;
  //
  inherited;
end;

procedure TCEMainForm.ActionsUpdate(AAction: TBasicAction; var Handled: Boolean);
var
  curr: TCESynMemo;
  hasEd: boolean;
begin
  if fEditWidg = nil then exit;
  //
  curr := fEditWidg.currentEditor;
  hasEd := curr <> nil;
  if hasEd then
  begin
    actCopy.Enabled := curr.SelAvail;
    actCut.Enabled := curr.SelAvail;
    actPaste.Enabled := curr.CanPaste;
    actUndo.Enabled := curr.CanUndo;
    actRedo.Enabled := curr.CanRedo;
    actMacPlay.Enabled := true;
    actMacStartStop.Enabled := true;
    //
    actCompAndRunFile.Enabled := true;
    actCompAndRunFileWithArgs.Enabled := true;
    //
    actSaveFile.Enabled := true;
    actSaveFileAs.Enabled := true;
    actAddCurrToProj.Enabled := true;
  end
  else begin
    actCopy.Enabled := false;
    actCut.Enabled := false ;
    actPaste.Enabled := false ;
    actUndo.Enabled := false ;
    actRedo.Enabled := false ;
    actMacPlay.Enabled := false;
    actMacStartStop.Enabled := false;
    //
    actCompAndRunFile.Enabled := false;
    actCompAndRunFileWithArgs.Enabled := false;
    //
    actSaveFile.Enabled := false;
    actSaveFileAs.Enabled := false;
    actAddCurrToProj.Enabled := false;
  end;

end;

procedure TCEMainForm.checkWidgetActions(const aWidget: TCEWidget);
var
  tlt: string;
  cnt, i: NativeInt;
  prt, itm: TMenuItem;
begin
  tlt := aWidget.contextName;
  if tlt = '' then exit;
  cnt := aWidget.contextActionCount;
  if cnt = 0 then exit;
  //
  prt := TMenuItem.Create(self);
  prt.Caption := tlt;
  mainMenu.Items.Add(prt);
  for i := 0 to cnt-1 do
  begin
    itm := TMenuItem.Create(self);
    itm.Action := aWidget.contextAction(i);
    prt.Add(itm);
  end;

end;
{$ENDREGION}

{$REGION file ******************************************************************}
procedure TCEMainForm.newFile;
var
  i, j: NativeInt;
  str: string;
begin
  if fEditWidg = nil then exit;
  //
  i := fEditWidg.editorCount;
  fEditWidg.addEditor;
  j := 0;
  while(true) do
  begin
    str := format('<new %d>',[j]);
    if findFile(str) = -1 then break;
    if j >= high(NativeInt) then break;
    j += 1;
  end;
  fEditWidg.editor[i].fileName := str;
  fEditWidg.editor[i].modified := true;
  fEditWidg.PageControl.PageIndex := i;
end;

function TCEMainForm.findFile(const aFilename: string): NativeInt;
var
  i: NativeInt;
begin
  result := -1;
  if fEditWidg = nil then exit;
  for i := 0 to fEditWidg.editorCount-1 do
    if fEditWidg.editor[i].fileName = aFilename then exit(i);
end;

procedure TCEMainForm.openFile(const aFilename: string);
var
  i: NativeInt;
begin
  if fEditWidg = nil then exit;
  //
  i := findFile(aFilename);
  if i > -1 then
  begin
    fEditWidg.PageControl.PageIndex := i;
    exit;
  end;
  i := fEditWidg.editorCount;
  fEditWidg.addEditor;
  fEditWidg.editor[i].Lines.LoadFromFile(aFilename);
  fEditWidg.editor[i].fileName := aFilename;
  fEditWidg.PageControl.PageIndex := i;
end;

procedure TCEMainForm.saveFile(const edIndex: NativeInt);
var
  str: string;
begin
  if fEditWidg = nil then exit;
  if edIndex >= fEditWidg.editorCount then exit;
  //
  str := fEditWidg.editor[edIndex].fileName;
  if str = '' then exit;
  try
    fEditWidg.editor[edIndex].Lines.SaveToFile(str);
  finally
    fEditWidg.editor[edIndex].modified := false;
  end;
end;

procedure TCEMainForm.saveFileAs(const edIndex: NativeInt; const aFilename: string);
begin
  if fEditWidg = nil then exit;
  if edIndex < 0 then exit;
  if edIndex >= fEditWidg.editorCount then exit;
  //
  try
    fEditWidg.editor[edIndex].Lines.SaveToFile(aFilename);
  finally
    fEditWidg.editor[edIndex].fileName := aFilename;
    fEditWidg.editor[edIndex].modified := false;
  end;
end;

procedure TCEMainForm.actOpenFileExecute(Sender: TObject);
begin
  if fEditWidg = nil then exit;
  //
  with TOpenDialog.Create(nil) do
  try
    if execute then
    begin
      openFile(filename);
    end;
  finally
    free;
  end;
end;

procedure TCEMainForm.actNewFileExecute(Sender: TObject);
begin
  newFile;
end;

procedure TCEMainForm.actNewRunnableExecute(Sender: TObject);
begin
  newFile;
  fEditWidg.currentEditor.Text :=
  'module runnable;' + #13#10 +
  '' + #13#10 +
  'import std.stdio;' + #13#10 +
  '' + #13#10 +
  'void main(string args[])' + #13#10 +
  '{' + #13#10 +
  '    writeln("runnable module is just a `toy feature`");' + #13#10 +
  '    writeln;' + #13#10 +
  '    writeln("coedit just saves a temporary d module before compiling it and running it...");' + #13#10 +
  '}' + #13#10;
end;

procedure TCEMainForm.actSaveFileAsExecute(Sender: TObject);
begin
  if fEditWidg = nil then exit;
  if fEditWidg.editorIndex < 0 then exit;
  //
  with TSaveDialog.Create(nil) do
  try
    if execute then
    begin
      saveFileAs(fEditWidg.editorIndex, filename);
    end;
  finally
    free;
  end;
end;

procedure TCEMainForm.actSaveFileExecute(Sender: TObject);
var
  str: string;
begin
  if fEditWidg = nil then exit;
  if fEditWidg.editorIndex < 0 then exit;
  //
  str := fEditWidg.editor[fEditWidg.editorIndex].fileName;
  if fileExists(str) then saveFile(fEditWidg.editorIndex)
  else actSaveFileAs.Execute;
end;

procedure TCEMainForm.actAddCurrToProjExecute(Sender: TObject);
var
  str: string;
begin
  if fEditWidg = nil then exit;
  if fEditWidg.editorIndex < 0 then exit;
  //
  str := fEditWidg.editor[fEditWidg.editorIndex].fileName;
  fProject.addSource(str);
end;
{$ENDREGION}

{$REGION edit ******************************************************************}
procedure TCEMainForm.actCopyExecute(Sender: TObject);
var
  curr: TCESynMemo;
begin
  curr := fEditWidg.currentEditor;
  if assigned(curr) then curr.CopyToClipboard;
end;

procedure TCEMainForm.actCutExecute(Sender: TObject);
var
  curr: TCESynMemo;
begin
  curr := fEditWidg.currentEditor;
  if assigned(curr) then curr.CutToClipboard;
end;

procedure TCEMainForm.actPasteExecute(Sender: TObject);
var
  curr: TCESynMemo;
begin
  curr := fEditWidg.currentEditor;
  if assigned(curr) then curr.PasteFromClipboard;
end;

procedure TCEMainForm.actUndoExecute(Sender: TObject);
var
  curr: TCESynMemo;
begin
  curr := fEditWidg.currentEditor;
  if assigned(curr) then curr.Undo;
end;

procedure TCEMainForm.actRedoExecute(Sender: TObject);
var
  curr: TCESynMemo;
begin
  curr := fEditWidg.currentEditor;
  if assigned(curr) then curr.Redo;
end;

procedure TCEMainForm.actMacPlayExecute(Sender: TObject);
var
  curr: TCESynMemo;
begin
  curr := fEditWidg.currentEditor;
  if assigned(curr) then fEditWidg.macRecorder.PlaybackMacro(curr);
end;

procedure TCEMainForm.actMacStartStopExecute(Sender: TObject);
var
  curr: TCESynMemo;
begin
  curr := fEditWidg.currentEditor;
  if assigned(curr) then
  begin
    if fEditWidg.macRecorder.State = msRecording then
      fEditWidg.macRecorder.Stop
    else fEditWidg.macRecorder.RecordMacro(curr);
  end;
end;


{$ENDREGION}

{$REGION run  ******************************************************************}
procedure TCEMainForm.ProcessOutputToMsg(const aProcess: TProcess);
const
  ioBuffSz = 2048;
var
  str: TMemoryStream;
  lns: TStringList;
  readCnt: LongInt;
  readSz: LongInt;
  msg: string;
begin
  If not (poUsePipes in aProcess.Options) then exit;
  //
  str := TMemorystream.Create;
  lns := TStringList.Create;
  readSz := 0;
  try
    while true do
    begin
      str.SetSize(readSz + ioBuffSz);
      readCnt := aProcess.Output.Read((str.Memory + readSz)^, ioBuffSz);
      if readCnt = 0 then break;
      Inc(readSz, readCnt);
    end;
    Str.SetSize(readSz);
    lns.LoadFromStream(Str);
    for msg in lns do fMesgWidg.addMessage(msg);
  finally
    str.Free;
    lns.Free;
  end;
end;

procedure TCEMainForm.compileAndRunFile(const edIndex: NativeInt; const runArgs: string = '');
var
  dmdproc: TProcess;
  runproc: TProcess;
  fname, temppath: string;
begin
  dmdproc := TProcess.Create(nil);
  runproc := TProcess.Create(nil);
  try
    temppath := GetTempDir;
    {$IFDEF DEBUG}{$WARNINGS OFF}{$HINTS OFF}{$ENDIF}
    fname := temppath + format('temp_%.8x',[LongWord(@dmdproc)]);
    {$IFDEF DEBUG}{$WARNINGS ON}{$HINTS ON}{$ENDIF}
    fEditWidg.editor[edIndex].Lines.SaveToFile(fname + '.d');

    dmdproc.Options:= [poWaitOnExit,poUsePipes,poStdErrToOutput];
    dmdproc.Executable:= 'dmd';
    dmdproc.Parameters.Text := '"'+ fname +'.d"';
    try
      dmdproc.Execute;
      ProcessOutputToMsg(dmdproc);
    finally
      DeleteFile(fname + '.d');
    end;

    if dmdProc.ExitStatus = 0 then
    begin
      runproc.Options:= [poWaitOnExit,poStderrToOutPut,poUsePipes];
      {$IFDEF MSWINDOWS}
      runproc.Executable := fname + '.exe';
      runproc.Parameters.Text := runArgs;
      {$ELSE}
      runproc.Executable := fname;
      {$ENDIF}
      runproc.Execute;
      ProcessOutputToMsg(runproc);
      {$IFDEF MSWINDOWS}
      DeleteFile(fname + '.exe');
      DeleteFile(fname + '.obj');
      {$ELSE}
      DeleteFile(fname);
      DeleteFile(fname + '.o');
      {$ENDIF}
    end;

  finally
    dmdproc.Free;
    runproc.Free;
  end;
end;

procedure TCEMainForm.actCompAndRunFileExecute(Sender: TObject);
begin
  if fEditWidg = nil then exit;
  if fEditWidg.editorIndex < 0 then exit;
  //
  compileAndRunFile(fEditWidg.editorIndex);
end;

procedure TCEMainForm.ActCompAndRunFileWithArgsExecute(Sender: TObject);
var
  runargs: string;
begin
  if fEditWidg = nil then exit;
  if fEditWidg.editorIndex < 0 then exit;
  //
  runargs := '';
  if InputQuery('Execution arguments', 'enter switches and arguments',
    runargs) then compileAndRunFile(fEditWidg.editorIndex, runargs);
end;
{$ENDREGION}

{$REGION view ******************************************************************}
{$ENDREGION}

{$REGION project ***************************************************************}
procedure TCEMainForm.projChange(sender: TObject);
var
  i: NativeInt;
begin
  for i:= 0 to WidgetList.Count-1 do
    widgetList.widget[i].projChange(fProject);
end;

procedure TCEMainForm.newProj;
begin
  fProject.reset;
end;

procedure TCEMainForm.saveProj;
begin
  saveCompToTxtFile(fProject, fProject.fileName);
end;

procedure TCEMainForm.saveProjAs(const aFilename: string);
begin
  fProject.fileName := aFilename;
  saveCompToTxtFile(fProject, aFilename);
end;

procedure TCEMainForm.openProj(const aFilename: string);
begin
  newProj;
  fProject.fileName := aFilename;
  loadCompFromTxtFile(fProject, aFilename);
end;

procedure TCEMainForm.closeProj;
begin
  newProj;
end;

procedure TCEMainForm.actNewProjExecute(Sender: TObject);
begin
  closeProj;
end;

procedure TCEMainForm.addSource(const aFilename: string);
begin
  if fProject.Sources.IndexOf(aFilename) >= 0 then exit;
  fProject.addSource(aFilename);
end;

procedure TCEMainForm.actSaveProjAsExecute(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do
  try
    if execute then saveProjAs(filename);
  finally
    Free;
  end;
end;

procedure TCEMainForm.actSaveProjExecute(Sender: TObject);
begin
  if fProject.fileName <> '' then saveProj
  else actSaveProjAs.Execute;
end;

procedure TCEMainForm.actOpenProjExecute(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    if execute then openProj(filename);
  finally
    Free;
  end;
end;
{$ENDREGION}
end.
