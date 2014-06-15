unit ce_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEditKeyCmds, Forms, Controls, Graphics,
  Dialogs, Menus, ActnList, ce_common, ce_widget, ce_messages, ce_editor,
  ce_project, ce_synmemo, ce_projconf, process, ce_dmdwrap;

type

  { TCEMainForm }
  TCEMainForm = class(TForm)
    actFileCompAndRun: TAction;
    actFileSaveAll: TAction;
    actFileClose: TAction;
    actFileAddToProj: TAction;
    actFileNewRun: TAction;
    actFileNew: TAction;
    actFileOpen: TAction;
    actFileSaveAs: TAction;
    actFileSave: TAction;
    actProjCompile: TAction;
    actProjCompileAndRun: TAction;
    ActFileCompAndRunWithArgs: TAction;
    actProjCompAndRunWithArgs: TAction;
    actProjClose: TAction;
    actProjOpts: TAction;
    actProjNew: TAction;
    actProjOpen: TAction;
    actProjSave: TAction;
    actProjSaveAs: TAction;
    actMacPlay: TAction;
    actMacStartStop: TAction;
    actEdCut: TAction;
    actEdRedo: TAction;
    actEdUndo: TAction;
    actEdPaste: TAction;
    actEdCopy: TAction;
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
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    mnuItemWin: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    procedure actFileAddToProjExecute(Sender: TObject);
    procedure actFileCloseExecute(Sender: TObject);
    procedure actFileCompAndRunExecute(Sender: TObject);
    procedure ActFileCompAndRunWithArgsExecute(Sender: TObject);
    procedure actFileSaveAllExecute(Sender: TObject);
    procedure actProjCompileExecute(Sender: TObject);
    procedure actEdCopyExecute(Sender: TObject);
    procedure actEdCutExecute(Sender: TObject);
    procedure ActionsUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure actMacPlayExecute(Sender: TObject);
    procedure actMacStartStopExecute(Sender: TObject);
    procedure actFileNewExecute(Sender: TObject);
    procedure actProjNewExecute(Sender: TObject);
    procedure actFileNewRunExecute(Sender: TObject);
    procedure actFileOpenExecute(Sender: TObject);
    procedure actProjOpenExecute(Sender: TObject);
    procedure actEdPasteExecute(Sender: TObject);
    procedure actProjCloseExecute(Sender: TObject);
    procedure actProjOptsExecute(Sender: TObject);
    procedure actEdRedoExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actProjSaveAsExecute(Sender: TObject);
    procedure actProjSaveExecute(Sender: TObject);
    procedure actEdUndoExecute(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
  private
    fProject: TCEProject;
    fWidgList: TCEWidgetList;
    fMesgWidg: TCEMessagesWidget;
    fEditWidg: TCEEditorWidget;
    fProjWidg: TCEProjectWidget;
    fPrjCfWidg: TCEProjectConfigurationWidget;

    // widget interfaces subroutines
    procedure checkWidgetActions(const aWidget: TCEWidget);
    procedure widgetShowFromAction(sender: TObject);

    // run & exec sub routines
    procedure ProcessOutputToMsg(const aProcess: TProcess);
    procedure compileAndRunFile(const edIndex: NativeInt; const runArgs: string = '');
    procedure compileProject(const aProject: TCEProject);
    procedure runProject(const aProject: TCEProject);

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
var
  act: TAction;
  itm: TMenuItem;
  widg: TCEWidget;
begin
  inherited create(aOwner);
  //
  fWidgList := TCEWidgetList.Create;
  fMesgWidg := TCEMessagesWidget.create(nil);
  fEditWidg := TCEEditorWidget.create(nil);
  fProjWidg := TCEProjectWidget.create(nil);
  fPrjCfWidg:= TCEProjectConfigurationWidget.create(nil);

  fWidgList.addWidget(@fMesgWidg);
  fWidgList.addWidget(@fEditWidg);
  fWidgList.addWidget(@fProjWidg);
  fWidgList.addWidget(@fPrjCfWidg);

  for widg in fWidgList do widg.Show;

  for widg in fWidgList do
  begin
    act := TAction.Create(self);
    act.Category := 'Window';
    act.Caption := widg.Caption;
    act.OnExecute := @widgetShowFromAction;
    act.Tag := ptrInt(widg);
    act.ImageIndex := 25;
    itm := TMenuItem.Create(self);
    itm.Action := act;
    itm.Tag := ptrInt(widg);
    mnuItemWin.Add(itm);
  end;

  newProj;

end;

destructor TCEMainForm.destroy;
begin
  fWidgList.Free;
  fMesgWidg.Free;
  fEditWidg.Free;
  fProjWidg.Free;
  fPrjCfWidg.Free;
  fProject.Free;
  //
  inherited;
end;

procedure TCEMainForm.ActionsUpdate(AAction: TBasicAction; var Handled: Boolean);
var
  curr: TCESynMemo;
  hasEd: boolean;
  hasProj: boolean;
begin
  if fEditWidg = nil then exit;
  //
  curr := fEditWidg.currentEditor;
  hasEd := curr <> nil;
  if hasEd then
  begin
    actEdCopy.Enabled := curr.SelAvail;
    actEdCut.Enabled := curr.SelAvail;
    actEdPaste.Enabled := curr.CanPaste;
    actEdUndo.Enabled := curr.CanUndo;
    actEdRedo.Enabled := curr.CanRedo;
    actMacPlay.Enabled := true;
    actMacStartStop.Enabled := true;
    //
    actFileCompAndRun.Enabled := true;
    actFileCompAndRunWithArgs.Enabled := true;
    actFileSave.Enabled := true;
    actFileSaveAs.Enabled := true;
    actFileClose.Enabled:=true;
    actFileSaveAll.Enabled:=true;
  end
  else begin
    actEdCopy.Enabled := false;
    actEdCut.Enabled := false ;
    actEdPaste.Enabled := false ;
    actEdUndo.Enabled := false ;
    actEdRedo.Enabled := false ;
    actMacPlay.Enabled := false;
    actMacStartStop.Enabled := false;
    //
    actFileCompAndRun.Enabled := false;
    actFileCompAndRunWithArgs.Enabled := false;
    actFileSave.Enabled := false;
    actFileSaveAs.Enabled := false;
    actFileClose.Enabled := false;
    actFileSaveAll.Enabled := false;
  end;

  hasProj := fProject <> nil;
  actProjSave.Enabled := hasProj;
  actProjSaveAs.Enabled := hasProj;
  actProjOpts.Enabled := hasProj;
  actProjClose.Enabled := hasProj;
  actProjCompile.Enabled := hasProj;
  actProjCompileAndRun.Enabled := hasProj;
  actProjCompAndRunWithArgs.Enabled := hasProj;

  actFileAddToProj.Enabled := hasEd and hasProj;

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

procedure TCEMainForm.actFileOpenExecute(Sender: TObject);
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

procedure TCEMainForm.actFileNewExecute(Sender: TObject);
begin
  newFile;
end;

procedure TCEMainForm.actFileNewRunExecute(Sender: TObject);
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

procedure TCEMainForm.actFileSaveAsExecute(Sender: TObject);
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

procedure TCEMainForm.actFileSaveExecute(Sender: TObject);
var
  str: string;
begin
  if fEditWidg = nil then exit;
  if fEditWidg.editorIndex < 0 then exit;
  //
  str := fEditWidg.editor[fEditWidg.editorIndex].fileName;
  if fileExists(str) then saveFile(fEditWidg.editorIndex)
  else actFileSaveAs.Execute;
end;

procedure TCEMainForm.actFileAddToProjExecute(Sender: TObject);
var
  str: string;
begin
  if fEditWidg = nil then exit;
  if fEditWidg.editorIndex < 0 then exit;
  //
  str := fEditWidg.editor[fEditWidg.editorIndex].fileName;
  fProject.addSource(str);
end;

procedure TCEMainForm.actFileCloseExecute(Sender: TObject);
begin
  fEditWidg.removeEditor(fEditWidg.editorIndex);
end;

procedure TCEMainForm.actFileSaveAllExecute(Sender: TObject);
var
  i: NativeInt;
begin
  for i:= 0 to fEditWidg.editorCount-1 do saveFile(i);
end;

procedure TCEMainForm.FormDropFiles(Sender: TObject;const FileNames: array of String);
var
  fname: string;
begin
  for fname in FileNames do
    openFile(fname);
end;
{$ENDREGION}

{$REGION edit ******************************************************************}
procedure TCEMainForm.actEdCopyExecute(Sender: TObject);
var
  curr: TCESynMemo;
begin
  curr := fEditWidg.currentEditor;
  if assigned(curr) then curr.CopyToClipboard;
end;

procedure TCEMainForm.actEdCutExecute(Sender: TObject);
var
  curr: TCESynMemo;
begin
  curr := fEditWidg.currentEditor;
  if assigned(curr) then curr.CutToClipboard;
end;

procedure TCEMainForm.actEdPasteExecute(Sender: TObject);
var
  curr: TCESynMemo;
begin
  curr := fEditWidg.currentEditor;
  if assigned(curr) then curr.PasteFromClipboard;
end;

procedure TCEMainForm.actEdUndoExecute(Sender: TObject);
var
  curr: TCESynMemo;
begin
  curr := fEditWidg.currentEditor;
  if assigned(curr) then curr.Undo;
end;

procedure TCEMainForm.actEdRedoExecute(Sender: TObject);
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
var
  str: TMemoryStream;
  lns: TStringList;
  readCnt: LongInt;
  readSz: LongInt;
  ioBuffSz: LongInt;
  msg: string;
begin
  If not (poUsePipes in aProcess.Options) then exit;
  //
  ioBuffSz := aProcess.PipeBufferSize;
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
    fMesgWidg.scrollToBack;
  end;
end;

procedure TCEMainForm.compileAndRunFile(const edIndex: NativeInt; const runArgs: string = '');
var
  dmdproc: TProcess;
  runproc: TProcess;
  fname, temppath, olddir: string;
begin
  olddir  := '';
  dmdproc := TProcess.Create(nil);
  runproc := TProcess.Create(nil);
  getDir(0, olddir);
  try

    fMesgWidg.addCeInf( 'compiling ' + fEditWidg.editor[edIndex].fileName );

    temppath := GetTempDir(false);
    chDir(temppath);
    {$IFDEF DEBUG}{$WARNINGS OFF}{$HINTS OFF}{$ENDIF}
    fname := temppath + format('temp_%.8x', [LongWord(@dmdproc)]);
    {$IFDEF DEBUG}{$WARNINGS ON}{$HINTS ON}{$ENDIF}
    fEditWidg.editor[edIndex].Lines.SaveToFile(fname + '.d');

    dmdproc.Options:= [poWaitOnExit, poStdErrToOutput, poUsePipes];
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

      fMesgWidg.addCeInf( fEditWidg.editor[edIndex].fileName
        + ' successfully compiled' );

      runproc.Options:= [poWaitOnExit, poStderrToOutPut, poUsePipes];
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
    end
    else
      fMesgWidg.addCeErr( fEditWidg.editor[edIndex].fileName
        + ' has not been compiled' );

  finally
    dmdproc.Free;
    runproc.Free;
    chDir(olddir);
  end;
end;

procedure TCEMainForm.compileProject(const aProject: TCEProject);
var
  dmdproc: TProcess;
  olddir, prjpath: string;
const
  // option -v causes an hang if poWaitOnExit is included
  procopts: array[boolean] of TProcessOptions = (
    [poWaitOnExit, poStdErrToOutput, poUsePipes],
    [poStdErrToOutput, poUsePipes]
  );
begin
  olddir := '';
  dmdproc := TProcess.Create(nil);
  getDir(0, olddir);
  try

    fMesgWidg.addCeInf( 'compiling ' + aProject.fileName );

    prjpath := extractFilePath(aProject.fileName);
    if directoryExists(prjpath) then chDir(prjpath);

    dmdproc.Options :=
      procopts[aProject.currentConfiguration.messagesOptions.verbose];

    dmdproc.Executable := 'dmd';
    dmdproc.Parameters.Text := aProject.getOpts;
    try
      dmdproc.Execute;
      ProcessOutputToMsg(dmdproc);
    finally
      if dmdProc.ExitStatus = 0 then
        fMesgWidg.addCeInf( aProject.fileName
          + ' successfully compiled' )
      else
        fMesgWidg.addCeErr( aProject.fileName
          + ' has not been compiled' );
    end;

  finally
    dmdproc.Free;
    chDir(olddir);
  end;
end;

procedure TCEMainForm.runProject(const aProject: TCEProject);
begin
  if aProject.currentConfiguration.outputOptions.binaryKind <>
    executable then exit;
end;

procedure TCEMainForm.actFileCompAndRunExecute(Sender: TObject);
begin
  if fEditWidg = nil then exit;
  if fEditWidg.editorIndex < 0 then exit;
  //
  compileAndRunFile(fEditWidg.editorIndex);
end;

procedure TCEMainForm.ActFileCompAndRunWithArgsExecute(Sender: TObject);
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

procedure TCEMainForm.actProjCompileExecute(Sender: TObject);
begin
  compileProject(fProject);
end;
{$ENDREGION}

{$REGION view ******************************************************************}
procedure TCEMainForm.widgetShowFromAction(sender: TObject);
begin
  TCEWidget( TComponent(sender).tag ).Show;
end;

{$ENDREGION}

{$REGION project ***************************************************************}
procedure TCEMainForm.projChange(sender: TObject);
var
  widg: TCEWidget;
begin
  for widg in WidgetList do
    widg.projChange(fProject);
end;

procedure TCEMainForm.closeProj;
var
  widg: TCEWidget;
begin
  for widg in WidgetList do widg.projClose(fProject);
  fProject.Free;
  fProject := nil;
end;

procedure TCEMainForm.newProj;
var
  widg: TCEWidget;
begin
  fProject := TCEProject.Create(self);
  for widg in WidgetList do widg.projNew(fProject);
  fProject.onChange := @projChange;
  fProject.beforeChanged;
  fProject.afterChanged;
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
  closeProj;
  newProj;
  fProject.beforeChanged;
  fProject.fileName := aFilename;
  loadCompFromTxtFile(fProject, aFilename);
  fProject.afterChanged;
end;

procedure TCEMainForm.actProjNewExecute(Sender: TObject);
begin
  closeProj;
  newProj;
end;

procedure TCEMainForm.actProjCloseExecute(Sender: TObject);
begin
  closeProj;
end;

procedure TCEMainForm.addSource(const aFilename: string);
begin
  if fProject.Sources.IndexOf(aFilename) >= 0 then exit;
  fProject.addSource(aFilename);
end;

procedure TCEMainForm.actProjSaveAsExecute(Sender: TObject);
begin
  with TSaveDialog.Create(nil) do
  try
    if execute then saveProjAs(filename);
  finally
    Free;
  end;
end;

procedure TCEMainForm.actProjSaveExecute(Sender: TObject);
begin
  if fProject.fileName <> '' then saveProj
  else actProjSaveAs.Execute;
end;

procedure TCEMainForm.actProjOpenExecute(Sender: TObject);
begin
  with TOpenDialog.Create(nil) do
  try
    if execute then openProj(filename);
  finally
    Free;
  end;
end;

procedure TCEMainForm.actProjOptsExecute(Sender: TObject);
begin
  fPrjCfWidg.Show;
end;
{$ENDREGION}
end.
