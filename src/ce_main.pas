unit ce_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEditKeyCmds, SynHighlighterLFM, Forms,
  AnchorDocking, AnchorDockStorage, AnchorDockOptionsDlg, Controls, Graphics,
  Dialogs, Menus, ActnList, ExtCtrls, ComCtrls, process,
  ce_common, ce_dmdwrap, ce_project, ce_synmemo, ce_widget, ce_messages,
  ce_editor, ce_projinspect, ce_projconf, ce_staticexplorer;

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
    actFileCompAndRunWithArgs: TAction;
    actProjSource: TAction;
    actProjRun: TAction;
    actProjRunWithArgs: TAction;
    actProjCompile: TAction;
    actProjCompileAndRun: TAction;
    actProjCompAndRunWithArgs: TAction;
    actProjClose: TAction;
    actProjOpts: TAction;
    actProjNew: TAction;
    actProjOpen: TAction;
    actProjSave: TAction;
    actProjSaveAs: TAction;
    actEdMacPlay: TAction;
    actEdMacStartStop: TAction;
    actEdCut: TAction;
    actEdRedo: TAction;
    actEdUndo: TAction;
    actEdPaste: TAction;
    actEdCopy: TAction;
    Actions: TActionList;
    ApplicationProperties1: TApplicationProperties;
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
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem51: TMenuItem;
    mnuItemWin: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    LfmSyn: TSynLFMSyn;
    procedure actFileAddToProjExecute(Sender: TObject);
    procedure actFileCloseExecute(Sender: TObject);
    procedure actFileCompAndRunExecute(Sender: TObject);
    procedure actFileCompAndRunWithArgsExecute(Sender: TObject);
    procedure actFileSaveAllExecute(Sender: TObject);
    procedure actProjCompAndRunWithArgsExecute(Sender: TObject);
    procedure actProjCompileAndRunExecute(Sender: TObject);
    procedure actProjCompileExecute(Sender: TObject);
    procedure actEdCopyExecute(Sender: TObject);
    procedure actEdCutExecute(Sender: TObject);
    procedure ActionsUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure actEdMacPlayExecute(Sender: TObject);
    procedure actEdMacStartStopExecute(Sender: TObject);
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
    procedure actProjRunExecute(Sender: TObject);
    procedure actProjRunWithArgsExecute(Sender: TObject);
    procedure actProjSaveAsExecute(Sender: TObject);
    procedure actProjSaveExecute(Sender: TObject);
    procedure actEdUndoExecute(Sender: TObject);
    procedure actProjSourceExecute(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
  private
    fUpdateCount: NativeInt;
    fProject: TCEProject;
    fWidgList: TCEWidgetList;
    fMesgWidg: TCEMessagesWidget;
    fEditWidg: TCEEditorWidget;
    fProjWidg: TCEProjectInspectWidget;
    fPrjCfWidg: TCEProjectConfigurationWidget;
    fStExpWidg: TCEStaticExplorerWidget;

    // widget interfaces subroutines
    procedure checkWidgetActions(const aWidget: TCEWidget);
    procedure widgetShowFromAction(sender: TObject);

    // run & exec sub routines
    procedure ProcessOutputToMsg(const aProcess: TProcess);
    procedure compileAndRunFile(const edIndex: NativeInt; const runArgs: string = '');
    procedure compileProject(const aProject: TCEProject);
    procedure runProject(const aProject: TCEProject; const runArgs: string = '');

    // file sub routines
    procedure newFile;
    function findFile(const aFilename: string): NativeInt;
    procedure saveFile(const edIndex: NativeInt);
    procedure saveFileAs(const edIndex: NativeInt; const aFilename: string);

    // project sub routines
    procedure saveProjSource(const aEditor: TCESynMemo);
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
    procedure docChangeNotify(Sender: TObject; const aIndex: Integer);
    procedure docFocusedNotify(Sender: TObject; const aIndex: Integer);
    //
    property WidgetList: TCEWidgetList read fWidgList;
    property MessageWidget: TCEMessagesWidget read fMesgWidg;
    property EditWidget: TCEEditorWidget read fEditWidg;
    property ProjectWidget: TCEProjectInspectWidget read fProjWidg;
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
  fProjWidg := TCEProjectInspectWidget.create(nil);
  fPrjCfWidg:= TCEProjectConfigurationWidget.create(nil);
  fStExpWidg:= TCEStaticExplorerWidget.create(nil);

  fWidgList.addWidget(@fMesgWidg);
  fWidgList.addWidget(@fEditWidg);
  fWidgList.addWidget(@fProjWidg);
  fWidgList.addWidget(@fPrjCfWidg);
  fWidgList.addWidget(@fStExpWidg);

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

  Height := 0;
  DockMaster.MakeDockSite(Self, [akBottom], admrpChild, false);
  DockMaster.OnShowOptions := @ShowAnchorDockOptions;
  DockMaster.HeaderStyle := adhsPoints;
  DockMaster.ManualDock(DockMaster.GetAnchorSite(fEditWidg), Self, alBottom);
  DockMaster.ManualDock(DockMaster.GetAnchorSite(fMesgWidg), Self, alBottom);
  width := width - fProjWidg.Width;
  DockMaster.ManualDock(DockMaster.GetAnchorSite(fProjWidg), Self, alRight);

  DockMaster.GetAnchorSite(fEditWidg).Header.HeaderPosition := adlhpTop;
end;

destructor TCEMainForm.destroy;
begin
  fWidgList.Free;
  fMesgWidg.Free;
  fEditWidg.Free;
  fProjWidg.Free;
  fPrjCfWidg.Free;
  fStExpWidg.Free;
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
  if fUpdateCount > 0 then exit;
  Inc(fUpdateCount);
  try
    curr := fEditWidg.currentEditor;
    hasEd := curr <> nil;
    if hasEd then
    begin
      actEdCopy.Enabled := curr.SelAvail;
      actEdCut.Enabled := curr.SelAvail;
      actEdPaste.Enabled := curr.CanPaste;
      {$IFDEF MSWINDOWS}
      // close file : raises a segfault on linux UndoStuff.>>fList<<.Count...
      actEdUndo.Enabled := curr.CanUndo;
      actEdRedo.Enabled := curr.CanRedo;
      {$ENDIF}
      actEdMacPlay.Enabled := true;
      actEdMacStartStop.Enabled := true;
      //
      actFileCompAndRun.Enabled := true;
      actFileCompAndRunWithArgs.Enabled := true;
      actFileSave.Enabled := true;
      actFileSaveAs.Enabled := true;
      actFileClose.Enabled := true;
      actFileSaveAll.Enabled := true;
    end
    else begin
      actEdCopy.Enabled := false;
      actEdCut.Enabled := false ;
      actEdPaste.Enabled := false;
      {$IFDEF MSWINDOWS}
      actEdUndo.Enabled := false;
      actEdRedo.Enabled := false;
      {$ENDIF}
      actEdMacPlay.Enabled := false;
      actEdMacStartStop.Enabled := false;
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
    actProjRun.Enabled := hasProj;
    actProjRunWithArgs.Enabled := hasProj;
    actProjSource.Enabled := hasProj;

    actFileAddToProj.Enabled := hasEd and hasProj;

  finally
    Dec(fUpdateCount);
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

procedure TCEMainForm.FormShow(Sender: TObject);
begin
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
  fEditWidg.editor[i].modified := false;
  fEditWidg.focusedEditorChanged;
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
  fEditWidg.focusedEditorChanged;
end;

procedure TCEMainForm.saveFile(const edIndex: NativeInt);
var
  str: string;
  i: NativeInt;
begin
  if fEditWidg = nil then exit;
  if edIndex >= fEditWidg.editorCount then exit;
  //
  if fEditWidg.editor[edIndex].Highlighter = LfmSyn then
  begin
    saveProjSource(fEditWidg.editor[edIndex]);
    exit;
  end;
  //
  str := fEditWidg.editor[edIndex].fileName;
  if str = '' then exit;
  try
    fEditWidg.editor[edIndex].Lines.SaveToFile(str);
  finally
    fEditWidg.editor[edIndex].modified := false;
  end;
  //
  for i := 0 to fWidgList.Count-1 do
    fWidgList.widget[i].docChanged(fEditWidg.editor[edIndex]);
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

procedure TCEMainForm.docChangeNotify(Sender: TObject; const aIndex: Integer);
var
  i: NativeInt;
begin
  for i := 0 to fWidgList.Count-1 do
    if fWidgList.widget[i] <> Sender then
      fWidgList.widget[i].docChanged(fEditWidg.editor[aIndex]);
end;

procedure TCEMainForm.docFocusedNotify(Sender: TObject; const aIndex: Integer);
var
  i: NativeInt;
begin
  for i := 0 to fWidgList.Count-1 do
    if fWidgList.widget[i] <> Sender then
      fWidgList.widget[i].docFocused(fEditWidg.editor[aIndex]);
end;

procedure TCEMainForm.actFileOpenExecute(Sender: TObject);
begin
  if fEditWidg = nil then exit;
  //
  with TOpenDialog.Create(nil) do
  try
    filter := 'D source|*.d|D interface|*.di|all files|*.*';
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
  if fEditWidg.editor[fEditWidg.editorIndex].Highlighter = LfmSyn
    then exit;
  //
  str := fEditWidg.editor[fEditWidg.editorIndex].fileName;
  fProject.addSource(str);
end;

procedure TCEMainForm.actFileCloseExecute(Sender: TObject);
var
  curr: TCESynMemo;
  i: NativeInt;
begin
  curr := fEditWidg.currentEditor;
  if curr.modified then if dlgOkCancel(
      'The latest mdofifications are not saved, continue ?') = mrCancel
      then exit;
  //
  for i := 0 to fWidgList.Count-1 do
    fWidgList.widget[i].docClose(fEditWidg.editor[fEditWidg.editorIndex]);
  //
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

procedure TCEMainForm.actEdMacPlayExecute(Sender: TObject);
var
  curr: TCESynMemo;
begin
  curr := fEditWidg.currentEditor;
  if assigned(curr) then fEditWidg.macRecorder.PlaybackMacro(curr);
end;

procedure TCEMainForm.actEdMacStartStopExecute(Sender: TObject);
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

// TODO: input handling
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
    fname := temppath + format('temp_%.8x', [NativeUInt(@dmdproc)]);
    {$IFDEF DEBUG}{$WARNINGS ON}{$HINTS ON}{$ENDIF}
    fEditWidg.editor[edIndex].Lines.SaveToFile(fname + '.d');

    {$IFDEF RELEASE}
    dmdProc.ShowWindow := swoHIDE;
    {$ENDIF}
    dmdproc.Options:= [poWaitOnExit, poStdErrToOutput, poUsePipes];
    dmdproc.Executable:= 'dmd';
    dmdproc.Parameters.Add(fname + '.d');
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
      repeat
        ProcessOutputToMsg(runproc);
      until
        not runproc.Active;
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
  // option -v causes a hang if poWaitOnExit is included
  procopts: array[boolean] of TProcessOptions = (
    [poWaitOnExit, poStdErrToOutput, poUsePipes],
    [poStdErrToOutput, poUsePipes]
  );
begin

  if aProject.Sources.Count = 0 then
  begin
    fMesgWidg.addCeErr( aProject.fileName + ' has no source files' );
    exit;
  end;

  olddir := '';
  dmdproc := TProcess.Create(nil);
  getDir(0, olddir);
  try

    fMesgWidg.addCeInf( 'compiling ' + aProject.fileName );

    prjpath := extractFilePath(aProject.fileName);
    if directoryExists(prjpath) then chDir(prjpath);

    {$IFDEF RELEASE}
    dmdProc.ShowWindow := swoHIDE;
    {$ENDIF}
    dmdproc.Options :=
      procopts[aProject.currentConfiguration.messagesOptions.verbose];

    dmdproc.Executable := 'dmd';
    aProject.getOpts(dmdproc.Parameters);
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

procedure TCEMainForm.runProject(const aProject: TCEProject; const runArgs: string = '');
var
  runproc: TProcess;
  procname: string;
begin
  if aProject.currentConfiguration.outputOptions.binaryKind <>
    executable then exit;

  runproc := TProcess.Create(nil);
  try
    runproc.Options := [poNewConsole, poStdErrToOutput];

    procname := aProject.currentConfiguration.pathsOptions.outputFilename;
    if procname <> '' then procname := aProject.getAbsoluteFilename(procname)
    else if aProject.Sources.Count > 0 then
    begin
      procname := extractFilename(aProject.Sources.Strings[0]);
      procname := procname[1..length(procname)-2];
      procname := extractFilePath(aProject.fileName) +
        DirectorySeparator + procname;
      {$IFDEF MSWINDOWS}
      procname += '.exe';
      {$ENDIF}
    end;

    if not fileExists(procname) then
    begin
      fMesgWidg.addCeErr('output executable missing: ' + procname);
      exit;
    end;

    runproc.Executable := procname;
    runproc.Parameters.Text := runArgs;
    runproc.Execute;

  finally
    runproc.Free;
  end;

end;

procedure TCEMainForm.actFileCompAndRunExecute(Sender: TObject);
begin
  if fEditWidg = nil then exit;
  if fEditWidg.editorIndex < 0 then exit;
  //
  compileAndRunFile(fEditWidg.editorIndex);
end;

procedure TCEMainForm.actFileCompAndRunWithArgsExecute(Sender: TObject);
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

procedure TCEMainForm.actProjCompileAndRunExecute(Sender: TObject);
begin
  compileProject(fProject);
  runProject(fProject);
end;

procedure TCEMainForm.actProjCompAndRunWithArgsExecute(Sender: TObject);
var
  runargs: string;
begin
  compileProject(fProject);
  //
  runargs := '';
  if InputQuery('Execution arguments', 'enter switches and arguments',
    runargs) then runProject(fProject, runargs);
end;

procedure TCEMainForm.actProjRunExecute(Sender: TObject);
begin
  runProject(fProject);
end;

procedure TCEMainForm.actProjRunWithArgsExecute(Sender: TObject);
var
  runargs: string;
begin
  runargs := '';
  if InputQuery('Execution arguments', 'enter switches and arguments',
    runargs) then runProject(fProject, runargs);
end;
{$ENDREGION}

{$REGION view ******************************************************************}
procedure TCEMainForm.widgetShowFromAction(sender: TObject);
var
  widg: TCEWidget;
  win: TControl;
begin
  widg := TCEWidget( TComponent(sender).tag );
  if widg = nil then exit;
  win := DockMaster.GetAnchorSite(widg);
  if win = nil then exit;
  win.Show;
  win.BringToFront;
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

procedure TCEMainForm.saveProjSource(const aEditor: TCESynMemo);
begin
  if fProject = nil then exit;
  if fProject.fileName <> aEditor.fileName then exit;
  //
  aEditor.modified := false;
  aEditor.Lines.SaveToFile(fProject.fileName);
  self.openProj(fProject.fileName);
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
end;

procedure TCEMainForm.saveProj;
begin
  fProject.saveToFile(fProject.fileName);
end;

procedure TCEMainForm.saveProjAs(const aFilename: string);
begin
  fProject.fileName := aFilename;
  fProject.saveToFile(fProject.fileName);
end;

procedure TCEMainForm.openProj(const aFilename: string);
begin
  closeProj;
  newProj;
  fProject.loadFromFile(aFilename);
end;

procedure TCEMainForm.actProjNewExecute(Sender: TObject);
begin
  if fProject <> nil then if fProject.modified then if dlgOkCancel(
    'The latest mdofifications are not saved, continue ?')
      = mrCancel then exit;

  closeProj;
  newProj;
end;

procedure TCEMainForm.actProjCloseExecute(Sender: TObject);
begin
  if fProject = nil then exit;
  if fProject.modified then if dlgOkCancel(
    'The latest mdofifications are not saved, continue ?')
      = mrCancel then exit;

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
  if fProject <> nil then if fProject.modified then if dlgOkCancel(
    'The latest mdofifications are not saved, continue ?')
      = mrCancel then exit;

  with TOpenDialog.Create(nil) do
  try
    if execute then openProj(filename);
  finally
    Free;
  end;
end;

procedure TCEMainForm.actProjOptsExecute(Sender: TObject);
var
  win: TControl;
begin
  win := DockMaster.GetAnchorSite(fPrjCfWidg);
  if win = nil then exit;
  win.Show;
  win.BringToFront;
end;

procedure TCEMainForm.actProjSourceExecute(Sender: TObject);
begin
  if fProject = nil then exit;
  if not fileExists(fProject.fileName) then exit;
  //
  openFile(fProject.fileName);
  EditWidget.currentEditor.Highlighter := LfmSyn;
end;
{$ENDREGION}
end.
