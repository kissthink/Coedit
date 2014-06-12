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
    Action1: TAction;
    actCut: TAction;
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
    Action3: TAction;
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
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    procedure actCompAndRunFileExecute(Sender: TObject);
    procedure ActCompAndRunFileWithArgsExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure ActionsUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure actMacPlayExecute(Sender: TObject);
    procedure actMacStartStopExecute(Sender: TObject);
    procedure actNewFileExecute(Sender: TObject);
    procedure actNewRunnableExecute(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actRedoExecute(Sender: TObject);
    procedure actSaveFileAsExecute(Sender: TObject);
    procedure actSaveFileExecute(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
  private
    fProject: TCEProject;
    fWidgList: TCEWidgetList;
    fMesgWidg: TCEMessagesWidget;
    fEditWidg: TCEEditorWidget;
    fProjWidg: TCEProjectWidget;
    //
    procedure ProcessOutputToMsg(const aProcess: TProcess);
    //
    procedure newFile;
    function findFile(const aFilename: string): NativeInt;
    procedure openFile(const aFilename: string);
    procedure saveFile(const edIndex: NativeInt);
    procedure saveFileAs(const edIndex: NativeInt; const aFilename: string);
    //
    procedure compileAndRunFile(const edIndex: NativeInt; const runArgs: string = '');
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
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

  fMesgWidg.Show;
  fEditWidg.Show;
  fProjWidg.Show;

  fProject := TCEProject.Create(self);
  fProjWidg.project := fProject;
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
  end;

end;
{$ENDREGION}

{$REGION file ******************************************************************}
procedure TCEMainForm.newFile;
var
  i: NativeInt;
  str: string;
begin
  if fEditWidg = nil then exit;
  //
  i := fEditWidg.editorCount;
  fEditWidg.addEditor;
  i := 0;
  while(true) do
  begin
    str := format('<new %d>',[i]);
    if findFile(str) = -1 then break;
    if i >= high(NativeInt) then break;
    i += 1;
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
    if fEditWidg.editor[i].fileName = aFilename then
    begin
      result := i;
      exit;
    end;
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
  '    writeln("coedit just saves a temporar d module before compiling it and running it...");' + #13#10 +
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
    temppath := '';
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

end.
