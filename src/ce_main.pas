unit ce_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEditKeyCmds, SynHighlighterLFM, Forms,
  AnchorDocking, AnchorDockStorage, AnchorDockOptionsDlg, Controls, Graphics,
  Dialogs, Menus, ActnList, ExtCtrls, process, XMLPropStorage, ComCtrls, dynlibs,
  ce_common, ce_dmdwrap, ce_project, ce_dcd, ce_plugin, ce_synmemo, ce_widget,
  ce_messages, ce_widgettypes, ce_editor, ce_projinspect, ce_projconf, ce_search,
  ce_staticexplorer, ce_miniexplorer, ce_libman, ce_libmaneditor;

type

  TCEMainForm = class;

  //TODO-cfeature: switches -f<sourcefile.d>, -p<project.coedit>, -noplug
  //TODO-cfeature: options
  //TODO-cwidget: options editor
  (**
   * Encapsulates the options in a writable component.
   *)
  TCEOptions = class(TComponent)
  private
    fFileMru, fProjMru: TMruFileList;
    fLeft, FTop, fWidth, fHeight: Integer;
    fErrorFlg: boolean;
    procedure setFileMru(aValue: TMruFileList);
    procedure setProjMru(aValue: TMruFileList);
    procedure saveLayout(str: TStream);
    procedure loadLayout(str: TStream);
    //
    procedure readerPropNoFound(Reader: TReader; Instance: TPersistent;
      var PropName: string; IsPath: boolean; var Handled, Skip: Boolean);
    procedure readerError(Reader: TReader; const Message: string;
      var Handled: Boolean);
  published
    property APP_Left: Integer read fLeft write fLeft;
    property APP_Top: Integer read fTop write fTop;
    property APP_Width: Integer read fWidth write fWidth;
    property APP_Height: Integer read fHeight write fHeight;
    //
    property MRU_Files: TMruFileList read fFileMru write setFileMru;
    property MRU_Projects: TMruFileList read fProjMru write setProjMru;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure defineProperties(Filer: TFiler); override;
    procedure saveToFile(const aFilename: string);
    procedure loadFromFile(const aFilename: string);
    procedure beforeSave;
    procedure afterLoad;
    //
    property hasLoaded: boolean read fErrorFlg;
  end;

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
    actEdFind: TAction;
    actEdFindNext: TAction;
    actFileOpenContFold: TAction;
    actProjOpenContFold: TAction;
    actProjOptView: TAction;
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
    actEdIndent: TAction;
    actEdUnIndent: TAction;
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
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem56: TMenuItem;
    MenuItem57: TMenuItem;
    MenuItem58: TMenuItem;
    MenuItem59: TMenuItem;
    MenuItem60: TMenuItem;
    MenuItem61: TMenuItem;
    mnuItemMruFile: TMenuItem;
    mnuItemMruProj: TMenuItem;
    mnuItemWin: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    LfmSyn: TSynLFMSyn;
    procedure actEdFindExecute(Sender: TObject);
    procedure actEdFindNextExecute(Sender: TObject);
    procedure actFileAddToProjExecute(Sender: TObject);
    procedure actFileCloseExecute(Sender: TObject);
    procedure actFileCompAndRunExecute(Sender: TObject);
    procedure actFileCompAndRunWithArgsExecute(Sender: TObject);
    procedure actFileOpenContFoldExecute(Sender: TObject);
    procedure actFileSaveAllExecute(Sender: TObject);
    procedure actEdIndentExecute(Sender: TObject);
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
    procedure actProjOpenContFoldExecute(Sender: TObject);
    procedure actProjOpenExecute(Sender: TObject);
    procedure actEdPasteExecute(Sender: TObject);
    procedure actProjCloseExecute(Sender: TObject);
    procedure actProjOptsExecute(Sender: TObject);
    procedure actEdRedoExecute(Sender: TObject);
    procedure actFileSaveAsExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actProjOptViewExecute(Sender: TObject);
    procedure actProjRunExecute(Sender: TObject);
    procedure actProjRunWithArgsExecute(Sender: TObject);
    procedure actProjSaveAsExecute(Sender: TObject);
    procedure actProjSaveExecute(Sender: TObject);
    procedure actEdUndoExecute(Sender: TObject);
    procedure actProjSourceExecute(Sender: TObject);
    procedure actEdUnIndentExecute(Sender: TObject);
    procedure ApplicationProperties1Exception(Sender: TObject; E: Exception);
    procedure ApplicationProperties1ShowHint(var HintStr: string;
      var CanShow: Boolean; var HintInfo: THintInfo);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
  private

    // oh no...a field which is not a class.^^
    fUpdateCount: NativeInt;

    fProject: TCEProject;
    fPlugList: TCEPlugDescriptorList;
    fWidgList: TCEWidgetList;
    fMesgWidg: TCEMessagesWidget;
    fEditWidg: TCEEditorWidget;
    fProjWidg: TCEProjectInspectWidget;
    fPrjCfWidg: TCEProjectConfigurationWidget;
    fStExpWidg: TCEStaticExplorerWidget;
    fFindWidg:  TCESearchWidget;
    fExplWidg: TCEMiniExplorerWidget;
    fLibMWidg: TCELibManEditorWidget;
    fProjMru: TMruFileList;
    fFileMru: TMruFileList;
    fLibMan: TLibraryManager;

    //Init - Fina
    procedure InitLibMan;
    procedure InitMRUs;
    procedure InitWidgets;
    procedure InitPlugins;
    procedure InitDocking;
    procedure InitSettings;
    procedure SaveSettings;
    procedure LoadDocking;
    procedure SaveDocking;
    procedure KillPlugs;

    // widget interfaces subroutines
    procedure checkWidgetActions(const aWidget: TCEWidget);
    procedure widgetShowFromAction(sender: TObject);

    // run & exec sub routines
    procedure ProcessOutputToMsg(const aProcess: TProcess;aCtxt: TMessageContext = mcUnknown);
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

    // mru
    procedure mruChange(Sender: TObject);
    procedure mruFileItemClick(Sender: TObject);
    procedure mruProjItemClick(Sender: TObject);
    procedure mruClearClick(Sender: TObject);

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
    property ProjectConfWidget: TCEProjectConfigurationWidget read fPrjCfWidg;
    property LibraryManager: TLibraryManager read fLibMan;
  end;

  procedure PlugDispatchToHost(aPlugin: TCEPlugin; opCode: LongWord; data0: Integer; data1, data2: Pointer); cdecl;

var
  CEMainForm: TCEMainForm;

implementation
{$R *.lfm}

uses
  SynMacroRecorder;

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEMainForm.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  InitMRUs;
  InitLibMan;
  InitWidgets;
  InitDocking;
  InitSettings;
  //
  newProj;
  InitPlugins;
end;

procedure TCEMainForm.InitLibMan;
var
  fname: string;
begin
  fLibMan := TLibraryManager.create(self);
  fname := getDocPath + 'libraryManager.txt';
  if fileExists(fname) then
    fLibMan.loadFromFile(fname);
end;

procedure TCEMainForm.InitMRUs;
begin
  fProjMru := TMruFileList.Create;
  fFileMru := TMruFileList.Create;
  fProjMru.objectTag := mnuItemMruProj;
  fFileMru.objectTag := mnuItemMruFile;
  fProjMru.OnChange := @mruChange;
  fFileMru.OnChange := @mruChange;
end;

procedure TCEMainForm.InitPlugins;
var
  pth: string;
  fname: string;
  i: NativeInt;
  lst: TStringList;
  hdl: TLibHandle;
  plg: PPlugDescriptor;
begin
  fPlugList := TCEPlugDescriptorList.Create;
  pth := extractFilePath(application.ExeName) + 'plugins';
  lst := TStringList.Create;
  try
    listFiles(lst, pth, false);
    for i := 0 to lst.Count-1 do
    begin
      fname := lst.Strings[i];
      if extractFileExt(fname) <> '.' + SharedSuffix then
        continue;
      hdl := LoadLibrary(fname);
      if hdl = NilHandle then
        continue;

      plg := new(PPlugDescriptor);
      plg^.Handle := hdl;
      plg^.HostCreatePlug   := THostCreatePlug(GetProcAddress(hdl, 'createPlug'));
      plg^.HostDestroyPlug  := THostDestroyPlug(GetProcAddress(hdl, 'destroyPlug'));
      plg^.HostDispatchToPlug := THostDispatchToPlug(GetProcAddress(hdl, 'dispatchToPlug'));
      if plg^.HostCreatePlug <> nil then
        plg^.Plugin := plg^.HostCreatePlug(@PlugDispatchToHost);

      if (plg^.HostCreatePlug = nil) or (plg^.HostDestroyPlug = nil) or
        (plg^.HostDispatchToPlug = nil) then
      begin
        Dispose(plg);
        {$IFDEF RELEASE}
        FreeLibrary(Hdl);
        {$ENDIF}
        continue;
      end;
      fPlugList.addPlugin(plg);
    end;
  finally
    lst.Free;
  end;
end;

procedure TCEMainForm.InitWidgets;
var
  widg: TCEWidget;
  act: TAction;
  itm: TMenuItem;
begin
  fWidgList := TCEWidgetList.Create;
  fMesgWidg := TCEMessagesWidget.create(self);
  fEditWidg := TCEEditorWidget.create(self);
  fProjWidg := TCEProjectInspectWidget.create(self);
  fPrjCfWidg:= TCEProjectConfigurationWidget.create(self);
  fStExpWidg:= TCEStaticExplorerWidget.create(self);
  fFindWidg := TCESearchWidget.create(self);
  fExplWidg := TCEMiniExplorerWidget.create(self);
  fLibMWidg := TCELibManEditorWidget.create(self);

  fWidgList.addWidget(@fMesgWidg);
  fWidgList.addWidget(@fEditWidg);
  fWidgList.addWidget(@fProjWidg);
  fWidgList.addWidget(@fPrjCfWidg);
  fWidgList.addWidget(@fStExpWidg);
  fWidgList.addWidget(@fFindWidg);
  fWidgList.addWidget(@fExplWidg);
  fWidgList.addWidget(@fLibMWidg);

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
end;

procedure TCEMainForm.InitDocking;
var
  i: NativeInt;
  aManager: TAnchorDockManager;
begin
  DockMaster.MakeDockSite(Self, [akBottom], admrpChild);
  DockMaster.OnShowOptions := @ShowAnchorDockOptions;
  DockMaster.HeaderStyle := adhsPoints;

  if DockManager is TAnchorDockManager then begin
    aManager:=TAnchorDockManager(DockManager);
    aManager.PreferredSiteSizeAsSiteMinimum:=false;
  end;
  Height := 0;

  for i := 0 to fWidgList.Count-1 do
  begin
    DockMaster.MakeDockable(fWidgList.widget[i],true);
    DockMaster.GetAnchorSite(fWidgList.widget[i]).Header.HeaderPosition := adlhpTop;
  end;

  DockMaster.ManualDock(DockMaster.GetAnchorSite(fEditWidg), DockMaster.GetSite(Self), alBottom);
  DockMaster.ManualDock(DockMaster.GetAnchorSite(fMesgWidg), DockMaster.GetSite(Self), alBottom);
  DockMaster.ManualDock(DockMaster.GetAnchorSite(fStExpWidg), DockMaster.GetSite(Self), alLeft);
  DockMaster.ManualDock(DockMaster.GetAnchorSite(fFindWidg),
    DockMaster.GetAnchorSite(fStExpWidg), alBottom, fStExpWidg);
  width := width - fProjWidg.Width;
  DockMaster.ManualDock(DockMaster.GetAnchorSite(fProjWidg), DockMaster.GetSite(Self), alRight);
  DockMaster.ManualDock(DockMaster.GetAnchorSite(fPrjCfWidg),
    DockMaster.GetAnchorSite(fProjWidg), alBottom, fProjWidg);
  DockMaster.GetAnchorSite(fEditWidg).Header.HeaderPosition := adlhpTop;

  DockMaster.GetAnchorSite(fExplWidg).Close;
  DockMaster.GetAnchorSite(fLibMWidg).Close;

  LoadDocking;
end;

procedure TCEMainForm.InitSettings;
var
  fname1: string;
  fname2: string;
  opts: TCEOptions;
begin
  fname1 := getDocPath + 'options.txt';
  fname2 := getDocPath + 'options.bak';
  opts := TCEOptions.create(nil);
  try
    if fileExists(fname1) then
    begin
      opts.loadFromFile(fname1);
      if opts.hasLoaded then
      begin
        if fileExists(fname2) then
           sysutils.deleteFile(fname2);
        if not fileExists(fname2) then
          fileutil.copyFile(fname1, fname2, false);
      end;
    end;
  finally
    opts.Free;
  end;
end;

procedure TCEMainForm.SaveSettings;
var
  opts: TCEOptions;
begin
  opts := TCEOptions.create(nil);
  try
    forceDirectory(getDocPath);
    fLibMan.saveToFile(getDocPath + 'libraryManager.txt');
    opts.saveToFile(getDocPath + 'options.txt');
  finally
    opts.Free;
  end;
end;

procedure TCEMainForm.SaveDocking;
var
  xcfg: TXMLConfigStorage;
begin
  xcfg := TXMLConfigStorage.Create(getDocPath + 'docking.xml',false);
  try
    DockMaster.SaveLayoutToConfig(xcfg);
    xcfg.WriteToDisk;
  finally
    xcfg.Free;
  end;
end;

procedure TCEMainForm.LoadDocking;
var
  xcfg: TXMLConfigStorage;
  str: TMemoryStream;
begin
  if not fileExists(getDocPath + 'docking.xml') then
    exit;
  xcfg := TXMLConfigStorage.Create(getDocPath + 'docking.xml', true);
  try
    try
      DockMaster.LoadLayoutFromConfig(xcfg, false);
    except
      exit;
    end;
    str := TMemoryStream.Create;
    try
      xcfg.SaveToStream(str);
      str.saveToFile(getDocPath + 'docking.bak')
    finally
      str.Free;
    end;
  finally
    xcfg.Free;
  end;
end;

procedure TCEMainForm.KillPlugs;
var
  descr: TPlugDescriptor;
  i: NativeInt;
begin
  if fPlugList = nil then exit;
  for i := 0 to fPlugList.Count-1 do
  begin
    descr := fPlugList.plugin[i];
    descr.HostDestroyPlug(descr.Plugin);
    {$IFDEF RELEASE}
    FreeLibrary(descr.Handle);
    {$ENDIF}
  end;
  while fPlugList.Count <> 0 do
  begin
    Dispose(PPlugDescriptor(fPlugList.Items[fPlugList.Count-1]));
    fPlugList.Delete(fPlugList.Count-1);
  end;
  fPlugList.Free;
end;

destructor TCEMainForm.destroy;
begin
  SaveSettings;
  //
  KillPlugs;
  //
  fWidgList.Free;
  fProjMru.Free;
  fFileMru.Free;
  fProject.Free;
  //
  inherited;
end;

procedure TCEMainForm.ApplicationProperties1Exception(Sender: TObject;E: Exception);
begin
  if fMesgWidg = nil then
    ce_common.dlgOkError(E.Message)
  else fMesgWidg.addCeErr(E.Message);
end;

procedure TCEMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  i: NativeInt;
  ed: TCESynMemo;
begin
  canClose := false;
  if fProject <> nil then if fProject.modified then
    if ce_common.dlgOkCancel('last project modifications are not saved, quit anyway ?')
        <> mrOK then exit;
  for i := 0 to fEditWidg.editorCount-1 do
  begin
    ed := fEditWidg.editor[i];
    if ed.modified then if ce_common.dlgOkCancel(format
      ('last "%s" modifications are not saved, quit anyway ?',
        [shortenPath(ed.fileName, 25)])) <> mrOK then exit;
  end;
  canClose := true;
  // saving doesnt work when csDestroying in comp.state.
  SaveDocking;
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
      actEdCopy.Enabled := curr.SelAvail and fEditWidg.Focused;     // allows copy/cut/paste by shortcut on widgets
      actEdCut.Enabled := curr.SelAvail and fEditWidg.Focused;      //
      actEdPaste.Enabled := curr.CanPaste and fEditWidg.Focused;
      {$IFDEF MSWINDOWS}
      // close file : raises a segfault on linux UndoStuff.>>fList<<.Count...
      actEdUndo.Enabled := curr.CanUndo;
      actEdRedo.Enabled := curr.CanRedo;
      {$ENDIF}
      actEdMacPlay.Enabled := true;
      actEdMacStartStop.Enabled := true;
      actEdIndent.Enabled := true;
      actEdUnIndent.Enabled := true;
      //
      actFileCompAndRun.Enabled := curr.isDSource;
      actFileCompAndRunWithArgs.Enabled := curr.isDSource;
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
      actEdIndent.Enabled := false;
      actEdUnIndent.Enabled := false;
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
    actProjOptView.Enabled := hasProj;

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
    itm := TMenuItem.Create(prt);
    itm.Action := aWidget.contextAction(i);
    prt.Add(itm);
  end;
end;

procedure TCEMainForm.mruChange(Sender: TObject);
var
  srcLst: TMruFileList;
  trgMnu: TMenuItem;
  itm: TMenuItem;
  fname: string;
  clickTrg: TNotifyEvent;
  i: NativeInt;
begin
  srcLst := TMruFileList(Sender);
  if srcLst = nil then exit;
  trgMnu := TMenuItem(srcLst.objectTag);
  if trgMnu = nil then exit;

  if fUpdateCount > 0 then exit;
  Inc(fUpdateCount);
  try
    if srcLst = fFileMru then
      clickTrg := @mruFileItemClick
    else if srcLst = fProjMru then
      clickTrg := @mruProjItemClick;

    trgMnu.Clear;

    itm := TMenuItem.Create(trgMnu);
    itm.Caption := 'Clear';
    itm.OnClick := @mruClearClick;
    itm.Tag := PtrInt(srcLst);
    trgMnu.Add(itm);
    trgMnu.AddSeparator;

    for i:= 0 to srcLst.Count-1 do
    begin
      fname := srcLst.Strings[i];
      itm := TMenuItem.Create(trgMnu);
      itm.Hint := fname;
      itm.Caption := shortenPath(fname, 50);
      itm.OnClick := clickTrg;
      trgMnu.Add(itm);
    end;

  finally
    Dec(fUpdateCount);
  end;
end;

procedure TCEMainForm.mruClearClick(Sender: TObject);
var
  srcLst: TMruFileList;
begin
  srcLst := TMruFileList(TmenuItem(Sender).Tag);
  if srcLst = nil then exit;
  //
  srcLst.Clear;
end;

procedure TCEMainForm.ApplicationProperties1ShowHint(var HintStr: string;
  var CanShow: Boolean; var HintInfo: THintInfo);
begin
  CanShow := true;
  {if EditWidget.currentEditor <> nil then
    if EditWidget.currentEditor.Focused then
      HintStr := EditWidget.getEditorHint;}
end;
{$ENDREGION}

{$REGION file ------------------------------------------------------------------}
procedure TCEMainForm.newFile;
begin
  if fEditWidg = nil then exit;
  fEditWidg.addEditor;
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
  fEditWidg.editor[i].loadFromFile(aFilename);
  fEditWidg.focusedEditorChanged;
  fFileMru.Insert(0,aFilename);
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
  fEditWidg.editor[edIndex].save;
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
  fEditWidg.editor[edIndex].saveToFile(aFilename);
  fFileMru.Insert(0, aFilename);
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

procedure TCEMainForm.mruFileItemClick(Sender: TObject);
begin
  openFile(TMenuItem(Sender).Hint);
end;

procedure TCEMainForm.actFileOpenExecute(Sender: TObject);
begin
  if fEditWidg = nil then exit;
  //
  with TOpenDialog.Create(nil) do
  try
    filter := DdiagFilter;
    if execute then
    begin
      openFile(filename);
    end;
  finally
    free;
  end;
end;

procedure TCEMainForm.actProjOpenContFoldExecute(Sender: TObject);
begin
  if fProject = nil then exit;
  if not fileExists(fProject.fileName) then exit;
  //
  DockMaster.GetAnchorSite(fExplWidg).Show;
  fExplWidg.expandPath(extractFilePath(fProject.fileName));
end;

procedure TCEMainForm.actFileNewExecute(Sender: TObject);
begin
  newFile;
end;

procedure TCEMainForm.actFileNewRunExecute(Sender: TObject);
begin
  newFile;
  fEditWidg.currentEditor.Text :=
  'module runnable;' + LineEnding +
  '' + LineEnding +
  'import std.stdio;' + LineEnding +
  '' + LineEnding +
  'void main(string args[])' + LineEnding +
  '{' + LineEnding +
  '    writeln("this is just a `toy feature`");' + LineEnding +
  '    writeln;' + LineEnding +
  '    writeln("coedit saves a temp d module before compiling it and running it...");' + LineEnding +
  '}';
end;

procedure TCEMainForm.actFileSaveAsExecute(Sender: TObject);
begin
  if fEditWidg = nil then exit;
  if fEditWidg.editorIndex < 0 then exit;
  //
  with TSaveDialog.Create(nil) do
  try
    Filter := DdiagFilter;
    if execute then
      saveFileAs(fEditWidg.editorIndex, filename);
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
  if fEditWidg.editor[fEditWidg.editorIndex].isProjectSource
    then exit;
  //
  str := fEditWidg.editor[fEditWidg.editorIndex].fileName;
  if fileExists(str) then fProject.addSource(str)
  else dlgOkInfo('the file has not been added to the project because it does not exist');
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
    fWidgList.widget[i].docClose(fEditWidg.currentEditor);
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
  i: NativeInt;
begin
  for i:= low(FileNames) to high(FileNames) do
    openFile(FileNames[i]);
end;
{$ENDREGION}

{$REGION edit ------------------------------------------------------------------}
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

procedure TCEMainForm.actEdIndentExecute(Sender: TObject);
var
  curr: TCESynMemo;
begin
  curr := fEditWidg.currentEditor;
  if assigned(curr) then curr.ExecuteCommand(ecBlockIndent, '', nil);
end;

procedure TCEMainForm.actEdUnIndentExecute(Sender: TObject);
var
  curr: TCESynMemo;
begin
  curr := fEditWidg.currentEditor;
  if assigned(curr) then curr.ExecuteCommand(ecBlockUnIndent, '', nil);
end;

procedure TCEMainForm.actEdFindExecute(Sender: TObject);
var
  win: TAnchorDockHostSite;
  ed: TCESynMemo;
  str: string;
begin
  win := DockMaster.GetAnchorSite(fFindWidg);
  if win = nil then exit;
  win.Show;
  win.BringToFront;
  ed := fEditWidg.currentEditor;
  if ed = nil then exit;
  if ed.SelAvail then
    str := ed.SelText
  else str := ed.Identifier;
  ffindwidg.cbToFind.Text := str;
  ffindwidg.cbToFindChange(nil);
end;

procedure TCEMainForm.actEdFindNextExecute(Sender: TObject);
begin
  ffindwidg.actFindNextExecute(nil);
end;
{$ENDREGION}

{$REGION run -------------------------------------------------------------------}
procedure TCEMainForm.ProcessOutputToMsg(const aProcess: TProcess; aCtxt: TMessageContext = mcUnknown);
var
  str: TMemoryStream;
  lns: TStringList;
  readCnt: LongInt;
  readSz: LongInt;
  ioBuffSz: LongInt;
  dt: PMessageItemData;
  i: NativeInt;
  msg: string;
begin
  If not (poUsePipes in aProcess.Options) then exit;
  //
  readCnt := 0;
  ioBuffSz := aProcess.PipeBufferSize;
  str := TMemorystream.Create;
  lns := TStringList.Create;
  readSz := 0;
  try
    repeat
      str.SetSize(readSz + ioBuffSz);
      readCnt := aProcess.Output.Read((str.Memory + readSz)^, ioBuffSz);
      Inc(readSz, readCnt);
    until readCnt = 0;
    Str.SetSize(readSz);
    lns.LoadFromStream(Str);
    for i:= 0 to lns.Count-1 do begin
      msg := lns.Strings[i];
      dt := newMessageData;
      dt^.ctxt := aCtxt;
      dt^.project := fProject;
      dt^.position := getLineFromDmdMessage(msg);
      dt^.editor := getFileFromDmdMessage(msg);
      if dt^.editor = nil then
        dt^.editor := EditWidget.currentEditor
      else
        dt^.ctxt := mcEditor;
      fEditWidg.endUpdatebyDelay; // messages would be cleared by the delayed module name detection.
      fMesgWidg.addMessage(msg, dt);
      application.ProcessMessages;
    end;
  finally
    str.Free;
    lns.Free;
    fMesgWidg.scrollToBack;
  end;
end;

// TODO-cfeature: input handling
procedure TCEMainForm.compileAndRunFile(const edIndex: NativeInt; const runArgs: string = '');
var
  editor: TCESynMemo;
  dmdproc: TProcess;
  runproc: TProcess;
  fname, temppath, olddir: string;
begin
  olddir  := '';
  dmdproc := TProcess.Create(nil);
  runproc := TProcess.Create(nil);
  editor  := fEditWidg.editor[edIndex];
  getDir(0, olddir);
  try

    fMesgWidg.ClearMessages(mcEditor);
    fMesgWidg.addCeInf('compiling ' + editor.fileName, mcEditor);

    temppath := GetTempDir(false);
    chDir(temppath);
    {$IFDEF DEBUG}{$WARNINGS OFF}{$HINTS OFF}{$ENDIF}
    fname := temppath + 'temp_' + uniqueObjStr(editor);
    {$IFDEF DEBUG}{$WARNINGS ON}{$HINTS ON}{$ENDIF}
    if fileExists(editor.fileName) then editor.save
    else editor.saveToFile(fname + '.d');
    fname := editor.fileName[1..length(editor.fileName) - length(extractFileExt(editor.fileName))];

    {$IFDEF RELEASE}
    dmdProc.ShowWindow := swoHIDE;
    {$ENDIF}
    dmdproc.Options := [poStdErrToOutput, poUsePipes];
    dmdproc.Executable := DCompiler;
    dmdproc.Parameters.Add(editor.fileName);
    dmdproc.Parameters.Add('-w');
    dmdproc.Parameters.Add('-wi');
    dmdproc.Parameters.Add('-of' + fname {$IFDEF WINDOWS}+ '.exe'{$ENDIF});
    LibraryManager.getLibFiles(nil, dmdproc.Parameters);
    LibraryManager.getLibSources(nil, dmdproc.Parameters);
    dmdproc.Execute;
    repeat ProcessOutputToMsg(dmdproc, mcEditor) until not dmdproc.Running;
    if (dmdProc.ExitStatus = 0) then
    begin
      ProcessOutputToMsg(dmdproc, mcEditor);
      fMesgWidg.addCeInf(editor.fileName + ' successfully compiled', mcEditor );
      runproc.Options := [poStderrToOutPut, poUsePipes];
      runproc.CurrentDirectory := extractFilePath(runProc.Executable);
      runproc.Parameters.Text := runArgs;
      runproc.Executable := fname {$IFDEF WINDOWS}+ '.exe'{$ENDIF};
      runproc.Execute;
      repeat ProcessOutputToMsg(runproc, mcEditor) until not runproc.Running;
      {$IFDEF MSWINDOWS}
       //sysutils.DeleteFile(fname + '.exe');
       //sysutils.DeleteFile(fname + '.obj');
      {$ELSE}
       sysutils.DeleteFile(fname);
       sysutils.DeleteFile(fname + '.o');
      {$ENDIF}
    end
    else begin
      ProcessOutputToMsg(dmdproc, mcEditor);
      fMesgWidg.addCeErr(editor.fileName  + ' has not been compiled', mcEditor );
    end;

  finally
    dmdproc.Free;
    runproc.Free;
    if extractFilePath(editor.fileName) = GetTempDir(false) then
       sysutils.DeleteFile(editor.fileName);
    chDir(olddir);
  end;
end;

procedure TCEMainForm.compileProject(const aProject: TCEProject);
var
  dmdproc: TProcess;
  ppproc: TProcess;
  olddir, prjpath: string;
  i: NativeInt;
begin

  fMesgWidg.ClearAllMessages;

  for i := 0 to fWidgList.Count-1 do
    fWidgList.widget[i].projCompile(aProject);

  with fProject.currentConfiguration do
  begin
    if preBuildProcess.executable <> '' then
      if ExeSearch(preBuildProcess.executable, '') <> '' then
      begin
        ppproc := TProcess.Create(nil);
        try
          preBuildProcess.setProcess(ppproc);
          if ppproc.CurrentDirectory = '' then
            ppproc.CurrentDirectory := extractFilePath(ppproc.Executable);
          ppproc.Execute;
          if not (poWaitOnExit in ppproc.Options) then
            if poUsePipes in ppproc.Options then
              repeat ProcessOutputToMsg(ppproc, mcProject) until not ppproc.Running;
        finally
          ppproc.Free;
        end;
      end
      else fMesgWidg.addCeWarn('the pre-compilation executable does not exist', mcProject);
  end;

  if aProject.Sources.Count = 0 then
  begin
    fMesgWidg.addCeWarn('the project has no source files', mcProject);
    exit;
  end;

  olddir := '';
  dmdproc := TProcess.Create(nil);
  getDir(0, olddir);
  try

    fMesgWidg.addCeInf( 'compiling ' + aProject.fileName, mcProject);
    application.ProcessMessages;

    prjpath := extractFilePath(aProject.fileName);
    if directoryExists(prjpath) then
    begin
      chDir(prjpath);
      dmdProc.CurrentDirectory := prjpath;
    end;
    {$IFDEF RELEASE}
    dmdProc.ShowWindow := swoHIDE;
    {$ENDIF}
    dmdproc.Options := [poStdErrToOutput, poUsePipes];
    dmdproc.Executable := DCompiler;
    aProject.getOpts(dmdproc.Parameters);
    dmdproc.Execute;

    repeat ProcessOutputToMsg(dmdproc, mcProject) until not dmdproc.Running;
    if (dmdProc.ExitStatus = 0) then
      fMesgWidg.addCeInf(aProject.fileName + ' successfully compiled', mcProject)
    else
      fMesgWidg.addCeErr(aProject.fileName + ' has not been compiled', mcProject);

    with fProject.currentConfiguration do
    begin
      if postBuildProcess.executable <> '' then
        if ExeSearch(postBuildProcess.executable, '') <> '' then
        begin
          ppproc := TProcess.Create(nil);
          try
            postBuildProcess.setProcess(ppproc);
            if ppproc.CurrentDirectory = '' then
              ppproc.CurrentDirectory := extractFilePath(ppproc.Executable);
            ppproc.Execute;
            if not (poWaitOnExit in ppproc.Options) then
              if poUsePipes in ppproc.Options then
                repeat ProcessOutputToMsg(ppproc, mcProject) until not ppproc.Running;
          finally
            ppproc.Free;
          end;
        end
        else fMesgWidg.addCeWarn('the post-compilation executable does not exist', mcProject);
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
  i: NativeInt;
begin
  if aProject.currentConfiguration.outputOptions.binaryKind <>
    executable then exit;

  for i := 0 to fWidgList.Count-1 do
    fWidgList.widget[i].projRun(aProject);

  runproc := TProcess.Create(nil);
  try
    aProject.currentConfiguration.runOptions.setProcess(runProc);
    runproc.Parameters.AddText(runArgs);
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
      fMesgWidg.addCeErr('output executable missing: ' + procname, mcProject);
      exit;
    end;

    // If poWaitonExit and if there are a lot of output then Coedit hangs.
    if poWaitonExit in runproc.Options then
    begin
      runproc.Options := runproc.Options - [poStderrToOutPut, poUsePipes];
      runproc.Options := runproc.Options + [poNewConsole];
    end;

    runproc.Executable := procname;
    if runproc.CurrentDirectory = '' then
      runproc.CurrentDirectory := extractFilePath(runproc.Executable);
    runproc.Execute;
    repeat ProcessOutputToMsg(runproc, mcProject) until not runproc.Running;

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
  if InputQuery('Execution arguments', '', runargs) then
    compileAndRunFile(fEditWidg.editorIndex, runargs);
end;

procedure TCEMainForm.actFileOpenContFoldExecute(Sender: TObject);
var
  curr: TCESynMemo;
begin
  curr := EditWidget.currentEditor;
  if curr = nil then exit;
  if not fileExists(curr.fileName) then exit;
  //
  DockMaster.GetAnchorSite(fExplWidg).Show;
  fExplWidg.expandPath(extractFilePath(curr.fileName));
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
  if InputQuery('Execution arguments', '', runargs) then
    runProject(fProject, runargs);
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
  if InputQuery('Execution arguments', '', runargs) then
    runProject(fProject, runargs);
end;
{$ENDREGION}

{$REGION view ------------------------------------------------------------------}
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

{$REGION project ---------------------------------------------------------------}
procedure TCEMainForm.projChange(sender: TObject);
var
  i: NativeInt;
begin
  for i := 0 to WidgetList.Count-1 do
    WidgetList.widget[i].projChange(fProject);
end;

procedure TCEMainForm.saveProjSource(const aEditor: TCESynMemo);
begin
  if fProject = nil then exit;
  if fProject.fileName <> aEditor.fileName then exit;
  //
  aEditor.saveToFile(fProject.fileName);
  openProj(fProject.fileName);
end;

procedure TCEMainForm.closeProj;
var
  i: NativeInt;
begin
  for i := 0 to WidgetList.Count-1 do
    WidgetList.widget[i].projClose(fProject);
  fProject.Free;
  fProject := nil;
end;

procedure TCEMainForm.newProj;
var
  i: NativeInt;
begin
  fProject := TCEProject.Create(nil);
  fProject.Name := 'CurrentProject';
  for i := 0 to WidgetList.Count-1 do
    WidgetList.widget[i].projNew(fProject);
  fProject.onChange := @projChange;
  fProject.libraryManager := fLibMan;
end;

procedure TCEMainForm.saveProj;
begin
  fProject.saveToFile(fProject.fileName);
end;

procedure TCEMainForm.saveProjAs(const aFilename: string);
begin
  fProject.fileName := aFilename;
  fProject.saveToFile(fProject.fileName);
  fProjMru.Insert(0,fProject.fileName);
end;

procedure TCEMainForm.openProj(const aFilename: string);
begin
  closeProj;
  newProj;
  fProject.loadFromFile(aFilename);
  fProjMru.Insert(0,aFilename);
end;

procedure TCEMainForm.mruProjItemClick(Sender: TObject);
begin
  openProj(TMenuItem(Sender).Hint);
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

procedure TCEMainForm.actProjOptViewExecute(Sender: TObject);
var
  lst: TStringList;
begin
  lst := TStringList.Create;
  try
    fProject.getOpts(lst);
    dlgOkInfo(lst.Text);
  finally
    lst.Free;
  end;
end;
{$ENDREGION}

{$REGION options ---------------------------------------------------------------}
constructor TCEOptions.create(aOwner: TComponent);
begin
  inherited;
  fFileMru := TMruFileList.Create;
  fProjMru := TMruFileList.Create;
  //
  fLeft := 0;
  fTop := 0;
  fWidth := 800;
  fHeight := 600;
end;

destructor TCEOptions.destroy;
begin
  fFileMru.Free;
  fProjMru.Free;
  inherited;
end;

procedure TCEOptions.readerPropNoFound(Reader: TReader; Instance: TPersistent;
  var PropName: string; IsPath: boolean; var Handled, Skip: Boolean);
begin
  Skip := true;
  Handled := true;
end;

procedure TCEOptions.readerError(Reader: TReader; const Message: string;
  var Handled: Boolean);
begin
  Handled := true;
  fErrorFlg := false;
end;

procedure TCEOptions.setFileMru(aValue: TMruFileList);
begin
  fFileMru.Assign(aValue);
end;

procedure TCEOptions.setProjMru(aValue: TMruFileList);
begin
  fProjMru.Assign(aValue);
end;

procedure TCEOptions.saveLayout(str: TStream);
var
  st: TXMLConfigStorage;
  cf: TPropStorageXMLConfig;
begin
  cf := TPropStorageXMLConfig.Create(nil);
  st := TXMLConfigStorage.Create(cf);
  try
    DockMaster.SaveLayoutToConfig(st);
    cf.SaveToStream(str);
    str.Position := 0;
  finally
    st.Free;
    cf.Free;
  end;
end;

procedure TCEOptions.loadLayout(str: TStream);
var
  st: TXMLConfigStorage;
  cf: TPropStorageXMLConfig;
begin
  cf := TPropStorageXMLConfig.Create(nil);
  st := TXMLConfigStorage.Create(cf);
  try
    cf.LoadFromStream(str);
    DockMaster.LoadLayoutFromConfig(st,true);
  finally
    st.Free;
    cf.Free;
  end;
end;

procedure TCEOptions.defineProperties(Filer: TFiler);
var
  i: NativeInt;
begin
  inherited;
  // Filer is either a TReader or a TWriter
  for i := 0 to CEMainForm.WidgetList.Count-1 do
    CEMainForm.WidgetList.widget[i].declareProperties(Filer);
end;

procedure TCEOptions.beforeSave;
var
  i: NativeInt;
begin
  fLeft   := CEMainForm.Left;
  fTop    := CEMainForm.Top;
  fWidth  := CEMainForm.Width;
  fHeight := CEMainForm.Height;
  //
  fFileMru.Assign(CEMainForm.fFileMru);
  fProjMru.Assign(CEMainForm.fProjMru);
  //
  for i := 0 to CEMainForm.WidgetList.Count-1 do
    CEMainForm.WidgetList.widget[i].beforeSave(nil);
end;

procedure TCEOptions.saveToFile(const aFilename: string);
begin
  fErrorFlg := true;
  beforeSave;
  ce_common.saveCompToTxtFile(self, aFilename);
end;

procedure TCEOptions.loadFromFile(const aFilename: string);
begin
  fErrorFlg := true;
  loadCompFromTxtFile(self, aFilename, @readerPropNoFound, @readerError);
  afterLoad;
end;

procedure TCEOptions.afterLoad;
var
  i: NativeInt;
begin
  CEMainForm.Left   := fLeft;
  CEMainForm.Top    := fTop;
  CEMainForm.Width  := fWidth;
  CEMainForm.Height := fHeight;
  if fLeft < 0 then fLeft := 0;
  if fTop < 0 then fTop := 0;
  if fWidth < 800 then fWidth := 800;
  if fHeight < 600 then fWidth := 600;
  //
  CEMainForm.fFileMru.Assign(fFileMru);
  CEMainForm.fProjMru.Assign(fProjMru);
  //
  for i := 0 to CEMainForm.WidgetList.Count-1 do
    CEMainForm.WidgetList.widget[i].afterLoad(nil);
end;
{$ENDREGION}

procedure PlugDispatchToHost(aPlugin: TCEPlugin; opCode: LongWord; data0: Integer; data1, data2: Pointer); cdecl;
var
  ctxt: NativeUint;
  oper: NativeUint;
begin

  if opCode = HELLO_PLUGIN then begin
      dlgOkInfo('Hello plugin');
      exit;
  end;

  ctxt := opCode and $0F000000;
  oper := opCode and $000FFFFF;

  case ctxt of
    CTXT_MSGS:
      case oper of
        DT_ERR:  CEMainForm.MessageWidget.addCeErr(PChar(data1));
        DT_INF:  CEMainForm.MessageWidget.addCeInf(PChar(data1));
        DT_WARN: CEMainForm.MessageWidget.addCeWarn(PChar(data1));
        else CEMainForm.MessageWidget.addCeWarn('unsupported dispatcher opCode');
      end;
    CTXT_DLGS:
      case oper of
        DT_ERR: dlgOkError(PChar(data1));
        DT_INF: dlgOkInfo(PChar(data1));
        DT_WARN: dlgOkInfo(PChar(data1));
        else CEMainForm.MessageWidget.addCeWarn('unsupported dispatcher opCode');
      end;
    else CEMainForm.MessageWidget.addCeWarn('unsupported dispatcher opCode');
  end;

end;

initialization
  RegisterClasses([TCEOptions]);
end.
