unit ce_main;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, FileUtil, SynEditKeyCmds, SynHighlighterLFM, Forms,
  AnchorDocking, AnchorDockStorage, AnchorDockOptionsDlg, Controls, Graphics,
  Dialogs, Menus, ActnList, ExtCtrls, process, XMLPropStorage, ComCtrls, dynlibs,
  ce_common, ce_dmdwrap, ce_project, ce_dcd, ce_plugin, ce_synmemo, ce_widget,
  ce_messages, ce_interfaces, ce_editor, ce_projinspect, ce_projconf, ce_search,
  ce_staticexplorer, ce_miniexplorer, ce_libman, ce_libmaneditor, ce_customtools,
  ce_observer, ce_writableComponent;

type

  // TODO-cfeature: options
  // TODO-cwidget: options editor
  // TODO-cwidget: custom tools editor
  // TODO-cfeature: tools menu

  { TCEMainForm }
  TCEMainForm = class(TForm, ICEMultiDocObserver, ICESessionOptionsObserver)
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

    fDoc: TCESynMemo;
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
    fTools: TCETools;

    // ICEMultiDocObserver
    procedure docNew(const aDoc: TCESynMemo);
    procedure docClosing(const aDoc: TCESynMemo);
    procedure docFocused(const aDoc: TCESynMemo);
    procedure docChanged(const aDoc: TCESynMemo);

    // ICESessionOptionsObserver
    procedure sesoptBeforeSave;
    procedure sesoptDeclareProperties(aFiler: TFiler);
    procedure sesoptAfterLoad;
    procedure optget_FileMRUItems(aWriter: TWriter);
    procedure optset_FileMRUItems(aReader: TReader);
    procedure optget_FileMRULimit(aWriter: TWriter);
    procedure optset_FileMRULimit(aReader: TReader);
    procedure optget_ProjMRUItems(aWriter: TWriter);
    procedure optset_ProjMRUItems(aReader: TReader);
    procedure optget_ProjMRULimit(aWriter: TWriter);
    procedure optset_ProjMRULimit(aReader: TReader);

    //Init - Fina
    procedure getCMdParams;
    procedure checkCompilo;
    procedure InitLibMan;
    procedure InitTools;
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
    procedure UpdateDockCaption(Exclude: TControl = nil); override;
    //
    procedure openFile(const aFilename: string);
    function expandSymbolicString(const symString: string): string;
    //
    property WidgetList: TCEWidgetList read fWidgList;
    property MessageWidget: TCEMessagesWidget read fMesgWidg;
    property LibraryManager: TLibraryManager read fLibMan;
  end;

  procedure PlugDispatchToHost(aPlugin: TCEPlugin; opCode: LongWord; data0: Integer; data1, data2: Pointer); cdecl;

var
  CEMainForm: TCEMainForm;

implementation
{$R *.lfm}

uses
  SynMacroRecorder, strutils, ce_options;

{$REGION Standard Comp/Obj------------------------------------------------------}
constructor TCEMainForm.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  EntitiesConnector.addObserver(self);
  //
  InitMRUs;
  InitLibMan;
  InitTools;
  InitWidgets;
  InitDocking;
  InitSettings;
  //
  newProj;
  checkCompilo;
  getCMdParams;
end;

procedure TCEMainForm.checkCompilo;
const
  msg = 'Coedit recquires DMD or DUB to be setup on this system' + LineEnding +
    'If DMD is setup please add it to the system PATH variable before using Coedit';
begin
  if exeInSysPath('dmd') or exeInSysPath('dub') then
    exit;
  ce_common.dlgOkError(msg);
  close;
end;

procedure TCEMainForm.getCMdParams;
var
  value: string;
  str: TStringList;
begin
  if application.ParamCount > 0 then
  begin
    value := application.Params[1];
    if value <> '' then
    begin
      str := TStringList.Create;
      try
        str.DelimitedText := value;
        for value in str do
        begin
          if fileExists(value) then
            openFile(value);
        end;
      finally
        str.Free;
      end;
    end;
  end;
  value := application.GetOptionValue('plugs');
  if value <> 'OFF' then
    InitPlugins;
  value := application.GetOptionValue('p', 'project');
  if (value <> '') and fileExists(value) then
    openProj(value);
  value := application.GetOptionValue('f', 'files');
  if value <> '' then
  begin
    str := TStringList.Create;
    try
      str.DelimitedText := value;
      for value in str do
      begin
        if fileExists(value) then
          openFile(value);
      end;
    finally
      str.Free;
    end;
  end;
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

procedure TCEMainForm.InitTools;
var
  fname: string;
begin
  fTools := TCETools.create(self);
  fname := getDocPath + 'tools.txt';
  if fileExists(fname) then
    fTools.loadFromFile(fname);
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
  DockMaster.HideHeaderCaptionFloatingControl := true;

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
  fname1 := getDocPath + 'options2.txt';
  fname2 := getDocPath + 'options2.bak';
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
    fTools.saveToFile(getDocPath + 'tools.txt');
    opts.saveToFile(getDocPath + 'options2.txt');
  finally
    opts.Free;
  end;
end;

procedure TCEMainForm.SaveDocking;
var
  xcfg: TXMLConfigStorage;
  i: NativeInt;
begin
  if WindowState = wsMinimized then
    WindowState := wsNormal;
  for i:= 0 to fWidgList.Count-1 do
  begin
    if DockMaster.GetAnchorSite(fWidgList.widget[i]).WindowState = wsMinimized then
      DockMaster.GetAnchorSite(fWidgList.widget[i]).WindowState := wsNormal;
    DockMaster.GetAnchorSite(fWidgList.widget[i]).Show;
  end;
  if not Visible then exit;
  //
  forceDirectory(getDocPath);
  xcfg := TXMLConfigStorage.Create(getDocPath + 'docking.xml',false);
  try
    DockMaster.SaveLayoutToConfig(xcfg);
    xcfg.WriteToDisk;
  finally
    xcfg.Free;
  end;
  //
  xcfg := TXMLConfigStorage.Create(getDocPath + 'dockingopts.xml',false);
  try
    DockMaster.SaveSettingsToConfig(xcfg);
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
  if fileExists(getDocPath + 'docking.xml') then
  begin
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
  if fileExists(getDocPath + 'dockingopts.xml') then
  begin
    xcfg := TXMLConfigStorage.Create(getDocPath + 'dockingopts.xml', true);
    try
      try
        DockMaster.LoadSettingsFromConfig(xcfg);
      except
        exit;
      end;
      str := TMemoryStream.Create;
      try
        xcfg.SaveToStream(str);
        str.saveToFile(getDocPath + 'dockingopts.bak')
      finally
        str.Free;
      end;
    finally
      xcfg.Free;
    end;
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
  EntitiesConnector.removeObserver(self);
  inherited;
end;

procedure TCEMainForm.UpdateDockCaption(Exclude: TControl = nil);
begin
  // otherwise dockmaster puts the widget list.
  Caption := 'Coedit';
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
  hasEd: boolean;
  hasProj: boolean;
begin
  if fEditWidg = nil then exit;
  if fUpdateCount > 0 then exit;
  Inc(fUpdateCount);
  try
    hasEd := fDoc <> nil;
    if hasEd then
    begin
      actEdCopy.Enabled := fDoc.SelAvail and fEditWidg.Focused;     // allows copy/cut/paste by shortcut on widgets
      actEdCut.Enabled := fDoc.SelAvail and fEditWidg.Focused;      //
      actEdPaste.Enabled := fDoc.CanPaste and fEditWidg.Focused;
      {$IFDEF MSWINDOWS}
      // close file : raises a segfault on linux UndoStuff.>>fList<<.Count...
      actEdUndo.Enabled := fDoc.CanUndo;
      actEdRedo.Enabled := fDoc.CanRedo;
      {$ENDIF}
      actEdMacPlay.Enabled := true;
      actEdMacStartStop.Enabled := true;
      actEdIndent.Enabled := true;
      actEdUnIndent.Enabled := true;
      //
      actFileCompAndRun.Enabled := fDoc.isDSource;
      actFileCompAndRunWithArgs.Enabled := fDoc.isDSource;
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

    for i:= 0 to srcLst.Count-1 do
    begin
      fname := srcLst.Strings[i];
      itm := TMenuItem.Create(trgMnu);
      itm.Hint := fname;
      itm.Caption := shortenPath(fname, 50);
      itm.OnClick := clickTrg;
      trgMnu.Add(itm);
    end;

    trgMnu.AddSeparator;
    itm := TMenuItem.Create(trgMnu);
    itm.Caption := 'Clear';
    itm.OnClick := @mruClearClick;
    itm.Tag := PtrInt(srcLst);
    trgMnu.Add(itm);

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
    begin
      HintStr := EditWidget.getEditorHint;
      CanShow := HintStr <> '';
    end;}
end;
{$ENDREGION}

{$REGION ICEMultiDocMonitor ----------------------------------------------------}
procedure TCEMainForm.docNew(const aDoc: TCESynMemo);
begin
  fDoc := aDoc;
end;

procedure TCEMainForm.docClosing(const aDoc: TCESynMemo);
begin
  if aDoc <> fDoc then exit;
  fDoc := nil;
end;

procedure TCEMainForm.docFocused(const aDoc: TCESynMemo);
begin
  fDoc := aDoc;
end;

procedure TCEMainForm.docChanged(const aDoc: TCESynMemo);
begin
  fDoc := aDoc;
end;
{$ENDREGION}

{$REGION file ------------------------------------------------------------------}
procedure TCEMainForm.newFile;
begin
  if fEditWidg = nil then exit;
  fEditWidg.addEditor;
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
  fDoc.Text :=
  'module runnable;' + LineEnding +
  '' + LineEnding +
  'import std.stdio;' + LineEnding +
  '' + LineEnding +
  'void main(string args[])' + LineEnding +
  '{' + LineEnding +
  '    // this file can be directly executed using menu file/compile & run' + LineEnding +
  '    // phobos and libman imports are allowed' + LineEnding +
  '    writeln("hello runnable module");' + LineEnding +
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
  if fDoc = nil then exit;
  //
  str := fDoc.fileName;
  if (str <> fDoc.tempFilename) and (fileExists(str)) then
    saveFile(fEditWidg.editorIndex)
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
begin
  if fDoc = nil then exit;
  if fDoc.modified then if dlgOkCancel(
      'The latest mdofifications are not saved, continue ?') = mrCancel
      then exit;
  //
  fEditWidg.removeEditor(fEditWidg.editorIndex);
end;

procedure TCEMainForm.actFileSaveAllExecute(Sender: TObject);
var
  i: NativeInt;
begin
  for i:= 0 to fEditWidg.editorCount-1 do
    saveFile(i);
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
begin
  if assigned(fDoc) then
    fDoc.CopyToClipboard;
end;

procedure TCEMainForm.actEdCutExecute(Sender: TObject);
begin
  if assigned(fDoc) then
    fDoc.CutToClipboard;
end;

procedure TCEMainForm.actEdPasteExecute(Sender: TObject);
begin
  if assigned(fDoc) then
    fDoc.PasteFromClipboard;
end;

procedure TCEMainForm.actEdUndoExecute(Sender: TObject);
begin
  if assigned(fDoc) then
    fDoc.Undo;
end;

procedure TCEMainForm.actEdRedoExecute(Sender: TObject);
begin
  if assigned(fDoc) then
    fDoc.Redo;
end;

procedure TCEMainForm.actEdMacPlayExecute(Sender: TObject);
begin
  if assigned(fDoc) then
    fEditWidg.macRecorder.PlaybackMacro(fDoc);
end;

procedure TCEMainForm.actEdMacStartStopExecute(Sender: TObject);
begin
  if assigned(fDoc) then
  begin
    if fEditWidg.macRecorder.State = msRecording then
      fEditWidg.macRecorder.Stop
    else fEditWidg.macRecorder.RecordMacro(fDoc);
  end;
end;

procedure TCEMainForm.actEdIndentExecute(Sender: TObject);
begin
  if assigned(fDoc) then
    fDoc.ExecuteCommand(ecBlockIndent, '', nil);
end;

procedure TCEMainForm.actEdUnIndentExecute(Sender: TObject);
begin
  if assigned(fDoc) then
    fDoc.ExecuteCommand(ecBlockUnIndent, '', nil);
end;

procedure TCEMainForm.actEdFindExecute(Sender: TObject);
var
  win: TAnchorDockHostSite;
  str: string;
begin
  win := DockMaster.GetAnchorSite(fFindWidg);
  if win = nil then exit;
  win.Show;
  win.BringToFront;
  if fDoc = nil then exit;
  //
  if fDoc.SelAvail then
    str := fDoc.SelText
  else str := fDoc.Identifier;
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
      if openFileFromDmdMessage(msg) then
        dt^.ctxt := mcEditor;
      dt^.editor := fDoc;
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

procedure TCEMainForm.compileAndRunFile(const edIndex: NativeInt; const runArgs: string = '');
var
  editor: TCESynMemo;
  dmdproc: TProcess;
  runproc: TProcess;
  fname: string;
begin
  dmdproc := TProcess.Create(nil);
  runproc := TProcess.Create(nil);
  editor  := fEditWidg.editor[edIndex];
  try

    fMesgWidg.ClearMessages(mcEditor);
    fMesgWidg.addCeInf('compiling ' + editor.fileName, mcEditor);

    if fileExists(editor.fileName) then editor.save
    else editor.saveToFile(editor.tempFilename);
    fname := editor.fileName[1..length(editor.fileName) - length(extractFileExt(editor.fileName))];

    {$IFDEF RELEASE}
    dmdProc.ShowWindow := swoHIDE;
    {$ENDIF}
    dmdproc.Options := [poStdErrToOutput, poUsePipes];
    dmdproc.Executable := DCompiler;
    dmdproc.Parameters.Add(editor.fileName);
    dmdproc.Parameters.Add('-w');
    dmdproc.Parameters.Add('-wi');
    dmdproc.Parameters.Add('-of' + fname + exeExt);
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
      runproc.Parameters.DelimitedText := expandSymbolicString(runArgs);
      runproc.Executable := fname + exeExt;
      runproc.Execute;
      repeat ProcessOutputToMsg(runproc, mcEditor) until not runproc.Running;
       sysutils.DeleteFile(fname + exeExt);
       sysutils.DeleteFile(fname + objExt);
    end
    else begin
      ProcessOutputToMsg(dmdproc, mcEditor);
      fMesgWidg.addCeErr(editor.fileName  + ' has not been compiled', mcEditor );
    end;

  finally
    dmdproc.Free;
    runproc.Free;
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

  with fProject.currentConfiguration do
  begin
    if preBuildProcess.executable <> '' then
      if exeInSysPath(preBuildProcess.executable) then
      begin
        ppproc := TProcess.Create(nil);
        try
          preBuildProcess.setProcess(ppproc);
          for i:= 0 to ppproc.Parameters.Count-1 do
            ppproc.Parameters.Strings[i] := expandSymbolicString(ppproc.Parameters.Strings[i]);
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
        if exeInSysPath(postBuildProcess.executable) then
        begin
          ppproc := TProcess.Create(nil);
          try
            postBuildProcess.setProcess(ppproc);
            for i:= 0 to ppproc.Parameters.Count-1 do
              ppproc.Parameters.Strings[i] := expandSymbolicString(ppproc.Parameters.Strings[i]);
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
  procname, prm: string;
  i: NativeInt;
begin
  if aProject.currentConfiguration.outputOptions.binaryKind <>
    executable then exit;

  runproc := TProcess.Create(nil);
  try
    aProject.currentConfiguration.runOptions.setProcess(runProc);
    prm := ''; i := 1;
    repeat
        prm := ExtractDelimited(i, runArgs, [' ']);
        prm := expandSymbolicString(prm);
        if prm <> '' then
          runProc.Parameters.AddText(prm);
        Inc(i);
    until prm = '';
    procname := aProject.outputFilename;

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
begin
  if fDoc = nil then exit;
  if not fileExists(fDoc.fileName) then exit;
  //
  DockMaster.GetAnchorSite(fExplWidg).Show;
  fExplWidg.expandPath(extractFilePath(fDoc.fileName));
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
var
  i: Integer;
  dt: double;
label
  _rbld,
  _run;
begin
  if fProject.currentConfiguration.outputOptions.binaryKind <> executable then
  begin
    // TODO-cfeature: define an alternative exe name for shared lib:
    // e.g: the dll produced by the proj. is the input filename of an host app.
    dlgOkInfo('Non executable projects cant be run');
    exit;
  end;
  if not fileExists(fProject.outputFilename) then
  begin
    if dlgOkCancel('The project output is missing, build ?') <> mrOK then
      exit;
    goto _rbld;
  end;
  dt := fileAge(fProject.outputFilename);
  for i := 0 to fProject.Sources.Count-1 do
  begin
    if fileAge(fProject.getAbsoluteSourceName(i)) > dt then
      if dlgOkCancel('The project sources have changed since last build, rebuild ?') = mrOK then
        goto _rbld
      else
        break;
  end;
  goto _run;
  _rbld:
    compileProject(fProject);
  _run:
    if fileExists(fProject.outputFilename) then
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
procedure TCEMainForm.saveProjSource(const aEditor: TCESynMemo);
begin
  if fProject = nil then exit;
  if fProject.fileName <> aEditor.fileName then exit;
  //
  aEditor.saveToFile(fProject.fileName);
  openProj(fProject.fileName);
end;

procedure TCEMainForm.closeProj;
begin
  fProject.Free;
  fProject := nil;
end;

procedure TCEMainForm.newProj;
begin
  fProject := TCEProject.Create(nil);
  fProject.Name := 'CurrentProject';
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
  fDoc.Highlighter := LfmSyn;
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

{$REGION ICESessionOptionsObserver ----------------------------------------------------}
procedure TCEMainForm.sesoptBeforeSave;
begin
end;

procedure TCEMainForm.sesoptDeclareProperties(aFiler: TFiler);
begin
  aFiler.DefineProperty('Menu_FileMRU_Items', @optset_FileMRUItems, @optget_FileMRUItems, true);
  aFiler.DefineProperty('Menu_FileMRU_Limit', @optset_FileMRULimit, @optget_FileMRULimit, true);
  aFiler.DefineProperty('Menu_ProjMRU_Items', @optset_ProjMRUItems, @optget_ProjMRUItems, true);
  aFiler.DefineProperty('Menu_ProjMRU_Limit', @optset_ProjMRULimit, @optget_ProjMRULimit, true);
end;

procedure TCEMainForm.sesoptAfterLoad;
begin
end;

procedure TCEMainForm.optget_FileMRUItems(aWriter: TWriter);
begin
  aWriter.WriteString(fFileMru.DelimitedText);
end;

procedure TCEMainForm.optset_FileMRUItems(aReader: TReader);
begin
  fFileMru.DelimitedText := aReader.ReadString;
end;

procedure TCEMainForm.optget_FileMRULimit(aWriter: TWriter);
begin
  aWriter.WriteInteger(fFileMru.maxCount);
end;

procedure TCEMainForm.optset_FileMRULimit(aReader: TReader);
begin
  fFileMru.maxCount := aReader.ReadInteger;
end;

procedure TCEMainForm.optget_ProjMRUItems(aWriter: TWriter);
begin
  aWriter.WriteString(fProjMru.DelimitedText);
end;

procedure TCEMainForm.optset_ProjMRUItems(aReader: TReader);
begin
  fProjMru.DelimitedText := aReader.ReadString;
end;

procedure TCEMainForm.optget_ProjMRULimit(aWriter: TWriter);
begin
  aWriter.WriteInteger(fProjMru.maxCount);
end;

procedure TCEMainForm.optset_ProjMRULimit(aReader: TReader);
begin
  fProjMru.maxCount := aReader.ReadInteger;
end;
{$ENDREGION}

function TCEMainForm.expandSymbolicString(const symString: string): string;
var
  elems: TStringList;
  elem: string;
  begs, ends: boolean;
  i: integer;
begin
  if symString = '' then
    exit('``');

  result := '';
  elems := TStringList.Create;
  try
    i := 0;
    elem := '';
    repeat
      inc(i);
      if not (symString[i] in ['<', '>']) then
        elem += symString[i]
      else
      begin
        if symString[i] = '<' then
          begs := true;
        ends := symString[i] = '>';
        elems.Add(elem);
        elem := '';
        if begs and ends then
        begin
          begs := false;
          ends := false;
          elems.Objects[elems.Count-1] := Self;
        end;
      end;
    until
      i = length(symString);
    elems.Add(elem);
    elem := '';
    for i:= 0 to elems.Count-1 do
    begin
      if elems.Objects[i] = nil then
        result += elems.Strings[i]
      else case elems.Strings[i] of
        '<','>' :
          continue;
        'CPF', 'CurrentProjectFile':
          begin
            if fProject <> nil then
              if fileExists(fProject.fileName) then
                result += fProject.fileName;
          end;
        'CPP', 'CurrentProjectPath':
          begin
            if fProject <> nil then
              if fileExists(fProject.fileName) then
                result += extractFilePath(fProject.fileName);
          end;
        'CPR', 'CurrentProjectRoot':
          begin
            if fProject <> nil then
              if directoryExists(fProject.getAbsoluteFilename(fProject.RootFolder)) then
                result += fProject.getAbsoluteFilename(fProject.RootFolder)
              else if directoryExists(fProject.RootFolder) then
                result += fProject.RootFolder;
          end;
        'CFF', 'CurrentFileFile':
          begin
            if fDoc <> nil then
              if fileExists(fDoc.fileName) then
                result += fDoc.fileName;
          end;
        'CFP', 'CurrentFilePath':
          begin
            if fDoc <> nil then
              if fileExists(fDoc.fileName) then
                result += extractFilePath(fDoc.fileName);
          end;
        'CI', 'CurrentIdentifier':
          begin
            if fDoc <> nil then
              result += fDoc.Identifier;
          end;
        'CAF', 'CoeditApplicationFile':
          result += application.ExeName;
        'CAP', 'CoeditApplicationPath':
          result += extractFilePath(Application.ExeName);
      end;
    end;
  finally
    elems.Free;
  end;
  // as the result may be used in TProcess.Parameter, it has not to be empty
  // otherwise next parameter switch can be considered as the parameter value,
  // eg --a=<CI> --b --c, the program will think that --b is --a value if <CI> is empty.
  if result = '' then
    result += '``';
end;

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
