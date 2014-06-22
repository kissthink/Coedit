unit ce_dmdwrap;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils;


type

  (*****************************************************************************
   * Base class for encapsulating some compiler options.
   * A descendant must be able to generate the related options
   * as a string representing the partial switches/arguments.
   *)
  TOptsGroup = class(TPersistent)
  private
    fOnChange: TNotifyEvent;
    procedure doChanged;
  protected
    property onChange: TNotifyEvent read fOnChange write fOnChange;
  public
    procedure getOpts(const aList: TStrings); virtual; abstract;
  end;

  (*****************************************************************************
   * Encapsulates the options/args related to the DDoc and JSON generation.
   *)
  TDocOpts = class(TOptsGroup)
  private
    fGenDoc: boolean;
    fDocDir: string;
    fGenJson: boolean;
    fJsonFname: string;
    procedure setGenDoc(const aValue: boolean);
    procedure setGenJSON(const aValue: boolean);
    procedure setDocDir(const aValue: string);
    procedure setJSONFile(const aValue: string);
  published
    property generateDocumentation: boolean read fGenDoc write setGenDoc;
    property generateJSON: boolean read fGenJson write setGenJSON;
    property DocumentationDirectory: string read fDocDir write setDocDir;
    property JSONFilename: string read fJsonFname write setJSONFile;
  public
    procedure assign(aValue: TPersistent); override;
    procedure getOpts(const aList: TStrings); override;
  end;


  (*****************************************************************************
   * Describes the different depreciation treatments.
   *)
    TDepHandling = (silent, warning, error);

  (*****************************************************************************
   * Encapsulates the options/args related to the compiler output messages.
   *)
  TMsgOpts = class(TOptsGroup)
  private
    fDepHandling : TDepHandling;
    fVerb: boolean;
    fWarn: boolean;
    fWarnEx: boolean;
    fVtls: boolean;
    fQuiet: boolean;
    procedure setDepHandling(const aValue: TDepHandling);
    procedure setVerb(const aValue: boolean);
    procedure setWarn(const aValue: boolean);
    procedure setWarnEx(const aValue: boolean);
    procedure setVtls(const aValue: boolean);
    procedure setQuiet(const aValue: boolean);
  published
    property depreciationHandling: TDepHandling read fDepHandling write setDepHandling;
    property verbose: boolean read fVerb write setVerb;
    property warnings: boolean read fWarn write setWarn;
    property additionalWarnings: boolean read fWarnEx write setWarnEx;
    property tlsInformations: boolean read fVtls write setVtls;
    property quiet: boolean read fQuiet write setQuiet;
  public
    constructor create;
    procedure assign(aValue: TPersistent); override;
    procedure getOpts(const aList: TStrings); override;
  end;

  (*****************************************************************************
   * Describes the target registry size
   *)
  TTargetSystem = (auto, os32bit, os64bit);
  (**
   * Describes the output kind
   *)
  TBinaryKind = (executable, staticlib, sharedlib, obj);


  (*****************************************************************************
   * Encapsulates the options/args related to the analysis & the code gen.
   *)
  TOutputOpts= class(TOptsGroup)
  private
    fTrgKind: TTargetSystem;
    fBinKind: TBinaryKind;
    fUt: boolean;
    fVerId: string;
    fInline: boolean;
    fNoBounds: boolean;
    fOptimz: boolean;
    fGenStack: boolean;
    fMain: boolean;
    fRelease: boolean;
    procedure setUt(const aValue: boolean);
    procedure setVerId(const aValue: string);
    procedure setTrgKind(const aValue: TTargetSystem);
    procedure setBinKind(const aValue: TBinaryKind);
    procedure setInline(const aValue: boolean);
    procedure setNoBounds(const aValue: boolean);
    procedure setOptims(const aValue: boolean);
    procedure setGenStack(const aValue: boolean);
    procedure setMain(const aValue: boolean);
    procedure setRelease(const aValue: boolean);
  published
    property targetKind: TTargetSystem read fTrgKind write setTrgKind;
    property binaryKind: TBinaryKind read fBinKind write setBinKind;
    property inlining: boolean read fInline write setInline;
    property noBoundsCheck: boolean read fNoBounds write setNoBounds;
    property optimizations: boolean read fOptimz write setOptims;
    property generateStackFrame: boolean read fGenStack write setGenStack;
    property addMain: boolean read fMain write setMain;
    property release: boolean read fRelease write setRelease;
    property unittest: boolean read fUt write setUt;
    property versionIdentifier: string read fVerId write setVerId;
  public
    procedure assign(aValue: TPersistent); override;
    procedure getOpts(const aList: TStrings); override;
  end;

  (*****************************************************************************
   * Encapsulates the options/args related to the debuging
   *)
  TDebugOpts = class(TOptsGroup)
  private
    fDbg: boolean;
    fDbgIdent: string;
    fDbgD: boolean;
    fDbgC: boolean;
    fMap: boolean;
    procedure setDbg(const aValue: boolean);
    procedure setDbgIdent(const aValue: string);
    procedure setDbgD(const aValue: boolean);
    procedure setDbgC(const aValue: boolean);
    procedure setMap(const aValue: boolean);
  published
    property debug: boolean read fDbg write setDbg;
    property debugIdentifier: string read fDbgIdent write setDbgIdent;
    property addDInformations: boolean read fDbgD write setDbgD;
    property addCInformations: boolean read fDbgC write setDbgC;
    property generateMapFile: boolean read fMap write setMap;
  public
    procedure assign(aValue: TPersistent); override;
    procedure getOpts(const aList: TStrings); override;
  end;

  (*****************************************************************************
   * Encapsulates the options/args related to the output and include paths
   *)
  TPathsOpts = class(TOptsGroup)
  private
    fSrcs: TStringList;
    fIncl: TStringList;
    fImpt: TStringList;
    fFname: string;
    fObjDir: string;
    procedure setFname(const aValue: string);
    procedure setObjDir(const aValue: string);
    procedure setSrcs(const aValue: TStringList);
    procedure setIncl(const aValue: TStringList);
    procedure setImpt(const aValue: TStringList);
  published
    property outputFilename: string read fFname write setFname;
    property objectDirectory: string read fObjDir write setObjDir;
    property Sources: TStringList read fSrcs write setSrcs; // not common srcs, made for static libs
    property Includes: TStringList read fIncl write setIncl;
    property Imports: TStringList read fImpt write setImpt;
  public
    constructor create;
    destructor destroy; override;
    procedure assign(aValue: TPersistent); override;
    procedure getOpts(const aList: TStrings); override;
  end;

  (*****************************************************************************
   * Encapsulates the unclassified and custom options/args
   *)
  TOtherOpts = class(TOptsGroup)
  private
    fCustom: TStringList;
    procedure setCustom(const aValue: TStringList);
  published
    property customOptions: TStringList read fCustom write setCustom;
  public
    constructor create;
    destructor destroy; override;
    procedure assign(aValue: TPersistent); override;
    procedure getOpts(const aList: TStrings); override;
  end;

  (*****************************************************************************
   * Encapsulates all the contextual options/args
   *)
  TCompilerConfiguration = class(TCollectionItem)
  private
    fName: string;
    fOnChanged: TNotifyEvent;
    fDocOpts: TDocOpts;
    fDebugOpts: TDebugOpts;
    fMsgOpts: TMsgOpts;
    fOutputOpts: TOutputOpts;
    fPathsOpts: TPathsOpts;
    fOthers: TOtherOpts;
    procedure doChanged;
    procedure subOptsChanged(sender: TObject);
    procedure setName(const aValue: string);
    procedure setDocOpts(const aValue: TDocOpts);
    procedure setDebugOpts(const aValue: TDebugOpts);
    procedure setMsgOpts(const aValue: TMsgOpts);
    procedure setOutputOpts(const aValue: TOutputOpts);
    procedure setPathsOpts(const aValue: TPathsOpts);
    procedure setOthers(const aValue: TOtherOpts);
  protected
    function nameFromID: string;
  published
    property name: string read fName write setName;
    property documentationOptions: TDocOpts read fDocOpts write setDocOpts;
    property debugingOptions: TDebugOpts read fDebugOpts write setDebugOpts;
    property messagesOptions: TMsgOpts read fMsgOpts write setMsgOpts;
    property outputOptions: TOutputOpts read fOutputOpts write setOutputOpts;
    property pathsOptions: TPathsOpts read fPathsOpts write setPathsOpts;
    property otherOptions: TOtherOpts read fOthers write setOthers;
  public
    constructor create(aCollection: TCollection); override;
    destructor destroy; override;
    procedure assign(aValue: TPersistent); override;
    procedure getOpts(const aList: TStrings);
    property onChanged: TNotifyEvent read fOnChanged write fOnChanged;
  end;

implementation

uses
  ce_common;

(*******************************************************************************
 * TOptsGroup
 *)
procedure TOptsGroup.doChanged;
begin
  if assigned(fOnChange) then fOnChange(self);
end;

(*******************************************************************************
 * TDocOpts
 *)
procedure TDocOpts.getOpts(const aList: TStrings);
begin
  if fGenDoc then aList.Add('-D');
  if fGenJson then aList.Add('-X');
  if fDocDir <> '' then aList.Add('-Dd' + fDocDir);
  if fJsonFname <> '' then aList.Add('-Xf' + fJsonFname);
end;

procedure TDocOpts.assign(aValue: TPersistent);
var
  src: TDocOpts;
begin
  if (aValue is TDocOpts) then
  begin
    src       := TDocOpts(aValue);
    fGenDoc   := src.fGenDoc;
    fGenJson  := src.fGenJson;
    fDocDir   := patchPlateformPath(src.fDocDir);
    fJsonFname:= patchPlateformPath(src.fJsonFname);
  end
  else inherited;
end;

procedure TDocOpts.setGenDoc(const aValue: boolean);
begin
  if fGenDoc = aValue then exit;
  fGenDoc := aValue;
  doChanged;
end;

procedure TDocOpts.setGenJSON(const aValue: boolean);
begin
  if fGenJson = aValue then exit;
  fGenJson := aValue;
  doChanged;
end;

procedure TDocOpts.setDocDir(const aValue: string);
begin
  if fDocDir = aValue then exit;
  fDocDir := patchPlateformPath(aValue);
  doChanged;
end;

procedure TDocOpts.setJSONFile(const aValue: string);
begin
  if fJsonFname = aValue then exit;
  fJsonFname := patchPlateformPath(aValue);
  doChanged;
end;

(*******************************************************************************
 * TMsgOpts
 *)
constructor TMsgOpts.create;
begin
  fDepHandling := TDepHandling.warning;
end;

procedure TMsgOpts.getOpts(const aList: TStrings);
var
  opt : string;
const
  DepStr : array[TDepHandling] of string = ('-d', '', '-de');
begin
  opt := DepStr[fDepHandling];
  if opt <> '' then aList.Add(opt);
  if fVerb then aList.Add('-v');
  if fWarn then aList.Add('-w');
  if fWarnEx then aList.Add('-wi');
  if fVtls then aList.Add('-vtls');
  if fQuiet then aList.Add('-quiet');
end;

procedure TMsgOpts.assign(aValue: TPersistent);
var
  src: TMsgOpts;
begin
  if (aValue is TMsgOpts) then
  begin
    src := TMsgOpts(aValue);
    fDepHandling := src.fDepHandling;
    fVerb   := src.fVerb;
    fWarn   := src.fWarn;
    fWarnEx := src.fWarnEx;
    fVtls   := src.fVtls;
    fQuiet  := src.fQuiet;
  end
  else inherited;
end;

procedure TMsgOpts.setDepHandling(const aValue: TDepHandling);
begin
  if fDepHandling = aValue then exit;
  fDepHandling := aValue;
  doChanged;
end;

procedure TMsgOpts.setVerb(const aValue: boolean);
begin
  if fVerb = aValue then exit;
  fVerb := aValue;
  doChanged;
end;

procedure TMsgOpts.setWarn(const aValue: boolean);
begin
  if fWarn = aValue then exit;
  fWarn := aValue;
  doChanged;
end;

procedure TMsgOpts.setWarnEx(const aValue: boolean);
begin
  if fWarnEx = aValue then exit;
  fWarnEx := aValue;
  doChanged;
end;

procedure TMsgOpts.setVtls(const aValue: boolean);
begin
  if fVtls = aValue then exit;
  fVtls := aValue;
  doChanged;
end;

procedure TMsgOpts.setQuiet(const aValue: boolean);
begin
  if fQuiet = aValue then exit;
  fQuiet := aValue;
  doChanged;
end;

(*******************************************************************************
 * TOutputOpts
 *)
procedure TOutputOpts.getOpts(const aList: TStrings);
var
  opt: string;
const
  trgKindStr: array[TTargetSystem] of string = ('', '-m32','-m64');
  binKindStr: array[TBinaryKind] of string = ('', '-lib', '-shared', '-c');
begin
  opt := binKindStr[fBinKind];
  if opt <> '' then aList.Add(opt);
  opt := trgKindStr[fTrgKind];
  if opt <> '' then aList.Add(opt);
  if fUt then aList.Add('-unittest');
  if fVerId <> '' then aList.Add('-version=' + fVerId);
  if fInline then aList.Add('-inline');
  if fNoBounds then aList.Add('-noboundscheck');
  if fOptimz then aList.Add('-O');
  if fGenStack then aList.Add('-gs');
  if fMain then aList.Add('-main');
  if fRelease then aList.Add('-release');
end;

procedure TOutputOpts.assign(aValue: TPersistent);
var
  src: TOutputOpts;
begin
  if (aValue is TOutputOpts) then
  begin
    src := TOutputOpts(aValue);
    fBinKind := src.fBinKind;
    fTrgKind := src.fTrgKind;
    fUt := src.fUt;
    fVerId := src.fVerId;
    fInline := src.fInline;
    fNoBounds := src.fNoBounds;
    fOptimz := src.fOptimz;
    fGenStack := src.fGenStack;
    fMain := src.fMain;
    fRelease := src.fRelease;
  end
  else inherited;
end;

procedure TOutputOpts.setUt(const aValue: boolean);
begin
  if fUt = aValue then exit;
  fUt := aValue;
  doChanged;
end;

procedure TOutputOpts.setVerId(const aValue: string);
begin
  if fVerId = aValue then exit;
  fVerId := aValue;
  doChanged;
end;

procedure TOutputOpts.setTrgKind(const aValue: TTargetSystem);
begin
  if fTrgKind = aValue then exit;
  fTrgKind := aValue;
  doChanged;
end;

procedure TOutputOpts.setBinKind(const aValue: TBinaryKind);
begin
  if fBinKind = aValue then exit;
  fBinKind := aValue;
  doChanged;
end;

procedure TOutputOpts.setInline(const aValue: boolean);
begin
  if fInline = aValue then exit;
  fInline := aValue;
  doChanged;
end;

procedure TOutputOpts.setNoBounds(const aValue: boolean);
begin
  if fNoBounds = aValue then exit;
  fNoBounds := aValue;
  doChanged;
end;

procedure TOutputOpts.setOptims(const aValue: boolean);
begin
  if fOptimz = aValue then exit;
  fOptimz := aValue;
  doChanged;
end;

procedure TOutputOpts.setGenStack(const aValue: boolean);
begin
  if fGenStack = aValue then exit;
  fGenStack := aValue;
  doChanged;
end;

procedure TOutputOpts.setMain(const aValue: boolean);
begin
  if fMain = aValue then exit;
  fMain := aValue;
  doChanged;
end;

procedure TOutputOpts.setRelease(const aValue: boolean);
begin
  if fRelease = aValue then exit;
  fRelease := aValue;
  doChanged;
end;

(*******************************************************************************
 * TDebugOpts
 *)
procedure TDebugOpts.getOpts(const aList: TStrings);
begin
  if fDbg then aList.Add('-debug');
  if fDbgIdent <> '' then aList.Add('-debug=' + fDbgIdent);
  if fDbgD then aList.Add('-g');
  if fDbgC then aList.Add('-gc');
  if fMap then aList.Add('-map');
end;

procedure TDebugOpts.assign(aValue: TPersistent);
var
  src: TDebugOpts;
begin
  if (aValue is TDebugOpts) then
  begin
    src := TDebugOpts(aValue);
    fDbg := src.fDbg;
    fDbgIdent := src.fDbgIdent;
    fDbgD := src.fDbgD;
    fDbgC := src.fDbgC;
    fMap := src.fMap;
  end
  else inherited;
end;

procedure TDebugOpts.setDbg(const aValue: boolean);
begin
  if fDbg = aValue then exit;
  fDbg := aValue;
  doChanged;
end;

procedure TDebugOpts.setDbgIdent(const aValue: string);
begin
  if fDbgIdent = aValue then exit;
  fDbgIdent := aValue;
  doChanged;
end;

procedure TDebugOpts.setDbgD(const aValue: boolean);
begin
  if fDbgD = aValue then exit;
  fDbgD := aValue;
  doChanged;
end;

procedure TDebugOpts.setDbgC(const aValue: boolean);
begin
  if fDbgC = aValue then exit;
  fDbgC := aValue;
  doChanged;
end;

procedure TDebugOpts.setMap(const aValue: boolean);
begin
  if fMap = aValue then exit;
  fMap := aValue;
  doChanged;
end;

(*******************************************************************************
 * TPathsOpts
 *)
constructor TPathsOpts.create;
begin
  fSrcs := TStringList.Create;
  fIncl := TStringList.Create;
  fImpt := TStringList.Create;
end;

procedure TPathsOpts.getOpts(const aList: TStrings);
var
  str: string;
begin
  for str in fSrcs do if str <> '' then
    aList.Add(str);
  for str in fIncl do if str <> '' then
    aList.Add('-I'+ str);
  for str in fImpt do if str <> '' then
    aList.Add('-J'+ str);
  if fFname <> '' then aList.Add('-of' + fFname);
  if fObjDir <> '' then aList.Add('-od' + fObjDir);
end;

procedure TPathsOpts.assign(aValue: TPersistent);
var
  src: TPathsOpts;
begin
  if (aValue is TPathsOpts) then
  begin
    src := TPathsOpts(aValue);
    fSrcs.Assign(src.fSrcs);
    fIncl.Assign(src.fIncl);
    fImpt.Assign(src.fImpt);
    fFName := patchPlateformPath(src.fFname);
    fObjDir := patchPlateformPath(src.fObjDir);
  end
  else inherited;
end;

destructor TPathsOpts.destroy;
begin
  fSrcs.free;
  fIncl.free;
  fImpt.free;
  inherited;
end;

procedure TPathsOpts.setFname(const aValue: string);
begin
  if fFname = aValue then exit;
  fFname := patchPlateformPath(aValue);
  doChanged;
end;

procedure TPathsOpts.setObjDir(const aValue: string);
begin
  if fObjDir = aValue then exit;
  fObjDir := patchPlateformPath(aValue);
  doChanged;
end;

procedure TPathsOpts.setSrcs(const aValue: TStringList);
begin
  fSrcs.Assign(aValue);
  patchPlateformPaths(fSrcs);
  doChanged;
end;

procedure TPathsOpts.setIncl(const aValue: TStringList);
begin
  fIncl.Assign(aValue);
  patchPlateformPaths(fIncl);
  doChanged;
end;

procedure TPathsOpts.setImpt(const aValue: TStringList);
begin
  fImpt.Assign(aValue);
  patchPlateformPaths(fImpt);
  doChanged;
end;

(*******************************************************************************
 * TOtherOpts
 *)
constructor TOtherOpts.create;
begin
  fCustom := TStringList.Create;
end;

procedure TOtherOpts.assign(aValue: TPersistent);
var
  src: TOtherOpts;
begin
  if (aValue is TOtherOpts) then
  begin
    src := TOtherOpts(aValue);
    fCustom.Assign(src.fCustom);
  end
  else inherited;
end;

destructor TOtherOpts.destroy;
begin
  fCustom.Destroy;
  inherited;
end;

procedure TOtherOpts.getOpts(const aList: TStrings);
var
  str: string;
begin
  for str in fCustom do if str <> '' then
    aList.Add(str);
end;

procedure TOtherOpts.setCustom(const aValue: TStringList);
begin
  fCustom.Assign(aValue);
  doChanged;
end;

(*******************************************************************************
 * TCompilerConfiguration
 *)
constructor TCompilerConfiguration.create(aCollection: TCollection);
begin
  inherited create(aCollection);

  fDocOpts    := TDocOpts.create;
  fDebugOpts  := TDebugOpts.create;
  fMsgOpts    := TMsgOpts.create;
  fOutputOpts := TOutputOpts.create;
  fPathsOpts  := TPathsOpts.create;
  fOthers     := TOtherOpts.create;

  fDocOpts.onChange     := @subOptsChanged;
  fDebugOpts.onChange   := @subOptsChanged;
  fMsgOpts.onChange     := @subOptsChanged;
  fOutputOpts.onChange  := @subOptsChanged;
  fPathsOpts.onChange   := @subOptsChanged;
  fOthers.onChange      := @subOptsChanged;

  fName := nameFromID;
end;

destructor TCompilerConfiguration.destroy;
begin
  fOnChanged := nil;
  fDocOpts.free;
  fDebugOpts.free;
  fMsgOpts.free;
  fOutputOpts.free;
  fPathsOpts.free;
  fOthers.free;
  inherited;
end;

procedure TCompilerConfiguration.assign(aValue: TPersistent);
var
  src: TCompilerConfiguration;
begin
  if (aValue is TCompilerConfiguration) then
  begin
    src := TCompilerConfiguration(aValue);
    fDocOpts.assign(src.fDocOpts);
    fDebugOpts.assign(src.fDebugOpts);
    fMsgOpts.assign(src.fMsgOpts);
    fOutputOpts.assign(src.fOutputOpts);
    fPathsOpts.assign(src.fPathsOpts);
    fOthers.assign(src.fOthers);
  end
  else inherited;
end;

function TCompilerConfiguration.nameFromID: string;
begin
  result := format('<configuration %d>',[ID]);
end;

procedure TCompilerConfiguration.getOpts(const aList: TStrings);
begin
  fDocOpts.getOpts(aList);
  fDebugOpts.getOpts(aList);
  fMsgOpts.getOpts(aList);
  fOutputOpts.getOpts(aList);
  fPathsOpts.getOpts(aList);
  fOthers.getOpts(aList);
end;

procedure TCompilerConfiguration.setName(const aValue: string);
begin
  if fName = aValue then exit;
  fName := aValue;
  if fName = '' then fName := nameFromID;
  Changed(true);
  doChanged;
end;

procedure TCompilerConfiguration.subOptsChanged(sender: TObject);
begin
  Changed(true);
  doChanged;
end;

procedure TCompilerConfiguration.doChanged;
begin
  if assigned(fOnChanged) then fOnChanged(self);
end;

procedure TCompilerConfiguration.setDocOpts(const aValue: TDocOpts);
begin
  fDocOpts.assign(aValue);
end;

procedure TCompilerConfiguration.setDebugOpts(const aValue: TDebugOpts);
begin
  fDebugOpts.assign(aValue);
end;

procedure TCompilerConfiguration.setMsgOpts(const aValue: TMsgOpts);
begin
  fMsgOpts.assign(aValue);
end;

procedure TCompilerConfiguration.setOutputOpts(const aValue: TOutputOpts);
begin
  fOutputOpts.assign(aValue);
end;

procedure TCompilerConfiguration.setPathsOpts(const aValue: TPathsOpts);
begin
  fPathsOpts.assign(aValue);
end;

procedure TCompilerConfiguration.setOthers(const aValue: TOtherOpts);
begin
  fOthers.Assign(aValue);
end;

initialization
  RegisterClasses([TCompilerConfiguration, TOtherOpts, TPathsOpts,
    TDebugOpts, TOutputOpts, TMsgOpts, TDocOpts]);
end.
