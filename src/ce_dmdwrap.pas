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
    function getOpts: string; virtual; abstract;
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
    function getOpts: string; override;
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
    fDepHandling : TDepHandling; // could be also related to analysis
    fVerb: boolean;
    fWarn: boolean;
    fWarnEx: boolean;
    fVtls: boolean;
    fQuiet: boolean;
    fProp: boolean;
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
    function getOpts: string; override;
  end;

  (*****************************************************************************
   * Encapsulates the options/args related to the analysis & the code gen.
   *)
  TOutputOpts= class(TOptsGroup)
  private
    fInline: boolean;
    fNoBounds: boolean;
    fOptims: boolean;
    fGenStack: boolean;
    fMain: boolean;
    fRelease: boolean;
    procedure setInline(const aValue: boolean);
    procedure setNoBounds(const aValue: boolean);
    procedure setOptims(const aValue: boolean);
    procedure setGenStack(const aValue: boolean);
    procedure setMain(const aValue: boolean);
    procedure setRelease(const aValue: boolean);
  published
    property inlining: boolean read fInline write setInline;
    property noBoundsCheck: boolean read fNoBounds write setNoBounds;
    property optimisations: boolean read fOptims write setOptims;
    property generateStackFrame: boolean read fGenStack write setGenStack;
    property addMain: boolean read fMain write setMain;
    property release: boolean read fRelease write setRelease;
  public
    function getOpts: string; override;
  end;

  (*****************************************************************************
   * Describes the target registry size
   *)
  TTargetSystem = (auto, os32bit, os64bit);
  (**
   * Describes the output kind
   *)
  TBinaryKind = (executable, staticlib, sharedlib);

  (**
   * Encapsulates the options/args related to the debuging
   *)
  TDebugOpts = class(TOptsGroup)
  private
    fDbg: boolean;
    fDbgIdent: string;
    fDbgD: boolean;
    fDbgC: boolean;
    fMap: boolean;
  published
  public
    //function getOpts: string; override;
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
    function getOpts: string; override;
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
    function getOpts: string; override;
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
    function getCmdLine: string;
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
    property cmdLine: string read getCmdLine;
    property onChanged: TNotifyEvent read fOnChanged write fOnChanged;
  end;

implementation

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
function TDocOpts.getOpts: string;
begin
  result := '';
  if fGenDoc then result += '-D ';
  if fGenJson then result += '-X ';
  if fDocDir <> '' then result += '-Dd' + '"' + fDocDir + '" ';
  if fJsonFname <> '' then result += '-Xf' + '"'+ fJsonFname + '" ';
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
  fDocDir := aValue;
  doChanged;
end;

procedure TDocOpts.setJSONFile(const aValue: string);
begin
  if fJsonFname = aValue then exit;
  fJsonFname := aValue;
  doChanged;
end;

(*******************************************************************************
 * TMsgOpts
 *)
function TMsgOpts.getOpts: string;
const
  DepStr : array[TDepHandling] of string = ('-d ','-dw ','-de ');
begin
  result := DepStr[fDepHandling];
  if fVerb then result += '-v ';
  if fWarn then result += '-w ';
  if fWarnEx then result += '-wi ';
  if fVtls then result += '-vtls ';
  if fQuiet then result += '-quiet ';
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
function TOutputOpts.getOpts: string;
begin
  result := '';
  if fInline then result += '-inline ';
  if fNoBounds then result += '-noboundscheck ';
  if fOptims then result += '-O ';
  if fGenStack then result += '-gs ';
  if fMain then result += '-main ';
  if fRelease then result += '-release ';
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
  if fOptims = aValue then exit;
  fOptims := aValue;
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
 * TPathsOpts
 *)
function TPathsOpts.getOpts: string;
var
  str: string;
begin
  result := '';
  for str in fSrcs do
    result += '"'+ str +'" ';
  for str in fIncl do
    result += '-I"'+ str +'" ';
  for str in fImpt do
    result += '-J"'+ str +'" ';
  if fFname <> '' then result += '-of"' + fFname + '" ';
  if fObjDir <> '' then result += '-od"' + fObjDir + '" ';
end;

constructor TPathsOpts.create;
begin
  fSrcs := TStringList.Create;
  fIncl := TStringList.Create;
  fImpt := TStringList.Create;
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
  fFname := aValue;
  doChanged;
end;

procedure TPathsOpts.setObjDir(const aValue: string);
begin
  if fObjDir = aValue then exit;
  fObjDir := aValue;
  doChanged;
end;

procedure TPathsOpts.setSrcs(const aValue: TStringList);
begin
  fSrcs.Assign(aValue);
  doChanged;
end;

procedure TPathsOpts.setIncl(const aValue: TStringList);
begin
  fIncl.Assign(aValue);
  doChanged;
end;

procedure TPathsOpts.setImpt(const aValue: TStringList);
begin
  fImpt.Assign(aValue);
  doChanged;
end;

(*******************************************************************************
 * TOtherOpts
 *)
constructor TOtherOpts.create;
begin
  fCustom := TStringList.Create;
end;

destructor TOtherOpts.destroy;
begin
  fCustom.Destroy;
  inherited;
end;

function TOtherOpts.getOpts: string;
var
  str: string;
begin
  result := '';
  for str in fCustom do
    result += str + ' ';
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
  fDocOpts.free;
  fDebugOpts.free;
  fMsgOpts.free;
  fOutputOpts.free;
  fPathsOpts.free;
  fOthers.free;
  inherited;
end;

function TCompilerConfiguration.nameFromID: string;
begin
  result := format('<configuration %d>',[ID]);
end;

function TCompilerConfiguration.getCmdLine: string;
begin
   result :=
    fDocOpts.getOpts + (*fDebugOpts.getOpts +*) fMsgOpts.getOpts
    + fOutputOpts.getOpts + fPathsOpts.getOpts + fOthers.getOpts;
   if result[length(result)] = ' ' then
    setlength(result, length(result)-1);
end;

procedure TCompilerConfiguration.setName(const aValue: string);
begin
  if fName = aValue then exit;
  fName := aValue;
  if fName = '' then fName := nameFromID;
  Changed(true);
end;

procedure TCompilerConfiguration.subOptsChanged(sender: TObject);
begin
  Changed(true);
  doChanged;
  {$IFDEF DEBUG}
    writeln( #13#10 + getCmdLine);
  {$ENDIF}
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

end.
