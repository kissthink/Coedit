unit ce_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ce_dmdwrap, ActnList;

type

  TCEProject = class;

  (**
   * An implementer is informed when a new document is added, focused or closed.
   *)
  ICEMultiDocMonitor = interface
    procedure docChange(const aNewIndex: integer);
    procedure docClose(const aNewIndex: integer);
  end;

  (**
   * An implementer adds some menu actions when its context is valid.
   *)
  ICEContextualActions = interface
    function contextName: string;
    function contextActionCount: integer;
    function contextAction(index: integer): TAction;
  end;

  (**
   * An implementer is informed when a project changes.
   *)
  ICEProjectMonitor = interface
    procedure projNew(const aProject: TCEProject);
    procedure projChange(const aProject: TCEProject);
    procedure projClose(const aProject: TCEProject);
  end;

  (*****************************************************************************
   * Writable project.
   *)
  TCEProject = class(TComponent)
  private
    fOnChange: TNotifyEvent;
    fModified: boolean;
    fFilename: string;
    fBasePath: string;
    fOptsColl: TCollection;
    fSrcs, fSrcsCop: TStringList;
    fConfIx: Integer;
    fChangedCount: NativeInt;
    procedure doChanged;
    procedure subMemberChanged(sender : TObject);
    procedure setOptsColl(const aValue: TCollection);
    procedure setFname(const aValue: string);
    procedure setSrcs(const aValue: TStringList);
    procedure setConfIx(aValue: Integer);
    function getConfig(const ix: integer): TCompilerConfiguration;
    function getSrcs: TStringList;
    function getCurrConf: TCompilerConfiguration;
  published
    property OptionsCollection: TCollection read fOptsColl write setOptsColl;
    property Sources: TStringList read fSrcs write setSrcs; // 'read' should return a copy to avoid abs/rel errors
    property ConfigurationIndex: Integer read fConfIx write setConfIx;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure beforeChanged;
    procedure afterChanged;
    procedure reset;
    function getAbsoluteSourceName(const aIndex: integer): string;
    function getAbsoluteFilename(const aFilename: string): string;
    procedure addSource(const aFilename: string);
    function addConfiguration: TCompilerConfiguration;
    function getOpts: string;
    //
    property configuration[ix: integer]: TCompilerConfiguration read getConfig;
    property currentConfiguration: TCompilerConfiguration read getCurrConf;
    property fileName: string read fFilename write setFname;
    property onChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  procedure saveCompToTxtFile(const aComp: TComponent; const aFilename: string);
  procedure loadCompFromTxtFile(const aComp: TComponent; const aFilename: string);
  function expandFilenameEx(const aBasePath, aFilename: string): string;
  function getModuleName(const aSource: TStrings): string;

implementation

(*****************************************************************************
 * Routines
 *)
procedure saveCompToTxtFile(const aComp: TComponent; const aFilename: string);
var
  str1, str2: TMemoryStream;
begin
  str1 := TMemoryStream.Create;
  str2 := TMemoryStream.Create;
  try
    str1.WriteComponent(aComp);
    str1.Position := 0;
    ObjectBinaryToText(str1,str2);
    str2.SaveToFile(aFilename);
  finally
    str1.Free;
    str2.Free;
  end;
end;

procedure loadCompFromTxtFile(const aComp: TComponent; const aFilename: string);
var
  str1, str2: TMemoryStream;
begin
  str1 := TMemoryStream.Create;
  str2 := TMemoryStream.Create;
  try
    str1.LoadFromFile(aFilename);
    str1.Position := 0;
    ObjectTextToBinary(str1,str2);
    str2.Position := 0;
    try
      str2.ReadComponent(aComp);
    except
    end;
  finally
    str1.Free;
    str2.Free;
  end;
end;

function expandFilenameEx(const aBasePath, aFilename: string): string;
var
  curr: string;
begin
  curr := '';
  getDir(0,curr);
  try
    if curr <> aBasePath then
      chDir(aBasePath);
    result := expandFileName(aFilename);
  finally
    chDir(curr);
  end;
end;

// TODO: block comments handling
function getModuleName(const aSource: TStrings): string;
var
  ln: string;
  pos, lcnt: NativeInt;
  id: string;
  tok: boolean;
begin
  result := '';
  tok := false;
  lcnt := -1;
  for ln in aSource do
  begin
    pos := 1;
    id := '';
    lcnt += 1;
    if lcnt > 100 then exit;

    while(true) do
    begin
      if pos > length(ln) then
        break;

      if ln[pos] in [#0..#32] then
      begin
        Inc(pos);
        id := '';
        continue;
      end;

      if tok then if ln[pos] = ';' then
        exit(id);

      id += ln[pos];
      Inc(pos);

      if id = '//' then
      begin
        Inc(pos, length(ln));
        break;
      end;

      if id = 'module' then
      begin
        tok := true;
        id := '';
        continue;
      end;

    end;
  end;
end;

(*****************************************************************************
 * TProject
 *)
constructor TCEProject.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  fSrcs := TStringList.Create;
  fSrcs.OnChange := @subMemberChanged;
  fSrcsCop := TStringList.Create;
  fOptsColl := TCollection.create(TCompilerConfiguration);
  reset;
end;

destructor TCEProject.destroy;
begin
  fOnChange := nil;
  fSrcs.free;
  fSrcsCop.Free;
  fOptsColl.free;
  inherited;
end;

function TCEProject.addConfiguration: TCompilerConfiguration;
begin
  result := TCompilerConfiguration(fOptsColl.Add);
  result.onChanged := @subMemberChanged;
end;

procedure TCEProject.setOptsColl(const aValue: TCollection);
var
  i: nativeInt;
begin
  fOptsColl.Assign(aValue);
  for i:= 0 to self.fOptsColl.Count-1 do
    Configuration[i].onChanged := @subMemberChanged;
end;

procedure TCEProject.addSource(const aFilename: string);
var
  relSrc, absSrc: string;
begin
  for relSrc in fSrcs do
  begin
    absSrc := expandFilenameEx(fBasePath,relsrc);
    if aFilename = absSrc then exit;
  end;
  fSrcs.Add(ExtractRelativepath(fBasePath,aFilename));
end;

procedure TCEProject.setFname(const aValue: string);
var
  oldAbs, newRel, oldBase: string;
  i: NativeInt;
begin
  if fFilename = aValue then exit;
  //
  beforeChanged;

  fFilename := aValue;
  oldBase := fBasePath;
  fBasePath := extractFilePath(fFilename);
  //
  for i:= 0 to fSrcs.Count-1 do
  begin
    oldAbs := expandFilenameEx(oldBase,fSrcs[i]);
    newRel := ExtractRelativepath(fBasePath, oldAbs);
    fSrcs[i] := newRel;
  end;
  //
  afterChanged;
end;

procedure TCEProject.setSrcs(const aValue: TStringList);
begin
  beforeChanged;
  fSrcs.Assign(aValue);
  afterChanged;
end;

procedure TCEProject.setConfIx(aValue: Integer);
begin
  if fConfIx = aValue then exit;
  beforeChanged;
  if aValue < 0 then aValue := 0;
  if aValue > fOptsColl.Count-1 then aValue := fOptsColl.Count-1;
  fConfIx := aValue;
  afterChanged;
end;

procedure TCEProject.subMemberChanged(sender : TObject);
begin
  beforeChanged;
  fModified := true;
  afterChanged;
end;

procedure TCEProject.beforeChanged;
begin
  Inc(fChangedCount);
end;

procedure TCEProject.afterChanged;
begin
  Dec(fChangedCount);
  if fChangedCount > 0 then
  begin
    {$IFDEF DEBUG}
    writeln('project update count > 0');
    {$ENDIF}
    exit;
  end;
  fChangedCount := 0;
  doChanged;
end;

procedure TCEProject.doChanged;
begin
  fModified := true;
  if assigned(fOnChange) then fOnChange(Self);
  {$IFDEF DEBUG}
  writeln(getOpts);
  {$ENDIF}
end;

function TCEProject.getConfig(const ix: integer): TCompilerConfiguration;
begin
  result := TCompilerConfiguration(fOptsColl.Items[ix]);
  result.onChanged := @subMemberChanged;
end;

function TCEProject.getCurrConf: TCompilerConfiguration;
begin
  result := TCompilerConfiguration(fOptsColl.Items[fConfIx]);
end;

function TCEProject.getSrcs: TStringList;
var
  str: TMemoryStream;
begin
  if not (csReading in componentState) or (csWriting in componentState) then
  begin
    str := TMemoryStream.Create;
    try
      fSrcs.SaveToStream(str);
      str.Position:=0;
      fSrcsCop.Clear;
      fSrcsCop.LoadFromStream(str);
    finally
      str.Free;
    end;
    result := fSrcsCop;
  end
  else result := fSrcs;
end;

procedure TCEProject.reset;
var
  defConf: TCompilerConfiguration;
begin
  beforeChanged;
  fConfIx := 0;
  fOptsColl.Clear;
  defConf := addConfiguration;
  defConf.name := 'default';
  fSrcs.Clear;
  fFilename := '';
  afterChanged;
end;

function TCEProject.getOpts: string;
var
  rel, abs: string;
begin
  result := '';
  if fConfIx = -1 then exit;
  for rel in fSrcs do
  begin
    abs := expandFilenameEx(fBasePath,rel);
    result += '"' + abs + '" ' ;
  end;
  result += TCompilerConfiguration(fOptsColl.Items[fConfIx]).getOpts;
end;

function TCEProject.getAbsoluteSourceName(const aIndex: integer): string;
begin
  if aIndex < 0 then exit('');
  if aIndex > fSrcs.Count-1 then exit('');
  result := expandFileNameEx(fBasePath, fSrcs.Strings[aIndex]);
end;

function TCEProject.getAbsoluteFilename(const aFilename: string): string;
begin
  result := expandFileNameEx(fBasePath, aFilename);
end;

end.

