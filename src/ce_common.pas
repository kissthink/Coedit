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
    procedure projChange(const aProject: TCEProject);
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
    fSrcs, fSrcsCop: TStringList; // an editor can be associated to a file using the Object[] property
    fConfIx: Integer;
    procedure doChanged;
    procedure subMemberChanged(sender : TObject);
    procedure setOptsColl(const aValue: TCollection);
    procedure setFname(const aValue: string);
    procedure setSrcs(const aValue: TStringList);
    procedure setConfIx(aValue: Integer);
    function getConfig(const ix: integer): TCompilerConfiguration;
    function getSrcs: TStringList;
  published
    property OptionsCollection: TCollection read fOptsColl write setOptsColl;
    property Sources: TStringList read fSrcs write setSrcs; // 'read' should return a copy to avoid abs/rel errors
    property ConfigurationIndex: Integer read fConfIx write setConfIx;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure reset;
    function getAbsoluteSourceName(const aIndex: integer): string;
    procedure addSource(const aFilename: string);
    function addConfiguration: TCompilerConfiguration;
    function getOpts: string;
    //
    property configuration[ix: integer]: TCompilerConfiguration read getConfig;
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
    str2.ReadComponent(aComp);
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

// TODO: comments handling
function getModuleName(const aSource: TStrings): string;
var
  ln: string;
  pos: NativeInt;
  id: string;
  tok: boolean;
begin
  result := '';
  tok := false;
  for ln in aSource do
  begin
    pos := 1;
    id := '';

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

      if tok then if ln[pos] = ';'then
      begin
        result := id;
        exit;
      end;

      id += ln[pos];
      Inc(pos);

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
  fSrcsCop := TStringList.Create;
  fSrcs.OnChange := @subMemberChanged;
  fOptsColl := TCollection.create(TCompilerConfiguration);
  reset;
end;

destructor TCEProject.destroy;
begin
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
begin
  fOptsColl.Assign(aValue);
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
  doChanged;
end;

procedure TCEProject.setSrcs(const aValue: TStringList);
begin
  fSrcs.Assign(aValue);
  doChanged;
end;

procedure TCEProject.setConfIx(aValue: Integer);
begin
  if fConfIx = aValue then exit;
  if aValue < 0 then aValue := 0;
  if aValue > fOptsColl.Count-1 then aValue := fOptsColl.Count-1;
  fConfIx := aValue;
  doChanged;
end;

procedure TCEProject.subMemberChanged(sender : TObject);
begin
  fModified := true;
  doChanged;
end;

procedure TCEProject.doChanged;
begin
  fModified := true;
  if assigned(fOnChange) then fOnChange(Self);
end;

function TCEProject.getConfig(const ix: integer): TCompilerConfiguration;
begin
  result := TCompilerConfiguration(fOptsColl.Items[ix]);
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
  fOptsColl.Clear;
  defConf := addConfiguration;
  defConf.name := 'default';
  fSrcs.Clear;
  fFilename := '';
  fModified := true;
  fConfIx := 0;
  doChanged;
end;

function TCEProject.getOpts: string;
var
  rel, abs: string;
begin
  result := '';
  for rel in fSrcs do
  begin
    abs := expandFilenameEx(fBasePath,rel);
    result += '"' + abs + '"';
  end;
  result += TCompilerConfiguration(fOptsColl.Items[fConfIx]).getOpts;
end;

function TCEProject.getAbsoluteSourceName(const aIndex: integer): string;
begin
  if aIndex < 0 then exit;
  if aIndex > fSrcs.Count-1 then exit;
  result := expandFileNameEx(fBasePath,fSrcs.Strings[aIndex]);
end;

end.

