unit ce_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ActnList, dialogs, forms, controls;

const

  DdiagFilter = 'D source|*.d|D interface|*.di|All files|*.*';

type

  (**
   * MRU list for strings
   *)
  TMRUList = class(TStringList)
  private
    fMaxCount: Integer;
    fObj: TObject;
  protected
    fChecking: boolean;
    procedure clearOutOfRange;
    procedure setMaxCount(aValue: Integer);
    function checkItem(const S: string): boolean; virtual;
    procedure Put(Index: Integer; const S: string); override;
  published
    property maxCount: Integer read fMaxCount write setMaxCount;
  public
    constructor Create;
    procedure Insert(Index: Integer; const S: string); override;
    property objectTag: TObject read fObj write fObj;
  end;

  (**
   * MRU list for filenames
   *)
  TMRUFileList = class(TMRUList)
  protected
    function checkItem(const S: string): boolean; override;
  end;

  (**
   * Save a component with a readable aspect.
   *)
  procedure saveCompToTxtFile(const aComp: TComponent; const aFilename: string);

  (**
   * Load a component.
   *)
  procedure loadCompFromTxtFile(const aComp: TComponent; const aFilename: string);

  (**
   * Converts a relative path to an absolute path.
   *)
  function expandFilenameEx(const aBasePath, aFilename: string): string;

  (**
   * Patches the directory separators from a string.
   * This is used to ensure that a project saved on a platform can be loaded
   * on another one.
   *)
   function patchPlateformPath(const aPath: string): string;
   procedure patchPlateformPaths(const sPaths: TStrings);

   (**
    * Ok/Cancel modal dialog
    *)
   function dlgOkCancel(const aMsg: string): TModalResult;

   (**
    * Info dialog
    *)
   function dlgOkInfo(const aMsg: string): TModalResult;

   (**
    * Returns an unique object identifier, based on its heap address.
    *)
   function uniqueObjStr(const aObject: Tobject): string;

   (**
    * Reduce a filename if its length is over the threshold defined by charThresh.
    * Even if the result is not usable anymore, it avoids any "visually-overloaded" MRU menus.
    *)
   function displayShortFilename(const aPath: string; charThresh: Word = 80): string;

implementation

constructor TMRUList.Create;
begin
  fMaxCount := 10;
end;

procedure TMRUList.clearOutOfRange;
begin
  while Count > fMaxCount do delete(Count-1);
end;

procedure TMRUList.setMaxCount(aValue: Integer);
begin
  if aValue < 0 then aValue := 0;
  if fMaxCount = aValue then exit;
  clearOutOfRange;
end;

function TMRUList.checkItem(const S: string): boolean;
var
  i: NativeInt;
begin
  i := indexOf(S);
  if i = -1 then exit(true);
  if i = 0 then exit(false);
  if Count < 2 then exit(false);
  exchange(i, i-1);
  exit( false);
end;

procedure TMRUList.Put(Index: Integer; const S: string);
begin
  if not (checkItem(S)) then exit;
  inherited;
  clearOutOfRange;
end;

procedure TMRUList.Insert(Index: Integer; const S: string);
begin
  if not (checkItem(S)) then exit;
  inherited;
  clearOutOfRange;
end;

function TMRUFileList.checkItem(const S: string): boolean;
begin
  exit( inherited checkItem(S) and fileExists(S));
end;

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
  getDir(0, curr);
  try
    if curr <> aBasePath then
      chDir(aBasePath);
    result := expandFileName(aFilename);
  finally
    chDir(curr);
  end;
end;

function patchPlateformPath(const aPath: string): string;
function patchProc(const src: string; const invalid: char): string;
var
  i: Integer;
  dir: string;
begin
  dir := ExtractFileDrive(src);
  if length(dir) > 0 then
    result := src[length(dir)+1..length(src)]
  else
    result := src;
  i := pos(invalid, result);
  if i <> 0 then
  begin
    repeat
      result[i] := directorySeparator;
      i := pos(invalid,result);
    until
      i = 0;
  end;
  result := dir + result;
end;
begin
  result := aPath;
  {$IFDEF MSWINDOWS}
  result := patchProc(result, '/');
  result := patchProc(result, ':');
  {$ENDIF}
  {$IFDEF LINUX}
  result := patchProc(result, '\');
  result := patchProc(result, ':');
  {$ENDIF}
  {$IFDEF MACOS}
  result := patchProc(result, '\');
  result := patchProc(result, '/');
  {$ENDIF}
end;

procedure patchPlateformPaths(const sPaths: TStrings);
var
  i: Integer;
  str: string;
begin
  for i:= 0 to sPaths.Count-1 do
  begin
    str := sPaths.Strings[i];
    sPaths.Strings[i] := patchPlateformPath(str);
  end;
end;

function dlgOkCancel(const aMsg: string): TModalResult;
const
  Btns = [mbOK,mbCancel];
begin
  exit( MessageDlg('Coedit', aMsg, mtConfirmation, Btns, ''));
end;

function dlgOkInfo(const aMsg: string): TModalResult;
const
  Btns = [mbOK];
begin
  exit( MessageDlg('Coedit', aMsg, mtInformation, Btns, ''));
end;

function uniqueObjStr(const aObject: Tobject): string;
begin
  {$HINTS OFF}{$WARNINGS OFF}
  exit( format('%.8X',[NativeUint(@aObject)]));
  {$HINTS ON}{$WARNINGS ON}
end;

function displayShortFilename(const aPath: string; charThresh: Word = 80): string;
var
  i: NativeInt;
  sepCnt: NativeInt;
  drv: string;
  pth1: string;
begin
  sepCnt := 0;
  if length(aPath) <= charThresh then
    exit(aPath);

  drv := extractFileDrive(aPath);
  i := length(aPath);
  while(i <> length(drv)+1) do
  begin
    Inc(sepCnt, Byte(aPath[i] = directorySeparator));
    if sepCnt = 2 then break;
    Dec(i);
  end;
  pth1 := aPath[i..length(aPath)];
  exit( format('%s%s...%s',[drv,directorySeparator,pth1]) );
end;

end.
