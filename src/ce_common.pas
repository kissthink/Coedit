unit ce_common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ActnList, dialogs, forms;

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
   * Extracts the module name of a D source file.
   *)
  function getModuleName(const aSource: TStrings): string;

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
    * Returns an unique object identifier, based on its heap address.
    *)
   function uniqueObjStr(const aObject: Tobject): string;


implementation

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

function patchPlateformPath(const aPath: string): string;
var
  i: Integer;
begin
  result := aPath;
  {$IFDEF MSWINDOWS}
  i := pos('/',result);
  if i <> 0 then
  begin
    repeat
      result[i] := directorySeparator;
      i := pos('/',result);
    until
      i = 0;
  end;
  i := pos(':',result);
  if i <> 0 then
  begin
    repeat
      result[i] := directorySeparator;
      i := pos(':',result);
    until
      i = 0;
  end;
  {$ENDIF}

  {$IFDEF LINUX}
  i := pos('\',result);
  if i <> 0 then
  begin
    repeat
      result[i] := directorySeparator;
      i := pos('\',result);
    until
      i = 0;
  end;
  i := pos(':',result);
  if i <> 0 then
  begin
    repeat
      result[i] := directorySeparator;
      i := pos(':',result);
    until
      i = 0;
  end;
  {$ENDIF}

  {$IFDEF MACOS}
  i := pos('\',result);
  if i <> 0 then
  begin
    repeat
      result[i] := directorySeparator;
      i := pos('\',result);
    until
      i = 0;
  end;
  i := pos('/',result);
  if i <> 0 then
  begin
    repeat
      result[i] := directorySeparator;
      i := pos('/',result);
    until
      i = 0;
  end;
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

function dlgOkCancel(const aMsg: string): TModalResult;
const
  Btns = [mbOK,mbCancel];
begin
  exit( MessageDlg('Coedit', aMsg, mtConfirmation, Btns, ''));
end;

function uniqueObjStr(const aObject: Tobject): string;
begin
  {$HINTS OFF}{$WARNINGS OFF}
  exit( format('%.8X',[NativeUint(@aObject)]));
  {$HINTS ON}{$WARNINGS ON}
end;

end.
