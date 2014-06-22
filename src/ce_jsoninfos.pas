unit ce_jsoninfos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jsonparser, fpjson;

type

  TJSonLoader = class(TThread)
  private
    parser: TJSONParser;
  public
    Filename: string;
    Data: TJsonData;
    procedure Execute; override;
  end;

  TDSourceInfo = record
    line: integer;
    infs: string;
  end;

  TDSourceInfos = array of TDSourceInfo;

  TInfKind = (ikModule, ikImport, ikFunction, ikAlias, ikClass, ikStruct,
    ikMixin, ikEnum, ikTemplate);

const
  InfKindStr: array[TInfKind] of string = ('module', 'import', 'function', 'alias',
    'class', 'structure', 'mixin', 'enum', 'template');

  InfKindNonModule = [ikImport..ikTemplate];

type
  TJSonInfos = class
  private
    fFilenames: TStringList;

    procedure FilesChanged(Sender: TObject);
    procedure scan;
  public
    fData: array of TJsonData;
    constructor create;
    destructor destroy; override;
    //
    function getFileModule(const index: integer): TDSourceInfos;
    function getMembers(const aFileIndex, aModuleIndex: integer; const aKind: TInfKind): TDSourceInfos;
    property Files: TStringList read fFilenames;
  end;

var
  JSONInfos: TJSonInfos;

implementation

procedure TJSonLoader.Execute;
var
  str: TMemoryStream;
begin
  str := TMemoryStream.create;
  parser := TJSONParser.Create(str);
  try
    str.LoadFromFile(Filename);
    Data := parser.parse;
  finally
    str.free;
    parser.free;
  end;
end;

constructor TJSonInfos.create;
begin
  fFilenames := TStringList.Create;
  fFilenames.OnChange := @FilesChanged;
end;

destructor TJSonInfos.destroy;
begin
  fFilenames.Free;
  inherited;
end;

procedure TJSonInfos.FilesChanged(Sender: TObject);
begin
  scan;
end;

procedure TJSonInfos.scan;
var
  fname: string;
  str: TmemoryStream;
begin
  setLength(fData,0);
  str := tMemoryStream.Create;
  for fname in fFilenames do
  begin
    str.LoadFromFile(fname);
    str.Position := 0;
    setLength(fData, length(fData)+1);
    fData[high(fData)] := GetJSON(str);
  end;
end;

function TJSonInfos.getFileModule(const index: integer): TDSourceInfos;
var
  memb: TJsonData;
  i: nativeInt;
begin
  setlength(result,0);
  memb := fData[index].GetPath('');
  for i := 0 to memb.Count-1 do
  begin
    if memb.Items[i].GetPath('kind').AsString <> 'module' then continue;
    setlength(result, length(result) + 1);
    result[high(result)].infs := memb.Items[i].GetPath('name').AsString;
    result[high(result)].line := 0;
  end;
end;

function TJSonInfos.getMembers(const aFileIndex, aModuleIndex: integer; const aKind: TInfKind): TDSourceInfos;
var
  memb: TJsonData;
  i: nativeInt;
begin
  setlength(result,0);
  memb := fData[aFileIndex].items[aModuleIndex].GetPath('members');
  for i := 0 to memb.Count-1 do
  begin
    if memb.Items[i].GetPath('kind').AsString <> InfKindStr[aKind]
      then continue;
    setlength(result, length(result) + 1);
    result[high(result)].infs := memb.Items[i].GetPath('name').AsString;
    if (aKind = ikModule) then result[high(result)].line := 0
    else result[high(result)].line := memb.Items[i].GetPath('line').AsInt64;
  end;
end;


initialization
  JSONInfos := TJSonInfos.create;
finalization
  JSONInfos.Free;
end.

