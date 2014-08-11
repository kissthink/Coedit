unit ce_libman;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ce_common, ce_dcd;

type

  (**
   * Represents a D static library. In a project libAlias allows to
   * resolve automatically the dependencies of a project.
   *)
  TLibraryItem = class(TCollectionItem)
  private
    fAlias: string;
    fSourcePath: string;
    fLibFile: string;
  published
    property libAlias: string read fAlias write fAlias;
    property libSourcePath: string read fSourcePath write fSourcePath;
    property libFile: string read fLibFile write fLibFile;
  end;

  (**
   * Represents all the D libraries present on this system.
   *)
  TLibraryManager = class(TComponent)
  private
    fCol: TCollection;
    procedure setCol(const aValue: TCollection);
    procedure readerPropNoFound(Reader: TReader; Instance: TPersistent;
      var PropName: string; IsPath: boolean; var Handled, Skip: Boolean);
    procedure readerError(Reader: TReader; const Message: string;
      var Handled: Boolean);
  published
    property libraries: TCollection read fCol write setCol;
  public
    constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    //
    procedure getLibFiles(const someAliases, aList: TStrings);
    procedure getLibSources(const someAliases, aList: TStrings);
    //
    procedure loadFromFile(const aFilename: string);
    procedure saveToFile(const aFilename: string);
    //
    procedure updateDCD;
  end;

implementation

constructor TLibraryManager.create(aOwner: TComponent);
begin
  inherited;
  fCol := TCollection.Create(TLibraryItem);
end;

destructor TLibraryManager.destroy;
begin
  fCol.Free;
  inherited;
end;

procedure TLibraryManager.setCol(const aValue: TCollection);
begin
  fCol.assign(aValue);
end;

procedure TLibraryManager.updateDCD;
var
  itm: TLibraryItem;
  i: NativeInt;
begin
  if not dcdOn then exit;
  //
  ce_dcd.freeServer;
  ce_dcd.createServer;
  for i := 0 to fCol.Count-1 do
  begin
    itm := TLibraryItem(fCol.Items[i]);
    ce_dcd.addDcdImport(itm.libSourcePath);
  end;

end;

procedure TLibraryManager.getLibFiles(const someAliases, aList: TStrings);
var
  itm: TLibraryItem;
  lst: TStringList;
  i, j: NativeInt;
begin
  for i := 0 to fCol.Count-1 do
  begin
    itm := TLibraryItem(fCol.Items[i]);
    if someAliases <> nil then
      if someAliases.IndexOf(itm.libAlias) = -1 then continue;
    // single lib files
    if fileExists(itm.libFile) then
    begin
      if aList.IndexOf(itm.libFile) <> -1 then continue;
      aList.Add(itm.libFile);
    end
    // folder of lib file
    else if directoryExists(itm.libFile) then
    begin
      lst := TStringList.Create;
      try
        listFiles(lst, itm.libFile);
        for j:= 0 to lst.Count-1 do
        begin
          {$IFDEF WINDOWS}
          if extractFileExt(lst.Strings[j]) = '.lib' then
          {$ELSE}
          if extractFileExt(lst.Strings[j]) = '.a' then
          {$ENDIF}
            if aList.IndexOf(lst.Strings[j]) = -1 then
              aList.Add(lst.Strings[j]);
        end;
      finally
        lst.Free;
      end;
    end;
  end;
end;

procedure TLibraryManager.getLibSources(const someAliases, aList: TStrings);
var
  itm: TLibraryItem;
  i: NativeInt;
begin
  for i := 0 to fCol.Count-1 do
  begin
    itm := TLibraryItem(fCol.Items[i]);
    if someAliases <> nil then
      if someAliases.IndexOf(itm.libAlias) = -1 then continue;
    //
    if aList.IndexOf(itm.libSourcePath) <> -1 then continue;
    aList.Add('-I' + itm.libSourcePath);
  end;
end;

procedure TLibraryManager.readerPropNoFound(Reader: TReader; Instance: TPersistent;
      var PropName: string; IsPath: boolean; var Handled, Skip: Boolean);
begin
  Skip := true;
  Handled := false;
end;

procedure TLibraryManager.readerError(Reader: TReader; const Message: string;
  var Handled: Boolean);
begin
  Handled := true;
end;

procedure TLibraryManager.loadFromFile(const aFilename: string);
begin
  loadCompFromTxtFile(self, aFilename, @readerPropNoFound, @readerError);
  updateDCD;
end;

procedure TLibraryManager.saveToFile(const aFilename: string);
begin
  saveCompToTxtFile(self, aFilename);
end;

initialization
  registerClasses([TLibraryManager, TLibraryItem]);
end.
