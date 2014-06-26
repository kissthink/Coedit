unit ce_d2syn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  SynEditHighlighter, SynEditHighlighterFoldBase, SynEditTypes;

const

  D2Kw: array[0..109] of string =
  ( 'abstract', 'alias', 'align', 'asm', 'assert', 'auto',
    'body', 'bool', 'break', 'byte',
    'case', 'cast', 'catch', 'cdouble', 'cent', 'cfloat', 'char', 'class',
    'const', 'continue', 'creal',
    'dchar', 'debug', 'default', 'delegate', 'delete', 'deprecated', 'do', 'double',
    'else', 'enum', 'export', 'extern',
    'false', 'final', 'finally', 'float', 'for', 'foreach',
    'foreach_reverse', 'function',
    'goto',
    'idouble', 'if', 'ifloat', 'immutable', 'import', 'in', 'inout', 'int',
    'interface', 'invariant', 'ireal', 'is',
    'lazy', 'long',
    'macro', 'mixin', 'module',
    'new', 'nothrow', 'null',
    'out', 'override',
    'package', 'pragma', 'private', 'protected', 'ptrdiff_t', 'public', 'pure',
    'real', 'ref', 'return',
    'size_t', 'scope', 'shared', 'short', 'static', 'string', 'struct',
    'super', 'switch', 'synchronized',
    'template', 'this', 'throw', 'true', 'try', 'typedef', 'typeid', 'typeof',
    'ubyte', 'ucent', 'uint', 'ulong', 'union', 'unittest', 'ushort',
    'version', 'void', 'volatile',
    'wchar', 'while', 'with',
    '__FILE__', '__MODULE__', '__LINE__', '__FUNCTION__', '__PRETTY_FUNCTION__'
  );

type


  {$IFDEF USE_DICT_LINKEDCHARMAP}
  PCharMap = ^TCharMap;
  TCharMap = record
    chars: array [Byte] of PCharMap;
  end;

  // slightly fatest then a hash-based-dictionary but huge memory use.
  TD2Dictionary = object
  private
    fRoot: TCharMap;
    fTerm: NativeInt;
    fFreeList: array of pointer;
    fLongest, fShortest: NativeInt;
    procedure addEntry(const aValue: string);
  public
    constructor create;
    destructor destroy;
    function find(const aValue: string): boolean;
  end;
  {$ELSE} {$IFDEF USE_DICT_GPERF}
  // TODO: a perfect hash dictionnary based on gperf
  {$ELSE}
  TD2DictionaryEntry = record
    filled: Boolean;
    values: array of string;
  end;
  TD2Dictionary = object
  private
    fLongest, fShortest: NativeInt;
    fEntries: array[Byte] of TD2DictionaryEntry;
    function toHash(const aValue: string): Byte;
    procedure addEntry(const aValue: string);
  public
    constructor create;
    destructor destroy;
    function find(const aValue: string): boolean;
  end;
  {$ENDIF}
  {$ENDIF}

  TTokenKind = (tkCommt, tkIdent, tkKeywd, tkStrng, tkBlank, tkSymbl, tkNumbr, tkCurrI);

  TRangeKind = (rkNone, rkString1, rkString2, rkBlockCom1, rkBlockCom2, rkAsm);

  TFoldKind = (fkBrackets, fkComments1, fkComments2);
  TFoldKinds = set of TFoldKind;

	TSynD2Syn = class (TSynCustomFoldHighlighter)
	private
    fWhiteAttrib: TSynHighlighterAttributes;
		fNumbrAttrib: TSynHighlighterAttributes;
		fSymblAttrib: TSynHighlighterAttributes;
		fIdentAttrib: TSynHighlighterAttributes;
		fCommtAttrib: TSynHighlighterAttributes;
		fStrngAttrib: TSynHighlighterAttributes;
    fKeywdAttrib: TSynHighlighterAttributes;
    fCurrIAttrib: TSynHighlighterAttributes;
    fKeyWords: TD2Dictionary;
    fCurrIdent: string;
    fLineBuf: string;
    fTokStart, fTokStop: Integer;
    fTokKind: TTokenKind;
    fRange: TRangeKind;
    fFoldKinds: TFoldKinds;
    fAttribLut: array[TTokenKind] of TSynHighlighterAttributes;
    // readNext is mostly used to advanced the reader head.
    function readNext: Char;
    function readCurr: Char;
    function readPrev: Char;
    procedure setFoldKinds(aValue: TFoldKinds);
    procedure setWhiteAttrib(aValue: TSynHighlighterAttributes);
    procedure setNumbrAttrib(aValue: TSynHighlighterAttributes);
    procedure setSymblAttrib(aValue: TSynHighlighterAttributes);
    procedure setIdentAttrib(aValue: TSynHighlighterAttributes);
    procedure setCommtAttrib(aValue: TSynHighlighterAttributes);
    procedure setStrngAttrib(aValue: TSynHighlighterAttributes);
    procedure setKeywdAttrib(aValue: TSynHighlighterAttributes);
    procedure setCurrIAttrib(aValue: TSynHighlighterAttributes);
    procedure doAttribChange(sender: TObject);
    procedure setCurrIdent(const aValue: string);
    procedure doChanged;
	published
    // Defines which kind of range can be folded, among curly brackets, block comments and nested comments
    property FoldKinds: TFoldKinds read fFoldKinds write setFoldKinds;
    property WhiteAttrib: TSynHighlighterAttributes read fWhiteAttrib write setWhiteAttrib;
    property NumbrAttrib: TSynHighlighterAttributes read fNumbrAttrib write setNumbrAttrib;
    property SymblAttrib: TSynHighlighterAttributes read fSymblAttrib write setSymblAttrib;
    property IdentAttrib: TSynHighlighterAttributes read fIdentAttrib write setIdentAttrib;
    property CommtAttrib: TSynHighlighterAttributes read fCommtAttrib write setCommtAttrib;
    property StrngAttrib: TSynHighlighterAttributes read fStrngAttrib write setStrngAttrib;
    property KeywdAttrib: TSynHighlighterAttributes read fKeywdAttrib write setKeywdAttrib;
    property CurrIAttrib: TSynHighlighterAttributes read fCurrIAttrib write setCurrIAttrib;
	public
		constructor create(aOwner: TComponent); override;
    destructor destroy; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    procedure setLine(const NewValue: String; LineNumber: Integer); override;
    procedure next; override;
    function  GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetToken: string; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    function GetEol: Boolean; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function GetRange: Pointer; override;
    property CurrentIdentifier: string read fCurrIdent write setCurrIdent;
	end;

implementation

function isWhite(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  result := c in [#0..#32];
end;

function isSpace(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  result := c in [#9,' '];
end;

function isAlpha(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  result := (c in ['a'..'z']) or (c in ['A'..'Z']);
end;

function isNumber(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  result := (c in ['0'..'9']);
end;

function isDigit(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  result := (c in ['0'..'1']);
end;

function isAlNum(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  result := isAlpha(c) or isNumber(c);
end;

function isHex(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  result := isNumber(c) or (c in ['A'..'F']) or (c in ['a'..'f']);
end;

function isSymbol(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  result := c in [';', '{', '}', '(', ')', '[', ']', ',', '.', ':' , '"', #39, '?'];
end;

function isOperator(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  result := c in ['/', '*', '-', '+', '%', '>', '<', '=', '!',
                  '&', '|', '^', '~', '$'];
end;

function isDoubleOperator(const s: string): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  result := false;
  case s[1] of
    '.': result := (s[2] = '.');

    '>': result := s[2] in ['>', '='];
    '<': result := s[2] in ['<', '=', '>'];
    '=': result := s[2] in ['=', '>'];
    '!': result := s[2] in ['=', '>', '<'];

    '+': result := s[2] in ['+', '='];
    '-': result := s[2] in ['-', '='];
    '/': result := s[2] in ['='];
    '*': result := s[2] in ['='];
    '%': result := s[2] in ['='];
    '~': result := s[2] in ['='];

    '&': result := s[2] in ['&', '='];
    '|': result := s[2] in ['|', '='];
    '^': result := s[2] in ['^', '='];
  end;
end;

function isTripleOperator(const s: string): boolean; {$IFNDEF DEBUG} inline; {$ENDIF}
begin
  result := false;
  case s[1] of
    '.': result := (s[2] = '.')   and (s[3] = '.');
    '^': result := (s[2] = '^')   and (s[3] = '=');
    '>': result := (s[2] = '>')   and (s[3] in ['>', '=']);
    '<': result := ((s[2] = '<')  and (s[3] in ['<', '=']))
                  or (s[2] = '>') and (s[3] = '=');
    '!': result := ((s[2] = '<')  and (s[3] in ['>', '=']))
                  or ((s[2] = '>')and (s[3] = '='));
  end;
end;

function isQuadOperator(const s: string): boolean; {$IFNDEF DEBUG} inline; {$ENDIF}
begin
  result := (s = '>>>=') or (s = '!<>=');
end;

{$IFDEF USE_DICT_LINKEDCHARMAP}
constructor TD2Dictionary.create;
var
  value: string;
  i: NativeInt;
begin
  fTerm := 1;
  for i := 0 to 255 do fRoot.chars[i] := nil;
  for value in D2Kw do
    addEntry(value);
end;

destructor TD2Dictionary.destroy;
var
  i: NativeInt;
begin
  for i := 0 to high(fFreeList) do
    FreeMem(fFreeList[i]);
end;

procedure TD2Dictionary.addEntry(const aValue: string);
var
  len, i, j: NativeInt;
  currMap: PCharMap;
  newMap: PCharMap;
begin
  len := length(aValue);
  if len > fLongest then fLongest := len;
  if len < fShortest then fShortest := len;
  currMap := @fRoot;
  for i := 1 to len do
  begin
    if (currMap^.chars[Byte(aValue[i])] = nil) then
    begin
      newMap := new(PCharMap);
      for j := 0 to 255 do newMap^.chars[j] := nil;
      setLength(fFreeList, length(fFreeList) + 1);
      fFreeList[high(fFreeList)] := newMap;
      currMap^.chars[Byte(aValue[i])] := newMap;
    end;
    if i < len then currMap := currMap^.chars[Byte(aValue[i])];
  end;
  currMap^.chars[0] := @fTerm;
end;

function TD2Dictionary.find(const aValue: string): boolean;
var
  len, i: NativeInt;
  currMap: PCharMap;
begin
  len := length(aValue);
  if len > fLongest then exit(false);
  if len < fShortest then exit(false);
  currMap := @fRoot;
  for i := 1 to len do
  begin
    if currMap^.chars[Byte(aValue[i])] = nil then exit(false);
    if i < len then currMap := currMap^.chars[Byte(aValue[i])];
  end;
  exit( currMap^.chars[0] = @fTerm );
end;
{$ELSE}
constructor TD2Dictionary.create;
var
  value: string;
begin
  for value in D2Kw do
    addEntry(value);
end;

destructor TD2Dictionary.destroy;
begin
end;

{$IFDEF DEBUG}{$R-}{$ENDIF}
function TD2Dictionary.toHash(const aValue: string): Byte;
var
  i: Integer;
begin
  result := 0;
	for i := 1 to length(aValue) do
	  result += (Byte(aValue[i]) shl (4 and (1-i))) xor 25;
end;
{$IFDEF DEBUG}{$R+}{$ENDIF}

procedure TD2Dictionary.addEntry(const aValue: string);
var
  hash: Byte;
begin
  if find(aValue) then exit;
  hash := toHash(aValue);
  fEntries[hash].filled := true;
  setLength(fEntries[hash].values, length(fEntries[hash].values) + 1);
  fEntries[hash].values[high(fEntries[hash].values)] := aValue;
  if fLongest <= length(aValue) then
    fLongest := length(aValue);
  if fShortest >= length(aValue) then
    fShortest := length(aValue);
end;

function TD2Dictionary.find(const aValue: string): boolean;
var
  hash: Byte;
  i: NativeInt;
begin
  result := false;
  if length(aValue) > fLongest then exit;
  if length(aValue) < fShortest then exit;
  hash := toHash(aValue);
  if (not fEntries[hash].filled) then exit(false);
  for i:= 0 to high(fEntries[hash].values) do
    if fEntries[hash].values[i] = aValue then exit(true);
end;
{$ENDIF}


constructor TSynD2Syn.create(aOwner: TComponent);
begin
	inherited create(aOwner);

  DefaultFilter:= 'D source|*.d|D interface|*.di';

  fKeyWords.create;

  fFoldKinds := [fkBrackets];

  fWhiteAttrib := TSynHighlighterAttributes.Create('White','White');
	fNumbrAttrib := TSynHighlighterAttributes.Create('Number','Number');
	fSymblAttrib := TSynHighlighterAttributes.Create('Symbol','Symbol');
	fIdentAttrib := TSynHighlighterAttributes.Create('Identifier','Identifier');
	fCommtAttrib := TSynHighlighterAttributes.Create('Comment','Comment');
	fStrngAttrib := TSynHighlighterAttributes.Create('String','String');
  fKeywdAttrib := TSynHighlighterAttributes.Create('Keyword','Keyword');
  fCurrIAttrib := TSynHighlighterAttributes.Create('CurrentIdentifier','CurrentIdentifier');

  fNumbrAttrib.Foreground := $000079F2;
  fSymblAttrib.Foreground := clMaroon;
  fIdentAttrib.Foreground := clBlack;
  fCommtAttrib.Foreground := clGreen;
  fStrngAttrib.Foreground := clBlue;
  fKeywdAttrib.Foreground := clNavy;

  fCurrIAttrib.Foreground := clBlack;
  fCurrIAttrib.FrameEdges := sfeAround;
  fCurrIAttrib.FrameColor := clGray;
  fCurrIAttrib.Background := cl3dlight;

  fCommtAttrib.Style := [fsItalic];
  fKeywdAttrib.Style := [fsBold];

  AddAttribute(fWhiteAttrib);
  AddAttribute(fNumbrAttrib);
  AddAttribute(fSymblAttrib);
  AddAttribute(fIdentAttrib);
  AddAttribute(fCommtAttrib);
  AddAttribute(fStrngAttrib);
  AddAttribute(fKeywdAttrib);
  AddAttribute(fCurrIAttrib);

  fAttribLut[TTokenKind.tkident] := fIdentAttrib;
  fAttribLut[TTokenKind.tkBlank] := fWhiteAttrib;
  fAttribLut[TTokenKind.tkCommt] := fCommtAttrib;
  fAttribLut[TTokenKind.tkKeywd] := fKeywdAttrib;
  fAttribLut[TTokenKind.tkNumbr] := fNumbrAttrib;
  fAttribLut[TTokenKind.tkStrng] := fStrngAttrib;
  fAttribLut[TTokenKind.tksymbl] := fSymblAttrib;
  fAttribLut[TTokenKind.tkCurrI] := fCurrIAttrib;

  SetAttributesOnChange(@doAttribChange);
  fTokStop := 1;
  next;
end;

destructor TSynD2Syn.destroy;
begin
  fKeyWords.destroy;
  inherited;
end;

procedure TSynD2Syn.doChanged;
begin
  BeginUpdate;
    fUpdateChange := true;
  EndUpdate;
end;

{$HINTS OFF}
procedure TSynD2Syn.doAttribChange(sender: TObject);
begin
  doChanged;
end;
{$HINTS ON}

procedure TSynD2Syn.setFoldKinds(aValue: TFoldKinds);
begin
  fFoldKinds := aValue;
  DoFoldConfigChanged(Self);
  doChanged;
end;

procedure TSynD2Syn.setWhiteAttrib(aValue: TSynHighlighterAttributes);
begin
  fWhiteAttrib.Assign(aValue);
end;

procedure TSynD2Syn.setNumbrAttrib(aValue: TSynHighlighterAttributes);
begin
  fNumbrAttrib.Assign(aValue);
end;

procedure TSynD2Syn.setSymblAttrib(aValue: TSynHighlighterAttributes);
begin
  fSymblAttrib.Assign(aValue);
end;

procedure TSynD2Syn.setIdentAttrib(aValue: TSynHighlighterAttributes);
begin
  fIdentAttrib.Assign(aValue);
end;

procedure TSynD2Syn.setCommtAttrib(aValue: TSynHighlighterAttributes);
begin
  fCommtAttrib.Assign(aValue);
end;

procedure TSynD2Syn.setStrngAttrib(aValue: TSynHighlighterAttributes);
begin
  fStrngAttrib.Assign(aValue);
end;

procedure TSynD2Syn.setKeywdAttrib(aValue: TSynHighlighterAttributes);
begin
  fKeywdAttrib.Assign(aValue);
end;

procedure TSynD2Syn.setCurrIAttrib(aValue: TSynHighlighterAttributes);
begin
  fCurrIAttrib.Assign(aValue);
end;

procedure TSynD2Syn.setCurrIdent(const aValue: string);
begin
  if aValue = '' then exit;
  if fCurrIdent = aValue then Exit;
  fCurrIdent := aValue;
  doChanged;
end;

procedure TSynD2Syn.setLine(const NewValue: String; LineNumber: Integer);
begin
  inherited;
  fLineBuf := NewValue + #10;
  fTokStop := 1;
  next;
end;

{$IFDEF DEBUG}{$R-}{$ENDIF}
function TSynD2Syn.readNext: Char; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  Inc(fTokStop);
  result := fLineBuf[fTokStop];
end;
{$IFDEF DEBUG}{$R+}{$ENDIF}

function TSynD2Syn.readCurr: Char; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  result := fLineBuf[fTokStop];
end;

// unlike readNext, readPrev doesn't change the reader head position.
function TSynD2Syn.readPrev: Char; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  result := fLineBuf[fTokStop-1];
end;

{
TODO:
- alternative attributes for ddoc comments.
- range: asm range.
- number literals: stricter.
- number literals: binary.
- string literals: delimited strings.
- string literals: token strings.
- string literals: escape bug: std.path/std.regex: "\\"
- comments: correct nested comments handling.
}
procedure TSynD2Syn.next;
label
  _postString1;
begin

  fTokStart := fTokStop;
  fTokStop  := fTokStart;

  // EOL
  if fTokStop > length(fLineBuf) then exit;

  // spaces
  if isWhite(readCurr) then
  begin
    fTokKind := tkBlank;
    while (isWhite(readNext)) do (*!*);
    exit;
  end;

  // line comment
  if fRange = rkNone then if (readCurr = '/') then
  begin
    if (readNext = '/') then
    begin
      while readNext <> #10 do (*!*);
      fTokKind := tkCommt;
      exit;
    end
    else
      Dec(fTokStop);
  end;

  // block comments 1
  if fRange = rkBlockCom1 then
  begin
    while(true) do
      begin
        if readCurr = #10 then break;
        if readNext = #10 then break;
        if (readPrev = '*') and (readCurr = '/') then break;
      end;
    if (readCurr = #10) then
    begin
      fRange := rkBlockCom1;
      fTokKind := tkCommt;
      exit;
    end;
    if (readCurr = '/') then
    begin
      fTokKind := tkCommt;
      fRange := rkNone;
      readNext;
      if fkComments1 in fFoldKinds then
        EndCodeFoldBlock;
      exit;
    end;
  end;
  if fRange <> rkBlockCom2 then if (readCurr <> #10) and (readCurr = '/') then if (readNext = '*') then
  begin
    if fRange = rkNone then
    begin
      while(true) do
      begin
        if readCurr = #10 then break;
        if readNext = #10 then break;
        if (readPrev = '*') and (readCurr = '/') then break;
      end;
      if (readCurr = #10) then
        begin
          fRange := rkBlockCom1;
          if fkComments1 in fFoldKinds then
            StartCodeFoldBlock(nil);
        end
      else readNext;
      fTokKind := tkCommt;
      exit;
    end;
  end
  else Dec(fTokStop);

  // block comments 2
  if fRange = rkBlockCom2 then
  begin
    while(true) do
      begin
        if readCurr = #10 then break;
        if readNext = #10 then break;
        if (readPrev = '+') and (readCurr = '/') then break;
      end;
    if (readCurr = #10) then
    begin
      fRange := rkBlockCom2;
      fTokKind := tkCommt;
      exit;
    end;
    if (readCurr = '/') then
    begin
      fTokKind := tkCommt;
      fRange := rkNone;
      readNext;
      if fkComments2 in fFoldKinds then
        EndCodeFoldBlock;
      exit;
    end;
  end;
  if fRange <> rkBlockCom1 then if (readCurr <> #10) and (readCurr = '/') then if (readNext = '+') then
  begin
    if fRange = rkNone then
    begin
      while(true) do
      begin
        if readCurr = #10 then break;
        if readNext = #10 then break;
        if (readPrev = '+') and (readCurr = '/') then break;
      end;
      if (readCurr = #10) then
      begin
        fRange := rkBlockCom2;
        if fkComments2 in fFoldKinds then
          StartCodeFoldBlock(nil);
      end
      else readNext;
      fTokKind := tkCommt;
      exit;
    end;
  end
  else Dec(fTokStop);

  // string 1
  if fRange = rkString1 then
  begin
    if (readCurr <> '"') then while (((readNext <> '"') or (readPrev = '\')) and (not (readCurr = #10))) do (*!*);
    if (readCurr = #10) then
    begin
      fRange := rkString1;
      fTokKind := tkStrng;
      exit;
    end;
    if (readCurr = '"') then
    begin
      fTokKind := tkStrng;
      fRange := rkNone;
      readNext;
      // check postfix
      if readCurr in ['c','w','d'] then
        readNext;
      exit;
    end;
  end;
  if fRange <> rkString2 then if (readCurr in ['r','x','"']) then
  begin
    if fRange = rkNone then
    begin
      // check hex/WYSIWYG prefix
      if readCurr in ['r','x'] then
      begin
        if not (readNext = '"') then
        begin
          Dec(fTokStop);
          goto _postString1;
        end;
      end;
      // go to end of string/eol
      while (((readNext <> '"') or (readPrev = '\')) and (not (readCurr = #10))) do (*!*);
      if (readCurr = #10) then fRange := rkString1
      else
      begin
        readNext;
        // check postfix
        if readCurr in ['c','w','d'] then
          readNext;
      end;
      fTokKind := tkStrng;
      exit;
    end;
  end;
  _postString1:

  // string 2
  if fRange = rkString2 then
  begin
    if (readCurr <> '`') then while ((readNext <> '`') and (not (readCurr = #10))) do (*!*);
    if (readCurr = #10) then
    begin
      fRange := rkString2;
      fTokKind := tkStrng;
      exit;
    end;
    if (readCurr = '`') then
    begin
      fTokKind := tkStrng;
      fRange := rkNone;
      readNext;
      // check postfix
      if readCurr in ['c','w','d'] then
        readNext;
      exit;
    end;
  end;
  if fRange <> rkString1 then if (readCurr = '`') then
  begin
    if fRange = rkNone then
    begin
      // go to end of string/eol
      while ((readNext <> '`') and (not (readCurr = #10))) do (*!*);
      if (readCurr = #10) then fRange := rkString2
      else
      begin
        readNext;
        // check postfix
        if readCurr in ['c','w','d'] then
          readNext;
      end;
      fTokKind := tkStrng;
      exit;
    end;
  end;

  // char literals
  if fRange = rkNone then if (readCurr = #39) then
  begin
    while (((readNext <> #39) or (readPrev = '\')) and (not (readCurr = #10))) do (*!*);
    if (readCurr = #39) then
    begin
      fTokKind := tkStrng;
      readNext;
      exit;
    end;
    fTokStop := fTokStart;
  end;

  // numbers 1
  if (isNumber(readCurr)) then
  begin
    while isAlNum(readNext) or (readCurr = '_') or (readCurr = '.') do (*!*);
    fTokKind := tkNumbr;
    exit;
  end;

  // symbols 1: ponctuation
  if isSymbol(readCurr) then
  begin
    fTokKind := tkSymbl;
    if (fkBrackets in fFoldKinds) then case readCurr of
      '{': StartCodeFoldBlock(nil);
      '}': EndCodeFoldBlock;
      end;
    readNext;
    exit;
  end;

  // symbols 2: operators
  if isOperator(readCurr) then
  begin
    fTokKind := tkSymbl;
    while isOperator(readNext) do (*!*);
    case fTokStop - fTokStart of
      1:begin
          if not isOperator(readCurr) then exit
            else Dec(fTokStop);
        end;
      2:begin
          if (not isOperator(readCurr)) and
            isDoubleOperator(fLineBuf[fTokStart..fTokStop-1])
          then exit
            else Dec(fTokStop, 2);
        end;
      3:begin
          if (not isOperator(readCurr)) and
            isTripleOperator(fLineBuf[fTokStart..fTokStop-1])
          then exit
            else Dec(fTokStop, 3);
        end;
      4:begin
          if (not isOperator(readCurr)) and
            isQuadOperator(fLineBuf[fTokStart..fTokStop-1])
          then exit
            else Dec(fTokStop, 4);
        end;
    end;
    fTokKind := tkIdent;
  end;

  // Keyword - Identifier
  if not isWhite(readCurr) then
  begin
    fTokKind := tkIdent;
    while(true) do
    begin
      if isWhite(readNext) then break;
      if isSymbol(readCurr) then break;
      if isOperator(readCurr) then break;
    end;
    if fKeyWords.find(fLineBuf[FTokStart..fTokStop-1]) then
      fTokKind := tkKeywd
    else
      if fLineBuf[FTokStart..fTokStop-1]  = fCurrIdent then
        fTokKind := tkCurrI;
    exit;
  end;

  if fLineBuf[fTokStop] = #10 then exit;

  // Should not happend
  assert(false);
end;

function TSynD2Syn.GetEol: Boolean;
begin
  result := fTokStop > length(fLineBuf);
end;

function TSynD2Syn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  result := fAttribLut[fTokKind];
end;

{$WARNINGS OFF} {$HINTS OFF}
procedure TSynD2Syn.SetRange(Value: Pointer);
begin
  inherited SetRange(Value);
  fRange := TRangeKind(PtrInt(CodeFoldRange.RangeType));
end;
{$HINTS ON} {$WARNINGS ON}

{$HINTS OFF}
function TSynD2Syn.GetRange: Pointer;
begin
  CodeFoldRange.RangeType := Pointer(PtrInt(fRange));
  Result := inherited GetRange;
end;
{$HINTS ON}

procedure TSynD2Syn.ResetRange;
begin
  fRange := rkNone;
end;

function TSynD2Syn.GetTokenPos: Integer;
begin
  result := fTokStart - 1;
end;

function TSynD2Syn.GetToken: string;
begin
  result := copy(fLineBuf, FTokStart, fTokStop - FTokStart);
end;

procedure TSynD2Syn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenStart  := @fLineBuf[FTokStart];
  TokenLength := fTokStop - FTokStart;
end;

function TSynD2Syn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result    := fCommtAttrib;
    SYN_ATTR_IDENTIFIER: Result := fIdentAttrib;
    SYN_ATTR_KEYWORD: Result    := fKeywdAttrib;
    SYN_ATTR_STRING: Result     := fStrngAttrib;
    SYN_ATTR_WHITESPACE: Result := fWhiteAttrib;
    SYN_ATTR_SYMBOL: Result     := fSymblAttrib;
    else Result := fIdentAttrib;
  end;
end;

function TSynD2Syn.GetTokenKind: integer;
var
  a: TSynHighlighterAttributes;
begin
  Result := SYN_ATTR_IDENTIFIER;
  a := GetTokenAttribute;
  if a = fIdentAttrib then Result := SYN_ATTR_IDENTIFIER  else
  if a = fWhiteAttrib then Result := SYN_ATTR_WHITESPACE  else
  if a = fCommtAttrib then Result := SYN_ATTR_COMMENT     else
  if a = fKeywdAttrib then Result := SYN_ATTR_KEYWORD     else
  if a = fStrngAttrib then Result := SYN_ATTR_STRING      else
  if a = fSymblAttrib then Result := SYN_ATTR_SYMBOL      else
  if a = fNumbrAttrib then Result := Ord(TTokenKind.tkNumbr);
end;

initialization
  registerClasses([TSynD2Syn]);
end.
