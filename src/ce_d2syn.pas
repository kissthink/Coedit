unit ce_d2syn;

{$I ce_defines.inc}

interface

uses
  Classes, SysUtils, Graphics,
  SynEditHighlighter, SynEditHighlighterFoldBase, SynEditTypes,
  ce_dlangutils;

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

  TD2DictionaryEntry = record
    filled: Boolean;
    values: array of string;
  end;

  TD2Dictionary = object
  private
    fLongest, fShortest: NativeInt;
    fEntries: array[Byte] of TD2DictionaryEntry;
    function toHash(const aValue: string): Byte; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure addEntry(const aValue: string);
  public
    constructor create;
    destructor destroy; // do not remove even if empty (compat with char-map version)
    function find(const aValue: string): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

  TTokenKind = (tkCommt, tkIdent, tkKeywd, tkStrng, tkBlank, tkSymbl, tkNumbr, tkCurrI, tkDDocs, tkAsblr);

  TRangeKind = (rkNone, rkString1, rkString2, rkTokString, rkBlockCom1, rkBlockCom2, rkBlockDoc1, rkBlockDoc2, rkAsm);

  TRangeKinds = set of TRangeKind;

  TFoldKind = (fkBrackets, fkComments1, fkComments2, fkStrings);
  TFoldKinds = set of TFoldKind;

  PRangeInfo = ^TRangeInfo;
  TRangeInfo = record
    kinds: TRangeKinds;
    nestedCommCount: integer;
    tkStrCurlyCount: integer;
  end;

  TSynD2SynRange = class(TSynCustomHighlighterRange)
  private
    nestedCommentsCount: Integer;
    tokenStringBracketsCount: Integer;
    rangeKinds: TRangeKinds;
  public
    procedure Assign(Src: TSynCustomHighlighterRange); override;
    function Compare(Range: TSynCustomHighlighterRange): integer; override;
    procedure Clear; override;
  end;

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
    fDDocsAttrib: TSynHighlighterAttributes;
    fAsblrAttrib: TSynHighlighterAttributes;
    fKeyWords: TD2Dictionary;
    fCurrIdent: string;
    fLineBuf: string;
    fTokStart, fTokStop: Integer;
    fTokKind: TTokenKind;
    fCurrRange: TSynD2SynRange;
    fFoldKinds: TFoldKinds;
    fAttribLut: array[TTokenKind] of TSynHighlighterAttributes;
    function readNext: Char; {$IFNDEF DEBUG}inline;{$ENDIF}
    function readCurr: Char; {$IFNDEF DEBUG}inline;{$ENDIF}
    function readPrev: Char; {$IFNDEF DEBUG}inline;{$ENDIF}
    function readPrevPrev: Char; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure setFoldKinds(aValue: TFoldKinds);
    procedure setWhiteAttrib(aValue: TSynHighlighterAttributes);
    procedure setNumbrAttrib(aValue: TSynHighlighterAttributes);
    procedure setSymblAttrib(aValue: TSynHighlighterAttributes);
    procedure setIdentAttrib(aValue: TSynHighlighterAttributes);
    procedure setCommtAttrib(aValue: TSynHighlighterAttributes);
    procedure setStrngAttrib(aValue: TSynHighlighterAttributes);
    procedure setKeywdAttrib(aValue: TSynHighlighterAttributes);
    procedure setCurrIAttrib(aValue: TSynHighlighterAttributes);
    procedure setDDocsAttrib(aValue: TSynHighlighterAttributes);
    procedure setAsblrAttrib(aValue: TSynHighlighterAttributes);
    procedure doAttribChange(sender: TObject);
    procedure setCurrIdent(const aValue: string);
    procedure doChanged;
  protected
    function GetRangeClass: TSynCustomHighlighterRangeClass; override;
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
    property DDocsAttrib: TSynHighlighterAttributes read fDDocsAttrib write setDDocsAttrib;
    property AsblrAttrib: TSynHighlighterAttributes read fAsblrAttrib write setAsblrAttrib;
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

procedure TSynD2SynRange.Assign(Src: TSynCustomHighlighterRange);
var
  src_t: TSynD2SynRange;
begin
  inherited;
  if Src is TSynD2SynRange then
  begin
    src_t := TSynD2SynRange(Src);
    rangeKinds := src_t.rangeKinds;
    nestedCommentsCount := src_t.nestedCommentsCount;
    tokenStringBracketsCount := src_t.tokenStringBracketsCount;
  end;
end;

function TSynD2SynRange.Compare(Range: TSynCustomHighlighterRange): integer;
var
  src_t: TSynD2SynRange;
begin
  result := inherited Compare(Range);
  if result <> 0 then exit;
  //
  if Range is TSynD2SynRange then
  begin
    src_t := TSynD2SynRange(Range);
    if src_t.rangeKinds <> rangeKinds then exit(1);
    if src_t.nestedCommentsCount <> nestedCommentsCount then exit(1);
    if src_t.tokenStringBracketsCount <> tokenStringBracketsCount then exit(1);
    exit(0);
  end;
end;

procedure TSynD2SynRange.Clear;
begin
  inherited;
  nestedCommentsCount := 0;
  tokenStringBracketsCount := 0;
  rangeKinds := [];
end;


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
  fDDocsAttrib := TSynHighlighterAttributes.Create('DDoc','DDoc');
  fAsblrAttrib := TSynHighlighterAttributes.Create('Asm','Asm');

  fNumbrAttrib.Foreground := $000079F2;
  fSymblAttrib.Foreground := clMaroon;
  fIdentAttrib.Foreground := clBlack;
  fCommtAttrib.Foreground := clGreen;
  fStrngAttrib.Foreground := clBlue;
  fKeywdAttrib.Foreground := clNavy;
  fAsblrAttrib.Foreground := clGray;

  fCurrIAttrib.Foreground := clBlack;
  fCurrIAttrib.FrameEdges := sfeAround;
  fCurrIAttrib.FrameColor := $D4D4D4;
  fCurrIAttrib.Background := $F0F0F0;

  fDDocsAttrib.Foreground := clTeal;

  fCommtAttrib.Style := [fsItalic];
  fKeywdAttrib.Style := [fsBold];
  fAsblrAttrib.Style := [fsBold];

  AddAttribute(fWhiteAttrib);
  AddAttribute(fNumbrAttrib);
  AddAttribute(fSymblAttrib);
  AddAttribute(fIdentAttrib);
  AddAttribute(fCommtAttrib);
  AddAttribute(fStrngAttrib);
  AddAttribute(fKeywdAttrib);
  AddAttribute(fCurrIAttrib);
  AddAttribute(fDDocsAttrib);
  AddAttribute(fAsblrAttrib);

  fAttribLut[TTokenKind.tkident] := fIdentAttrib;
  fAttribLut[TTokenKind.tkBlank] := fWhiteAttrib;
  fAttribLut[TTokenKind.tkCommt] := fCommtAttrib;
  fAttribLut[TTokenKind.tkKeywd] := fKeywdAttrib;
  fAttribLut[TTokenKind.tkNumbr] := fNumbrAttrib;
  fAttribLut[TTokenKind.tkStrng] := fStrngAttrib;
  fAttribLut[TTokenKind.tksymbl] := fSymblAttrib;
  fAttribLut[TTokenKind.tkCurrI] := fCurrIAttrib;
  fAttribLut[TTokenKind.tkDDocs] := fDDocsAttrib;
  fAttribLut[TTokenKind.tkAsblr] := fAsblrAttrib;

  SetAttributesOnChange(@doAttribChange);
  fTokStop := 1;
  next;
end;

destructor TSynD2Syn.destroy;
begin
  fCurrRange.Free;
  fKeyWords.destroy;
  inherited;
end;

function TSynD2Syn.GetRangeClass: TSynCustomHighlighterRangeClass;
begin
  result := TSynD2SynRange;
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

procedure TSynD2Syn.setDDocsAttrib(aValue: TSynHighlighterAttributes);
begin
  fDDocsAttrib.Assign(aValue);
end;

procedure TSynD2Syn.setAsblrAttrib(aValue: TSynHighlighterAttributes);
begin
  fAsblrAttrib.Assign(aValue);
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
function TSynD2Syn.readNext: Char;
begin
  Inc(fTokStop);
  result := fLineBuf[fTokStop];
end;
{$IFDEF DEBUG}{$R+}{$ENDIF}

function TSynD2Syn.readCurr: Char;
begin
  result := fLineBuf[fTokStop];
end;

function TSynD2Syn.readPrev: Char;
begin
  result := fLineBuf[fTokStop-1];
end;

function TSynD2Syn.readPrevPrev: Char;
begin
  result := fLineBuf[fTokStop-2];
end;

//TODO-cnumber literals: stricter, separate parser for each form (bin,dec,hex,float,etc)
//TODO-cstring literals: delimited strings.
//TODO-ccomments: correct nested comments handling (inc/dec)
//TODO-cfeature: something like pascal {$region} : /*folder blabla*/  /*endfolder*/

{$BOOLEVAL ON}
procedure TSynD2Syn.next;
var
  reader: PChar;
label
  _postString1;
procedure readerReset;
begin
  fTokStop := fTokStart;
  reader := @fLineBuf[fTokStop];
end;
procedure readerNext;
begin
  Inc(reader);
  Inc(fTokStop);
end;
begin

  fTokStart := fTokStop;
  fTokStop  := fTokStart;

  // EOL
  if fTokStop > length(fLineBuf) then exit;
  readerReset;

  // spaces
  if isWhite(readCurr) then
  begin
    fTokKind := tkBlank;
    while (isWhite(readNext)) do (*!*);
    exit;
  end;

  // line comment
  if fCurrRange.rangeKinds = [] then if readDelim(reader, fTokStop, '//') then
  begin
    fTokKind := tkCommt;
    if readDelim(reader, fTokStop, '/') then
      fTokKind := tkDDocs;
    readLine(reader, fTokStop);
    exit;
  end else readerReset;

  // block comments 1
  if fCurrRange.rangeKinds = [] then if readDelim(reader, fTokStop, '/*') then
  begin
    if readDelim(reader, fTokStop, '*') then fTokKind := tkDDocs
      else fTokKind := tkCommt;
    if readUntil(reader, fTokStop, '*/') then
      exit;
    if fTokKind = tkDDocs then
      fCurrRange.rangeKinds += [rkBlockDoc1]
    else
      fCurrRange.rangeKinds += [rkBlockCom1];
    readLine(reader, fTokStop);
    if fkComments1 in fFoldKinds then
      StartCodeFoldBlock(nil);
    exit;
  end else readerReset;
  if (rkBlockCom1 in fCurrRange.rangeKinds) or (rkBlockDoc1 in fCurrRange.rangeKinds) then
  begin
    if (rkBlockDoc1 in fCurrRange.rangeKinds) then fTokKind := tkDDocs
      else fTokKind := tkCommt;
    if readUntil(reader, fTokStop, '*/') then
    begin
      fCurrRange.rangeKinds -= [rkBlockDoc1, rkBlockCom1];
      if fkComments1 in fFoldKinds then
        EndCodeFoldBlock;
      exit;
    end;
    readLine(reader, fTokStop);
    exit;
  end;

  // block comments 2
  if fCurrRange.rangeKinds = [] then if readDelim(reader, fTokStop, '/+') then
  begin
    if readDelim(reader, fTokStop, '+') then fTokKind := tkDDocs
      else fTokKind := tkCommt;
    if readUntil(reader, fTokStop, '+/') then
      exit;
    if fTokKind = tkDDocs then fCurrRange.rangeKinds += [rkBlockDoc2]
      else fCurrRange.rangeKinds += [rkBlockCom2];
    readLine(reader, fTokStop);
    if fkComments2 in fFoldKinds then
      StartCodeFoldBlock(nil);
    exit;
  end else readerReset;
  if (rkBlockCom2 in fCurrRange.rangeKinds) or (rkBlockDoc2 in fCurrRange.rangeKinds) then
  begin
    if (rkBlockDoc2 in fCurrRange.rangeKinds) then fTokKind := tkDDocs
      else fTokKind := tkCommt;
    if readUntil(reader, fTokStop, '+/') then
    begin
      fCurrRange.rangeKinds -= [rkBlockDoc2, rkBlockCom2];

      if fkComments2 in fFoldKinds then
        EndCodeFoldBlock;
      exit;
    end;
    readLine(reader, fTokStop);
    exit;
  end;

  // string 1
  if fCurrRange.rangeKinds = [] then if readDelim(reader, fTokStop, stringPrefixes) then
  begin
    if readPrev in ['r','x'] then
    begin
      if not (readCurr = '"') then
        goto _postString1;
      readerNext;
    end;
    fTokKind := tkStrng;
    while(true) do
    begin
      if not readUntilAmong(reader, fTokStop, stringStopChecks) then
        break;
      if readCurr = '\' then
      begin
        readerNext;
        if readWhile(reader, fTokStop, '\') then
          continue;
        if readCurr = '"' then
          readerNext;
        continue;
      end
      else if readCurr = '"' then
      begin
        readerNext;
        readDelim(reader, fTokStop, stringPostfixes);
        exit;
      end;
    end;
    fCurrRange.rangeKinds += [rkString1];
    if fkStrings in fFoldKinds then
      StartCodeFoldBlock(nil);
    exit;
  end else _postString1: readerReset;
  if rkString1 in fCurrRange.rangeKinds then
  begin
    fTokKind := tkStrng;
    while(true) do
    begin
      if not readUntilAmong(reader, fTokStop, stringStopChecks) then
        break;
      if readCurr = '\' then
      begin
        readerNext;
        if readWhile(reader, fTokStop, '\') then
          continue;
        if readCurr = '"' then
          readerNext;
        continue;
      end
      else if readCurr = '"' then
      begin
        readerNext;
        fCurrRange.rangeKinds -= [rkString1];
        readDelim(reader, fTokStop, stringPostfixes);
        if fkStrings in fFoldKinds then
          EndCodeFoldBlock();
        exit;
      end
      else break;
    end;
    readLine(reader, fTokStop);
    exit;
  end;

  // string 2
  if fCurrRange.rangeKinds = [] then if readDelim(reader, fTokStop, '`') then
  begin
    fTokKind := tkStrng;
    if readUntil(reader, fTokStop, '`') then
    begin
      readDelim(reader, fTokStop, stringPostfixes);
      exit;
    end;
    fCurrRange.rangeKinds += [rkString2];
    readLine(reader, fTokStop);
    if fkStrings in fFoldKinds then
      StartCodeFoldBlock(nil);
    exit;
  end else readerReset;
  if rkString2 in fCurrRange.rangeKinds then
  begin
    fTokKind := tkStrng;
    if readUntil(reader, fTokStop, '`') then
    begin
      fCurrRange.rangeKinds -= [rkString2];
      if fkStrings in fFoldKinds then
        EndCodeFoldBlock();
      readDelim(reader, fTokStop, stringPostfixes);
      exit;
    end;
    readLine(reader, fTokStop);
    exit;
  end;

  //token string
  if fCurrRange.rangeKinds = [] then if readDelim(reader, fTokStop, 'q{') then
  begin
    fTokKind := tkStrng;
    inc(fCurrRange.tokenStringBracketsCount);
    while readUntilAmong(reader, fTokStop, ['{','}']) do
    begin
      if readCurr = '{' then inc(fCurrRange.tokenStringBracketsCount) else
      if readCurr = '}' then dec(fCurrRange.tokenStringBracketsCount);
      readerNext;
      if fCurrRange.tokenStringBracketsCount = 0 then
        exit;
    end;
    fCurrRange.rangeKinds += [rkTokString];
    readLine(reader, fTokStop);
    if fkStrings in fFoldKinds then
      StartCodeFoldBlock(nil);
    exit;
  end else readerReset;
  if rkTokString in fCurrRange.rangeKinds then
  begin
    fTokKind := tkStrng;
    while readUntilAmong(reader, fTokStop, ['{','}']) do
    begin
      if readCurr = '{' then inc(fCurrRange.tokenStringBracketsCount) else
      if readCurr = '}' then dec(fCurrRange.tokenStringBracketsCount);
      readerNext;
      if fCurrRange.tokenStringBracketsCount = 0 then
      begin
        fCurrRange.rangeKinds -= [rkTokString];
        if fkStrings in fFoldKinds then
          EndCodeFoldBlock();
        exit;
      end;
    end;
    readLine(reader, fTokStop);
    exit;
  end;

  // char literals
  if fCurrRange.rangeKinds = [] then if (readCurr = #39) then
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
      '}': begin EndCodeFoldBlock; if (readCurr = '}') and (rkAsm in fCurrRange.rangeKinds) then fCurrRange.rangeKinds -= [rkAsm]; end;
      end;
    readNext;
    exit;
  end;

  // symbols 2: operators
  if isOperator1(readCurr) then
  begin
    fTokKind := tkSymbl;
    while isOperator1(readNext) do (*!*);
    case fTokStop - fTokStart of
      4:begin
          if (not isOperator1(readCurr)) and
            isOperator4(fLineBuf[fTokStart..fTokStop-1])
          then exit
            else Dec(fTokStop, 4);
        end;
      3:begin
          if (not isOperator1(readCurr)) and
            isOperator3(fLineBuf[fTokStart..fTokStop-1])
          then exit
            else Dec(fTokStop, 3);
        end;
      2:begin
          if (not isOperator1(readCurr)) and
            isOperator2(fLineBuf[fTokStart..fTokStop-1])
          then exit
            else Dec(fTokStop, 2);
        end;
      1:begin
          if not isOperator1(readCurr) then exit
            else Dec(fTokStop);
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
      if isOperator1(readCurr) then break;
    end;
    if fKeyWords.find(fLineBuf[FTokStart..fTokStop-1]) then
      fTokKind := tkKeywd
    else
      if fLineBuf[FTokStart..fTokStop-1]  = fCurrIdent then
        fTokKind := tkCurrI;
    //check asm range
    if fLineBuf[FTokStart..fTokStop-1] = 'asm' then
      fCurrRange.rangeKinds += [rkAsm];
    exit;
  end;

  if fLineBuf[fTokStop] = #10 then exit;

  // Should not happend
  assert(false);
end;
{$BOOLEVAL OFF}

function TSynD2Syn.GetEol: Boolean;
begin
  result := fTokStop > length(fLineBuf);
end;

function TSynD2Syn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  if (rkAsm in fCurrRange.rangeKinds) and (fTokKind <> tkSymbl) and (fTokKind <> tkKeywd) then
    result := fAttribLut[tkAsblr]
  else
    result := fAttribLut[fTokKind];
end;


procedure TSynD2Syn.SetRange(Value: Pointer);
var
  distant: TSynD2SynRange;
begin
  inherited SetRange(Value);
  distant := TSynD2SynRange(CodeFoldRange.RangeType);
  //
  fCurrRange.rangeKinds := distant.rangeKinds;
  fCurrRange.tokenStringBracketsCount := distant.tokenStringBracketsCount;
  fCurrRange.nestedCommentsCount := distant.nestedCommentsCount;
end;

function TSynD2Syn.GetRange: Pointer;
var
  distant: TSynD2SynRange;
begin
  distant := TSynD2SynRange(inherited GetRange);
  if (distant = nil) then
    distant := TSynD2SynRange.Create(nil);
  distant.rangeKinds := fCurrRange.rangeKinds;
  distant.tokenStringBracketsCount := fCurrRange.tokenStringBracketsCount;
  distant.nestedCommentsCount := fCurrRange.nestedCommentsCount;
  //
  CodeFoldRange.RangeType := Pointer(distant);
  Result := inherited GetRange;
end;

procedure TSynD2Syn.ResetRange;
begin
  if fCurrRange = nil then
    fCurrRange := TSynD2SynRange.Create(nil)
  else
    fCurrRange.Clear;
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
