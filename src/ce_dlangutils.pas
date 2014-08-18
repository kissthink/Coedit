unit ce_dlangutils;

{$MODE OBJFPC}{$H+}

interface

uses
  SysUtils;

function isWhite(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isSpace(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isAlpha(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isNumber(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isBit(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isAlNum(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isHex(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isSymbol(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isPtrOperator(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isOperator1(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isOperator2(const s: string): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isOperator3(const s: string): boolean; {$IFNDEF DEBUG} inline; {$ENDIF}
function isOperator4(const s: string): boolean; {$IFNDEF DEBUG} inline; {$ENDIF}
function isStringPostfix(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isIdentifier(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
function isFirstIdentifier(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}

implementation

{$BOOLEVAL ON}
function isWhite(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit(c in [#0..#32]);
end;

function isSpace(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit(c in [#9,' ']);
end;

function isAlpha(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit((c in ['a'..'z']) or (c in ['A'..'Z']));
end;

function isNumber(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit(c in ['0'..'9']);
end;

function isBit(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit((c in ['0'..'1']));
end;

function isAlNum(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit(isAlpha(c) or isNumber(c));
end;

function isHex(const c: Char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit(isNumber(c) or (c in ['A'..'F']) or (c in ['a'..'f']));
end;

function isSymbol(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit(c in [';', '{', '}', '(', ')', '[', ']', ',', '.', ':' , '"', #39, '?', '$']);
end;

function isPtrOperator(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit(c in ['&', '*']);
end;

function isOperator1(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit(c in ['/', '*', '-', '+', '%', '>', '<', '=', '!',
                  '&', '|', '^', '~']);
end;

function isOperator2(const s: string): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
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
    '*': result := s[2] in ['=', '*']; // **: pointers
    '%': result := s[2] in ['='];
    '~': result := s[2] in ['='];

    '&': result := s[2] in ['&', '='];
    '|': result := s[2] in ['|', '='];
    '^': result := s[2] in ['^', '='];
  end;
end;

function isOperator3(const s: string): boolean; {$IFNDEF DEBUG} inline; {$ENDIF}
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
    '*': result := (s[2] = '*')   and (s[3] = '*'); // ***: pointers
  end;
end;

function isOperator4(const s: string): boolean; {$IFNDEF DEBUG} inline; {$ENDIF}
begin
  result := (s = '>>>=') or (s = '!<>=') or (s = '****');
end;

function isStringPostfix(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit(c in ['c', 'w', 'd']);
end;

function isIdentifier(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit((not isSymbol(c)) and (not isOperator1(c)) and (not isWhite(c)));
end;

function isFirstIdentifier(const c: char): boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
begin
  exit(isIdentifier(c) and (not isNumber(c)));
end;
{$BOOLEVAL OFF}

end.
