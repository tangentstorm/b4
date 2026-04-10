{$mode objfpc}{$H+}
unit orb;
{ ORB - Oberon Symbol Table, ported from N. Wirth's ORB.Mod (2014)
  Adapted for Free Pascal. Import/Export simplified (no .smb files). }
interface
uses ors;

const
  versionkey = 1;
  maxTypTab = 64;

  { object classes }
  clsHead  = 0;
  clsConst = 1; clsVar = 2; clsPar = 3; clsFld = 4; clsTyp = 5;
  clsSProc = 6; clsSFunc = 7; clsMod = 8;

  { type forms }
  tByte = 1; tBool = 2; tChar = 3; tInt = 4; tReal = 5; tSet = 6;
  tPointer = 7; tNilTyp = 8; tNoTyp = 9; tProc = 10;
  tString = 11; tArray = 12; tRecord = 13;

type
  PObj = ^TObj;
  PType = ^TType;

  TObj = record
    cls: integer;       { class: Head, Const, Var, Par, Fld, Typ, SProc, SFunc, Mod }
    lev: integer;       { declaration level }
    exno: integer;      { export number }
    expo: boolean;      { exported? }
    rdo: boolean;       { read-only? }
    next: PObj;
    dsc: PObj;          { descendants (scope chain / parameters / fields) }
    typ: PType;
    name: ors.Ident;
    val: longint;
  end;

  TType = record
    form: integer;      { Byte, Bool, Char, Int, Real, Set, Pointer, ... }
    ref: integer;
    mno: integer;
    nofpar: integer;    { for procedures: nof params; for records: ext level }
    len: longint;       { for arrays; for records: TD address }
    dsc: PObj;          { fields / parameters }
    typobj: PObj;       { type object }
    base: PType;        { element type / base record / pointed-to type }
    size: longint;      { in bytes }
  end;

var
  topScope: PObj;
  universe: PObj;
  system_: PObj;       { underscore to avoid Pascal keyword conflict }
  byteType, boolType, charType: PType;
  intType, realType, setType, nilType, noType, strType: PType;
  nofmod: integer;

procedure NewObj(var obj: PObj; const id: ors.Ident; cls: integer);
function thisObj: PObj;
function thisimport(mod_: PObj): PObj;
function thisfield(rec: PType): PObj;
procedure OpenScope;
procedure CloseScope;
procedure MakeFileName(var fname: ors.Ident; const name, ext: string);
procedure Init;

implementation

var
  dummy: PObj;

function NewType(form: integer; size: longint): PType;
var tp: PType;
begin
  new(tp);
  fillchar(tp^, sizeof(TType), 0);
  tp^.form := form;
  tp^.size := size;
  result := tp;
end;

procedure NewObj(var obj: PObj; const id: ors.Ident; cls: integer);
var new_: PObj; x: PObj;
begin
  x := topScope;
  while (x^.next <> nil) and (x^.next^.name <> id) do x := x^.next;
  if x^.next = nil then begin
    new(new_);
    fillchar(new_^, sizeof(TObj), 0);
    new_^.name := id;
    new_^.cls := cls;
    new_^.next := nil;
    new_^.rdo := false;
    new_^.dsc := nil;
    x^.next := new_;
    obj := new_;
  end else begin
    obj := x^.next;
    ors.Mark('mult def');
  end;
end;

function thisObj: PObj;
var s, x: PObj;
begin
  s := topScope;
  repeat
    x := s^.next;
    while (x <> nil) and (x^.name <> ors.id) do x := x^.next;
    s := s^.dsc;
  until (x <> nil) or (s = nil);
  result := x;
end;

function thisimport(mod_: PObj): PObj;
var obj: PObj;
begin
  if mod_^.rdo then begin
    if mod_^.name[1] <> #0 then begin
      obj := mod_^.dsc;
      while (obj <> nil) and (obj^.name <> ors.id) do obj := obj^.next;
    end else obj := nil;
  end else obj := nil;
  result := obj;
end;

function thisfield(rec: PType): PObj;
var fld: PObj;
begin
  fld := rec^.dsc;
  while (fld <> nil) and (fld^.name <> ors.id) do fld := fld^.next;
  result := fld;
end;

procedure OpenScope;
var s: PObj;
begin
  new(s);
  fillchar(s^, sizeof(TObj), 0);
  s^.cls := clsHead;
  s^.dsc := topScope;
  s^.next := nil;
  topScope := s;
end;

procedure CloseScope;
begin
  topScope := topScope^.dsc;
end;

procedure MakeFileName(var fname: ors.Ident; const name, ext: string);
begin
  fname := copy(name, 1, IdLen - 5) + ext;
end;

procedure Init;
begin
  topScope := universe;
  nofmod := 1;
end;

procedure enter(const name: string; cls: integer; tp: PType; n: longint);
var obj: PObj;
begin
  new(obj);
  fillchar(obj^, sizeof(TObj), 0);
  obj^.name := name;
  obj^.cls := cls;
  obj^.typ := tp;
  obj^.val := n;
  obj^.dsc := nil;
  if cls = clsTyp then tp^.typobj := obj;
  obj^.next := system_;
  system_ := obj;
end;

initialization
  byteType := NewType(tInt, 1);
  boolType := NewType(tBool, 4);  { 4 bytes so nt works correctly }
  charType := NewType(tChar, 1);
  intType  := NewType(tInt, 4);
  realType := NewType(tReal, 4);
  setType  := NewType(tSet, 4);
  nilType  := NewType(tNilTyp, 4);
  noType   := NewType(tNoTyp, 4);
  strType  := NewType(tString, 8);

  { assign ref values used for type identity }
  byteType^.ref := tByte;
  boolType^.ref := tBool;
  charType^.ref := tChar;
  intType^.ref  := tInt;
  realType^.ref := tReal;
  setType^.ref  := tSet;
  nilType^.ref  := tNilTyp;
  noType^.ref   := tNoTyp;
  strType^.ref  := tString;

  { initialize universe with standard identifiers }
  { n = procno*10 + nofpar }
  system_ := nil;
  enter('UML',   clsSFunc, intType,  132);
  enter('SBC',   clsSFunc, intType,  122);
  enter('ADC',   clsSFunc, intType,  112);
  enter('ROR',   clsSFunc, intType,   92);
  enter('ASR',   clsSFunc, intType,   82);
  enter('LSL',   clsSFunc, intType,   72);
  enter('LEN',   clsSFunc, intType,   61);
  enter('CHR',   clsSFunc, charType,  51);
  enter('ORD',   clsSFunc, intType,   41);
  enter('FLT',   clsSFunc, realType,  31);
  enter('FLOOR', clsSFunc, intType,   21);
  enter('ODD',   clsSFunc, boolType,  11);
  enter('ABS',   clsSFunc, intType,    1);
  enter('LED',   clsSProc, noType,   81);
  enter('UNPK',  clsSProc, noType,   72);
  enter('PACK',  clsSProc, noType,   62);
  enter('NEW',   clsSProc, noType,   51);
  enter('ASSERT',clsSProc, noType,   41);
  enter('EXCL',  clsSProc, noType,   32);
  enter('INCL',  clsSProc, noType,   22);
  enter('DEC',   clsSProc, noType,   11);
  enter('INC',   clsSProc, noType,    1);
  enter('SET',     clsTyp, setType,   0);
  enter('BOOLEAN', clsTyp, boolType,  0);
  enter('BYTE',   clsTyp, byteType,  0);
  enter('CHAR',   clsTyp, charType,  0);
  enter('LONGREAL',clsTyp, realType, 0);
  enter('REAL',   clsTyp, realType,   0);
  enter('LONGINT',clsTyp, intType,    0);
  enter('INTEGER',clsTyp, intType,    0);

  topScope := nil;
  OpenScope;
  topScope^.next := system_;
  universe := topScope;

  { initialize SYSTEM pseudo-module }
  system_ := nil;
  enter('H',     clsSFunc, intType,  201);
  enter('COND',  clsSFunc, boolType, 191);
  enter('SIZE',  clsSFunc, intType,  181);
  enter('ADR',   clsSFunc, intType,  171);
  enter('VAL',   clsSFunc, intType,  162);
  enter('REG',   clsSFunc, intType,  151);
  enter('BIT',   clsSFunc, boolType, 142);
  enter('LDREG', clsSProc, noType,  142);
  enter('LDPSR', clsSProc, noType,  131);
  enter('COPY',  clsSProc, noType,  123);
  enter('PUT',   clsSProc, noType,  112);
  enter('GET',   clsSProc, noType,  102);

  new(dummy);
  fillchar(dummy^, sizeof(TObj), 0);
  dummy^.cls := clsVar;
  dummy^.typ := intType;
end.
