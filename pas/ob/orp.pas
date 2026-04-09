{$mode objfpc}{$H+}
unit orp;
{ ORP - Oberon Parser, ported from N. Wirth's ORP.Mod (2014)
  Adapted for Free Pascal, calling org.pas for b4vm code generation. }
interface
uses ors, orb, org;

procedure Module;

implementation

type
  PtrBase = ^PtrBaseDesc;
  PtrBaseDesc = record
    name: ors.Ident;
    typ: orb.PType;
    next: PtrBase;
  end;

var
  sym: integer;
  dc: longint;
  level, exno, version: integer;
  newSF: boolean;
  modid: ors.Ident;
  pbsList: PtrBase;
  dummy: orb.PObj;

  { forward-declared procedure variables }
  expression: procedure(var x: org.Item);
  Type_: procedure(var tp: orb.PType);
  FormalType: procedure(var tp: orb.PType; dim: integer);

procedure Check(s: integer; const msg: string);
begin
  if sym = s then ors.Get(sym) else ors.Mark(msg);
end;

procedure qualident(var obj: orb.PObj);
begin
  obj := orb.thisObj;
  ors.Get(sym);
  if obj = nil then begin ors.Mark('undef'); obj := dummy end;
  if (sym = ors.sPERIOD) and (obj^.cls = orb.clsMod) then begin
    ors.Get(sym);
    if sym = ors.sIDENT then begin
      obj := orb.thisimport(obj);
      ors.Get(sym);
      if obj = nil then begin ors.Mark('undef'); obj := dummy end;
    end else begin ors.Mark('identifier expected'); obj := dummy end;
  end;
end;

procedure CheckBool(var x: org.Item);
begin
  if x.typ^.form <> orb.tBool then begin ors.Mark('not Boolean'); x.typ := orb.boolType end;
end;

procedure CheckInt(var x: org.Item);
begin
  if x.typ^.form <> orb.tInt then begin ors.Mark('not Integer'); x.typ := orb.intType end;
end;

procedure CheckReal(var x: org.Item);
begin
  if x.typ^.form <> orb.tReal then begin ors.Mark('not Real'); x.typ := orb.realType end;
end;

procedure CheckSet(var x: org.Item);
begin
  if x.typ^.form <> orb.tSet then begin ors.Mark('not Set'); x.typ := orb.setType end;
end;

procedure CheckSetVal(var x: org.Item);
begin
  if x.typ^.form <> orb.tInt then begin
    ors.Mark('not Int'); x.typ := orb.setType;
  end else if x.mode = orb.clsConst then begin
    if (x.a < 0) or (x.a >= 32) then ors.Mark('invalid set');
  end;
end;

procedure CheckConst(var x: org.Item);
begin
  if x.mode <> orb.clsConst then begin ors.Mark('not a constant'); x.mode := orb.clsConst end;
end;

procedure CheckReadOnly(var x: org.Item);
begin
  if x.rdo then ors.Mark('read-only');
end;

procedure CheckExport(var expo: boolean);
begin
  if sym = ors.sTIMES then begin
    expo := true; ors.Get(sym);
    if level <> 0 then ors.Mark('remove asterisk');
  end else expo := false;
end;

function IsExtension(t0, t1: orb.PType): boolean;
begin
  result := (t0 = t1) or ((t1 <> nil) and IsExtension(t0, t1^.base));
end;

function CompTypes(t0, t1: orb.PType; varpar: boolean): boolean;
begin
  result := (t0 = t1)
    or (t0^.form = orb.tArray) and (t1^.form = orb.tArray) and CompTypes(t0^.base, t1^.base, varpar)
    or (t0^.form = orb.tPointer) and (t1^.form = orb.tPointer) and IsExtension(t0^.base, t1^.base)
    or (t0^.form = orb.tRecord) and (t1^.form = orb.tRecord) and IsExtension(t0, t1)
    or (t0^.form in [orb.tPointer, orb.tProc]) and (t1^.form = orb.tNilTyp)
    or (t0^.form = orb.tNilTyp) and (t1^.form in [orb.tPointer, orb.tProc])
    or (not varpar) and (t0^.form = orb.tInt) and (t1^.form = orb.tInt);
end;

{ ---- Type test ---- }

procedure TypeTest(var x: org.Item; T: orb.PType; guard: boolean);
begin
  { simplified: just set type }
  if not guard then begin
    org.MakeConstItem(x, orb.boolType, 1);
    x.typ := orb.boolType;
  end else
    x.typ := T;
end;

{ ---- Selector ---- }

procedure selector(var x: org.Item);
var y: org.Item; obj: orb.PObj;
begin
  while (sym = ors.sLBRAK) or (sym = ors.sPERIOD) or (sym = ors.sARROW)
      or (sym = ors.sLPAREN) and (x.typ^.form in [orb.tRecord, orb.tPointer]) do begin
    if sym = ors.sLBRAK then begin
      repeat
        ors.Get(sym); expression(y);
        if x.typ^.form = orb.tArray then begin
          CheckInt(y); org.Index(x, y); x.typ := x.typ^.base;
        end else ors.Mark('not an array');
      until sym <> ors.sCOMMA;
      Check(ors.sRBRAK, 'no ]');
    end else if sym = ors.sPERIOD then begin
      ors.Get(sym);
      if sym = ors.sIDENT then begin
        if x.typ^.form = orb.tPointer then begin org.DeRef(x); x.typ := x.typ^.base end;
        if x.typ^.form = orb.tRecord then begin
          obj := orb.thisfield(x.typ);
          ors.Get(sym);
          if obj <> nil then begin org.Field(x, obj); x.typ := obj^.typ end
          else ors.Mark('undef');
        end else ors.Mark('not a record');
      end else ors.Mark('ident?');
    end else if sym = ors.sARROW then begin
      ors.Get(sym);
      if x.typ^.form = orb.tPointer then begin org.DeRef(x); x.typ := x.typ^.base end
      else ors.Mark('not a pointer');
    end else if (sym = ors.sLPAREN) and (x.typ^.form in [orb.tRecord, orb.tPointer]) then begin
      { type guard }
      ors.Get(sym);
      if sym = ors.sIDENT then begin
        qualident(obj);
        if obj^.cls = orb.clsTyp then TypeTest(x, obj^.typ, true)
        else ors.Mark('guard type expected');
      end else ors.Mark('not an identifier');
      Check(ors.sRPAREN, ') missing');
    end;
  end;
end;

{ ---- Parameters ---- }

procedure Parameter(par: orb.PObj);
var x: org.Item; varpar: boolean;
begin
  expression(x);
  if par <> nil then begin
    varpar := par^.cls = orb.clsPar;
    if CompTypes(par^.typ, x.typ, varpar) then begin
      if not varpar then org.ValueParam(x)
      else begin
        if not par^.rdo then CheckReadOnly(x);
        org.VarParam(x, par^.typ);
      end;
    end else if (not varpar) and (par^.typ^.form = orb.tInt) and (x.typ^.form = orb.tInt) then
      org.ValueParam(x)
    else if (x.typ^.form = orb.tString) and (x.b = 2) and (par^.cls = orb.clsVar) and (par^.typ^.form = orb.tChar) then begin
      org.StrToChar(x); org.ValueParam(x);
    end else if (x.typ^.form = orb.tArray) and (par^.typ^.form = orb.tArray) and
        (x.typ^.base^.form = par^.typ^.base^.form) and (par^.typ^.len < 0) then
      org.OpenArrayParam(x)
    else if (x.typ^.form = orb.tString) and varpar and par^.rdo and
        (par^.typ^.form = orb.tArray) and (par^.typ^.base^.form = orb.tChar) and (par^.typ^.len < 0) then
      org.StringParam(x)
    else ors.Mark('incompatible parameters');
  end;
end;

procedure ParamList(var x: org.Item);
var n: integer; par: orb.PObj;
begin
  par := x.typ^.dsc; n := 0;
  if sym <> ors.sRPAREN then begin
    Parameter(par); n := 1;
    while sym <= ors.sCOMMA do begin
      Check(sym, 'comma?');
      if par <> nil then par := par^.next;
      inc(n); Parameter(par);
    end;
    Check(ors.sRPAREN, ') missing');
  end else ors.Get(sym);
  if n < x.typ^.nofpar then ors.Mark('too few params')
  else if n > x.typ^.nofpar then ors.Mark('too many params');
end;

{ ---- Standard functions and procedures ---- }

procedure StandFunc(var x: org.Item; fct: longint; restyp: orb.PType);
var y: org.Item; n, npar: longint;
begin
  Check(ors.sLPAREN, 'no (');
  npar := fct mod 10; fct := fct div 10;
  expression(x); n := 1;
  while sym = ors.sCOMMA do begin ors.Get(sym); expression(y); inc(n) end;
  Check(ors.sRPAREN, 'no )');
  if n = npar then begin
    case fct of
      0: begin CheckInt(x); org.Abs_(x); restyp := x.typ end; { ABS }
      1: begin CheckInt(x); org.Odd_(x) end; { ODD }
      2: begin CheckReal(x); org.Floor_(x) end; { FLOOR }
      3: begin CheckInt(x); org.Float_(x) end; { FLT }
      4: begin { ORD }
        if x.typ^.form <= orb.tProc then org.Ord_(x)
        else if (x.typ^.form = orb.tString) and (x.b = 2) then org.StrToChar(x)
        else ors.Mark('bad type');
      end;
      5: begin CheckInt(x); org.Ord_(x) end; { CHR }
      6: begin { LEN }
        if x.typ^.form = orb.tArray then org.Len_(x)
        else ors.Mark('not an array');
      end;
      7, 8, 9: begin { LSL, ASR, ROR }
        CheckInt(y);
        if x.typ^.form in [orb.tInt, orb.tSet] then begin
          org.Shift(fct - 7, x, y); restyp := x.typ;
        end else ors.Mark('bad type');
      end;
      17: org.Adr_(x); { ADR }
      18: begin { SIZE }
        if x.mode = orb.clsTyp then org.MakeConstItem(x, orb.intType, x.typ^.size)
        else ors.Mark('must be a type');
      end;
    else
      ors.Mark('unsupported function');
    end;
    x.typ := restyp;
  end else ors.Mark('wrong nof params');
end;

{$I orp2.inc}

end.
