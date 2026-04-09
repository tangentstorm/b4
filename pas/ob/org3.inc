{ org3.inc — arithmetic, boolean, set, and relational operators }

procedure Neg(var x: Item);
begin
  if x.typ^.form = orb.tInt then begin
    if x.mode = orb.clsConst then x.a := -x.a
    else begin load(x); Emit(oC0); Emit(oSW); Emit(oSB) end;
  end else if x.typ^.form = orb.tSet then begin
    if x.mode = orb.clsConst then x.a := not x.a
    else begin load(x); Emit(oNT) end;
  end else
    ors.Mark('bad type for negation');
end;

procedure AddOp(op: longint; var x, y: Item);
begin
  if op = ors.sPLUS then begin
    if (x.mode = orb.clsConst) and (y.mode = orb.clsConst) then
      x.a := x.a + y.a
    else begin load(x); load(y); Emit(oAD) end;
  end else begin { minus }
    if (x.mode = orb.clsConst) and (y.mode = orb.clsConst) then
      x.a := x.a - y.a
    else begin load(x); load(y); Emit(oSW); Emit(oSB) end;
      { b4 sb does: -pop + pop, so we need swap first for x-y }
  end;
end;

procedure MulOp(var x, y: Item);
begin
  if (x.mode = orb.clsConst) and (y.mode = orb.clsConst) then
    x.a := x.a * y.a
  else begin load(x); load(y); Emit(oML) end;
end;

procedure DivOp(op: longint; var x, y: Item);
begin
  if op = ors.sDIV then begin
    if (x.mode = orb.clsConst) and (y.mode = orb.clsConst) then begin
      if y.a > 0 then x.a := x.a div y.a
      else ors.Mark('bad divisor');
    end else begin
      load(x); load(y);
      Emit(oSW); { b4 dv: pop divisor, pop dividend }
      Emit(oDV);
    end;
  end else begin { MOD }
    if (x.mode = orb.clsConst) and (y.mode = orb.clsConst) then begin
      if y.a > 0 then x.a := x.a mod y.a
      else ors.Mark('bad modulus');
    end else begin
      load(x); load(y);
      Emit(oSW);
      Emit(oMD);
    end;
  end;
end;

procedure RealOp(op: integer; var x, y: Item);
begin
  ors.Mark('REAL not supported on b4vm');
end;

{ ---- Boolean operators with short-circuit evaluation ---- }

procedure Not_(var x: Item);
var t: longint;
begin
  if x.mode <> mCond then loadCond(x);
  { flip condition: swap true/false chains }
  x.r := 1 - x.r;
  { swap chain pointers }
  t := x.a; x.a := x.b; x.b := t;
end;

procedure And1(var x: Item);
begin
  { x & ... : if x is false, short-circuit }
  if x.mode <> mCond then loadCond(x);
  { emit: if TOS = 0 then jump to false chain }
  load(x);
  Emit(oH0);       { hop if zero }
  Emit(byte(0));    { placeholder offset }
  x.a := pc - 1;   { save fixup location (false chain) }
  x.b := 0;
end;

procedure And2(var x, y: Item);
begin
  if y.mode <> mCond then loadCond(y);
  { merge false chains }
  x.a := y.a; { take y's false chain }
  x.b := y.b;
  x.r := y.r;
end;

procedure Or1(var x: Item);
begin
  { x OR ... : if x is true, short-circuit }
  if x.mode <> mCond then loadCond(x);
  load(x);
  { if nonzero, jump to true chain }
  Emit(oDU);       { duplicate for test }
  Emit(oH0);       { hop if zero (i.e., skip the jump if false) }
  Emit(byte(3));    { skip over the jm }
  Emit(oZP);       { drop the dup (we'll push 1 later) }
  Emit(oHP);       { hop forward }
  Emit(byte(0));    { placeholder }
  x.b := pc - 1;   { save fixup location (true chain) }
  x.a := 0;
  Emit(oZP);       { drop the zero }
end;

procedure Or2(var x, y: Item);
begin
  if y.mode <> mCond then loadCond(y);
  x.a := y.a;
  x.b := y.b;
  x.r := y.r;
end;

{ ---- Relational operators ---- }
{ These compare two values and leave a boolean (0 or -1) on the stack }

procedure EmitCompare(op: integer);
begin
  { stack has: x y (x pushed first, y on top)
    b4 eq: pops two, pushes -1 if equal, 0 otherwise
    b4 lt: pops t=TOS, pops n=NOS, pushes -1 if n<t, 0 otherwise }
  case op of
    ors.sEQL: Emit(oEQ);
    ors.sNEQ: begin Emit(oEQ); Emit(oNT) end; { NOT equal }
    ors.sLSS: Emit(oLT);
    ors.sGEQ: begin Emit(oLT); Emit(oNT) end; { NOT less = GEQ }
    ors.sGTR: begin Emit(oSW); Emit(oLT) end;  { swap and less }
    ors.sLEQ: begin Emit(oSW); Emit(oLT); Emit(oNT) end; { swap, less, not = LEQ }
  end;
end;

procedure IntRelation(op: integer; var x, y: Item);
begin
  if (x.mode = orb.clsConst) and (y.mode = orb.clsConst) then begin
    { constant fold }
    case op of
      ors.sEQL: if x.a = y.a then x.a := 1 else x.a := 0;
      ors.sNEQ: if x.a <> y.a then x.a := 1 else x.a := 0;
      ors.sLSS: if x.a < y.a then x.a := 1 else x.a := 0;
      ors.sGEQ: if x.a >= y.a then x.a := 1 else x.a := 0;
      ors.sGTR: if x.a > y.a then x.a := 1 else x.a := 0;
      ors.sLEQ: if x.a <= y.a then x.a := 1 else x.a := 0;
    end;
    x.typ := orb.boolType;
  end else begin
    load(x); load(y);
    EmitCompare(op);
    { result is -1 (true) or 0 (false) on stack }
    { normalize to 1/0: -1 AND 1 = 1, 0 AND 1 = 0 }
    Emit(oC1); Emit(oAN);
    x.mode := mReg;
    x.typ := orb.boolType;
  end;
end;

procedure SetRelation(op: integer; var x, y: Item);
begin
  if (op = ors.sEQL) or (op = ors.sNEQ) then begin
    load(x); load(y);
    EmitCompare(op);
    Emit(oC1); Emit(oAN);
    x.mode := mReg;
    x.typ := orb.boolType;
  end else
    ors.Mark('illegal set relation');
end;

procedure RealRelation(op: integer; var x, y: Item);
begin
  ors.Mark('REAL not supported on b4vm');
end;

procedure StringRelation(op: integer; var x, y: Item);
begin
  ors.Mark('string comparison not yet implemented');
  load(x); load(y);
  Emit(oEQ); Emit(oC1); Emit(oAN);
  x.mode := mReg;
  x.typ := orb.boolType;
end;

{ ---- Set operators ---- }

procedure Singleton(var x: Item);
begin
  { x := {x} — create set with single element }
  if x.mode = orb.clsConst then
    x.a := longint(1) shl x.a
  else begin
    load(x);
    Emit(oC1); Emit(oSW); Emit(oSH); { 1 << x }
  end;
end;

procedure Set_(var x, y: Item);
begin
  { x := {x..y} }
  if (x.mode = orb.clsConst) and (y.mode = orb.clsConst) then begin
    if x.a <= y.a then
      x.a := longint(longint(2) shl y.a) - (longint(1) shl x.a)
    else
      x.a := 0;
  end else begin
    { runtime: ((-1) << x) AND NOT ((-2) << y) }
    load(x);
    Emit(oN1); Emit(oSW); Emit(oSH); { -1 << x }
    load(y);
    EmitPushInt(-2);
    Emit(oSW); Emit(oSH); { -2 << y }
    Emit(oNT);             { NOT (-2 << y) }
    Emit(oAN);             { AND }
    x.mode := mReg;
  end;
end;

procedure In_(var x, y: Item);
begin
  { x IN y: test if bit x is set in y }
  load(y); load(x);
  Emit(oC1); Emit(oAD); { x+1 }
  Emit(oSW);
  { rotate y right by (x+1), then test sign bit }
  { Actually: shift right by x, test bit 0 }
  load(x);
  Emit(oC0); Emit(oSW); Emit(oSB); { negate for right shift }
  Emit(oSH);  { shift right }
  Emit(oC1); Emit(oAN); { mask bit 0 }
  x.mode := mReg;
  x.typ := orb.boolType;
end;

procedure SetOp(op: longint; var x, y: Item);
begin
  if (x.mode = orb.clsConst) and (y.mode = orb.clsConst) then begin
    case op of
      ors.sPLUS:  x.a := x.a or y.a;   { union }
      ors.sMINUS: x.a := x.a and (not y.a); { difference }
      ors.sTIMES: x.a := x.a and y.a;  { intersection }
      ors.sRDIV:  x.a := x.a xor y.a;  { symmetric diff }
    end;
  end else begin
    load(x); load(y);
    case op of
      ors.sPLUS:  Emit(oOR);
      ors.sMINUS: begin Emit(oNT); Emit(oAN) end;
      ors.sTIMES: Emit(oAN);
      ors.sRDIV:  Emit(oXR);
    end;
    x.mode := mReg;
  end;
end;
