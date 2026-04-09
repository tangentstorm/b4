{ org3.inc — arithmetic, boolean, set, relational operators (b4a text) }

procedure Neg(var x: Item);
begin
  if x.typ^.form = orb.tInt then begin
    if x.mode = orb.clsConst then x.a := -x.a
    else begin load(x); Wr('c0 sw sb ') end;
  end else if x.typ^.form = orb.tSet then begin
    if x.mode = orb.clsConst then x.a := not x.a
    else begin load(x); Wr('nt ') end;
  end else ors.Mark('bad type for negation');
end;

procedure AddOp(op: longint; var x, y: Item);
begin
  if op = ors.sPLUS then begin
    if (x.mode = orb.clsConst) and (y.mode = orb.clsConst) then x.a := x.a + y.a
    else begin load(x); load(y); Wr('ad ') end;
  end else begin
    if (x.mode = orb.clsConst) and (y.mode = orb.clsConst) then x.a := x.a - y.a
    else begin load(x); load(y); Wr('sb ') end;
  end;
end;

procedure MulOp(var x, y: Item);
begin
  if (x.mode = orb.clsConst) and (y.mode = orb.clsConst) then x.a := x.a * y.a
  else begin load(x); load(y); Wr('ml ') end;
end;

procedure DivOp(op: longint; var x, y: Item);
begin
  if op = ors.sDIV then begin
    if (x.mode = orb.clsConst) and (y.mode = orb.clsConst) then begin
      if y.a > 0 then x.a := x.a div y.a else ors.Mark('bad divisor');
    end else begin load(x); load(y); Wr('dv ') end;
  end else begin
    if (x.mode = orb.clsConst) and (y.mode = orb.clsConst) then begin
      if y.a > 0 then x.a := x.a mod y.a else ors.Mark('bad modulus');
    end else begin load(x); load(y); Wr('md ') end;
  end;
end;

procedure RealOp(op: integer; var x, y: Item);
begin ors.Mark('REAL not supported on b4vm') end;

{ ---- Boolean operators ---- }

procedure Not_(var x: Item);
var t: longint;
begin
  if x.mode = orb.clsConst then begin
    if x.a = 0 then x.a := 1 else x.a := 0;
  end else begin
    load(x);
    { boolean NOT: if 0 push 1, else push 0 }
    { use: eq with 0 — (x 0 eq) gives -1 when x=0, 0 when x<>0, then c1 an }
    Wr('nt ');
  end;
end;

procedure And1(var x: Item);
begin
  load(x); { first operand on stack }
end;

procedure And2(var x, y: Item);
begin
  load(y);
  Wr('an '); { bitwise AND then normalize }
  x.mode := mReg;
end;

procedure Or1(var x: Item);
begin
  load(x); { first operand on stack }
end;

procedure Or2(var x, y: Item);
begin
  load(y);
  Wr('or '); { bitwise OR then normalize }
  x.mode := mReg;
end;

{ ---- Relational operators ---- }

procedure EmitCompare(op: integer);
begin
  case op of
    ors.sEQL: Wr('eq ');
    ors.sNEQ: Wr('eq nt ');
    ors.sLSS: Wr('lt ');
    ors.sGEQ: Wr('lt nt ');
    ors.sGTR: Wr('sw lt ');
    ors.sLEQ: Wr('sw lt nt ');
  end;
end;

procedure IntRelation(op: integer; var x, y: Item);
begin
  if (x.mode = orb.clsConst) and (y.mode = orb.clsConst) then begin
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
    load(x); load(y); EmitCompare(op);
    x.mode := mReg; x.typ := orb.boolType;
  end;
end;

procedure SetRelation(op: integer; var x, y: Item);
begin
  if (op = ors.sEQL) or (op = ors.sNEQ) then begin
    load(x); load(y); EmitCompare(op);
    x.mode := mReg; x.typ := orb.boolType;
  end else ors.Mark('illegal set relation');
end;

procedure RealRelation(op: integer; var x, y: Item);
begin ors.Mark('REAL not supported on b4vm') end;

procedure StringRelation(op: integer; var x, y: Item);
begin
  { load addresses into registers K, L. Result bytes in M, N. }
  if x.typ^.form = orb.tString then
    Wr('li >_str' + IntToStr(x.a) + ' ')
  else loadAdr(x);
  Wr('!K ');
  if y.typ^.form = orb.tString then
    Wr('li >_str' + IntToStr(y.a) + ' ')
  else loadAdr(y);
  WrLn('!L');
  { loop: read bytes into M and N, compare }
  WrLn('.w @K rb !M @L rb !N @M @N eq @M an');
  WrLn('.d @K c1 ad !K @L c1 ad !L .o');
  { exit: compare final M and N }
  WrLn('@M @N');
  EmitCompare(op);
  x.mode := mReg;
  x.typ := orb.boolType;
end;

{ ---- Set operators ---- }

procedure Singleton(var x: Item);
begin
  if x.mode = orb.clsConst then x.a := longint(1) shl x.a
  else begin load(x); Wr('c1 sw sh ') end;
end;

procedure Set_(var x, y: Item);
begin
  if (x.mode = orb.clsConst) and (y.mode = orb.clsConst) then begin
    if x.a <= y.a then x.a := longint(2) shl y.a - longint(1) shl x.a else x.a := 0;
  end else begin
    load(x); Wr('n1 sw sh '); load(y); Wr('li FFFFFFFE sh nt an ');
    x.mode := mReg;
  end;
end;

procedure In_(var x, y: Item);
begin
  load(y); load(x);
  Wr('c0 sw sb sh c1 an '); { shift right by x, mask bit 0 }
  x.mode := mReg; x.typ := orb.boolType;
end;

procedure SetOp(op: longint; var x, y: Item);
begin
  if (x.mode = orb.clsConst) and (y.mode = orb.clsConst) then begin
    case op of
      ors.sPLUS:  x.a := x.a or y.a;
      ors.sMINUS: x.a := x.a and (not y.a);
      ors.sTIMES: x.a := x.a and y.a;
      ors.sRDIV:  x.a := x.a xor y.a;
    end;
  end else begin
    load(x); load(y);
    case op of
      ors.sPLUS:  Wr('or ');
      ors.sMINUS: Wr('nt an ');
      ors.sTIMES: Wr('an ');
      ors.sRDIV:  Wr('xr ');
    end;
    x.mode := mReg;
  end;
end;
