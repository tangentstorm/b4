{ orp2.inc — expressions, statements, declarations, module }

procedure element(var x: org.Item);
var y: org.Item;
begin
  expression(x); CheckSetVal(x);
  if sym = ors.sUPTO then begin
    ors.Get(sym); expression(y); CheckSetVal(y); org.Set_(x, y)
  end else
    org.Singleton(x);
  x.typ := orb.setType;
end;

procedure set_(var x: org.Item);
var y: org.Item;
begin
  if sym >= ors.sIF then begin
    if sym <> ors.sRBRACE then ors.Mark('} missing');
    org.MakeConstItem(x, orb.setType, 0);
  end else begin
    element(x);
    while (sym < ors.sRPAREN) or (sym > ors.sRBRACE) do begin
      if sym = ors.sCOMMA then ors.Get(sym)
      else if sym <> ors.sRBRACE then ors.Mark('missing comma');
      element(y); org.SetOp(ors.sPLUS, x, y);
    end;
  end;
end;

procedure factor(var x: org.Item);
var obj: orb.PObj; rx: longint;
begin
  if (sym < ors.sCHAR) or (sym > ors.sIDENT) then begin
    ors.Mark('expression expected');
    repeat ors.Get(sym) until (sym >= ors.sCHAR) and (sym <= ors.sIDENT);
  end;
  if sym = ors.sIDENT then begin
    qualident(obj);
    if obj^.cls = orb.clsSFunc then
      StandFunc(x, obj^.val, obj^.typ)
    else begin
      org.MakeItem(x, obj, level); selector(x);
      if sym = ors.sLPAREN then begin
        ors.Get(sym);
        if (x.typ^.form = orb.tProc) and (x.typ^.base^.form <> orb.tNoTyp) then begin
          org.PrepCall(x, rx); ParamList(x); org.Call(x, rx);
          x.typ := x.typ^.base;
        end else begin ors.Mark('not a function'); ParamList(x) end;
      end;
    end;
  end
  else if sym = ors.sINT then begin
    org.MakeConstItem(x, orb.intType, ors.ival); ors.Get(sym)
  end
  else if sym = ors.sREAL then begin
    org.MakeRealItem(x, ors.rval); ors.Get(sym)
  end
  else if sym = ors.sCHAR then begin
    org.MakeConstItem(x, orb.charType, ors.ival); ors.Get(sym)
  end
  else if sym = ors.sNIL then begin
    ors.Get(sym); org.MakeConstItem(x, orb.nilType, 0)
  end
  else if sym = ors.sSTRING then begin
    org.MakeStringItem(x, ors.slen); ors.Get(sym)
  end
  else if sym = ors.sLPAREN then begin
    ors.Get(sym); expression(x); Check(ors.sRPAREN, 'no )')
  end
  else if sym = ors.sLBRACE then begin
    ors.Get(sym); set_(x); Check(ors.sRBRACE, 'no }')
  end
  else if sym = ors.sNOT then begin
    ors.Get(sym); factor(x); CheckBool(x); org.Not_(x)
  end
  else if sym = ors.sFALSE then begin
    ors.Get(sym); org.MakeConstItem(x, orb.boolType, 0)
  end
  else if sym = ors.sTRUE then begin
    ors.Get(sym); org.MakeConstItem(x, orb.boolType, 1)
  end
  else begin
    ors.Mark('not a factor'); org.MakeItem(x, nil, level)
  end;
end;

procedure term(var x: org.Item);
var y: org.Item; op, f: integer;
begin
  factor(x); f := x.typ^.form;
  while (sym >= ors.sTIMES) and (sym <= ors.sAND) do begin
    op := sym; ors.Get(sym);
    if op = ors.sTIMES then begin
      if f = orb.tInt then begin factor(y); CheckInt(y); org.MulOp(x, y) end
      else if f = orb.tSet then begin factor(y); CheckSet(y); org.SetOp(op, x, y) end
      else ors.Mark('bad type');
    end else if (op = ors.sDIV) or (op = ors.sMOD) then begin
      CheckInt(x); factor(y); CheckInt(y); org.DivOp(op, x, y)
    end else if op = ors.sRDIV then begin
      if f = orb.tSet then begin factor(y); CheckSet(y); org.SetOp(op, x, y) end
      else ors.Mark('bad type');
    end else begin
      CheckBool(x); org.And1(x); factor(y); CheckBool(y); org.And2(x, y)
    end;
  end;
end;

procedure SimpleExpression(var x: org.Item);
var y: org.Item; op: integer;
begin
  if sym = ors.sMINUS then begin
    ors.Get(sym); term(x);
    if x.typ^.form in [orb.tInt, orb.tSet] then org.Neg(x) else CheckInt(x);
  end else if sym = ors.sPLUS then begin
    ors.Get(sym); term(x)
  end else
    term(x);
  while (sym >= ors.sPLUS) and (sym <= ors.sOR) do begin
    op := sym; ors.Get(sym);
    if op = ors.sOR then begin
      org.Or1(x); CheckBool(x); term(y); CheckBool(y); org.Or2(x, y)
    end else if x.typ^.form = orb.tInt then begin
      term(y); CheckInt(y); org.AddOp(op, x, y)
    end else begin
      CheckSet(x); term(y); CheckSet(y); org.SetOp(op, x, y)
    end;
  end;
end;

procedure expression0(var x: org.Item);
var y: org.Item; obj: orb.PObj; rel: integer;
begin
  SimpleExpression(x);
  if (sym >= ors.sEQL) and (sym <= ors.sGEQ) then begin
    rel := sym; ors.Get(sym); SimpleExpression(y);
    if CompTypes(x.typ, y.typ, false) then begin
      if x.typ^.form in [orb.tChar, orb.tInt] then org.IntRelation(rel, x, y)
      else if x.typ^.form = orb.tSet then org.SetRelation(rel, x, y)
      else if x.typ^.form in [orb.tPointer, orb.tProc, orb.tNilTyp] then begin
        if rel <= ors.sNEQ then org.IntRelation(rel, x, y)
        else ors.Mark('only = or #');
      end else ors.Mark('illegal comparison');
    end else if ((x.typ^.form = orb.tArray) and (x.typ^.base^.form = orb.tChar)) or
               (x.typ^.form = orb.tString) then begin
      { string or char array comparison }
      if ((y.typ^.form = orb.tArray) and (y.typ^.base^.form = orb.tChar)) or
         (y.typ^.form = orb.tString) then
        org.StringRelation(rel, x, y)
      else ors.Mark('incompatible types');
    end else ors.Mark('incompatible types');
    x.typ := orb.boolType;
  end else if sym = ors.sIN then begin
    ors.Get(sym); SimpleExpression(y);
    if (x.typ^.form = orb.tInt) and (y.typ^.form = orb.tSet) then org.In_(x, y)
    else ors.Mark('illegal operands of IN');
    x.typ := orb.boolType;
  end else if sym = ors.sIS then begin
    ors.Get(sym); qualident(obj); TypeTest(x, obj^.typ, false);
    x.typ := orb.boolType;
  end;
end;

{ ---- Statements ---- }

procedure OutProc(pno: longint);
var x, y: org.Item;
begin
  case pno of
    outLn: begin
      { Out.Ln — no parameters, may or may not have () }
      if sym = ors.sLPAREN then begin
        ors.Get(sym); Check(ors.sRPAREN, ') expected');
      end;
      org.WrLn('lb 0A lb 6F io');
    end;
    outChar: begin
      Check(ors.sLPAREN, '( expected');
      expression(x);
      Check(ors.sRPAREN, ') expected');
      org.load(x); org.WrLn('lb 6F io');
    end;
    outInt: begin
      Check(ors.sLPAREN, '( expected');
      expression(x);
      Check(ors.sCOMMA, ', expected');
      expression(y);
      Check(ors.sRPAREN, ') expected');
      org.load(x);
      org.load(y);
      org.WrLn('^O');
    end;
    outStr: begin
      Check(ors.sLPAREN, '( expected');
      expression(x);
      Check(ors.sRPAREN, ') expected');
      { string printing: iterate chars, call rtWriteChar for each }
      { for now, just drop it }
      ors.Mark('Out.String not yet implemented');
    end;
  end;
end;

procedure InProc(pno: longint);
var x: org.Item;
begin
  case pno of
    inLine: begin
      { In.Line — read a line, no params }
      if sym = ors.sLPAREN then begin
        ors.Get(sym); Check(ors.sRPAREN, ') expected');
      end;
      org.WrLn('rt_rline');
    end;
    inChar: begin
      { In.Char(VAR ch) — read one char into variable }
      Check(ors.sLPAREN, '( expected');
      expression(x);
      Check(ors.sRPAREN, ') expected');
      org.loadAdr(x); { push address of variable }
      org.WrLn('rt_rch');
    end;
    inInt: begin
      { In.Int(VAR n) — read line and parse integer }
      Check(ors.sLPAREN, '( expected');
      expression(x);
      Check(ors.sRPAREN, ') expected');
      org.loadAdr(x); { push address of variable }
      org.WrLn('rt_rint');
    end;
  end;
end;

procedure StandProc(pno: longint);
var nap, npar: longint;
    x, y, z: org.Item;
begin
  { check for Out module procedures }
  if (pno div 10) in [outInt, outChar, outLn, outStr] then begin
    OutProc(pno div 10);
    exit;
  end;
  { check for In module procedures }
  if (pno div 10) in [inInt, inChar, inLine] then begin
    InProc(pno div 10);
    exit;
  end;
  Check(ors.sLPAREN, 'no (');
  npar := pno mod 10; pno := pno div 10;
  expression(x); nap := 1;
  if sym = ors.sCOMMA then begin
    ors.Get(sym); expression(y); nap := 2; z.typ := orb.noType;
    while sym = ors.sCOMMA do begin ors.Get(sym); expression(z); inc(nap) end;
  end else y.typ := orb.noType;
  Check(ors.sRPAREN, 'no )');
  if (npar = nap) or (pno in [0, 1]) then begin
    if pno in [0, 1] then begin
      CheckInt(x); CheckReadOnly(x);
      if y.typ <> orb.noType then CheckInt(y);
      org.Increment(pno, x, y);
    end else if pno in [2, 3] then begin
      CheckSet(x); CheckReadOnly(x); CheckInt(y);
      org.Include(pno - 2, x, y);
    end else if pno = 4 then begin
      CheckBool(x); org.Assert_(x);
    end else if pno = 5 then begin
      CheckReadOnly(x);
      if (x.typ^.form = orb.tPointer) and (x.typ^.base^.form = orb.tRecord) then
        org.New_(x)
      else ors.Mark('not a pointer to record');
    end else if pno = 10 then begin
      { SYSTEM.GET(addr, VAR val) }
      CheckInt(x); org.Get_(x, y);
    end else if pno = 11 then begin
      { SYSTEM.PUT(addr, val) }
      CheckInt(x); org.Put_(x, y);
    end else if pno = 12 then begin
      { SYSTEM.COPY }
      org.Copy_(x, y, z);
    end;
  end else ors.Mark('wrong nof parameters');
end;

procedure StatSequence;
var obj: orb.PObj;
    x, y, z, w: org.Item;
    L0, L1, rx: longint;
begin
  repeat
    obj := nil;
    if not ((sym = ors.sIDENT) or (sym >= ors.sIF) and (sym <= ors.sFOR)
        or (sym >= ors.sSEMICOLON)) then begin
      ors.Mark('statement expected');
      repeat ors.Get(sym) until (sym = ors.sIDENT) or (sym >= ors.sIF);
    end;
    if sym = ors.sIDENT then begin
      qualident(obj); org.MakeItem(x, obj, level);
      if x.mode = orb.clsSProc then StandProc(obj^.val)
      else begin
        selector(x);
        if sym = ors.sBECOMES then begin
          ors.Get(sym); CheckReadOnly(x); expression(y);
          if CompTypes(x.typ, y.typ, false) or
             (x.typ^.form = orb.tInt) and (y.typ^.form = orb.tInt) then begin
            if (x.typ^.form <= orb.tPointer) or (x.typ^.form = orb.tProc) then
              org.Store(x, y)
            else if y.typ^.size <> 0 then
              org.StoreStruct(x, y);
          end else if (x.typ^.form = orb.tChar) and (y.typ^.form = orb.tString) and (y.b = 2) then begin
            org.StrToChar(y); org.Store(x, y)
          end else if (x.typ^.form = orb.tArray) and (x.typ^.base^.form = orb.tChar) and
              (y.typ^.form = orb.tString) then
            org.CopyString(y, x)
          else ors.Mark('illegal assignment');
        end else if sym = ors.sEQL then begin
          ors.Mark('should be :='); ors.Get(sym); expression(y)
        end else if sym = ors.sLPAREN then begin
          ors.Get(sym);
          if (x.typ^.form = orb.tProc) and (x.typ^.base^.form = orb.tNoTyp) then begin
            org.PrepCall(x, rx); ParamList(x); org.Call(x, rx);
          end else begin ors.Mark('not a procedure'); ParamList(x) end;
        end else if x.typ^.form = orb.tProc then begin
          if x.typ^.nofpar > 0 then ors.Mark('missing parameters');
          if x.typ^.base^.form = orb.tNoTyp then begin
            org.PrepCall(x, rx); org.Call(x, rx)
          end else ors.Mark('not a procedure');
        end else if x.mode = orb.clsTyp then
          ors.Mark('illegal assignment')
        else ors.Mark('not a procedure');
      end;
    end else if sym = ors.sIF then begin
      ors.Get(sym); expression(x); CheckBool(x); org.CFJump(x);
      Check(ors.sTHEN, 'no THEN'); StatSequence; L0 := 0;
      while sym = ors.sELSIF do begin
        ors.Get(sym); org.FJump(L0); org.Fixup(x); expression(x); CheckBool(x);
        org.CFJump(x); Check(ors.sTHEN, 'no THEN'); StatSequence;
      end;
      if sym = ors.sELSE then begin
        ors.Get(sym); org.FJump(L0); org.Fixup(x); StatSequence
      end else org.Fixup(x);
      org.FixLink(L0); Check(ors.sEND, 'no END');
    end else if sym = ors.sWHILE then begin
      ors.Get(sym); L0 := org.Here; expression(x); CheckBool(x); org.CFJump(x);
      Check(ors.sDO, 'no DO'); StatSequence; org.BJump(L0);
      while sym = ors.sELSIF do begin
        ors.Get(sym); org.Fixup(x); expression(x); CheckBool(x); org.CFJump(x);
        Check(ors.sDO, 'no DO'); StatSequence; org.BJump(L0);
      end;
      org.Fixup(x); Check(ors.sEND, 'no END');
    end else if sym = ors.sREPEAT then begin
      ors.Get(sym); L0 := org.Here; StatSequence;
      if sym = ors.sUNTIL then begin
        ors.Get(sym); expression(x); CheckBool(x); org.CBJump(x, L0)
      end else ors.Mark('missing UNTIL');
    end else if sym = ors.sFOR then begin
      ors.Get(sym);
      if sym = ors.sIDENT then begin
        qualident(obj); org.MakeItem(x, obj, level); CheckInt(x); CheckReadOnly(x);
        if sym = ors.sBECOMES then begin
          ors.Get(sym); expression(y); CheckInt(y); org.For0(x, y);
          L0 := org.Here;
          Check(ors.sTO, 'no TO'); expression(z); CheckInt(z);
          obj^.rdo := true;
          if sym = ors.sBY then begin
            ors.Get(sym); expression(w); CheckConst(w); CheckInt(w)
          end else org.MakeConstItem(w, orb.intType, 1);
          Check(ors.sDO, 'no DO');
          org.For1(x, y, z, w, L1);
          StatSequence; Check(ors.sEND, 'no END');
          org.For2(x, y, w); org.BJump(L0); org.FixLink(L1);
          obj^.rdo := false;
        end else ors.Mark(':= expected');
      end else ors.Mark('identifier expected');
    end else if sym = ors.sCASE then begin
      { Numeric CASE: compile as IF/ELSIF chain }
      { Save case expression to register N, then compare in each arm }
      ors.Get(sym);
      expression(x); org.load(x);
      org.Wr('!N '); { save case value to register N }
      Check(ors.sOF, 'OF expected');
      L0 := 0;
      if (sym <> ors.sEND) and (sym <> ors.sBAR) then begin
        { first arm: label : statements }
        expression(y); CheckConst(y); { case label must be constant }
        org.Wr('@N '); org.WrPushInt(y.a); org.Wr('eq ');
        org.CFJump(x);
        Check(ors.sCOLON, ': expected');
        StatSequence;
        while sym = ors.sBAR do begin
          ors.Get(sym);
          org.FJump(L0);
          org.Fixup(x);
          expression(y); CheckConst(y);
          org.Wr('@N '); org.WrPushInt(y.a); org.Wr('eq ');
          org.CFJump(x);
          Check(ors.sCOLON, ': expected');
          StatSequence;
        end;
        org.Fixup(x);
        org.FixLink(L0);
      end;
      Check(ors.sEND, 'no END');
    end;
    org.CheckRegs;
    if sym = ors.sSEMICOLON then ors.Get(sym)
    else if sym < ors.sSEMICOLON then ors.Mark('missing semicolon?');
  until sym > ors.sSEMICOLON;
end;

{ ---- Type and declaration parsing ---- }

procedure IdentList(cls: integer; var first: orb.PObj);
var obj: orb.PObj;
begin
  if sym = ors.sIDENT then begin
    orb.NewObj(first, ors.id, cls); ors.Get(sym); CheckExport(first^.expo);
    while sym = ors.sCOMMA do begin
      ors.Get(sym);
      if sym = ors.sIDENT then begin
        orb.NewObj(obj, ors.id, cls); ors.Get(sym); CheckExport(obj^.expo)
      end else ors.Mark('ident?');
    end;
    if sym = ors.sCOLON then ors.Get(sym) else ors.Mark(':?');
  end else first := nil;
end;

procedure ArrayType(var tp: orb.PType);
var x: org.Item; len: longint;
begin
  new(tp); fillchar(tp^, sizeof(orb.TType), 0);
  tp^.form := orb.tNoTyp;
  if sym = ors.sOF then len := -1
  else begin
    expression(x);
    if (x.mode = orb.clsConst) and (x.typ^.form = orb.tInt) and (x.a >= 0) then len := x.a
    else begin len := 0; ors.Mark('not a valid length') end;
  end;
  if sym = ors.sOF then begin
    ors.Get(sym); Type_(tp^.base);
  end else if sym = ors.sCOMMA then begin
    ors.Get(sym); ArrayType(tp^.base)
  end else begin ors.Mark('missing OF'); tp^.base := orb.intType end;
  if len >= 0 then
    tp^.size := ((len * tp^.base^.size) + 3) div 4 * 4
  else
    tp^.size := 2 * org.WordSize;
  tp^.form := orb.tArray; tp^.len := len;
end;

procedure RecordType(var tp: orb.PType);
var obj, obj0, new_, bot: orb.PObj;
    typ0, ftp: orb.PType;
    offset, off, n: longint;
begin
  new(tp); fillchar(tp^, sizeof(orb.TType), 0);
  tp^.form := orb.tNoTyp; tp^.base := nil;
  tp^.mno := -level; tp^.nofpar := 0;
  offset := 0; bot := nil;
  if sym = ors.sLPAREN then begin
    ors.Get(sym); { record extension }
    if sym = ors.sIDENT then begin
      qualident(obj0);
      if obj0^.cls = orb.clsTyp then begin
        if obj0^.typ^.form = orb.tRecord then begin
          tp^.base := obj0^.typ;
          tp^.nofpar := tp^.base^.nofpar + 1;
          bot := tp^.base^.dsc; { inherit base fields }
          offset := tp^.base^.size;
        end else ors.Mark('invalid extension');
      end else ors.Mark('type expected');
    end else ors.Mark('ident expected');
    Check(ors.sRPAREN, 'no )');
  end;
  while sym = ors.sIDENT do begin
    n := 0; obj := bot;
    while sym = ors.sIDENT do begin
      obj0 := obj;
      while (obj0 <> nil) and (obj0^.name <> ors.id) do obj0 := obj0^.next;
      if obj0 <> nil then ors.Mark('mult def');
      new(new_); fillchar(new_^, sizeof(orb.TObj), 0);
      ors.CopyId(new_^.name); new_^.cls := orb.clsFld;
      new_^.next := obj; obj := new_; inc(n);
      ors.Get(sym); CheckExport(new_^.expo);
      if (sym <> ors.sCOMMA) and (sym <> ors.sCOLON) then ors.Mark('comma expected')
      else if sym = ors.sCOMMA then ors.Get(sym);
    end;
    Check(ors.sCOLON, 'colon expected'); Type_(ftp);
    if ftp^.size > 1 then offset := (offset + 3) div 4 * 4;
    offset := offset + n * ftp^.size; off := offset; obj0 := obj;
    while obj0 <> bot do begin
      obj0^.typ := ftp; obj0^.lev := 0;
      off := off - ftp^.size; obj0^.val := off;
      obj0 := obj0^.next;
    end;
    bot := obj;
    if sym = ors.sSEMICOLON then ors.Get(sym)
    else if sym <> ors.sEND then ors.Mark('; or END');
  end;
  tp^.form := orb.tRecord; tp^.dsc := bot;
  tp^.size := (offset + 3) div 4 * 4;
end;

procedure FPSection(var adr: longint; var nofpar: integer);
var obj, first: orb.PObj; tp: orb.PType;
    parsize: longint; cl: integer; rdo: boolean;
begin
  if sym = ors.sVAR then begin ors.Get(sym); cl := orb.clsPar end
  else cl := orb.clsVar;
  IdentList(cl, first); FormalType(tp, 0); rdo := false;
  if (cl = orb.clsVar) and (tp^.form >= orb.tArray) then begin
    cl := orb.clsPar; rdo := true
  end;
  if (tp^.form = orb.tArray) and (tp^.len < 0) or (tp^.form = orb.tRecord) then
    parsize := 2 * org.WordSize
  else parsize := org.WordSize;
  obj := first;
  while obj <> nil do begin
    inc(nofpar); obj^.cls := cl; obj^.typ := tp;
    obj^.rdo := rdo; obj^.lev := level; obj^.val := adr;
    adr := adr + parsize; obj := obj^.next;
  end;
end;

procedure ProcedureType(ptype: orb.PType; var parblksize: longint);
var obj: orb.PObj; size: longint; nofpar: integer;
begin
  ptype^.base := orb.noType; size := parblksize; nofpar := 0; ptype^.dsc := nil;
  if sym = ors.sLPAREN then begin
    ors.Get(sym);
    if sym = ors.sRPAREN then ors.Get(sym)
    else begin
      FPSection(size, nofpar);
      while sym = ors.sSEMICOLON do begin ors.Get(sym); FPSection(size, nofpar) end;
      Check(ors.sRPAREN, 'no )');
    end;
    ptype^.nofpar := nofpar; parblksize := size;
    if sym = ors.sCOLON then begin
      ors.Get(sym);
      if sym = ors.sIDENT then begin
        qualident(obj);
        if (obj^.cls = orb.clsTyp) and (obj^.typ^.form in [orb.tByte..orb.tPointer, orb.tProc]) then
          ptype^.base := obj^.typ
        else ors.Mark('illegal function type');
      end else ors.Mark('type identifier expected');
    end;
  end;
end;

procedure FormalType0(var tp: orb.PType; dim: integer);
var obj: orb.PObj; dmy: longint;
begin
  if sym = ors.sIDENT then begin
    qualident(obj);
    if obj^.cls = orb.clsTyp then tp := obj^.typ
    else begin ors.Mark('not a type'); tp := orb.intType end;
  end else if sym = ors.sARRAY then begin
    ors.Get(sym); Check(ors.sOF, 'OF ?');
    if dim >= 1 then ors.Mark('multi-dim open arrays not implemented');
    new(tp); fillchar(tp^, sizeof(orb.TType), 0);
    tp^.form := orb.tArray; tp^.len := -1;
    tp^.size := 2 * org.WordSize;
    FormalType(tp^.base, dim + 1);
  end else if sym = ors.sPROCEDURE then begin
    ors.Get(sym); orb.OpenScope;
    new(tp); fillchar(tp^, sizeof(orb.TType), 0);
    tp^.form := orb.tProc; tp^.size := org.WordSize;
    dmy := 0; ProcedureType(tp, dmy);
    tp^.dsc := orb.topScope^.next; orb.CloseScope;
  end else begin ors.Mark('identifier expected'); tp := orb.noType end;
end;

procedure Type0(var tp: orb.PType);
var dmy: longint; obj: orb.PObj; ptb: PtrBase;
begin
  tp := orb.intType;
  if (sym <> ors.sIDENT) and (sym < ors.sARRAY) then begin
    ors.Mark('not a type');
    repeat ors.Get(sym) until (sym = ors.sIDENT) or (sym >= ors.sARRAY);
  end;
  if sym = ors.sIDENT then begin
    qualident(obj);
    if obj^.cls = orb.clsTyp then begin
      if (obj^.typ <> nil) and (obj^.typ^.form <> orb.tNoTyp) then tp := obj^.typ;
    end else ors.Mark('not a type or undefined');
  end else if sym = ors.sARRAY then begin
    ors.Get(sym); ArrayType(tp)
  end else if sym = ors.sRECORD then begin
    ors.Get(sym); RecordType(tp); Check(ors.sEND, 'no END')
  end else if sym = ors.sPOINTER then begin
    ors.Get(sym); Check(ors.sTO, 'no TO');
    new(tp); fillchar(tp^, sizeof(orb.TType), 0);
    tp^.form := orb.tPointer; tp^.size := org.WordSize;
    tp^.base := orb.intType;
    if sym = ors.sIDENT then begin
      obj := orb.thisObj; ors.Get(sym);
      if obj <> nil then begin
        if (obj^.cls = orb.clsTyp) and (obj^.typ^.form in [orb.tRecord, orb.tNoTyp]) then
          tp^.base := obj^.typ
        else ors.Mark('no valid base type');
      end else begin
        { forward reference }
        new(ptb); ors.CopyId(ptb^.name);
        ptb^.typ := tp; ptb^.next := pbsList; pbsList := ptb;
      end;
    end else begin
      Type_(tp^.base);
      if tp^.base^.form <> orb.tRecord then ors.Mark('must point to record');
    end;
  end else if sym = ors.sPROCEDURE then begin
    ors.Get(sym); orb.OpenScope;
    new(tp); fillchar(tp^, sizeof(orb.TType), 0);
    tp^.form := orb.tProc; tp^.size := org.WordSize;
    dmy := 0; ProcedureType(tp, dmy);
    tp^.dsc := orb.topScope^.next; orb.CloseScope;
  end else ors.Mark('illegal type');
end;

{ ---- Declarations and procedures ---- }

procedure Declarations(var varsize: longint);
var obj, first: orb.PObj;
    x: org.Item; tp: orb.PType; ptbase: PtrBase;
    expo: boolean; id_: ors.Ident;
begin
  pbsList := nil;
  if (sym < ors.sCONST) and (sym <> ors.sEND) then begin
    ors.Mark('declaration?');
    repeat ors.Get(sym) until (sym >= ors.sCONST) or (sym = ors.sEND);
  end;
  if sym = ors.sCONST then begin
    ors.Get(sym);
    while sym = ors.sIDENT do begin
      ors.CopyId(id_); ors.Get(sym); CheckExport(expo);
      if sym = ors.sEQL then ors.Get(sym) else ors.Mark('= ?');
      expression(x);
      if (x.typ^.form = orb.tString) and (x.b = 2) then org.StrToChar(x);
      orb.NewObj(obj, id_, orb.clsConst); obj^.expo := expo;
      if x.mode = orb.clsConst then begin
        obj^.val := x.a; obj^.lev := x.b; obj^.typ := x.typ
      end else begin
        ors.Mark('expression not constant'); obj^.typ := orb.intType
      end;
      Check(ors.sSEMICOLON, '; missing');
    end;
  end;
  if sym = ors.sTYPE then begin
    ors.Get(sym);
    while sym = ors.sIDENT do begin
      ors.CopyId(id_); ors.Get(sym); CheckExport(expo);
      if sym = ors.sEQL then ors.Get(sym) else ors.Mark('=?');
      Type_(tp);
      orb.NewObj(obj, id_, orb.clsTyp); obj^.typ := tp;
      obj^.expo := expo; obj^.lev := level; tp^.typobj := obj;
      if expo and (obj^.typ^.form = orb.tRecord) then begin
        obj^.exno := exno; inc(exno)
      end else obj^.exno := 0;
      if tp^.form = orb.tRecord then begin
        ptbase := pbsList;
        while ptbase <> nil do begin
          if obj^.name = ptbase^.name then ptbase^.typ^.base := obj^.typ;
          ptbase := ptbase^.next;
        end;
        if level = 0 then org.BuildTD(tp, dc);
      end;
      Check(ors.sSEMICOLON, '; missing');
    end;
  end;
  if sym = ors.sVAR then begin
    ors.Get(sym);
    while sym = ors.sIDENT do begin
      IdentList(orb.clsVar, first); Type_(tp);
      obj := first;
      while obj <> nil do begin
        obj^.typ := tp; obj^.lev := level;
        if tp^.size > 1 then varsize := (varsize + 3) div 4 * 4;
        obj^.val := varsize; varsize := varsize + obj^.typ^.size;
        if obj^.expo then begin obj^.exno := exno; inc(exno) end;
        obj := obj^.next;
      end;
      Check(ors.sSEMICOLON, '; missing');
    end;
  end;
  varsize := (varsize + 3) div 4 * 4;
  ptbase := pbsList;
  while ptbase <> nil do begin
    if ptbase^.typ^.base^.form = orb.tInt then ors.Mark('undefined pointer base');
    ptbase := ptbase^.next;
  end;
  if (sym >= ors.sCONST) and (sym <= ors.sVAR) then ors.Mark('declaration in bad order');
end;

procedure ProcedureDecl;
var proc: orb.PObj;
    tp: orb.PType;
    procid: ors.Ident;
    x: org.Item;
    locblksize, parblksize, L: longint;
begin
  ors.Get(sym);
  if sym = ors.sTIMES then ors.Get(sym); { skip interrupt marker }
  if sym = ors.sIDENT then begin
    ors.CopyId(procid); ors.Get(sym);
    orb.NewObj(proc, ors.id, orb.clsConst); parblksize := 4;
    new(tp); fillchar(tp^, sizeof(orb.TType), 0);
    tp^.form := orb.tProc; tp^.size := org.WordSize; proc^.typ := tp;
    CheckExport(proc^.expo);
    if proc^.expo then begin proc^.exno := exno; inc(exno) end;
    orb.OpenScope; inc(level); proc^.val := -1;
    tp^.base := orb.noType;
    ProcedureType(tp, parblksize);
    Check(ors.sSEMICOLON, 'no ;'); locblksize := parblksize;
    Declarations(locblksize);
    { assign a unique proc ID }
    proc^.val := org.pc; inc(org.pc);
    proc^.typ^.dsc := orb.topScope^.next;
    if sym = ors.sPROCEDURE then begin
      { nested procs: skip over them, emit label after }
      L := 0; org.FJump(L);
      repeat ProcedureDecl; Check(ors.sSEMICOLON, 'no ;') until sym <> ors.sPROCEDURE;
      org.FixLink(L);
      proc^.typ^.dsc := orb.topScope^.next;
    end;
    org.WrLn('');
    org.WrLbl(org.modname + '_P' + IntToStr(proc^.val));
    org.WrComment('PROCEDURE ' + procid);
    org.Enter(parblksize, locblksize, false);
    if sym = ors.sBEGIN then begin ors.Get(sym); StatSequence end;
    if sym = ors.sRETURN then begin
      ors.Get(sym); expression(x);
      if tp^.base = orb.noType then ors.Mark('this is not a function')
      else if not CompTypes(tp^.base, x.typ, false) then ors.Mark('wrong result type');
    end else if tp^.base^.form <> orb.tNoTyp then begin
      ors.Mark('function without result'); tp^.base := orb.noType
    end;
    org.Return(tp^.base^.form, x, locblksize, false);
    orb.CloseScope; dec(level);
    Check(ors.sEND, 'no END');
    if sym = ors.sIDENT then begin
      if ors.id <> procid then ors.Mark('no match');
      ors.Get(sym);
    end else ors.Mark('no proc id');
  end;
end;

{ ---- Built-in Out module ---- }

procedure RegisterOutModule(const alias: ors.Ident);
var mod_, proc: orb.PObj; tp: orb.PType;
begin
  { create module object }
  new(mod_); fillchar(mod_^, sizeof(orb.TObj), 0);
  mod_^.name := alias;
  mod_^.cls := orb.clsMod;
  mod_^.rdo := true;
  mod_^.typ := orb.noType;
  mod_^.dsc := nil;

  { Out.Ln: PROCEDURE }
  new(proc); fillchar(proc^, sizeof(orb.TObj), 0);
  proc^.name := 'Ln'; proc^.cls := orb.clsSProc;
  proc^.val := outLn * 10 + 0; { 0 params }
  proc^.typ := orb.noType;
  proc^.next := mod_^.dsc; mod_^.dsc := proc;

  { Out.Char: PROCEDURE(ch: CHAR) }
  new(proc); fillchar(proc^, sizeof(orb.TObj), 0);
  proc^.name := 'Char'; proc^.cls := orb.clsSProc;
  proc^.val := outChar * 10 + 1; { 1 param }
  proc^.typ := orb.noType;
  proc^.next := mod_^.dsc; mod_^.dsc := proc;

  { Out.Int: PROCEDURE(n: INTEGER; width: INTEGER) }
  new(proc); fillchar(proc^, sizeof(orb.TObj), 0);
  proc^.name := 'Int'; proc^.cls := orb.clsSProc;
  proc^.val := outInt * 10 + 2; { 2 params }
  proc^.typ := orb.noType;
  proc^.next := mod_^.dsc; mod_^.dsc := proc;

  { Out.String: PROCEDURE(s: ARRAY OF CHAR) }
  new(proc); fillchar(proc^, sizeof(orb.TObj), 0);
  proc^.name := 'String'; proc^.cls := orb.clsSProc;
  proc^.val := outStr * 10 + 1; { 1 param }
  proc^.typ := orb.noType;
  proc^.next := mod_^.dsc; mod_^.dsc := proc;

  { link into top scope }
  mod_^.next := orb.topScope^.next;
  orb.topScope^.next := mod_;
end;

procedure RegisterInModule(const alias: ors.Ident);
var mod_, proc: orb.PObj;
begin
  new(mod_); fillchar(mod_^, sizeof(orb.TObj), 0);
  mod_^.name := alias;
  mod_^.cls := orb.clsMod;
  mod_^.rdo := true;
  mod_^.typ := orb.noType;
  mod_^.dsc := nil;

  { In.Line: PROCEDURE — read a line from stdin }
  new(proc); fillchar(proc^, sizeof(orb.TObj), 0);
  proc^.name := 'Line'; proc^.cls := orb.clsSProc;
  proc^.val := inLine * 10 + 0;
  proc^.typ := orb.noType;
  proc^.next := mod_^.dsc; mod_^.dsc := proc;

  { In.Char: PROCEDURE(VAR ch: CHAR) }
  new(proc); fillchar(proc^, sizeof(orb.TObj), 0);
  proc^.name := 'Char'; proc^.cls := orb.clsSProc;
  proc^.val := inChar * 10 + 1;
  proc^.typ := orb.noType;
  proc^.next := mod_^.dsc; mod_^.dsc := proc;

  { In.Int: PROCEDURE(VAR n: INTEGER) }
  new(proc); fillchar(proc^, sizeof(orb.TObj), 0);
  proc^.name := 'Int'; proc^.cls := orb.clsSProc;
  proc^.val := inInt * 10 + 1;
  proc^.typ := orb.noType;
  proc^.next := mod_^.dsc; mod_^.dsc := proc;

  mod_^.next := orb.topScope^.next;
  orb.topScope^.next := mod_;
end;

{ ---- Module ---- }

procedure Module;
var key: longint;
    obj, smod: orb.PObj;
    impid, impid1: ors.Ident;
begin
  writeln('  compiling...');
  ors.Get(sym);
  if sym = ors.sMODULE then begin
    ors.Get(sym);
    if sym = ors.sTIMES then begin version := 0; ors.Get(sym) end
    else version := 1;
    orb.Init; orb.OpenScope;
    if sym = ors.sIDENT then begin
      ors.CopyId(modid); ors.Get(sym);
      writeln('  module ', modid);
    end else ors.Mark('identifier expected');
    Check(ors.sSEMICOLON, 'no ;');
    level := 0; dc := 0; exno := 1; key := 0;
    if sym = ors.sIMPORT then begin
      ors.Get(sym);
      while sym = ors.sIDENT do begin
        ors.CopyId(impid); ors.Get(sym);
        if sym = ors.sBECOMES then begin
          ors.Get(sym);
          if sym = ors.sIDENT then begin ors.CopyId(impid1); ors.Get(sym) end
          else ors.Mark('id expected');
        end else impid1 := impid;
        { register built-in Out module }
        if impid1 = 'Out' then RegisterOutModule(impid);
        if impid1 = 'In' then RegisterInModule(impid);
        if impid1 = 'SYSTEM' then begin
          { link SYSTEM pseudo-module from orb.pas }
          new(smod); fillchar(smod^, sizeof(orb.TObj), 0);
          smod^.name := impid;
          smod^.cls := orb.clsMod;
          smod^.rdo := true;
          smod^.typ := orb.noType;
          smod^.dsc := orb.system_;
          smod^.next := orb.topScope^.next;
          orb.topScope^.next := smod;
        end;
        if sym = ors.sCOMMA then ors.Get(sym)
        else if sym = ors.sIDENT then ors.Mark('comma missing');
      end;
      Check(ors.sSEMICOLON, 'no ;');
    end;
    org.Open(version);
    org.modname := modid;
    { open output file }
    assign(org.out_, modid + '.b4a');
    rewrite(org.out_);
    org.WrLn('# ' + modid + '.b4a — generated by oberonc');
    org.WrLn('');
    Declarations(dc); org.SetDataSize((dc + 3) div 4 * 4);
    if org.varsize > 0 then begin
      org.WrComment('reserve ' + IntToStr(org.varsize) + ' bytes for global variables');
      org.WrLbl('_globals');
      org.WrLn(':+' + IntToHex(org.varsize, 2));
    end;
    org.WrComment('jump to module body (entry point)');
    org.WrLn('jm >' + modid + '_init');
    org.WrLn('');
    org.EmitRuntime;
    while sym = ors.sPROCEDURE do begin ProcedureDecl; Check(ors.sSEMICOLON, 'no ;') end;
    org.Header;
    if sym = ors.sBEGIN then begin ors.Get(sym); StatSequence end;
    Check(ors.sEND, 'no END');
    if sym = ors.sIDENT then begin
      if ors.id <> modid then ors.Mark('no match');
      ors.Get(sym);
    end else ors.Mark('identifier missing');
    if sym <> ors.sPERIOD then ors.Mark('period missing');
    if ors.errcnt = 0 then begin
      org.Close(modid, key, exno);
    end else
      writeln('  compilation FAILED');
  end else ors.Mark('must start with MODULE');
end;

initialization
  new(dummy); fillchar(dummy^, sizeof(orb.TObj), 0);
  dummy^.cls := orb.clsVar; dummy^.typ := orb.intType;
  expression := @expression0;
  Type_ := @Type0;
  FormalType := @FormalType0;
