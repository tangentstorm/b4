{ org2.inc — load/store, selectors, parameter passing }

procedure load(var x: Item);
var addr: longint;
begin
  case x.mode of
    orb.clsConst: begin
      if x.typ^.form = orb.tProc then begin
        { procedure constant — push its code address }
        EmitPushInt(codebase + x.a);
      end else
        EmitPushInt(x.a);
      x.mode := mReg;
    end;
    orb.clsVar: begin
      { variable: r=level, a=offset }
      if x.r > 0 then begin
        { local variable: FP + offset }
        EmitRegRd(rF);
        EmitPushInt(x.a);
        Emit(oAD);
        if x.typ^.size = 1 then Emit(bRB) else Emit(bRI);
      end else begin
        { global variable: GlobalBase + offset }
        addr := GlobalBase + x.a;
        EmitPushInt(addr);
        if x.typ^.size = 1 then Emit(bRB) else Emit(bRI);
      end;
      x.mode := mReg;
    end;
    orb.clsPar: begin
      { VAR parameter: address is stored at FP+a, actual offset is b }
      EmitRegRd(rF);
      EmitPushInt(x.a);
      Emit(oAD);
      Emit(bRI); { load the address }
      if x.b <> 0 then begin
        EmitPushInt(x.b);
        Emit(oAD);
      end;
      if x.typ^.size = 1 then Emit(bRB) else Emit(bRI);
      x.mode := mReg;
    end;
    mCond: begin
      { boolean condition — materialize as 0 or 1 on stack }
      { a = false-chain, b = true-chain }
      { We need to emit: if condition then push 1 else push 0 }
      { For now, just mark as on stack }
      { The conditional jump chains need to be fixed up }
      { true path: push 1, jump over false }
      FixLink(x.b);
      Emit(oC1);
      FJump(x.a);  { jump over the false case }
      FixLink(x.a);
      Emit(oC0);
      FixLink(x.a);
      x.mode := mReg;
    end;
    mReg: ; { already on stack, nothing to do }
  end;
end;

procedure loadAdr(var x: Item);
var addr: longint;
begin
  case x.mode of
    orb.clsVar: begin
      if x.r > 0 then begin
        { local: FP + offset }
        EmitRegRd(rF);
        EmitPushInt(x.a);
        Emit(oAD);
      end else begin
        { global: GlobalBase + offset }
        addr := GlobalBase + x.a;
        EmitPushInt(addr);
      end;
      x.mode := mReg;
    end;
    orb.clsPar: begin
      { VAR param: load address from frame }
      EmitRegRd(rF);
      EmitPushInt(x.a);
      Emit(oAD);
      Emit(bRI);
      if x.b <> 0 then begin
        EmitPushInt(x.b);
        Emit(oAD);
      end;
      x.mode := mReg;
    end;
  else
    ors.Mark('address error');
  end;
end;

procedure loadCond(var x: Item);
begin
  if x.typ^.form = orb.tBool then begin
    if x.mode = orb.clsConst then begin
      { constant boolean }
      if x.a = 0 then x.r := 0  { always false }
      else x.r := 1;  { always true }
    end else begin
      load(x);
      { value is on stack; compare with 0 }
      { we set up chains: a = false chain, b = true chain }
      x.r := 1; { NE = true }
    end;
    x.mode := mCond;
    x.a := 0; x.b := 0;
  end else
    ors.Mark('not Boolean?');
end;

procedure Store(var x, y: Item);
begin
  load(y); { value on stack }
  case x.mode of
    orb.clsVar: begin
      if x.r > 0 then begin
        { local }
        EmitRegRd(rF);
        EmitPushInt(x.a);
        Emit(oAD);
      end else begin
        { global }
        EmitPushInt(GlobalBase + x.a);
      end;
      if x.typ^.size = 1 then Emit(bWB) else Emit(bWI);
    end;
    orb.clsPar: begin
      EmitRegRd(rF);
      EmitPushInt(x.a);
      Emit(oAD);
      Emit(bRI);
      if x.b <> 0 then begin
        EmitPushInt(x.b);
        Emit(oAD);
      end;
      if x.typ^.size = 1 then Emit(bWB) else Emit(bWI);
    end;
  else
    ors.Mark('bad mode in Store');
  end;
end;

procedure StoreStruct(var x, y: Item);
var words: longint;
begin
  { copy structured value: load dest addr, src addr, then copy word by word }
  loadAdr(x);
  loadAdr(y);
  if x.typ^.size > 0 then begin
    words := (x.typ^.size + 3) div 4;
    EmitPushInt(words);
    { loop: read word from src, write to dest, advance both, dec counter }
    { TOS = count, NOS = src_addr, below = dest_addr }
    Emit(oDC);        { move count to CS }
    { loop top: }
    Emit(oSW);        { swap so src is on top }
    Emit(oDU);        { dup src addr }
    Emit(bRI);        { read word from src }
    Emit(oDC);        { stash word on CS }
    Emit(oC4); Emit(oAD); { advance src by 4 }
    Emit(oSW);        { swap so dest is on top }
    Emit(oDU);        { dup dest addr }
    Emit(oCD);        { get word from CS }
    Emit(oSW);        { swap: addr word -> word addr }
    Emit(bWI);        { write word }
    Emit(oC4); Emit(oAD); { advance dest by 4 }
    Emit(oSW);        { swap so src is back on top }
    Emit(oNX);        { decrement CS counter, loop if >0 }
    Emit(byte(-14));  { hop back to loop top }
    Emit(oZP);        { drop src addr }
    Emit(oZP);        { drop dest addr }
  end;
end;

procedure CopyString(var x, y: Item);
begin
  { simplified: treat as StoreStruct }
  StoreStruct(y, x);
end;

procedure StrToChar(var x: Item);
begin
  x.typ := orb.charType;
  if strx > 0 then dec(strx, 4);
  x.a := ord(strbuf[x.a]);
end;

procedure Field(var x: Item; y: orb.PObj);
begin
  { x := x.y — add field offset }
  if x.mode = orb.clsVar then
    x.a := x.a + y^.val
  else if x.mode = orb.clsPar then
    x.b := x.b + y^.val
  else begin
    { already have address on stack; add offset }
    if y^.val <> 0 then begin
      EmitPushInt(y^.val);
      Emit(oAD);
    end;
  end;
end;

procedure Index(var x, y: Item);
var s: longint;
begin
  s := x.typ^.base^.size;
  if (y.mode = orb.clsConst) and (x.typ^.len >= 0) then begin
    { constant index }
    if (y.a < 0) or (y.a >= x.typ^.len) then ors.Mark('bad index');
    if x.mode = orb.clsVar then
      x.a := x.a + y.a * s
    else if x.mode = orb.clsPar then
      x.b := x.b + y.a * s;
  end else begin
    { dynamic index: compute address }
    loadAdr(x);
    load(y);
    if s <> 1 then begin
      EmitPushInt(s);
      Emit(oML);
    end;
    Emit(oAD);
    { result is address on stack; mark as RegI-like }
    x.mode := mReg;
    { caller will need to ri/wi through this address }
  end;
end;

procedure DeRef(var x: Item);
begin
  load(x); { load pointer value = address }
  { now TOS has the address of the pointed-to object }
  x.mode := mReg;
  { the pointer value IS the address; mark for subsequent field/index }
end;

procedure ValueParam(var x: Item);
begin
  load(x); { push value onto data stack }
end;

procedure VarParam(var x: Item; ftype: orb.PType);
begin
  loadAdr(x); { push address onto data stack }
  if (ftype^.form = orb.tArray) and (ftype^.len < 0) then begin
    { open array: also push length }
    if x.typ^.len >= 0 then
      EmitPushInt(x.typ^.len)
    else
      EmitPushInt(0); { unknown length }
  end;
end;

procedure OpenArrayParam(var x: Item);
begin
  loadAdr(x);
  if x.typ^.len >= 0 then
    EmitPushInt(x.typ^.len)
  else
    EmitPushInt(0);
end;

procedure StringParam(var x: Item);
begin
  { push address of string constant }
  EmitPushInt(codebase + pc + varsize + x.a); { strings stored after code }
  EmitPushInt(x.b); { length }
end;
