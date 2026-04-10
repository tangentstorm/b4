{ org2.inc — load/store, selectors, parameter passing (b4a text output) }

procedure load(var x: Item);
begin
  case x.mode of
    orb.clsConst: begin
      if x.typ^.form = orb.tProc then
        Wr('li >' + x.lbl + ' ')
      else
        WrPushInt(x.a);
      x.mode := mReg;
    end;
    orb.clsVar: begin
      WrLoadVar(x.r, x.a, x.typ^.size);
      x.mode := mReg;
    end;
    orb.clsPar: begin
      WrRegRd(6); WrPushInt(x.a); Wr('ad ri ');
      if x.b <> 0 then begin WrPushInt(x.b); Wr('ad ') end;
      if x.typ^.size = 1 then Wr('rb ') else Wr('ri ');
      x.mode := mReg;
    end;
    mRegI: begin
      Wr('@H ');
      if x.typ^.size = 1 then Wr('rb ') else Wr('ri ');
      x.mode := mReg;
    end;
    mReg: ; { already on stack }
  end;
end;

procedure loadAdr(var x: Item);
begin
  case x.mode of
    orb.clsVar: begin
      WrLoadAdr(x.r, x.a);
      x.mode := mReg;
    end;
    orb.clsPar: begin
      WrRegRd(6); WrPushInt(x.a); Wr('ad ri ');
      if x.b <> 0 then begin WrPushInt(x.b); Wr('ad ') end;
      x.mode := mReg;
    end;
  else ors.Mark('address error');
  end;
end;

procedure loadCond(var x: Item);
begin
  if x.typ^.form = orb.tBool then begin
    if x.mode <> mReg then load(x);
    x.mode := mReg;
  end else ors.Mark('not Boolean?');
end;

procedure Store(var x, y: Item);
begin
  if x.mode in [mReg, mRegI] then begin
    { LHS address was saved to P by parser before RHS evaluation }
    load(y);
    Wr('@P wi ');
  end else begin
    load(y);
    case x.mode of
      orb.clsVar: WrStoreVar(x.r, x.a, x.typ^.size);
      orb.clsPar: begin
        WrRegRd(6); WrPushInt(x.a); Wr('ad ri ');
        if x.b <> 0 then begin WrPushInt(x.b); Wr('ad ') end;
        if x.typ^.size = 1 then Wr('wb ') else Wr('wi ');
      end;
    else ors.Mark('bad mode in Store');
    end;
  end;
  WrLn('');
end;

procedure StoreStruct(var x, y: Item);
var lbl: string;
begin
  loadAdr(x); loadAdr(y);
  if x.typ^.size > 0 then begin
    WrPushInt((x.typ^.size + 3) div 4);
    lbl := NewLabel;
    Wr('dc '); WrLbl(lbl); Wr('sw du ri dc c4 ad sw du cd sw wi c4 ad sw nx ');
    WrPushInt(-(pc));  { placeholder — assembler handles this with label }
    WrLn('zp zp');
  end;
end;

procedure CopyString(var x, y: Item);
begin StoreStruct(y, x) end;

procedure StrToChar(var x: Item);
begin
  x.typ := orb.charType;
  if strx > 0 then dec(strx, 4);
  x.a := ord(strbuf[x.a]);
end;

procedure Field(var x: Item; y: orb.PObj);
begin
  if x.mode = orb.clsVar then x.a := x.a + y^.val
  else if x.mode = orb.clsPar then x.b := x.b + y^.val
  else begin
    if y^.val <> 0 then begin WrPushInt(y^.val); Wr('ad ') end;
  end;
end;

procedure Index(var x, y: Item);
var s: longint;
begin
  s := x.typ^.base^.size;
  if (y.mode = orb.clsConst) and (x.typ^.len >= 0) then begin
    if (y.a < 0) or (y.a >= x.typ^.len) then ors.Mark('bad index');
    if x.mode = orb.clsVar then x.a := x.a + y.a * s
    else if x.mode = orb.clsPar then x.b := x.b + y.a * s;
  end else begin
    loadAdr(x); load(y);
    if s <> 1 then begin WrPushInt(s); Wr('ml ') end;
    Wr('ad !H ');
    x.mode := mRegI;
  end;
end;

procedure DeRef(var x: Item);
begin
  load(x);
  x.mode := mRegI;
end;

procedure ValueParam(var x: Item);
begin load(x) end;

procedure VarParam(var x: Item; ftype: orb.PType);
begin
  loadAdr(x);
  if (ftype^.form = orb.tArray) and (ftype^.len < 0) then begin
    if x.typ^.len >= 0 then WrPushInt(x.typ^.len) else WrPushInt(0);
  end else if ftype^.form = orb.tRecord then begin
    WrPushInt(0); { dummy type tag }
  end;
end;

procedure OpenArrayParam(var x: Item);
begin
  loadAdr(x);
  if x.typ^.len >= 0 then WrPushInt(x.typ^.len) else WrPushInt(0);
end;

procedure StringParam(var x: Item);
begin
  Wr('li >_str' + IntToStr(x.a) + ' ');
  WrPushInt(x.b);
end;
