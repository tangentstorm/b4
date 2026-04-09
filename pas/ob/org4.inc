{ org4.inc — control flow, procedures, inline functions, .b4x output }

{ ---- Branch / Control flow ---- }

function Here: longint;
begin
  result := pc;
end;

procedure FJump(var L: longint);
begin
  { unconditional forward jump; L accumulates a fixup chain }
  Emit(oJM);
  Emit4(L);      { write previous chain head as placeholder }
  L := pc - 4;   { new chain head = address of the 4-byte field }
end;

procedure FixLink(L: longint);
var L1: longint;
    target: longint;
begin
  { fix up a chain of forward jumps to point to current pc }
  target := codebase + pc;
  while (L > 0) and (L < pc) do begin
    L1 := longint(code[L]) or (longint(code[L+1]) shl 8)
        or (longint(code[L+2]) shl 16) or (longint(code[L+3]) shl 24);
    code[L]   := byte(target);
    code[L+1] := byte(target shr 8);
    code[L+2] := byte(target shr 16);
    code[L+3] := byte(target shr 24);
    L := L1;
  end;
end;

procedure CFJump(var x: Item);
begin
  { conditional forward jump: jump if false }
  if x.mode <> mCond then loadCond(x);
  load(x); { materialize boolean on stack }
  { TOS is boolean: 0=false, nonzero=true. }
  { Use nt+h0+jm pattern: invert, skip jm if was-true (now 0) }
  Emit(oNT);      { invert: true(-1)->0, false(0)->-1 }
  Emit(oH0);      { hop if zero (original was true): skip jm }
  Emit(byte(5));   { skip 5 bytes (the jm instruction) }
  Emit(oJM);
  Emit4(x.a);     { chain to previous false-jump, 0 = end of chain }
  x.a := pc - 4;  { update chain head }
end;

procedure BJump(L: longint);
begin
  { unconditional backward jump to L }
  Emit(oJM);
  Emit4(codebase + L);
end;

procedure CBJump(var x: Item; L: longint);
begin
  { conditional backward jump for REPEAT..UNTIL: loop back when false }
  if x.mode <> mCond then loadCond(x);
  load(x);
  { TOS = boolean. UNTIL exits when TRUE. Loop when FALSE (0). }
  { nt + h0 + jm pattern: invert, skip jm if was-true }
  Emit(oNT);      { invert }
  Emit(oH0);      { hop if zero (original true): skip the jm, exit loop }
  Emit(byte(5));
  Emit(oJM);      { jump back to loop start }
  Emit4(codebase + L);
end;

procedure Fixup(var x: Item);
begin
  FixLink(x.a);
end;

{ ---- Procedure call / entry / exit ---- }

procedure PrepCall(var x: Item; var r: longint);
begin
  { prepare for call: if x is indirect (proc var), load it }
  if x.mode > orb.clsPar then load(x);
  r := 0; { not used for b4 (no register saving needed — stack-based) }
end;

procedure Call(var x: Item; r: longint);
begin
  if x.mode = orb.clsConst then begin
    { direct call to known address }
    Emit(oCL);
    Emit4(codebase + x.a);
  end else begin
    { indirect call via value on stack }
    { b4 doesn't have indirect call opcode; store to temp register, invoke }
    EmitRegWr(rG); { temporarily use G register }
    Emit(oINV + rG); { invoke G }
  end;
  if x.typ^.base^.form <> orb.tNoTyp then begin
    { function: result is on data stack }
    x.mode := mReg;
  end;
end;

procedure Enter(parblksize, locblksize: longint; int_: boolean);
var npar, off: longint;
begin
  { procedure prologue }
  { save old frame pointer }
  EmitRegRd(rF);   { @F }
  Emit(oDC);        { push to control stack }
  { allocate frame for locals: use heap pointer }
  EmitRegRd(rF + (ord('_') - ord('F'))); { @_ (heap pointer) }
  EmitRegWr(rF);    { !F — set frame pointer }
  { advance heap by local size }
  if locblksize > 0 then begin
    EmitPushInt(locblksize);
    EmitRegRd(rF + (ord('_') - ord('F')));
    Emit(oAD);
    EmitRegWr(rF + (ord('_') - ord('F'))); { !_ }
  end;
  { store parameters from data stack into frame }
  { params are on DS, last param on top. Store from highest offset down. }
  npar := (parblksize - WordSize) div WordSize; { number of 4-byte params }
  off := parblksize - WordSize; { highest param offset }
  while off >= WordSize do begin
    { TOS is the param for this offset; store it }
    EmitRegRd(rF);
    EmitPushInt(off);
    Emit(oAD);
    Emit(bWI);
    off := off - WordSize;
  end;
end;

procedure Return(form: integer; var x: Item; size: longint; int_: boolean);
begin
  { procedure epilogue }
  if form <> orb.tNoTyp then
    load(x); { push return value onto data stack }
  { restore heap pointer from frame pointer }
  EmitRegRd(rF);
  EmitRegWr(rF + (ord('_') - ord('F'))); { !_ }
  { restore old frame pointer from control stack }
  Emit(oCD);        { pop from control stack }
  EmitRegWr(rF);    { !F }
  { return }
  Emit(oRT);
end;

{ ---- FOR loop ---- }

procedure For0(var x, y: Item);
begin
  load(y); { load initial value }
end;

procedure For1(var x, y, z, w: Item; var L: longint);
begin
  { store initial value and check bounds }
  Emit(oDU); { duplicate initial value for comparison }
  load(z);   { load limit }
  if w.a > 0 then begin
    { ascending: jump out if init > limit }
    Emit(oSW); Emit(oLT); { limit < init? }
    Emit(oC1); Emit(oAN);
  end else begin
    { descending: jump out if init < limit }
    Emit(oLT);
    Emit(oC1); Emit(oAN);
  end;
  { if condition true, skip loop }
  Emit(oH0);     { hop if zero (condition false = enter loop) }
  Emit(byte(5));
  Emit(oJM);
  Emit4(0);
  L := pc - 4;
  { store initial value into loop variable }
  Store(x, y);
end;

procedure For2(var x, y, w: Item);
begin
  { increment loop variable by step }
  load(x);
  EmitPushInt(w.a);
  Emit(oAD);
  { store back }
  Emit(oDU); { keep copy for next comparison }
  Store(x, y);
end;

{ ---- Inline standard procedures ---- }

procedure Increment(upordown: longint; var x, y: Item);
var z: Item;
begin
  { INC(x) or INC(x, y) / DEC(x) or DEC(x, y) }
  { load current value, add/subtract, store back }
  z := x; { save target for store }
  load(x); { push current value }
  if y.typ^.form = orb.tNoTyp then
    Emit(oC1)
  else
    load(y);
  if upordown = 0 then Emit(oAD) else begin Emit(oSW); Emit(oSB) end;
  { now new value is on stack; store back to original location }
  { emit store address }
  if z.mode = orb.clsVar then begin
    if z.r > 0 then begin
      EmitRegRd(rF); EmitPushInt(z.a); Emit(oAD);
    end else
      EmitPushInt(GlobalBase + z.a);
    if z.typ^.size = 1 then Emit(bWB) else Emit(bWI);
  end else if z.mode = orb.clsPar then begin
    EmitRegRd(rF); EmitPushInt(z.a); Emit(oAD); Emit(bRI);
    if z.b <> 0 then begin EmitPushInt(z.b); Emit(oAD) end;
    if z.typ^.size = 1 then Emit(bWB) else Emit(bWI);
  end;
end;

procedure Include(inorex: longint; var x, y: Item);
begin
  load(x);
  load(y);
  Emit(oC1); Emit(oSW); Emit(oSH); { 1 << y = bit mask }
  if inorex = 0 then Emit(oOR) { INCL: set union }
  else begin Emit(oNT); Emit(oAN) end; { EXCL: clear bit }
  Store(x, y);
end;

procedure Assert_(var x: Item);
begin
  if x.mode <> mCond then loadCond(x);
  load(x);
  { if false (0), halt }
  Emit(oH0);     { hop if zero }
  Emit(byte(1));  { skip over halt }
  Emit(oHL);      { halt on assertion failure }
end;

procedure New_(var x: Item);
begin
  { simplified: allocate from heap }
  loadAdr(x);
  { push size of pointed-to type }
  EmitPushInt(x.typ^.base^.size);
  { get current heap pointer }
  EmitRegRd(rF + (ord('_') - ord('F'))); { @_ }
  Emit(oDU);
  { advance heap }
  Emit(oDC); { save addr on CS }
  Emit(oAD); { new heap = old + size }
  EmitRegWr(rF + (ord('_') - ord('F'))); { !_ }
  Emit(oCD); { get allocated addr back }
  { store pointer: stack has dest_addr, new_obj_addr }
  Emit(oSW);
  Emit(bWI); { write pointer }
end;

procedure Pack(var x, y: Item);
begin
  ors.Mark('PACK not supported on b4vm');
end;

procedure Unpk(var x, y: Item);
begin
  ors.Mark('UNPK not supported on b4vm');
end;

procedure Led(var x: Item);
begin
  load(x);
  Emit(oZP); { just discard - no LEDs on b4vm }
end;

procedure Get_(var x, y: Item);
begin
  load(x); { address }
  Emit(bRI); { read word at address }
  Store(y, x); { store into y }
end;

procedure Put_(var x, y: Item);
begin
  load(y); { value }
  load(x); { address }
  Emit(bWI);
end;

procedure Copy_(var x, y, z: Item);
begin
  ors.Mark('SYSTEM.COPY not supported');
end;

procedure LDPSR(var x: Item);
begin
  ors.Mark('LDPSR not supported on b4vm');
end;

procedure LDREG(var x, y: Item);
begin
  ors.Mark('LDREG not supported on b4vm');
end;

{ ---- Inline standard functions ---- }

procedure Abs_(var x: Item);
begin
  if x.mode = orb.clsConst then
    x.a := abs(x.a)
  else begin
    load(x);
    { if negative, negate: dup, test < 0, if so negate }
    Emit(oDU);
    Emit(oC0);
    Emit(oLT); { is TOS < 0? }
    Emit(oH0); { hop if zero (not negative) }
    Emit(byte(3)); { skip negate }
    Emit(oC0); Emit(oSW); Emit(oSB); { 0 - x }
  end;
end;

procedure Odd_(var x: Item);
begin
  load(x);
  Emit(oC1);
  Emit(oAN); { x AND 1 }
  x.typ := orb.boolType;
end;

procedure Floor_(var x: Item);
begin
  ors.Mark('FLOOR not supported on b4vm (no REAL)');
end;

procedure Float_(var x: Item);
begin
  ors.Mark('FLT not supported on b4vm (no REAL)');
end;

procedure Ord_(var x: Item);
begin
  if x.mode <> mReg then load(x);
end;

procedure Len_(var x: Item);
begin
  if x.typ^.len >= 0 then begin
    x.mode := orb.clsConst;
    x.a := x.typ^.len;
  end else begin
    { open array: length follows address on stack }
    ors.Mark('open array LEN not yet implemented');
  end;
end;

procedure Shift(fct: longint; var x, y: Item);
begin
  load(x);
  if fct = 0 then begin { LSL }
    load(y); Emit(oSH);
  end else if fct = 1 then begin { ASR }
    load(y); Emit(oC0); Emit(oSW); Emit(oSB); { negate shift count }
    Emit(oSH);
  end else begin { ROR - approximate with shifts }
    load(y); Emit(oC0); Emit(oSW); Emit(oSB);
    Emit(oSH);
  end;
  x.mode := mReg;
end;

procedure ADC_(var x, y: Item);
begin load(x); load(y); Emit(oAD); x.mode := mReg end;

procedure SBC_(var x, y: Item);
begin load(x); load(y); Emit(oSW); Emit(oSB); x.mode := mReg end;

procedure UML_(var x, y: Item);
begin load(x); load(y); Emit(oML); x.mode := mReg end;

procedure Bit_(var x, y: Item);
begin
  load(x); Emit(bRI);
  load(y);
  Emit(oC0); Emit(oSW); Emit(oSB); { negate for right shift }
  Emit(oSH);
  Emit(oC1); Emit(oAN);
  x.mode := mReg; x.typ := orb.boolType;
end;

procedure Register_(var x: Item);
begin
  ors.Mark('REG not supported on b4vm');
  EmitPushInt(0);
  x.mode := mReg;
end;

procedure H_(var x: Item);
begin
  ors.Mark('H not supported on b4vm');
  EmitPushInt(0);
  x.mode := mReg;
end;

procedure Adr_(var x: Item);
begin
  if x.mode in [orb.clsVar, orb.clsPar] then
    loadAdr(x)
  else
    ors.Mark('not addressable');
end;

procedure Condition_(var x: Item);
begin
  ors.Mark('COND not supported on b4vm');
end;

{ ---- Type descriptors (simplified) ---- }

procedure BuildTD(T: orb.PType; var dc: longint);
begin
  { simplified: just record the size in the TD area }
  T^.len := dc;
  dc := dc + 16; { reserve 16 bytes per type descriptor }
end;

procedure TypeTest(var x: Item; T: orb.PType; varpar, isguard: boolean);
begin
  { simplified type test: always pass for now }
  if not isguard then begin
    EmitPushInt(1); { always true }
    x.mode := mReg;
    x.typ := orb.boolType;
  end;
end;

{ ---- Module header and output ---- }

procedure Header;
begin
  { module body entry point: procedures are already emitted, so pc is past them }
  { codebase was already set in SetDataSize }
  entry := codebase + pc;
  { emit: set up global base register G }
  EmitPushInt(GlobalBase);
  EmitRegWr(rG);
end;

procedure Close(var modid: ors.Ident; key, nofent: longint);
var
  img: array of byte;
  f: file;
  fname: string;
  i: longint;
  totalsize: longint;
  strbase: longint;
begin
  { emit halt at end of module body }
  Emit(oHL);

  { calculate layout }
  strbase := codebase + pc;
  totalsize := strbase + strx;

  { build memory image }
  setlength(img, B4MemSize);
  fillchar(img[0], B4MemSize, 0);

  { write entry point to register \ (RGO) at byte offset $70 }
  { register \ = ord('\') - ord('@') = 28, byte offset = 28*4 = $70 }
  img[$70] := byte(entry);
  img[$71] := byte(entry shr 8);
  img[$72] := byte(entry shr 16);
  img[$73] := byte(entry shr 24);

  { also set IP register (RIP=$21) to entry point }
  { RIP is at byte offset $21*4 = $84 }
  img[$84] := byte(entry);
  img[$85] := byte(entry shr 8);
  img[$86] := byte(entry shr 16);
  img[$87] := byte(entry shr 24);

  { also set heap pointer (_=RHP at index $1F, offset $7C) }
  { actually RHP = ord('_')-ord('@') = 31, offset = 31*4 = $7C }
  i := totalsize;
  img[$7C] := byte(i);
  img[$7D] := byte(i shr 8);
  img[$7E] := byte(i shr 16);
  img[$7F] := byte(i shr 24);

  { copy code into image }
  for i := 0 to pc - 1 do
    if codebase + i < B4MemSize then
      img[codebase + i] := code[i];

  { copy strings into image }
  for i := 0 to strx - 1 do
    if strbase + i < B4MemSize then
      img[strbase + i] := byte(strbuf[i]);

  { write .b4x file }
  fname := modid + '.b4x';
  assignfile(f, fname);
  {$I-}
  rewrite(f, 1);
  {$I+}
  if ioresult <> 0 then begin
    writeln('Error: cannot create ', fname);
    exit;
  end;
  blockwrite(f, img[0], B4MemSize);
  closefile(f);

  writeln('  code size: ', pc, ' bytes');
  writeln('  data size: ', varsize, ' bytes');
  writeln('  strings:   ', strx, ' bytes');
  writeln('  output:    ', fname);
end;
