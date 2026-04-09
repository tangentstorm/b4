{ org4.inc — control flow, procedures, inline functions, .b4x output }

{ ---- Runtime library (emitted at start of code area) ---- }

procedure EmitRuntime;
var jmpatch, digitloop, printloop, h0pos: longint;
begin
  { These small subroutines are emitted before user code.
    They use b4's io opcode with 'o' command for stdout output. }

  { --- rt_writechar: (ch --) write one character --- }
  rtWriteChar := codebase + pc;
  EmitLB(ord('o'));   { push 'o' command }
  Emit(oIO);          { io: pops cmd='o', pops ch, writes to stdout }
  Emit(oRT);          { return }

  { --- rt_writeln: ( -- ) write newline --- }
  rtWriteLn := codebase + pc;
  EmitLB(10);         { push newline char }
  EmitLB(ord('o'));
  Emit(oIO);
  Emit(oRT);

  { --- rt_writeint: (n width --) write signed integer --- }
  { width is ignored; just prints the number }
  rtWriteInt := codebase + pc;
  Emit(oZP);          { drop width parameter }
  { handle n=0 special case }
  Emit(oDU);          { dup n }
  Emit(oH0);          { if zero, hop past the jm to 'print 0' code }
  Emit(byte(7));       { hop to h0_addr+7 = past the 5-byte jm }
  Emit(oJM); jmpatch := pc; Emit4(0);  { jump to main logic — patched below }
  { n=0 case: print '0' and return }
  Emit(oZP);          { drop the 0 (dup from above) }
  EmitLB(ord('0'));
  EmitLB(ord('o'));
  Emit(oIO);
  Emit(oRT);          { return — CS still has our return addr }
  { patch the jm to here }
  code[jmpatch]   := byte(codebase + pc);
  code[jmpatch+1] := byte((codebase + pc) shr 8);
  code[jmpatch+2] := byte((codebase + pc) shr 16);
  code[jmpatch+3] := byte((codebase + pc) shr 24);
  { check if negative }
  Emit(oDU);          { dup n }
  Emit(oC0);
  Emit(oLT);          { n < 0 ? }
  Emit(oC1); Emit(oAN); { normalize to 0/1 }
  Emit(oH0);          { if zero (not negative), skip neg handling }
  Emit(byte(10));      { skip: lb '-' lb 'o' io c0 sw sb = 2+2+1+1+1+1=8 bytes + 2 for h0 itself = skip to h0+10 }
  EmitLB(ord('-'));
  EmitLB(ord('o'));
  Emit(oIO);          { print '-' }
  Emit(oC0); Emit(oSW); Emit(oSB); { negate: 0 - n }
  { save return address from CS to register I }
  Emit(oCD);           { pop return addr from CS }
  EmitRegWr(9);        { !I — save to register I }
  { now n is positive on TOS; push digits to CS }
  Emit(oC0);          { digit count = 0 }
  Emit(oSW);          { DS: [count n] }
  { digit_loop: }
  digitloop := pc;
  Emit(oDU);          { dup n -- DS: [count n n] }
  EmitLB(10);
  Emit(oMD);          { md pops 10, pops n, pushes n MOD 10 }
  EmitLB(ord('0'));
  Emit(oAD);          { digit + '0' = ASCII }
  Emit(oDC);          { push ASCII char to CS }
  Emit(oSW);          { DS: [n count] }
  Emit(oC1); Emit(oAD); { count + 1 }
  Emit(oSW);          { DS: [count+1 n] }
  EmitLB(10);
  Emit(oDV);          { dv pops 10, pops n, pushes n DIV 10 }
  Emit(oDU);          { dup new_n }
  Emit(oC0);
  Emit(oEQ);          { new_n = 0? -- returns -1 if equal }
  Emit(oC1); Emit(oAN);{ 0 or 1 }
  h0pos := pc;
  Emit(oH0);          { if zero (new_n <> 0), loop back }
  Emit(byte(digitloop - h0pos)); { relative hop back }
  Emit(oZP);          { drop the 0 (exhausted n) }
  { CS has digit chars (deepest = most significant), DS has count }
  { print_loop: }
  printloop := pc;
  Emit(oCD);          { pop char from CS to DS }
  EmitLB(ord('o'));
  Emit(oIO);          { print it }
  Emit(oC1); Emit(oSB); { sb: -1 + count = count-1 }
  Emit(oDU);          { dup count }
  Emit(oC0);
  Emit(oEQ);          { count = 0? }
  Emit(oC1); Emit(oAN);
  h0pos := pc;
  Emit(oH0);          { if zero (count <> 0), loop }
  Emit(byte(printloop - h0pos));
  Emit(oZP);          { drop count (0) }
  { restore return address and return }
  EmitRegRd(9);        { @I — push saved return addr }
  Emit(oDC);           { push to CS }
  Emit(oRT);          { return }
end;

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
  { ensure boolean value is on the data stack }
  if x.mode <> mReg then load(x);
  { TOS is boolean: 0=false, nonzero=true (1 or -1). }
  { If true (nonzero), skip the jm. If false (0), take the jm. }
  { h0 hops when TOS=0. So: if false, h0 does NOT skip, falls to... }
  { Wait: we want false to jump away. h0 jumps when 0 (false). }
  { So: h0 with offset past the body works if body is small. }
  { For general case: if TOS nonzero (true), skip jm; if zero (false), fall to jm. }
  { Use: h0 +5 to skip jm when false... no, h0 hops when 0! }
  { OK: h0 hops when TOS=0. We want to jump (take jm) when false (0). }
  { So DON'T invert. When false(0), h0 hops PAST the jm? No, we want to TAKE jm. }
  { Solution: h0 skips when false. But we want false to take jm. }
  { Actual: push value, if nonzero skip jm (continue to body). If zero fall to jm. }
  { h0 hops when 0. We want 0 to fall to jm, nonzero to skip jm. }
  { So we DON'T use h0 for this. Use: dup, h0 (if 0 hop to jm), zp, hop past jm. }
  { Pattern: value on TOS. }
  {   h0 +3   — if false(0), hop to the jm (which is 3 bytes ahead: h0+offset+zp) }
  {   zp      — drop value (was true) }
  {   hp +5   — skip the jm (5 bytes) }
  {   jm <target> — jump to else/end }
  { h0 pops TOS and hops if zero. }
  { false(0): h0 hops past the hp to the jm. true(nonzero): h0 falls through. }
  Emit(oH0);      { if false (0), hop past hp to jm }
  Emit(byte(4));   { hop to h0_addr+4 = past h0(1)+offset(1)+hp(1)+offset(1) = jm }
  Emit(oHP);       { true: unconditional hop past jm }
  Emit(byte(5));   { skip 5 bytes (jm instruction) }
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
  { h0 pops and hops when 0. For UNTIL: exit when true, loop when false. }
  { false(0) → h0 hops past hp to jm (loops back). true(nonzero) → falls to hp (exits). }
  if x.mode <> mReg then load(x);
  Emit(oH0);      { if false (0), hop to jm }
  Emit(byte(4));   { hop past h0+offset+hp+offset to jm }
  Emit(oHP);       { true: skip jm, exit loop }
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

var heapPatch: longint; { code offset of the heap init placeholder }

procedure Header;
begin
  { module body entry point: procedures are already emitted, so pc is past them }
  { codebase was already set in SetDataSize }
  entry := codebase + pc;
  { emit: set up global base register G }
  EmitPushInt(GlobalBase);
  EmitRegWr(rG);
  { emit: initialize heap pointer _ }
  Emit(oLI); heapPatch := pc; Emit4(0); { placeholder — patched in Close }
  EmitRegWr(31);      { !_ }
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
  { patch the heap init in Header to point past all code+data }
  code[heapPatch]   := byte(totalsize);
  code[heapPatch+1] := byte(totalsize shr 8);
  code[heapPatch+2] := byte(totalsize shr 16);
  code[heapPatch+3] := byte(totalsize shr 24);

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
