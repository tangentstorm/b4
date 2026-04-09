{ org4.inc — control flow, procedures, runtime, output (b4a text) }

{ ---- Control flow ---- }
{ With b4a labels, we don't need fixup chains! Labels are strings. }
{ We store the label name in x.a as an index into a label table. }

var
  labels: array[0..255] of string;
  nlabels: longint = 0;

function AllocLabel: longint;
begin result := nlabels; labels[nlabels] := NewLabel; inc(nlabels) end;

var inWhile: boolean = false;
    elseEmitted: boolean = false;

function Here: longint;
begin
  { For WHILE loops, emit .w to save loop address }
  Wr('.w ');
  inWhile := true;
  result := 0;
end;

procedure FJump(var L: longint);
begin
  Wr('.e '); elseEmitted := true;
  L := L + 1; { just count nesting for FixLink }
end;

procedure CFJump(var x: Item);
begin
  if x.mode <> mReg then load(x);
  if inWhile then begin
    Wr('.d ');
    inWhile := false;
    x.a := -1; { mark as WHILE — Fixup should be no-op }
  end else begin
    Wr('.i ');
    x.a := 0; { mark as IF — Fixup emits .t }
  end;
  elseEmitted := false;
end;

procedure BJump(L: longint);
begin
  { Use .o from the assembler: hops back to .w and patches .d }
  WrLn('.o');
end;

procedure CBJump(var x: Item; L: longint);
begin
  if x.mode <> mReg then load(x);
  { REPEAT..UNTIL: condition true = exit, false = loop back. }
  { .d tests and hops forward when false (0 = keep looping? no...) }
  { Actually for UNTIL: exit when true. .o hops back on nonzero. }
  { UNTIL uses nt + .d: invert so .d exits when original was true. }
  Wr('nt .d .o ');
end;

procedure Fixup(var x: Item);
begin
  { x.a = -1: WHILE context (.d), no .t needed (.o handles it) }
  { x.a = 0: IF without else, close .i with .t }
  { elseEmitted: .e was emitted, Fixup is a no-op (.t comes from FixLink) }
  if (x.a >= 0) and (not elseEmitted) then Wr('.t ');
  elseEmitted := false;
end;

procedure FixLink(L: longint);
var i: longint;
begin
  for i := 1 to L do Wr('.t ');
end;

{ ---- Procedures ---- }

procedure PrepCall(var x: Item; var r: longint);
begin
  if x.mode > orb.clsPar then load(x);
  r := 0;
end;

procedure Call(var x: Item; r: longint);
begin
  if x.mode = orb.clsConst then begin
    { direct call: x.a is the procedure's label index or name }
    Wr(modname + '_P' + IntToStr(x.a) + ' ');
  end else begin
    Wr('!H ^H '); { indirect: save to H, invoke }
  end;
  WrLn('');
  if x.typ^.base^.form <> orb.tNoTyp then
    x.mode := mReg;
end;

procedure Enter(parblksize, locblksize: longint; int_: boolean);
var npar, off: longint;
begin
  WrComment('enter: parblk=' + IntToStr(parblksize) + ' locblk=' + IntToStr(locblksize));
  Wr('@F dc @_ !F '); { save old FP, set FP = heap }
  if locblksize > 0 then begin
    WrPushInt(locblksize); Wr('@_ ad !_ '); { advance heap }
  end;
  { store params from DS into frame }
  npar := (parblksize - WordSize) div WordSize;
  off := parblksize - WordSize;
  while off >= WordSize do begin
    Wr('@F '); WrPushInt(off); Wr('ad wi ');
    off := off - WordSize;
  end;
  WrLn('');
end;

procedure Return(form: integer; var x: Item; size: longint; int_: boolean);
begin
  if form <> orb.tNoTyp then load(x);
  WrLn('@F !_ cd !F rt');
end;

{ ---- FOR loops ---- }

procedure For0(var x, y: Item);
begin load(y) end;

procedure For1(var x, y, z, w: Item; var L: longint);
var lbl: string;
begin
  { store initial value, check bounds }
  Wr('du '); load(z);
  L := AllocLabel; lbl := labels[L];
  if w.a > 0 then Wr('sw lt ')
  else Wr('lt ');
  WrLn('nt .i jm >' + lbl + ' .t');
  Store(x, y);
end;

procedure For2(var x, y, w: Item);
begin
  load(x); WrPushInt(w.a); Wr('ad du ');
  Store(x, y);
end;

{ ---- Standard procedures ---- }

procedure Increment(upordown: longint; var x, y: Item);
var saved: Item;
begin
  saved := x;
  load(x);
  if y.typ^.form = orb.tNoTyp then Wr('c1 ')
  else load(y);
  if upordown = 0 then Wr('ad ') else Wr('sb ');
  { store back }
  if saved.mode = orb.clsVar then
    WrStoreVar(saved.r, saved.a, saved.typ^.size)
  else if saved.mode = orb.clsPar then begin
    WrRegRd(6); WrPushInt(saved.a); Wr('ad ri ');
    if saved.b <> 0 then begin WrPushInt(saved.b); Wr('ad ') end;
    if saved.typ^.size = 1 then Wr('wb ') else Wr('wi ');
  end;
  WrLn('');
end;

procedure Include(inorex: longint; var x, y: Item);
begin
  load(x); load(y); Wr('c1 sw sh ');
  if inorex = 0 then Wr('or ') else Wr('nt an ');
  Store(x, y);
end;

procedure Assert_(var x: Item);
begin load(x); WrLn('nt .i hl .t') end;

procedure New_(var x: Item);
begin
  loadAdr(x);
  WrPushInt(x.typ^.base^.size);
  WrLn('@_ du dc ad !_ cd sw wi');
end;

procedure Pack(var x, y: Item);
begin ors.Mark('PACK not supported') end;

procedure Unpk(var x, y: Item);
begin ors.Mark('UNPK not supported') end;

procedure Led(var x: Item);
begin load(x); WrLn('zp') end;

procedure Get_(var x, y: Item);
begin load(x); Wr('ri '); Store(y, x) end;

procedure Put_(var x, y: Item);
begin load(y); load(x); WrLn('wi') end;

procedure Copy_(var x, y, z: Item);
begin ors.Mark('SYSTEM.COPY not supported') end;

procedure LDPSR(var x: Item);
begin ors.Mark('LDPSR not supported') end;

procedure LDREG(var x, y: Item);
begin ors.Mark('LDREG not supported') end;

{ ---- Inline functions ---- }

procedure Abs_(var x: Item);
begin
  if x.mode = orb.clsConst then x.a := abs(x.a)
  else begin load(x); WrLn('du c0 lt .i c0 sw sb .t') end;
end;

procedure Odd_(var x: Item);
begin load(x); Wr('c1 an '); x.typ := orb.boolType end;

procedure Floor_(var x: Item);
begin ors.Mark('FLOOR not supported (no REAL)') end;

procedure Float_(var x: Item);
begin ors.Mark('FLT not supported (no REAL)') end;

procedure Ord_(var x: Item);
begin if x.mode <> mReg then load(x) end;

procedure Len_(var x: Item);
begin
  if x.typ^.len >= 0 then begin x.mode := orb.clsConst; x.a := x.typ^.len end
  else ors.Mark('open array LEN not yet implemented');
end;

procedure Shift(fct: longint; var x, y: Item);
begin
  load(x);
  if fct = 0 then begin load(y); Wr('sh ') end { LSL }
  else begin load(y); Wr('c0 sw sb sh ') end;   { ASR/ROR: negate shift }
  x.mode := mReg;
end;

procedure ADC_(var x, y: Item);
begin load(x); load(y); Wr('ad '); x.mode := mReg end;
procedure SBC_(var x, y: Item);
begin load(x); load(y); Wr('sb '); x.mode := mReg end;
procedure UML_(var x, y: Item);
begin load(x); load(y); Wr('ml '); x.mode := mReg end;

procedure Bit_(var x, y: Item);
begin load(x); Wr('ri '); load(y); Wr('c0 sw sb sh c1 an '); x.mode := mReg; x.typ := orb.boolType end;

procedure Register_(var x: Item);
begin ors.Mark('REG not supported'); WrPushInt(0); x.mode := mReg end;

procedure H_(var x: Item);
begin ors.Mark('H not supported'); WrPushInt(0); x.mode := mReg end;

procedure Adr_(var x: Item);
begin
  if x.mode in [orb.clsVar, orb.clsPar] then loadAdr(x)
  else ors.Mark('not addressable');
end;

procedure Condition_(var x: Item);
begin ors.Mark('COND not supported') end;

procedure BuildTD(T: orb.PType; var dc: longint);
begin T^.len := dc; dc := dc + 16 end;

procedure TypeTest(var x: Item; T: orb.PType; varpar, isguard: boolean);
begin
  if not isguard then begin WrPushInt(1); x.mode := mReg; x.typ := orb.boolType end;
end;

{ ---- Runtime library ---- }

procedure EmitRuntime;
begin
  WrComment('--- runtime library ---');

  WrLbl('rt_wch'); WrLn('lb 6F io rt');  { (ch --) write char: 'o'=$6F }
  WrLbl('rt_wln'); WrLn('lb 0A lb 6F io rt');  { write newline }

  { rt.wint: (n width --) write integer }
  WrLbl('O'); WrLbl('rt_wint');
  WrLn('zp du .i jm >_wi_main .t');
  WrLbl('_wi_zero'); WrLn('zp lb 30 lb 6F io rt');
  WrLbl('_wi_main');
  WrLn('du c0 lt .i lb 2D lb 6F io c0 sw sb .t');
  WrLn('cd !I c0 sw');
  WrComment('digit extraction loop: stack [count n]');
  WrLn('.w du .d du lb 0A md lb 30 ad dc sw c1 ad sw lb 0A dv .o zp');
  WrComment('print digits from CS: stack [count]');
  WrLn('.w du .d cd lb 6F io c1 sb .o zp @I dc rt');

  { rt.rline: ( -- ) read line into buffer at $3C00 }
  WrLbl('rt_rline');
  WrLn('li 00003C00 lb FF lb 69 io li 00003BFC wi c0 li 00003BF8 wi rt');

  { rt.rch: (addr --) read char from buffer }
  WrLbl('rt_rch');
  WrLn('li 00003BF8 ri li 00003C00 ad rb sw wi');
  WrLn('li 00003BF8 ri c1 ad li 00003BF8 wi rt');

  { rt.rint: (addr --) read line, parse int, store at addr }
  WrLbl('rt_rint');
  WrLn('!J');
  WrComment('inline readline');
  WrLn('li 00 3C 00 00 lb FF lb 69 io li FC 3B 00 00 wi c0 li F8 3B 00 00 wi');
  WrLn('c0 !K c0 !L c0 !M');
  WrComment('check for minus sign');
  WrLn('li 00 3C 00 00 rb lb 2D eq .i c1 !M c1 !K .t');
  WrComment('digit loop: check char, accumulate if digit');
  { Read char at buf[K]. If ch >= ''0'' and ch <= ''9'', it''s a digit. }
  { Use: read ch, dup, check >= ''0'' AND <= ''9'' as combined boolean. }
  { .w condition .d body .o }
  WrLn('.w @K li 00 3C 00 00 ad rb du lb 30 lt nt ov lb 3A lt an');
  WrComment('  stack: (ch flag) -- flag is true if ''0''<=ch<=''9''');
  WrLn('.d lb 30 sb @L lb 0A ml ad !L @K c1 ad !K .o');
  WrComment('  not a digit: drop ch');
  WrLn('zp');
  WrComment('apply sign and store');
  WrLn('@L @M .i c0 sw sb .t @J wi rt');

  WrComment('--- end runtime ---');
  WrLn('');
end;

{ ---- Module header and output ---- }

procedure Header;
begin
  WrComment('--- module body ---');
  WrLbl(modname + '_init');
  { initialize heap pointer to end of all code }
  WrLn('li >' + modname + '_heap !_');
end;

procedure Close(var modid: ors.Ident; key, nofent: longint);
var i: longint;
begin
  WrLn('hl');
  WrLbl(modid + '_heap');
  WrLn('');
  WrLn('');
  { emit string data }
  if strx > 0 then begin
    WrComment('--- string constants ---');
    for i := 0 to strx - 1 do begin
      if i mod 16 = 0 then begin
        if i > 0 then WrLn('');
        WrLbl('_str' + IntToStr(i));
      end;
      Wr(IntToHex(ord(strbuf[i]), 2) + ' ');
    end;
    WrLn('');
  end;
  writeln('  code output: ', modid, '.b4a');
  system.close(out_);
end;

initialization
  nlabels := 0;
end.
