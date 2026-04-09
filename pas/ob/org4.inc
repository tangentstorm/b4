{ org4.inc — control flow, procedures, runtime, output (b4a text) }

{ ---- Control flow ---- }
{ With b4a labels, we don't need fixup chains! Labels are strings. }
{ We store the label name in x.a as an index into a label table. }

var
  labels: array[0..255] of string;
  nlabels: longint = 0;

function AllocLabel: longint;
begin result := nlabels; labels[nlabels] := NewLabel; inc(nlabels) end;

function Here: longint;
var idx: longint;
begin
  idx := AllocLabel;
  WrLbl(labels[idx]);
  result := idx;
end;

procedure FJump(var L: longint);
var idx: longint;
begin
  idx := AllocLabel;
  Wr('jm >' + labels[idx] + ' ');
  if L <> 0 then begin
    { chain: not needed with labels, L just holds the label index }
  end;
  L := idx;
end;

procedure CFJump(var x: Item);
var lbl: string;
begin
  if x.mode <> mReg then load(x);
  { TOS is boolean. Jump to label if false (0). }
  { b4a: use h0 for short hops or jm for general jumps. }
  { Since h0 only takes a byte offset, use the pattern: }
  { test value, if nonzero skip the jm, else take the jm }
  x.a := AllocLabel;
  lbl := labels[x.a];
  { h0 04 = if zero, hop +4 (to jm). hp 07 = if nonzero, hop +7 (past jm). }
  WrLn('9C 04 9B 07 jm >' + lbl);
end;

procedure BJump(L: longint);
begin
  WrLn('jm >' + labels[L]);
end;

procedure CBJump(var x: Item; L: longint);
begin
  if x.mode <> mReg then load(x);
  { REPEAT..UNTIL: loop back when false (0). }
  WrLn('9C 04 9B 07 jm >' + labels[L]);
end;

procedure Fixup(var x: Item);
begin
  { emit the label that the forward jump targets }
  WrLbl(labels[x.a]);
end;

procedure FixLink(L: longint);
begin
  if L <> 0 then WrLbl(labels[L]);
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
    Wr('>' + modname + '.P' + IntToStr(x.a) + ' ');
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
  if w.a > 0 then Wr('sw lt c1 an ')
  else Wr('lt c1 an ');
  WrLn('9C 04 9B 07 jm >' + lbl);
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
begin load(x); WrLn('9C 02 9B 01 hl') end;

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
  else begin load(x); WrLn('du c0 lt c1 an 9C 05 c0 sw sb') end;
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

  WrLbl('rt.wch'); WrLn('lb 6F io rt');  { (ch --) write char: 'o'=$6F }
  WrLbl('rt.wln'); WrLn('lb 0A lb 6F io rt');  { write newline }

  { rt.wint: (n width --) write integer }
  WrLbl('rt.wint');
  WrLn('zp du 9C 07 jm >_wi.main');
  WrLbl('_wi.zero'); WrLn('zp lb 30 lb 6F io rt');
  WrLbl('_wi.main');
  WrLn('du c0 lt c1 an 9C 0A lb 2D lb 6F io c0 sw sb');
  WrLn('cd !I c0 sw');
  WrComment('digit extraction loop: stack [count n]');
  WrLbl('_wi.dloop');
  WrLn('du lb 0A md lb 30 ad dc sw c1 ad sw lb 0A dv');
  WrLn('du 9C 04 9B 07 jm >_wi.ddone');
  WrLn('jm >_wi.dloop');
  WrLbl('_wi.ddone'); WrLn('zp');
  WrComment('print digits from CS: stack [count]');
  WrLbl('_wi.ploop');
  WrLn('cd lb 6F io c1 sb du 9C 04 9B 07 jm >_wi.pdone');
  WrLn('jm >_wi.ploop');
  WrLbl('_wi.pdone'); WrLn('zp @I dc rt');

  { rt.rline: ( -- ) read line into buffer at $3C00 }
  WrLbl('rt.rline');
  WrLn('li 00003C00 lb FF lb 69 io li 00003BFC wi c0 li 00003BF8 wi rt');

  { rt.rch: (addr --) read char from buffer }
  WrLbl('rt.rch');
  WrLn('li 00003BF8 ri li 00003C00 ad rb sw wi');
  WrLn('li 00003BF8 ri c1 ad li 00003BF8 wi rt');

  { rt.rint: (addr --) read line, parse int, store at addr }
  WrLbl('rt.rint');
  WrLn('!J rt.rline');
  WrLn('c0 !K c0 !L c0 !M');
  WrLn('li 00003C00 rb lb 2D eq c1 an 9C 06 c1 !M c1 !K');
  WrLbl('_ri.dloop');
  WrLn('@K li 00003C00 ad rb lb 30 sb');
  WrLn('du c0 lt c1 an .i zp jm >_ri.done .t');
  WrLn('du lb 09 sw lt c1 an .i zp jm >_ri.done .t');
  WrLn('@L lb 0A ml ad !L @K c1 ad !K jm >_ri.dloop');
  WrLbl('_ri.done');
  WrLn('@L @M 9C 05 c0 sw sb @J wi rt');

  WrComment('--- end runtime ---');
  WrLn('');
end;

{ ---- Module header and output ---- }

procedure Header;
begin
  WrComment('--- module body ---');
  WrLbl(modname + '.init');
end;

procedure Close(var modid: ors.Ident; key, nofent: longint);
var i: longint;
begin
  WrLn('hl');
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
