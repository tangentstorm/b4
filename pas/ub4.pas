{$mode delphi}{$i xpc}
unit ub4; { the b4 virtual machine }
interface uses uhw;

type
  value = longint;
  block = array[0..1023] of byte;

const
  stacksize = 256;
  datasize = stacksize;
  retnsize = stacksize;
  cellsize = sizeof(value);
  {-- memory layout --}
  maxcell = 4095;
  maxbyte = ((maxcell+1) * cellsize)-1;
  { return stack }
  maxretn = maxcell;
  minretn = maxretn-retnsize;
  { data stack }
  maxdata = minretn-1;
  mindata = maxdata-datasize;
  { io buffer }
  maxbuff = mindata-1;
  minbuff = maxbuff-256;
  { scratch workspace }
  maxheap = maxbuff; { so you can read/write the i/o buffer }
  minheap = 256;
  maxblok = 1023;
type
  address = 0..maxcell;
  opcodes = $80..$BF;
  stack = array[0..stacksize-1] of value;
  pstack = ^stack;
  regs = array[0..63] of value;

var
  ram  : array[0..maxcell*cellsize] of byte;
  disk : file of block;
  term : uhw.TB4Device;
  vw   : byte = 4;
  ob   : string = '';


const {-- these are all offsets into the ram array --}
{$MACRO ON}
  {$define creg:=-ord('@')}
  { internal names for non-letter character registers }
  RGO = ord('\') creg; { ^\ is address of 'main' at start }
  RLP = ord('^') creg; { ^^ list pointer }
  RHP = ord('_') creg; { ^_ heap/'here' pointer }
  { potential private registers go from $20 (32) to $3F (63) }
  RMZ = $20; {memory size}
  RIP = $21; {instruction pointer}
  RDA = $22; {ds address}
  RDS = $23; {ds height}
  RDZ = $24; {ds size (max height)}
  RCA = $25; {cs address}
  RCS = $26; {cs height}
  RCZ = $27; {cs size (max height)}
  RST = $28; {state flag}
  RDB = $29; {debug flag}
  RVW = $30; {value width (1=byte 4=int)}
  RED = $31; {used by the debugger's editor}
  RBP = $32; {internal breakpoint}
{$TYPEDADDRESS OFF}
  rg: ^regs = @ram[0];
  ds : ^stack = @ram[mindata];
  cs : ^stack = @ram[minretn];
{$TYPEDADDRESS ON}
{$MACRO OFF}

  procedure open( path : string );
  procedure boot;
  function step : value;
  procedure runa(adr:address);
  procedure runr(reg:char);
  function rdval(adr:address): value;
  procedure wrval(adr:address; v:value);

  { these internal ops are used in the assembler and/or hardware}
  procedure dput( val : value );
  function dpop:value;
  function tos:value;
  procedure dswp;
  procedure zap(v :value);
  procedure runop(op : byte);
  function rega(ch : char):value;
  function regn(ch : char):byte;
  function b4mat(v : value):string;
  procedure WriteStack(pre : string; s:pstack; count:integer);


implementation
uses math, sysutils{format};

procedure boot;
  begin
    fillchar(ram, (maxcell + 1) * cellsize, 0);
    rg[RDS] := -1;
    rg[RCS] := -1;
    rg[RIP] := minheap;
    rg[RHP] := minheap;
    rg[RED] := minheap;
    rg[RBP] := maxcell;
  end;

{ helpers }

procedure zap( v : value );
  begin { discards a value (for turbo pascal) }
  end;

procedure fail(msg: string);
  begin writeln(msg); halt
  end;

procedure todo(op : string);
  begin fail('todo: ' + op);
  end;


{ memory routines }

function rdval(adr:address) : value;
  { read a longint (little-endian) }
  var i : byte;
  begin
    result := 0;
    for i := 0 to 3 do result := result + (byte(ram[adr+i]) shl (8*i));
  end;

procedure wrval(adr: address; v:value);
  var i :byte;
  begin
    for i:= 0 to 3 do begin ram[adr] := byte(v shr (8*i)); inc(adr); end
  end;

function mget(a:address):value;
  begin result := ram[a]
  end;

function bget(a:address):byte;
  begin result := byte(ram[a])
  end;

procedure mset(a:address; v:value);
  begin ram[a] := v
  end;


{ data stack }

procedure dput( val : value );
  begin
    inc(rg[RDS]);
    if rg[RDS] = datasize then rg[RDS] := 0;
    ds[rg[RDS]] := val;
  end;

function dpop : value;
  begin
    if rg[RDS] = -1 then fail('underflow(ds)');
    dpop := ds[rg[RDS]]; dec(rg[RDS]);
  end;

function tos : value;
  begin tos := ds[rg[RDS]]
  end;

function nos : value;
  begin
    if rg[RDS] = 0
      then nos := ds[datasize-1]
      else nos := ds[rg[RDS]-1]
  end;

procedure dswp;
  var a,b : value;
  begin a := dpop; b := dpop; dput(a); dput(b);
  end;

function dpon:value; { pop the nos }
  begin dswp; result:=dpop
  end;


{ return stack }

procedure cput( val : value );
  begin
    inc(rg[RCS]);
    if rg[RCS] = retnsize then rg[RCS] := 0;
    cs[rg[RCS]] := val;
  end;

function cpop : value;
  begin
    if rg[RCS] = -1 then fail('underflow(cs)');
    cpop := cs[rg[RCS]]; dec(rg[RCS]);
  end;

function toc : value;
  begin toc := cs[rg[RCS]]
  end;


{ file routines }

procedure open(path:string);
  begin
    assign(disk, path);
  {$i-}
      reset(disk);
      if ioresult <> 0 then rewrite(disk);
      if ioresult <> 0 then fail('couldn''t open '+ path);
  {$i+}
  end;

procedure blok( n : value );
  begin
    if (n < 0) or (n > maxblok)
      then seek(disk, 0)
      else seek(disk, n);
  end;

procedure load;
  var tmp : block;
  begin
    read(disk, tmp);
    move(tmp, ram[minbuff], 1024);
  end;

function size : word;
  { size of disk, in blocks }
  begin size := filesize(disk)
  end;

function grow : word;
  var i : word;
  begin
    i := size;
    if i + 1 < maxblok then begin inc(i); seek(disk, i) end;
    grow := i;
  end;

procedure save;
  var tmp : block;
  begin
    move(ram[minbuff], tmp, 1024);
    write(disk, tmp);
  end;

function regn(ch:char):byte; begin result := ord(upcase(ch)) - ord('@') end;
function rega(ch : char):value; begin result := 4 * regn(ch) end;

procedure go(addr :integer); inline;
  begin
    rg[RIP] := math.max(addr,$100)-1;
  end;

procedure hop(); inline;
  begin
    go(rg[RIP] + int8(ram[rg[RIP]+1]));
  end;

procedure rb; begin dput(ram[dpop]) end;   { read byte }
procedure wb; var t:value; begin t:= dpop; ram[t]:= byte(dpop) end;  { write byte  }
procedure ri; begin dput(rdval(dpop)) end; { read integer }
procedure wi; var t:value; begin t := dpop; wrval(t, dpop) end; { write integer }

function oreg(op:byte):value; begin oreg := rega(chr(64+op mod 32)) end; // like rega but takes an opcode
procedure er(a:value); inline; begin cput(rg[RIP]+1); go(rdval(a)) end; { register }
procedure rr(a:value); inline; begin dput(rdval(a)) end; { read register }
procedure wr(a:value); inline; begin wrval(a, dpop) end; { write register }
procedure ir(a:value); inline; begin rr(a); wrval(a,tos+vw); end; { read+inc register }

procedure opio;
  begin
    case chr(dpop) of
      'e': ob:=ob+chr(dpop)
    end
  end;

procedure runop(op : byte);
  var t,a : value;
  begin
    if op=0 then //ok
    else if op<$20 then {^R} er(oreg(op))
    else if op<$40 then {@R} rr(oreg(op))
    else if op<$60 then {!R} wr(oreg(op))
    else if op<$80 then {+R} ir(oreg(op))
    else case op of
      { Do not reformat this function! mkoptbl.pas uses it! }
      $80 : {ad} dput(dpop  + dpop);
      $81 : {sb} dput(-dpop + dpop);
      $82 : {ml} dput(dpop * dpop);
      $83 : {dv} begin t:=dpop; dput(dpop div t) end;
      $84 : {md} begin t:=dpop; dput(dpop mod t) end;
      $85 : {sh} begin dswp; dput(dpop shl dpop) end;
      $86 : {an} dput(dpop and dpop);
      $87 : {or} dput(dpop or dpop);
      $88 : {xr} dput(dpop xor dpop);
      $89 : {nt} dput(not dpop);
      $8A : {eq} if dpop =  dpop then dput(-1) else dput(0);
      $8B : {lt} begin t:=dpop; if dpop <  t then dput(-1) else dput(0) end;
      $8C : {du} dput(tos);
      $8D : {sw} dswp;
      $8E : {ov} dput(nos);
      $8F : {zp} zap(dpop);
      $90 : {dc} cput(dpop);
      $91 : {cd} dput(cpop);
      $92 : {rv} if vw=1 then rb else ri; // todo: dynamic dispatch through fn ptr
      $93 : {wv} if vw=1 then wb else wi; // todo: same
      $94 : {lb} begin dput(bget(rg[RIP]+1)); inc(rg[RIP]) end;
      $95 : {li} begin dput(rdval(rg[RIP]+1)); inc(rg[RIP],3) end;
      $96 : {jm} go(rdval(rg[RIP]+1));
      $97 : {hp} hop;
      $98 : {h0} if dpop = 0 then hop else inc(rg[RIP]);
      $99 : {cl} begin cput(rg[RIP]+4); rg[RIP]:=rdval(rg[RIP]+1)-1 end; { call }
      $9A : {rt} begin rg[RIP] := cpop-1; if rg[RIP]=-1 then rg[RST]:=0 end;
      $9B : {nx} begin if toc > 0 then begin t:=cpop; dec(t); cput(t) end;
                   if toc = 0 then begin zap(cpop); inc(rg[RIP]) end
                   else hop end;
      $BE : {tm} term.invoke(chr(dpop));
      $C0 : {vb} vw := 1;
      $C1 : {vi} vw := 4;
      $FB : {ds} WriteStack('ds: ', ds, rg^[RDS]);
      $FC : {hx} write(format('(%x)', [dpop]));
      $FD : {io} opio;
      $FE : {db} rg[RDB] := 1;
      $FF : {hl} halt;
      else { no-op };
    end
  end;

function step : value;
  { execute next instruction, then increment and return the IP }
  begin
    runop(ram[rg[RIP]]);
    inc(rg[RIP]);
    step := rg[RIP];
  end;

procedure run;
  begin rg[RST]:=1; while rg[RST]=1 do step
  end;

procedure runa(adr:address);
  begin cput(rg[RIP]); cput(0); go(adr+1); run; rg[RIP]:=cpop;
  end;

procedure runr(reg:char);
  begin runa(rega(reg))
  end;


// format as hex in b4 style
function b4mat(v : value):string;
begin
  if v < 0 then result := Format('-%x',[-v])
  else result := Format('%x',[v])
end;

procedure WriteStack(pre : string; s:pstack; count:integer);
  var v: ub4.value; i:integer=0;
begin
  Write(pre);
  Write('[');
  if count > 0 then for i := 1 to count do begin
    if i>1 then Write(' ');
    v := s^[i];
    Write(b4mat(v));
  end;
  WriteLn(']');
end;


begin
  term := TB4Device.create;
end.
