{$mode delphi}{$i xpc}
unit ub4; { the b4 virtual machine }
interface uses uhw,sysutils;

type
  value = longint;
  block = array[0..1023] of byte;
  EB4Exception = class(Exception);

const
  stacksize = 256;
  cellsize = sizeof(value);
  {-- memory layout --}
  maxcell = 4095;
  maxbyte = ((maxcell+1) * cellsize)-1;
  { ctrl stack }
  ctrlsize = stacksize;
  maxctrl = maxcell;
  minctrl = maxctrl-ctrlsize;
  { data stack }
  datasize = stacksize;
  maxdata = minctrl-1;
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
  mem  : array[0..maxcell*cellsize] of byte;
  disk : file of block;
  term : uhw.TB4Device;
  vw   : byte = 4;
  ob   : string = '';


const {-- these are all offsets into the mem array --}
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
  rg: ^regs = @mem[0];
  ds : ^stack = @mem[mindata];
  cs : ^stack = @mem[minctrl];
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
uses math, ub4ops;

procedure boot;
  begin
    fillchar(mem, (maxcell + 1) * cellsize, 0);
    rg[RDS] := 0;
    rg[RCS] := 0;
    rg[RIP] := minheap;
    rg[RHP] := minheap;
    rg[RED] := minheap;
    rg[RBP] := maxcell;
  end;

{ helpers }

procedure zap( v : value );
  begin { discards a value (for turbo pascal) }
  end;

function dis(b:byte):string;
  begin
    case b of
      $00: result := '..';
      $01..$1F: result := '^'+chr(64+b);
      $20..$3F: result := '@'+chr(64+b-$20);
      $40..$5F: result := '!'+chr(64+b-$40);
      $60..$7F: result := '^'+chr(64+b-$60);
      $80..$FF: begin
                  result := ub4ops.optbl[b];
                  if result = '' then result := format('%0.2x', [b])
                end
    end
  end;

procedure fail(msg: string);
  begin
    writeln(msg);
    writeln('ip: ', rg[RIP],' op: ', dis(mem[rg[RIP]]));
    WriteStack('ds: ', ds, rg^[RDS]);
    WriteStack('cs: ', cs, rg^[RCS]);
    raise EB4Exception.Create(msg);
  end;

procedure todo(op : string);
  begin fail('todo: ' + op);
  end;


{ memory routines }

function rdval(adr:address) : value;
  { read a longint (little-endian) }
  var p : ^value;
  begin
    if (adr+4)>=length(mem) then fail('read past mem(rdval)');
    {$TYPEDADDRESS off}
    p := @mem[adr];
    {$TYPEDADDRESS on}
    result := p^;
  end;

procedure wrval(adr: address; v:value);
  var p : ^value;
  begin
    {$TYPEDADDRESS off}
    p := @mem[adr];
    {$TYPEDADDRESS on}
    p^ := v;
  end;

function mget(a:address):value;
  begin result := mem[a]
  end;

function bget(a:address):byte;
  begin result := byte(mem[a])
  end;

procedure mset(a:address; v:value);
  begin mem[a] := v
  end;


{ data stack }

procedure dput( val : value );
  begin
    if rg[RDS]+1 = datasize then fail('overflow(ds)');
    ds[rg[RDS]] := val;
    inc(rg[RDS]);
  end;

function dpop : value;
  begin
    if rg[RDS] = 0 then fail('underflow(ds)');
    dpop := ds[rg[RDS]-1]; dec(rg[RDS]);
  end;

function tos : value;
  begin
    if rg[RDS] < 1 then fail('underflow(tos)')
    else tos := ds[rg[RDS]-1]
  end;

function nos : value;
  begin
    if rg[RDS] < 2 then fail('underflow(nos)')
    else nos := ds[rg[RDS]-2]
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
    if rg[RCS]+1 = ctrlsize then fail('overflow(cs)');
    cs[rg[RCS]] := val;
    inc(rg[RCS]);
  end;

function cpop : value;
  begin
    if rg[RCS] = 0 then fail('underflow(cs)');
    cpop := cs[rg[RCS]-1]; dec(rg[RCS]);
  end;

function toc : value;
  begin
    if rg[RCS] < 1 then fail('underflow(toc)')
    else toc := cs[rg[RCS]-1]
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
    move(tmp, mem[minbuff], 1024);
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
    move(mem[minbuff], tmp, 1024);
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
    go(rg[RIP] + int8(mem[rg[RIP]+1]));
  end;

procedure rb; begin dput(mem[dpop]) end;   { read byte }
procedure wb; var t:value; begin t:= dpop; mem[t]:= byte(dpop) end;  { write byte  }
procedure ri; begin dput(rdval(dpop)) end; { read integer }
procedure wi; var t:value; begin t := dpop; wrval(t, dpop) end; { write integer }

function oregn(o:byte):byte; begin result := o mod 32 end;
procedure oper(r:byte); inline; begin cput(rg[RIP]+1); go(rg[r]) end; { eval reg }
procedure oprr(r:byte); inline; begin dput(rg[r]) end; { read register }
procedure opwr(r:byte); inline; begin rg[r] := dpop end; { write register }
procedure opir(r:byte); inline;
  var v:value; begin v:=rg[r]; rg[r]+=dpop; dput(v) end; { read+inc register }

procedure opio;
  begin
    case chr(dpop) of
      'e': ob:=ob+chr(dpop)
    end
  end;

// write a string from memory
procedure wl(a:address);
  var i,b: byte;
  begin
    b := mem[a];
    write(format('(@%x[%x]"', [a,b]));
    for i := 1 to b do write(chr(mem[a+i]));
    writeln('")');
  end;


procedure runop(op : byte);
  var t,a : value;
  begin
    if op=0 then //ok
    else if op<$20 then {^R} oper(oregn(op))
    else if op<$40 then {@R} oprr(oregn(op))
    else if op<$60 then {!R} opwr(oregn(op))
    else if op<$80 then {+R} opir(oregn(op))
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
      $92 : {rb} rb;
      $93 : {wb} wb;
      $94 : {ri} ri;
      $95 : {wi} wi;
      $96 : {lb} begin dput(bget(rg[RIP]+1)); inc(rg[RIP]) end;
      $97 : {li} begin dput(rdval(rg[RIP]+1)); inc(rg[RIP],3) end;
      $98 .. $99 : ;
      $9A : {jm} go(rdval(rg[RIP]+1));
      $9B : {hp} hop;
      $9C : {h0} if dpop = 0 then hop else inc(rg[RIP]);
      $9D : {cl} begin cput(rg[RIP]+4); rg[RIP]:=rdval(rg[RIP]+1)-1 end; { call }
      $9E : {rt} begin rg[RIP] := cpop-1; if rg[RIP]=-1 then rg[RST]:=0 end;
      $9F : {nx} begin if toc > 0 then begin t:=cpop; dec(t); cput(t) end;
                   if toc = 0 then begin zap(cpop); inc(rg[RIP]) end
                   else hop end;
      $BE : {tm} term.invoke(chr(dpop));
      $C0 : {c0} dput(0);
      $C1 : {c1} dput(1);
      $F9 : {wl} wl(dpop);
      $FA : {ds} WriteStack('ds: ', ds, rg^[RDS]);
      $FB : {cs} WriteStack('cs: ', cs, rg^[RCS]);
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
    runop(mem[rg[RIP]]);
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

procedure WriteStack(pre : string; s:pstack; count:value);
  var v: value; i:integer=0;
begin
  Write(pre);
  Write('[');
  if count > 0 then for i := 0 to count-1 do begin
    if i>0 then Write(' ');
    Write(b4mat(s^[i]));
  end;
  WriteLn(']');
end;

begin
  term := TB4Device.create;
end.
