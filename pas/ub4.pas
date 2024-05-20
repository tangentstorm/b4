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
  regs = array[0..63] of value;


var
  ram  : array[0..maxcell*cellsize] of byte;
  disk : file of block;
  term : uhw.TB4Device;
  vw   : byte = 4;

const {-- these are all offsets into the ram array --}
{$TYPEDADDRESS OFF}
  rg: ^regs = @ram[0];
  reg_ip : ^value = @ram[$20*4]; { instruction pointer }
  reg_dp : ^value = @ram[$24*4]; { data stack pointer }
  reg_rp : ^value = @ram[$28]; { retn stack pointer }
  reg_hp : ^value = @ram[4*ord('_')]; { heap pointer }
  reg_ep : ^value = @ram[$38]; { the editor pointer }
  reg_db : ^value = @ram[$3C]; { debug flag }
  reg_bp : ^value = @ram[$40]; { breakpoint }
  ds : ^stack = @ram[mindata];
  rs : ^stack = @ram[minretn];
{$TYPEDADDRESS ON}
  RGO = $1C; { ^\ "go" pointer }
  RLP = $1E; { ^^ list pointer }

  procedure open( path : string );
  procedure boot;
  function step : value;
  function rdval(adr:address): value;
  procedure wrval(adr:address; v:value);

  { these internal ops are used in the assembler and/or hardware}
  procedure dput( val : value );
  function dpop:value;
  function tos:value;
  procedure dswp;
  procedure zap(v :value);
  procedure runop(op : byte);
  function regn(ch : char):byte;
  function rega(ch : char):value;


implementation
uses math;

procedure boot;
  begin
    fillchar(ram, (maxcell + 1) * cellsize, 0);
    reg_dp^ := -1;
    reg_rp^ := -1;
    reg_ip^ := minheap;
    reg_hp^ := minheap;
    reg_ep^ := minheap;
    reg_bp^ := maxcell;
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
    inc(reg_dp^);
    if reg_dp^ = datasize then reg_dp^ := 0;
    ds[reg_dp^] := val;
  end;

function dpop : value;
  begin
    if reg_dp^ = -1 then fail('underflow(data)');
    dpop := ds[reg_dp^]; dec(reg_dp^);
  end;

function tos : value;
  begin tos := ds[reg_dp^]
  end;

function nos : value;
  begin
    if reg_dp^ = 0
      then nos := ds[datasize-1]
      else nos := ds[reg_dp^-1]
  end;

procedure dswp;
  var a,b : value;
  begin a := dpop; b := dpop; dput(a); dput(b);
  end;

function dpon:value; { pop the nos }
  begin dswp; result:=dpop
  end;


{ return stack }

procedure rput( val : value );
  begin
    inc(reg_rp^);
    if reg_rp^ = retnsize then reg_rp^ := 0;
    rs[reg_rp^] := val;
  end;

function rpop : value;
  begin
    if reg_rp^ = -1 then fail('underflow(retn)');
    rpop := rs[reg_rp^]; dec(reg_rp^);
  end;

function tor : value;
  begin tor := rs[reg_rp^]
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
    reg_ip^ := math.max(addr,$100)-1;
  end;

procedure hop(); inline;
  begin
    go(reg_ip^ + int8(ram[reg_ip^+1]));
  end;

procedure rb; begin dput(ram[dpop]) end;   { read byte }
procedure wb; var t:value; begin t:= dpop; ram[t]:= byte(dpop) end;  { write byte  }
procedure ri; begin dput(rdval(dpop)) end; { read integer }
procedure wi; var t:value; begin t := dpop; wrval(t, dpop) end; { write integer }

function oreg(op:byte):value; begin oreg := rega(chr(64+op mod 32)) end; // like rega but takes an opcode
procedure er(a:value); inline; begin go(rdval(a)) end; { register }
procedure rr(a:value); inline; begin dput(rdval(a)) end; { read register }
procedure wr(a:value); inline; begin wrval(a, dpop) end; { write register }
procedure ir(a:value); inline; begin rr(a); wrval(a,tos+vw); end; { read+inc register }


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
      $90 : {dc} rput(dpop);
      $91 : {cd} dput(rpop);
      $92 : {rv} if vw=1 then rb else ri; // todo: dynamic dispatch through fn ptr
      $93 : {wv} if vw=1 then wb else wi; // todo: same
      $94 : {lb} begin dput(bget(reg_ip^+1)); inc(reg_ip^) end;
      $95 : {li} begin dput(rdval(reg_ip^+1)); inc(reg_ip^,3) end;
      $96 : {jm} go(rdval(reg_ip^+1));
      $97 : {hp} hop;
      $98 : {h0} if dpop = 0 then hop else inc(reg_ip^);
      $99 : {cl} begin rput(reg_ip^+4); reg_ip^:=rdval(reg_ip^+1)-1 end; { call }
      $9A : {rt} reg_ip^ := rpop-1;
      $9B : {nx} begin if tor > 0 then begin t:=rpop; dec(t); rput(t) end;
                   if tor = 0 then begin zap(rpop); inc(reg_ip^) end
                   else hop end;
      $BE : {tm} term.invoke(chr(dpop));
      $C0 : {vb} vw := 1;
      $C1 : {vi} vw := 4;
      $FE : {db} reg_db^ := 1;
      $FF : {hl} halt;
      else { no-op };
    end
  end;

function step : value;
  { execute next instruction, then increment and return the IP }
  begin
    runop(ram[reg_ip^]);
    inc(reg_ip^);
    step := reg_ip^;
  end;

begin
  term := TB4Device.create;
end.
