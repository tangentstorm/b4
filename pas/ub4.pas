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


var
  ram  : array[0..maxcell*cellsize] of byte;
  disk : file of block;
  term : uhw.TB4Term;

const {-- these are all offsets into the ram array --}
{$TYPEDADDRESS OFF}
  reg_ip : ^value = @ram[$20]; { instruction pointer }
  reg_dp : ^value = @ram[$24]; { data stack pointer }
  reg_rp : ^value = @ram[$28]; { retn stack pointer }
  reg_hp : ^value = @ram[$2C]; { heap pointer }
  reg_lp : ^value = @ram[$30]; { last dictionary entry }
  reg_ap : ^value = @ram[$34]; { the 'a' register }
  reg_ep : ^value = @ram[$38]; { the editor pointer }
  reg_db : ^value = @ram[$3C]; { debug flag }
  reg_bp : ^value = @ram[$40]; { breakpoint }
  ds : ^stack = @ram[mindata];
  rs : ^stack = @ram[minretn];
{$TYPEDADDRESS ON}

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
  function rega(ch : char):value;


implementation

procedure boot;
  begin
    fillchar(ram, (maxcell + 1) * cellsize, 0);
    reg_dp^ := -1;
    reg_rp^ := -1;
    reg_ip^ := minheap;
    reg_hp^ := minheap;
    reg_ep^ := minheap;
    reg_ap^ := minheap;
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

function rega(ch : char):value;
  begin
    result := 4 * (ord(upcase(ch)) - ord('@'))
  end;


procedure runop(op : byte);
  var t,a : value;
  begin
    case op of
      { Do not reformat this function! mkoptbl.pas uses it! }
      $80 : {lb  } begin dput(bget(reg_ip^+1)); inc(reg_ip^) end;
      $81 : {li  } begin dput(rdval(reg_ip^+1)); inc(reg_ip^,3) end;
      $82 : {du  } dput(tos);
      $83 : {sw  } dswp;
      $84 : {ov  } dput(nos);
      $85 : {zp  } zap(dpop);
      $86 : {dc  } rput(dpop);
      $87 : {cd  } dput(rpop);
      $88 : {ad  } dput(dpop  + dpop);
      $89 : {sb  } dput(-dpop + dpop);
      $8A : {ml  } dput(dpop * dpop);
      $8B : {dv  } begin t:=dpop; dput(dpop div t) end;
      $8C : {md  } begin t:=dpop; dput(dpop mod t) end;
      $8D : {sh  } begin dswp; dput(dpop shl dpop) end;
      $8E : {an  } dput(dpop and dpop);
      $8F : {or  } dput(dpop or dpop);
      $90 : {xr  } dput(dpop xor dpop);
      $91 : {nt  } dput(not dpop);
      $92 : {eq  } if dpop =  dpop then dput(-1) else dput(0);
      $93 : {lt  } begin t:=dpop; if dpop <  t then dput(-1) else dput(0) end;
      $94 : {gt  } begin t:=dpop; if dpop >  t then dput(-1) else dput(0) end;
      $95 : {le  } begin t:=dpop; if dpop <= t then dput(-1) else dput(0) end;
      $96 : {hl  } halt;
      $97 : {jm  } reg_ip^ := rdval(reg_ip^+1)-1;
      $98 : {hp  } todo('hp'); { hop }
      $99 : {j0  } if dpop = 0 then begin reg_ip^ := rdval(reg_ip^+1)-1 end
                   else inc(reg_ip^,4) { skip over the address };
      $A0 : {h0  } todo('h0'); { hop if 0 }
      $A1 : {cl  } begin rput(reg_ip^+4); reg_ip^:=rdval(reg_ip^+1)-1 end; { call }
      $A2 : {rt  } reg_ip^ := rpop-1;
      $A3 : {r0  } if tos = 0 then begin zap(dpop); reg_ip^ := rpop end;
      $A4 : {nx  } begin if tor > 0 then begin t:=rpop; dec(t); rput(t) end;
                     if tor = 0 then begin zap(rpop); inc(reg_ip^,3) end
                     else reg_ip^:=rdval(reg_ip^+1)-1; end;
      $A5 : {rb  } dput(ram[dpop]);    { read byte }
      $A6 : {wb  } begin t:= dpop; ram[t]:= byte(dpop); end;  { write byte  }
      $A7 : {ri  } dput(rdval(dpop));
      $A8 : {wi  } begin t := dpop; wrval(t, dpop); end;
      $A9 : {rx  } begin
                     a := rega('X'); t := rdval(a);
                     dput(rdval(t));
                     wrval(a, t+4)
                   end;
      $AB : {ry  } begin
                     a := rega('Y'); t := rdval(a);
                     dput(rdval(t));
                     wrval(a, t+4)
                   end;
      $AC : {wz  } begin
                     a := rega('Z'); t := rdval(a);
                     wrval(t, dpop);
                     wrval(a, t+4)
                   end;
      $B0 : {tg  } term.invoke(vtTG);
      $B1 : {ta  } term.invoke(vtTA);
      $B2 : {tw  } term.invoke(vtTW);
      $B3 : {tr  } term.invoke(vtTR);
      $B4 : {tk  } term.invoke(vtTK);
      $B5 : {ts  } term.invoke(vtTS);
      $B6 : {tl  } term.invoke(vtTS);
      $B7 : {tc  } term.invoke(vtTS);
      $B8 : {db  } reg_db^ := 1;
      { reserved: } $B9..$BF : begin end;
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
  term := TB4Term.create;
end.
