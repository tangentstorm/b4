{$mode delphi}{$i xpc}
unit ub4; { the b4 virtual machine }
interface uses xpc, kvm, kbd;

type
  value = longint;
  block = array[0..1023] of byte;

const
  {-- memory layout --}
  maxcell = 4095;
  { return stack }
  maxretn = maxcell;
  minretn = maxretn-256;
  { data stack }
  maxdata = minretn-1;
  mindata = maxdata-256;
  { io buffer }
  maxbuff = mindata-1;
  minbuff = maxbuff-256;
  { scratch workspace }
  maxwork = minbuff-1;
  minwork = maxwork-256;
  maxheap = maxbuff; { so you can read/write the i/o buffer }
  minheap = 256;
  maxblok = 1023;
type
  address = 0..maxcell;
  opcodes = $80..$BF;
var
  ram  : array[0..maxcell] of value;
  bytes: array[0..maxcell*sizeof(value)] of byte;
  disk : file of block;
const {-- these are all offsets into the ram array --}
  reg_ip : ^value = @ram[$00]; { instruction pointer }
  reg_dp : ^value = @ram[$04]; { data stack pointer }
  reg_rp : ^value = @ram[$08]; { retn stack pointer }
  reg_hp : ^value = @ram[$0C]; { heap pointer }
  reg_lp : ^value = @ram[$10]; { last dictionary entry }
  reg_ap : ^value = @ram[$14]; { the 'a' register }
  reg_ep : ^value = @ram[$18]; { the editor pointer }
  reg_db : ^value = @ram[$1C]; { debug flag }
  reg_bp : ^value = @ram[$20]; { breakpoint }

  procedure open( path : string );
  procedure boot;
  function step : value;
  function rdval(adr:address): value;
  procedure wrval(adr:address; v: value);

  { these internal ops are used in the assembler }
  procedure dput( val : value );
  function dpop:value;
  procedure swap;


implementation

procedure boot;
  begin
    fillchar(ram, (maxcell + 1) * sizeof(value), 0);
    reg_dp^ := maxdata;
    reg_rp^ := maxretn;
    reg_ip^ := minheap;
    reg_hp^ := minheap;
    reg_ep^ := minheap;
    reg_ap^ := minheap;
    reg_bp^ := maxcell;
  end;

procedure halt;
  begin
    system.halt;
  end;

procedure open(path:string);
  begin
    assign(disk, path);
  {$i-}
      reset(disk);
      if ioresult <> 0 then rewrite(disk);
      if ioresult <> 0 then
        begin
          writeln('error: couldn''t open ', path);
          halt;
        end;
  {$i+}
  end;

function dpop : value;
  begin
    dpop := ram[reg_dp^]; inc(reg_dp^);
    if reg_dp^ > maxdata then reg_dp^ := mindata;
  end;

function rpop : value;
  begin
    rpop := ram[reg_rp^]; inc(reg_rp^);
    if reg_rp^ > maxretn then reg_rp^ := minretn;
  end;

function tos : value;
  begin
    tos := ram[reg_dp^]
  end;

function tor : value;
  begin
    tor := ram[reg_rp^]
  end;

function nos : value;
  begin
    if reg_dp^ = mindata
      then nos := ram[maxdata]
      else nos := ram[reg_dp^+1]
  end;

procedure zap( v : value );
  begin { discards a value (for turbo pascal) }
  end;

{ the stacks are really rings, so no over/underflows }
procedure dput( val : value );
  begin
    dec(reg_dp^);
    if reg_dp^ < mindata then reg_dp^ := maxdata;
    ram[reg_dp^] := val;
  end;

procedure rput( val : value );
  begin
    dec(reg_rp^);
    if reg_rp^ < minretn then reg_rp^ := maxretn;
    ram[reg_rp^] := val;
  end;

procedure comma;
  begin
    ram[reg_hp^] := dpop;
    inc(reg_hp^);
  end;

procedure bye;
  begin
    reg_ip^ := maxheap
  end;

procedure swap;
  var tmp : value;
  begin
    tmp := dpop; rput(dpop); dput(tmp); dput(rpop);
  end;

{$IFDEF TURBO}
function xmax : byte;
  begin
    xmax := lo( kvm.windmax ) - lo( kvm.windmin )
  end;

function maxy : byte;
  begin
    ymax := hi( kvm.windmax ) - hi( kvm.windmin )
  end;
{$ENDIF}

procedure goxy( x, y : byte );
  begin
    kvm.gotoxy( x+1, y+1 )
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
  begin
    size := filesize(disk)
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

procedure getc;
  begin dput(value(kbd.readkey));
    { we have 32 bits & only need 16. we can store extended keys in one cell }
    if tos = 0 then begin zap(dpop); dput(value(kbd.readkey) shl 8) end
  end;

procedure todo(op : string);
  begin
    writeln('todo: ', op);
    halt
  end;


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

function dpon:value;
  begin swap; result:=dpop
  end;


function step : value;
  var t,x,y : value;
  { execute next instruction, then increment and return the IP }
  begin
    case ram[reg_ip^] of
      { Do not reformat this function! mkoptbl.pas uses it! }
      $80 : {si  } begin dput(bget(reg_ip^+1)); inc(reg_ip^) end;
      $81 : {li  } begin dput(rdval(reg_ip^+1)); inc(reg_ip^,3) end;
      $82 : {sw  } swap;
      $83 : {du  } dput(tos);
      $84 : {ov  } dput(nos);
      $85 : {zp  } zap(dpop);
      $86 : {dr  } rput(dpop);
      $87 : {rd  } dput(rpop);
      $88 : {ad  } dput(dpop  + dpop);
      $89 : {sb  } dput(-dpop + dpop);
      $8A : {ml  } dput(dpop * dpop);
      $8B : {dv  } dput(dpop div dpop);
      $8C : {md  } dput(dpop mod dpop);
      $8D : {ng  } dput(-dpop);
      $8E : {sl  } begin swap; dput(dpop shl dpop) end;
      $8F : {sr  } begin swap; dput(dpop shr dpop) end;
      $90 : {an  } dput(dpop and dpop);
      $91 : {or  } dput(dpop or dpop);
      $92 : {xr  } dput(dpop xor dpop);
      $93 : {nt  } dput(dpop xor dpop);
      $94 : {eq  } if dpop =  dpop then dput(-1) else dput(0);
      $95 : {gt  } if dpop >  dpop then dput(-1) else dput(0);
      $96 : {lt  } if dpop <  dpop then dput(-1) else dput(0);
      $97 : {ne  } if dpop <> dpop then dput(-1) else dput(0);
      $98 : {ge  } if dpop >= dpop then dput(-1) else dput(0);
      $99 : {le  } if dpop <= dpop then dput(-1) else dput(0);
      $9A : {dx  } todo('dx');
      $9B : {dy  } todo('dy');
      $9C : {dz  } todo('dz');
      $9D : {dc  } todo('dc');
      $9E : {xd  } todo('xd');
      $9F : {yd  } todo('yd');
      $A0 : {zd  } todo('zd');
      $A1 : {cd  } todo('cd');
      $A2 : {hl  } halt;
      $A3 : {jm  } reg_ip^ := rdval(reg_ip^+1)-1;
      $A4 : {j0  } if dpop = 0 then begin reg_ip^ := rdval(reg_ip^+1)-1 end
                   else inc(reg_ip^) { skip over the address };
      $A5 : {hp  } todo('hp'); { hop }
      $A6 : {h0  } todo('h0'); { hop if 0 }
      $A7 : {h1  } todo('h1'); { hop if 1 }
      $A8 : {nx  } begin if tor > 0 then mset(reg_rp^, tor-1);
                     if tor = 0 then begin zap(rpop); inc(reg_ip^,3) end
                     else reg_ip^:=rdval(reg_ip^+1)-1; end;
      $A9 : {cl  } begin rput(reg_ip^+4); reg_ip^:=rdval(reg_ip^+1)-1 end; { call }
      $AA : {rt  } reg_ip^ := rpop-1;
      $AB : {r0  } if tos = 0 then begin zap(dpop); reg_ip^ := rpop end;
      $AC : {r1  } if tos<> 0 then begin zap(dpop); reg_ip^ := rpop end;
      $AD : {ev  } todo('ev'); { eval - like call, but address comes from stack }
      $AE : {rm  } dput(ram[dpop]);    { read byte }
      $AF : {wm  } begin t:= dpop; ram[t]:= dpop; end;  { write byte  }
      $B0 : {tg  } begin swap; kvm.gotoxy(dpop mod (xMax+1), dpop mod (yMax+1)) end;
      $B1 : {ta  } kvm.textattr := dpop;
      $B2 : {tw  } if tos in [$00..$ff] then write(chr(dpop)) else write('[',dpop,']');
      $B3 : {tr  } getc;
      $B4 : {tk  } if keypressed then dput(-1) else dput(0);
      $B5 : {ts  } kvm.clrscr;
      $B6 : {tl  } kvm.clreol;
      $B7 : {tc  } begin dput(kvm.wherex); dput(kvm.wherey) end;
      $B8 : {db  } reg_db^ := 1;
      $B9 : {rv  } dput(rdval(dpop));
      $BA : {wv  } begin t := dpop; wrval(t, dpop); end;
      { reserved: } $BB..$BF : begin end;
      else { no-op };
    end;
    inc(reg_ip^);
    step := reg_ip^;
  end;

begin
end.
