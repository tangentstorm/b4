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
  reg_ip : ^value = @ram[0]; { instruction pointer }
  reg_dp : ^value = @ram[1]; { data stack pointer }
  reg_rp : ^value = @ram[2]; { retn stack pointer }
  reg_hp : ^value = @ram[3]; { heap pointer }
  reg_lp : ^value = @ram[4]; { last dictionary entry }
  reg_ap : ^value = @ram[5]; { the 'a' register }
  reg_ep : ^value = @ram[6]; { the editor pointer }
  reg_db : ^value = @ram[7]; { debug flag }
  reg_bp : ^value = @ram[8]; { breakpoint }

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


function step : value;
  var t : value;
  { execute next instruction, then increment and return the IP }
  begin
    case ram[reg_ip^] of
      { Do not reformat this function! mkoptbl.pas uses it! }
      $80 : {ok  } ; { no-op }
      $81 : {si  } begin inc(reg_ip^); dput(ram[reg_ip^]) end;
      $82 : {li  } begin inc(reg_ip^); dput(ram[reg_ip^]) end; { todo: long int }
      $83 : {sw  } swap;
      $84 : {du  } dput(tos);
      $85 : {ov  } dput(nos);
      $86 : {zp  } zap(dpop);
      $87 : {dr  } rput(dpop);
      $88 : {rd  } dput(rpop);
      $89 : {ad  } dput(dpop  + dpop);
      $8A : {sb  } dput(-dpop + dpop);
      $8B : {ml  } dput(dpop * dpop);
      $8C : {dv  } dput(dpop div dpop);
      $8D : {md  } dput(dpop mod dpop);
      $8E : {ng  } dput(-dpop);
      $8F : {sl  } begin swap; dput(dpop shl dpop) end;
      $90 : {sr  } begin swap; dput(dpop shr dpop) end;
      $91 : {an  } dput(dpop and dpop);
      $92 : {or  } dput(dpop or dpop);
      $93 : {xr  } dput(dpop xor dpop);
      $94 : {nt  } dput(dpop xor dpop);
      $95 : {eq  } if dpop =  dpop then dput(-1) else dput(0);
      $96 : {gt  } if dpop >  dpop then dput(-1) else dput(0);
      $97 : {lt  } if dpop <  dpop then dput(-1) else dput(0);
      $98 : {ne  } if dpop <> dpop then dput(-1) else dput(0);
      $99 : {ge  } if dpop >= dpop then dput(-1) else dput(0);
      $9A : {le  } if dpop <= dpop then dput(-1) else dput(0);
      $9B : {dx  } todo('dx');
      $9C : {dy  } todo('dy');
      $9D : {dz  } todo('dz');
      $9E : {dc  } todo('dc');
      $9F : {xd  } todo('xd');
      $A0 : {yd  } todo('yd');
      $A1 : {zd  } todo('zd');
      $A2 : {cd  } todo('cd');
      $A3 : {hl  } halt;
      $A4 : {jm  } reg_ip^ := ram[reg_ip^+1]-1;
      $A5 : {j0  } if dpop = 0 then begin reg_ip^ := ram[reg_ip^+1]-1 end
                   else inc(reg_ip^) { skip over the address };
      $A6 : {hp  } todo('hp'); { hop }
      $A7 : {h0  } todo('h0'); { hop if 0 }
      $A8 : {h1  } todo('h1'); { hop if 1 }
      $A9 : {nx  } begin if tor > 0 then ram[reg_rp^]:=tor-1;
                     if tor = 0 then begin zap(rpop); inc(reg_ip^) end
                     else reg_ip^:=ram[reg_ip^+1]-1; end;
      $AA : {cl  } begin rput(reg_ip^+4); reg_ip^:=rdval(reg_ip^+1)-1 end; { call }
      $AB : {rt  } reg_ip^ := rpop-1;
      $AC : {r0  } if tos = 0 then begin zap(dpop); reg_ip^ := rpop end;
      $AD : {r1  } if tos<> 0 then begin zap(dpop); reg_ip^ := rpop end;
      $AE : {ev  } todo('ev'); { eval - like call, but address comes from stack }
      $AF : {rm  } dput(ram[dpop]);    { read memory }
      $B0 : {wm  } ram[dpop] := dpop;  { write memory }
      $B1 : {tg  } begin swap; kvm.gotoxy(dpop mod (xMax+1), dpop mod (yMax+1)) end;
      $B2 : {ta  } kvm.textattr := dpop;
      $B3 : {tw  } if tos in [$00..$ff] then write(chr(dpop)) else write('[',dpop,']');
      $B4 : {tr  } getc;
      $B5 : {tk  } if keypressed then dput(-1) else dput(0);
      $B6 : {ts  } kvm.clrscr;
      $B7 : {tl  } kvm.clreol;
      $B8 : {tc  } begin dput(kvm.wherex); dput(kvm.wherey) end;
      $B9 : {db  } reg_db^ := 1;
      $BA : {rv  } dput(rdval(dpop));
      $BB : {wv  } begin t := dpop; wrval(t, dpop); end;
      { reserved: } $BC..$BF : begin end;
      else { no-op };
    end;
    inc(reg_ip^);
    step := reg_ip^;
  end;

begin
end.
