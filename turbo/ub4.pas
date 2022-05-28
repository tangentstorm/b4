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
  minheap = 64;
  maxblok = 1023;
var
  ram  : array[0..maxcell] of value;
  bytes: array[0..maxcell*sizeof(value)] of byte;
  disk : file of block;
const {-- these are all offsets into the ram array --}
  ip    =  0; { instruction pointer }
  dp    =  1; { data stack pointer }
  rp    =  2; { retn stack pointer }
  hp    =  3; { heap pointer }
  last  =  4; { last dictionary entry }
  ap    =  5; { the 'a' register }
  ep    =  6; { the editor pointer }
  dbg   =  7; { debug flag }
  ml    = 64; { main loop }

  procedure open( path : string );
  procedure boot;
  function step : value;

  { these internal ops are used in the assembler }
  procedure dput( val : value );
  function dpop:value;
  procedure swap;


implementation

procedure boot;
  begin
    fillchar(ram, (maxcell + 1) * sizeof(value), 0);
    ram[dp] := maxdata;
    ram[rp] := maxretn;
    ram[ip] := 64;
    ram[hp] := 64;
    ram[ap] := 64;
    ram[ep] := 64;
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
    dpop := ram[ram[dp]]; inc(ram[dp]);
    if ram[dp] > maxdata then ram[dp] := mindata;
  end;

function rpop : value;
  begin
    rpop := ram[ram[rp]]; inc(ram[rp]);
    if ram[rp] > maxretn then ram[rp] := minretn;
  end;

function tos : value;
  begin
    tos := ram[ram[dp]]
  end;

function nos : value;
  begin
    if ram[dp] = mindata
      then nos := ram[maxdata]
      else nos := ram[ram[dp]]
  end;

procedure zap( v : value );
  begin { discards a value (for turbo pascal) }
  end;

{ the stacks are really rings, so no over/underflows }
procedure dput( val : value );
  begin
    dec(ram[dp]);
    if ram[dp] < mindata then ram[dp] := maxdata;
    ram[ram[dp]] := val;
  end;

procedure rput( val : value );
  begin
    dec(ram[rp]);
    if ram[rp] < minretn then ram[rp] := maxretn;
    ram[ram[rp]] := val;
  end;

procedure comma;
  begin
    ram[ram[hp]] := dpop;
    inc(ram[hp]);
  end;

procedure bye;
  begin
    ram[ip] := maxheap
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

function step : value;
  { execute next instruction, then increment and return the IP }
  begin
    case ram[ram[ip]] of
      { Do not reformat this function! mkoptbl.pas uses it! }
      00 : {ok  } ; { no-op }
      01 : {si  } ; { todo: short int }
      02 : {li  } begin inc(ram[ip]); dput(ram[ram[ip]]) end;
      03 : {sw  } swap;
      04 : {du  } dput(tos);
      05 : {ov  } dput(nos);
      06 : {zp  } zap(dpop);
      07 : {dr  } rput(dpop);
      08 : {rd  } dput(rpop);
      09 : {ad  } dput(dpop  + dpop);
      10 : {sb  } dput(-dpop + dpop);
      11 : {ml  } dput(dpop * dpop);
      12 : {dv  } dput(dpop div dpop);
      13 : {md  } dput(dpop mod dpop);
      14 : {ng  } dput(-dpop);
      15 : {sl  } begin swap; dput(dpop shl dpop) end;
      16 : {sr  } begin swap; dput(dpop shr dpop) end;
      17 : {an  } dput(dpop and dpop);
      18 : {or  } dput(dpop or dpop);
      19 : {xr  } dput(dpop xor dpop);
      20 : {nt  } dput(dpop xor dpop);
      21 : {eq  } if dpop =  dpop then dput(-1) else dput(0);
      22 : {gt  } if dpop >  dpop then dput(-1) else dput(0);
      23 : {lt  } if dpop <  dpop then dput(-1) else dput(0);
      24 : {ne  } if dpop <> dpop then dput(-1) else dput(0);
      25 : {ge  } if dpop >= dpop then dput(-1) else dput(0);
      26 : {le  } if dpop <= dpop then dput(-1) else dput(0);
      27 : {dx  } todo('dx');
      28 : {dy  } todo('dy');
      29 : {dz  } todo('dz');
      30 : {dc  } todo('dc');
      31 : {xd  } todo('xd');
      32 : {yd  } todo('yd');
      33 : {zd  } todo('zd');
      34 : {cd  } todo('cd');
      35 : {hl  } halt;
      36 : {jm  } ram[ip] := ram[ram[ip]+1];
      37 : {j0  } if dpop = 0 then begin ram[ip] := ram[ram[ip]+1] end
                  else inc(ram[ip]) { skip over the address };
      38 : {hp  } todo('hp'); { hop }
      39 : {h0  } todo('h0'); { hop if 0 }
      40 : {h1  } todo('h1'); { hop if 1 }
      41 : {nx  } todo('nx'); { next }
      42 : {cl  } todo('cl'); { call }
      43 : {rt  } ram[ip] := rpop;
      44 : {r0  } if tos = 0 then begin zap(dpop); ram[ip] := rpop end;
      45 : {r1  } if tos<> 0 then begin zap(dpop); ram[ip] := rpop end;
      46 : {ev  } todo('ev'); { eval - like call, but address comes from stack }
      47 : {rm  } dput(ram[dpop]);    { read memory }
      48 : {wm  } ram[dpop] := dpop;  { write memory }
      49 : {goxy} begin swap; kvm.gotoxy(dpop mod (xMax+1), dpop mod (yMax+1)) end;
      50 : {attr} kvm.textattr := dpop;
      51 : {putc} write(chr(dpop));
      52 : {getc} getc;
      53 : {boot} boot;
      54 : {load} load;
      55 : {save} save;
      56 : {keyp} if keypressed then dput(-1) else dput(0);
      57 : {cscr} kvm.clrscr;
      58 : {ceol} kvm.clreol;
      59 : {crxy} begin dput(kvm.wherex); dput(kvm.wherey) end;
      { reserved: } 60 .. 63 : begin end;
      else rput(ram[ip]); ram[ip] := ram[ram[ip]]
    end;
    inc(ram[ip]);
    step := ram[ip];
  end;

begin
end.
