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
  dbgf  =  7; { debug flag }

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
    ram[ip] := minheap;
    ram[hp] := minheap;
    ram[ep] := minheap;
    ram[ap] := minheap;
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

function tor : value;
  begin
    tor := ram[ram[rp]]
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
      $80 : {ok  } ; { no-op }
      $81 : {si  } begin inc(ram[ip]); dput(ram[ram[ip]]) end;
      $82 : {li  } begin inc(ram[ip]); dput(ram[ram[ip]]) end; { todo: long int }
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
      $A4 : {jm  } ram[ip] := ram[ram[ip]+1]-1;
      $A5 : {j0  } if dpop = 0 then begin ram[ip] := ram[ram[ip]+1]-1 end
                   else inc(ram[ip]) { skip over the address };
      $A6 : {hp  } todo('hp'); { hop }
      $A7 : {h0  } todo('h0'); { hop if 0 }
      $A8 : {h1  } todo('h1'); { hop if 1 }
      $A9 : {nx  } begin if tor > 0 then ram[ram[rp]]:=tor-1;
                     if tor = 0 then begin zap(rpop); inc(ram[ip]) end
                     else ram[ip]:=ram[ram[ip]+1]-1; end;
      $AA : {cl  } begin rput(ram[ip]); ram[ip] := ram[ram[ip]+1]-1 end; { call }
      $AB : {rt  } ram[ip] := rpop;
      $AC : {r0  } if tos = 0 then begin zap(dpop); ram[ip] := rpop end;
      $AD : {r1  } if tos<> 0 then begin zap(dpop); ram[ip] := rpop end;
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
      $B9 : {boot} boot;
      $BA : {load} load;
      $BB : {save} save;
      $BC : {dbg } ram[dbgf] := 1;
      { reserved: } $BD .. $BF : begin end;
      else { no-op };
    end;
    inc(ram[ip]);
    step := ram[ip];
  end;

begin
end.
