unit ub4;
interface uses crt;
{
  this contains the virtual machine
  and the code to load and save blocks.
}

type
  value = longint;
  block = array[0..1023] of byte;

const
  {-- memory layout --}
  maxcell = 4095;
  maxretn = maxcell;
  minretn = maxretn-256;
  maxdata = minretn-1;
  mindata = maxdata-256;
  maxbuff = mindata-1;
  minbuff = maxbuff-256;
  maxheap = maxbuff; { so you can read/write the i/o buffer }
  maxblok = 1023;
var
  ram  : array[0..maxcell] of value;
  disk : file of block;
const {-- these are all offsets into the ram array --}
  ip    = 0; { instruction pointer }
  dp    = 1; { data stack pointer }
  rp    = 2; { addr stack pointer }
  heap  = 3; { heap pointer }
	last  = 4; { last dictionary entry }
  ml    = 64; { main loop }


  procedure open( path : string );
  procedure boot;
  function step : value;

implementation

procedure boot;
  begin
    fillchar(ram, (maxcell + 1) * sizeof(value), 0);
    ram[dp] := maxdata;
    ram[rp] := maxretn;
    ram[ip] := 64;
  end;

procedure halt;
  begin
    normvideo;
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
  begin
    { discards a value }
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
  end;

procedure comma;
  begin
		ram[ram[heap]] := dpop;
    inc(ram[heap]);
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

function maxx : byte;
  begin
    maxx := lo( crt.windmax ) - lo( crt.windmin )
  end;

function maxy : byte;
  begin
    maxy := hi( crt.windmax ) - hi( crt.windmin )
  end;

procedure goxy( x, y : byte );
  begin
    crt.gotoxy( x+1, y+1 )
  end;

procedure blok( n : value );
  begin
    if (n < 0) or (n > maxblok)
      then seek(disk, 0)
      else seek(disk, n);
  end;

procedure load;
  var i : word; tmp : block;
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
  var i : word; tmp : block;
  begin
    i := size;
    if i + 1 < maxblok then seek(disk, i + 1)
  end;

procedure save;
  var i : word; tmp : block;
  begin
    move(ram[minbuff], tmp, 1024);
    write(disk, tmp);
  end;

function step : value;
  { execute next instruction, then increment and return the IP }
  begin
    case ram[ram[ip]] of
      { Do not reformat this function! mkb4asm.pas uses it! }
      00 : {nop } begin end;
      01 : {lit } begin inc(ram[ip]); dput(ram[ram[ip]]) end;
      02 : {jmp } ram[ram[ip]] := ram[ram[ip]+1];
      03 : {jwz } if tos = 0 then
                  begin
                    zap(dpop);
                    ram[ram[ip]] := ram[ram[ip]+1];
                  end;
      04 : {ret } ram[ip] := rpop;
      05 : {rwz } if tos = 0 then
                   begin
                     zap(dpop);
                     ram[ip] := rpop;
                   end;
      06 : {eq  } if dpop =  dpop then dput(-1) else dput(0);
      07 : {ne  } if dpop <> dpop then dput(-1) else dput(0);
      08 : {gt  } if dpop >  dpop then dput(-1) else dput(0);
      09 : {lt  } if dpop <  dpop then dput(-1) else dput(0);
      10 : {lte } if dpop <= dpop then dput(-1) else dput(0);
      11 : {gte } if dpop >= dpop then dput(-1) else dput(0);
      12 : {and } dput(dpop and dpop);
      13 : {or  } dput(dpop or dpop);
      14 : {xor } dput(dpop xor dpop);
      15 : {add } dput(dpop  + dpop);
      16 : {sub } dput(-dpop + dpop);
      17 : {mul } dput(dpop * dpop);
      18 : {dvm } begin
                   rput(tos mod nos);
                   dput(dpop div dpop);
                   dput(rpop);
                 end;
      19 : {shl } begin swap; dput(dpop shl dpop) end;
      20 : {shr } begin swap; dput(dpop shr dpop) end;
      21 : {push} rput(dpop);
      22 : {pop } dput(rpop);
      23 : {inc } inc(ram[ram[dp]]);
      24 : {dec } dec(ram[ram[dp]]);
      25 : {get } dput(ram[dpop]);
      26 : {set } ram[dpop] := dpop;
      27 : {dup } dput(tos);
      28 : {zap } zap(dpop);
      29 : {swap} swap;
      30 : {over} dput(nos);
      31 : {goxy} begin
                   swap;
                   crt.gotoxy(dpop mod (maxY + 1),
                              dpop mod (maxY + 1))
                 end;
      32 : {attr} crt.textattr := dpop;
      33 : {putc} write(chr(dpop));
      34 : {getc} begin
                   dput(value(crt.readkey));
                   { we have 32 bits and only need 16,
                     so we can store extended keys in
                     one cell }
                   if tos = 0 then
                     begin
                       zap(dpop);
                       dput(value(crt.readkey) shl 8)
                     end
                 end;
      35 : {halt} halt;
      36 : {boot} boot;
      37 : {load} load;
      38 : {save} save;
      39 : {keyp} if keypressed then dput(-1) else dput(0);
      { reserved: } 40 .. 63 : begin end;
      else
        rput(ram[ip]);
        ram[ip] := ram[ram[ip]];
    end;
    inc(ram[ip]);
    step := ram[ip];
  end;

begin
end.