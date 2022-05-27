{$mode delphi}{$i xpc}
{ this is the main entry point for b4 }
program b4;
uses xpc, cli, ub4, ub4asm, ub4ops, kvm, cw, kbd;

const pgsz = 8 * 9; { should be multiple of 8 to come out even }


procedure dump;
  { this displays the visual debugger }
  var x,y, oldattr :word; i, r : value; literal, target : boolean;
  begin
    x := wherex; y := wherey; oldattr := textattr; clrscr;

    gotoxy(0,17);
    literal := false; target := false; r := 0;

    { find the right page }
    while (r < (maxcell - pgsz)) and not( (ram[ep]<r) and (ram[ep] <= r+pgsz-1) )
    do inc(r,pgsz);

    { draw ram }
    for i := r to r + pgsz-1 do
      begin
        if (i=ram[ip]) and (i=ram[ep]) then begin bg('m'); fg('M') end
        else if i=ram[ip] then begin bg('c'); fg('m') end
        else if i=ram[ep] then begin bg('r'); fg('M') end
        else begin bg('k'); fg('m') end;
        if i mod 8 = 0 then writeln;
        write(i:5);
        if literal or (i < 64) { 0..63 is a register } then
          begin fg('y'); write(ram[i]:5); literal := false end
        else if target then
          begin if i = ram[ep] then fg('k') else fg('r');
                write(ram[i]:5); target := false
          end
        else if i > high(ram) then begin fg('K'); write('xxxxx') end
        else if (ram[i] in [1..high(optbl)]) then
          begin
            fg('W');
            write(optbl[ram[i]]:5);
            if ram[i] = 1 then literal := true;
            if ram[i] in [2..3] then target := true;
          end
        else begin fg('b'); write(ram[i]:5) end;
      end;

      { draw the data stack }
      gotoxy(0,16); bg('k'); clreol;
      fg('Y'); write('<', maxdata-ram[dp] ,'> '); fg('y');
      for i := maxdata-1 downto ram[dp] do write(ram[i],' ');

      gotoxy(x,y); textattr:=oldattr;
  end;


var ch : ansichar; debug, pause : boolean;
begin
  open('disk.b4'); boot; clrscr;
  assign(input, 'bios.b4a');
  reset(input); b4as;
  debug := false; pause:=false;
  while ram[ip] <= maxheap do
    repeat
      if ram[ip] = 64 then debug := true; { breakpoint! }
      if debug then
        begin
          dump; pause := true;
          ch := readkey;
          case upcase(ch) of
            ^N : inc(ram[ep],8);
            ^P : if ram[ep] >= 8 then dec(ram[ep],8);
            ^F : inc(ram[ep]);
            ^B : dec(ram[ep]);
            ^V : inc(ram[ep], pgsz);
            'I': ram[ep] := ram[ip];
            'S',
            ' ': pause := false;
            'G': debug := false;
            'Q': halt;
            '0'..'9' : fillchar(ram[minbuff],
                                (maxbuff-minbuff)*sizeof(value), ch);
          end;
          if ram[ep] < 0 then ram[ep] := maxcell;
          if ram[ep] > maxcell then ram[ep] := 0;
        end
    until (debug and pause) or (step >= maxheap);
  close(disk);
  writeln('done. press any key to continue');
  repeat until keypressed;
end.
