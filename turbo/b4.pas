{ this is the main entry point for b4 }
program b4;
uses ub4, ub4asm, ub4ops, crt;

procedure dump;
  { this displays the visual debugger }
  var x,y, oldattr :byte; i, r : value; literal, target : boolean;
  const pgsz = 56; { should be multiple of 8 to come out even }
  begin
    x := wherex; y := wherey; oldattr := textattr;

    gotoxy(1,18);
    literal := false; target := false; r := 0;

    { find the right page }
    while (r < (maxcell - pgsz)) and
      not((ram[ep] >= r) and (ram[ep] <= r+pgsz-1)) do
      inc(r,pgsz);

    { draw ram }
    for i := r to r + pgsz-1 do
      begin
        if (i=ram[ip]) and (i=ram[ep]) then
          begin textbackground(magenta); textcolor(lightmagenta) end
        else if i=ram[ip] then
          begin textbackground(cyan); textcolor(magenta) end
        else if i=ram[ep] then
          begin textbackground(red); textcolor(lightmagenta) end
        else
          begin textbackground(black); textcolor(magenta) end;
        write(i:5);
        if literal or (i < 64) { 0..63 is a register } then
          begin textcolor(6); write(ram[i]:5); literal := false end
        else if target then
          begin if i = ram[ep] then textcolor(0) else textcolor(4);
                write(ram[i]:5); target := false
          end
        else if (ram[i] in [1..pgsz-1]) then
          begin
            textcolor(15);
            write(optbl[ram[i]]:5);
            if ram[i] = 1 then literal := true;
            if ram[i] in [2..3] then target := true;
          end
        else begin textcolor(1); write(ram[i]:5) end;
      end;

      { draw the data stack }
      gotoxy(1,17); textbackground(0); clreol; textcolor(0);
      textcolor(14); write('<', maxdata-ram[dp] ,'> ');
      textcolor(brown);
      for i := maxdata-1 downto ram[dp] do write(ram[i],' ');

      textbackground(black);
      for r := 1 to 16 do
        begin
          gotoxy(57, r);
          if r < 3 then textcolor(blue) else
          textcolor(lightblue); for i := 1 to 24 do write('#');
        end;
      gotoxy(x,y); textattr:=oldattr;
  end;

var ch : char; debug, pause : boolean;
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
            ^P : dec(ram[ep],8);
            ^F : inc(ram[ep]);
            ^B : dec(ram[ep]);
            'I': ram[ep] := ram[ip];
            'S',
            ' ': pause := false;
            'G': debug := false;
            'Q': halt;
            '0'..'9' : fillchar(ram[minbuff], (maxbuff-minbuff)*sizeof(value), ch);
          end;
          if ram[ep] < 0 then ram[ep] := maxcell;
          if ram[ep] > maxcell then ram[ep] := 0;
        end
    until (debug and pause) or (step >= maxheap);
  close(disk);
  writeln('done. press any key to continue');
  repeat until keypressed;
end.
