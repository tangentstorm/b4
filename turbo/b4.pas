{ this is the main entry point for b4 }
program b4;
uses ub4, ub4asm, ub4ops, crt;

procedure dump;
  { this displays the visual debugger }
  var x,y, oldattr :byte; i, r : integer; literal, target : boolean;
  begin
    x:= wherex; y:=wherey; oldattr := textattr;
    writeln; gotoxy(1,16);
    literal := false; target := false; r := 0;
    while (r < (maxheap - 64)) and not (ram[ip] in [r..r+63]) do inc(r,64);
    for i := r to r + 63 do
      begin
        if i=ram[ip] then textbackground(1) else textbackground(0);
        textcolor(5); write(i:4,':');
        if literal then
          begin textcolor(6); write(ram[i]:5); literal := false end
        else if target then
          begin textcolor(4); write(ram[i]:5); target := false end
        else if (ram[i] in [1..63]) then
          begin
            textcolor(15);
            write(optbl[ram[i]]:5);
            if ram[i] = 1 then literal := true;
            if ram[i] in [2..3] then target := true;
          end
        else begin textcolor(3); write(ram[i]:5) end;
      end;
      gotoxy(1,23); textbackground(cyan); clreol; textcolor(0);
      textcolor(14); write('<', maxdata-ram[dp] ,'> ');
      textcolor(0);
      for i := maxdata-1 downto ram[dp] do write(ram[i],' ');
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
          case upcase(readkey) of
            'S',
            ' ': pause := false;
            'C': debug := false;
            'Q': halt;
          end
        end
    until (debug and pause) or (step >= maxheap);
  close(disk);
  writeln('done. press any key to continue');
  repeat until keypressed;
end.
