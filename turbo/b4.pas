{$mode delphi}{$i xpc}
{ this is the main entry point for b4 }
program b4;
uses xpc, ub4, ub4asm, ub4ops, kvm, kbd;

const pgsz = 8 * 8; { should be multiple of 8 to come out even }

procedure draw_stack(x,y : byte; id:char; minaddr,maxaddr:value);
  var i : value;
  begin
    gotoxy(x,y); bg('k'); clreol; fg('w'); write(id,':');
    fg('Y'); write(' <'); fg('y'); write(maxaddr-minaddr); fg('Y');
    write('> '); fg('y');
    for i := maxaddr-1 downto minaddr do write(ram[i],' ');
  end;

procedure wv(k: string; v:value); { write value }
  begin fg('w'); write(k); fg('k'); write(': ');
    fg('W'); write(v); write(' ')
  end;


procedure dump;
  { this displays the visual debugger }
  var x, y, oldattr: word; i, pg: value; literal, target: boolean;
  begin
    x := wherex; y := wherey; oldattr := textattr;

    { draw the data and return stacks }
    draw_stack(0, 13, 'd', ram[dp], maxdata);
    draw_stack(0, 14, 'r', ram[rp], maxretn);

    { draw some important registers }
    gotoxy(0, 15); bg('K'); clreol;
    wv('ip', ram[ip]); wv('hp', ram[hp]); wv('ep',ram[ep]);

    { draw ram }
    gotoxy(0,16); pg := pgsz * (ram[ep] div pgsz);
    literal := false; target := false; { next cell is literal or jump target }
    for i := pg to pg + pgsz-1 do
      begin
        if (i=ram[ip]) and (i=ram[ep]) then begin bg('m'); fg('M') end
        else if i=ram[ip] then begin bg('c'); fg('m') end
        else if i=ram[ep] then begin bg('r'); fg('M') end
        else begin bg('k'); fg('m') end;
        if (i>pg) and (i mod 8 = 0) then writeln;
        write(i:5);
        if literal or (i < 64) { 0..63 is a register } then
          begin fg('y'); write(ram[i]:5); literal := false end
        else if target then
          begin if i = ram[ep] then fg('k') else fg('r');
                write(ram[i]:5); target := false
          end
        else if i > high(ram) then begin fg('K'); write('xxxxx') end
        else if (ram[i] in [1..high(optbl)]) then
          begin fg('W'); write(optbl[ram[i]]:5);
            if ram[i] = 1 then literal := true;
            if ram[i] in [2..3] then target := true;
          end
        else begin fg('b'); write(ram[i]:5) end;
      end;
    { for ui debugging, draw line numbers on the right: }
    bg('K'); fg('k'); for i := 0 to 24 do begin gotoxy(xMax-1,i); write(i:2) end;
    gotoxy(0,24); for i := 1 to 8 do write(i*10:10);
    { restore cursor position and color so we don't break the vm }
    gotoxy(x,y); textattr := oldattr;
  end;


var ch: ansichar; pause: boolean = false;
begin
  open('disk.b4'); boot; clrscr;
  assign(input, 'bios.b4a');
  reset(input); b4as;
  ram[dbg] := 1;
  while ram[ip] <= maxheap do begin
    if ram[dbg]=1 then begin
      if not pause then ram[ep] := ram[ip];
      pause := true;
      dump;
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
        'G': ram[dbg] := 0;
        'Q': halt;
        '0'..'9' : fillchar(ram[minbuff],
                            (maxbuff-minbuff)*sizeof(value), ch);
      end;
      if ram[ep] < 0 then ram[ep] := maxcell;
      if ram[ep] > maxcell then ram[ep] := 0;
    end; { if ram[debug] }
    if not (pause and (ram[dbg]=1)) then step;
  end; { while }
  close(disk);
  {$IFDEF pauseafter} { for turbo pascal }
  writeln('done. press any key to continue');
  repeat until keypressed;
  {$ENDIF}
end.
