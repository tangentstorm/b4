{$mode delphi}{$i xpc}
{ this is the main entry point for b4 }
program b4;
uses xpc, ub4, ub4asm, ub4ops, kvm, kbd,
 sysutils; // for format

const pgsz = 8 * 8; { should be multiple of 8 to come out even }

var opli, opsi, opjm, opj0, opcl : value;

procedure draw_stack(x,y : byte; id:char; minaddr,maxaddr:value);
  var i : value;
  begin
    gotoxy(x,y); bg('k'); clreol; fg('w'); write(id,':');
    fg('Y'); write(' <'); fg('y'); write(maxaddr-minaddr); fg('Y');
    write('> '); fg('y');
    for i := maxaddr-1 downto minaddr do write(hex(ram[i],1),' ');
  end;

procedure wv(k: string; v:value); { write value }
  begin fg('w'); write(k); fg('k'); write(': ');
    fg('W'); write(hex(v,4)); write(' ')
  end;


procedure draw_help(x,y : byte);
  begin gotoxy(x,y); write('keys. (0)top (I)P (q)uit (s)tep (o)ver, (r)un, to (c)ursor, (q)uit')
  end;

procedure dump_dict;
  var i : byte;
  begin
    for i := 0 to ub4asm.ents do begin
      write(dict[i].id:8, ': ', hex(dict[i].adr,4) );
      if (i>0) and (i mod 4 = 0) then writeln;
    end;
    repeat until keypressed;
  end;

procedure dump;
  { this displays the visual debugger }
  var x, y, oldattr: word; i, pg: value;
      literal, target, hide: boolean; id: ub4asm.ident;
  begin
    x := wherex; y := wherey; oldattr := textattr;
    id := 'call';
    { draw the data and return stacks }
    draw_stack(0, 13, 'd', ram[dp], maxdata);
    draw_stack(0, 14, 'r', ram[rp], maxretn);

    { draw some important registers }
    gotoxy(0, 15); bg('K'); clreol;
    wv('ip', ram[ip]); wv('dp',ram[dp]);  wv('rp', ram[rp]);
    wv('hp', ram[hp]); wv('ep',ram[ep]);  wv('last',ram[last]);

    { draw ram }
    gotoxy(0,16); pg := pgsz * (ram[ep] div pgsz);
    hide := false; literal := false; target := false; { next cell is literal or jump target }
    for i := pg to pg + pgsz-1 do begin
      if (i mod 8 = 0) and (i>pg) then begin bg('k'); writeln; clreol end;
      { color cell based on ip / editor cursor positions }
      if (i=ram[ip]) and (i=ram[ep]) then begin bg('m'); fg('M') end
      else if i=ram[ip] then begin bg('c'); fg('m') end
      else if i=ram[ep] then begin bg('r'); fg('M') end
      else begin bg('k'); fg('m') end;
      if (i mod 8 = 0) then write(hex(i,4));
      { literal numbers (after si/li) }
      if hide then begin hide := false; target := false; end
      else if literal or (i < 32) then begin fg('y'); write(hex(ram[i],2):4); literal := false end
      else if target then { target adress for jump/etc }
          begin if i = ram[ep] then fg('k') else fg('r');
                write(hex(ram[i],2):4); target := false
          end
      { past end of memory }
      else if i > high(ram) then begin fg('K'); write('xx') end
      { opcodes }
      else if ram[i] in [$80 .. $BF] then
        begin
          if ram[i] in [opli,opsi] then literal := true;
          if ram[i] in [opjm,opj0,opcl] then target := true;
          if (ram[i] = opcl) and find_ident(ram[i+1], id)
            then begin fg('W'); write(' ',format('%-7s',[id])); hide := true end
            else begin fg('C'); write(optbl[byte(ram[i])] :4) end;
        end
      { ascii characters }
      else if (ram[i] >= 32) and (ram[i] < 128) then
        begin fg('g'); write('  '''); write(chr(ram[i])) end
      { anything else }
      else begin fg('b'); write(hex(ram[i],2):4) end
    end;
    { for ui debugging, draw line numbers on the right: }
    bg('K'); fg('k'); for i := 0 to 24 do begin gotoxy(xMax-1,i); write(i:2) end;
    gotoxy(0,24); for i := 1 to 8 do write(i*10:10);
    { help text }
    draw_help(0, 24);
    { restore cursor position and color so we don't break the vm }
    gotoxy(x,y); textattr := oldattr;
  end;


function step_over_addr: ub4.address;
  begin
    repeat if ram[ep] < ub4.maxcell then inc(ram[ep])
    until (ram[ram[ep]] = ub4.maxcell)
       or (ram[ram[ep]] in [low(ub4.opcodes)..high(ub4.opcodes)]);
    result := ram[ep];
  end;

var ch: ansichar; pause: boolean = false;
begin
  opli := b4opc('li');  opjm := b4opc('jm'); opj0 := b4opc('j0'); opcl := b4opc('cl');
  open('disk.b4'); boot; clrscr;
  assign(input, 'bios.b4a');
  reset(input); b4as;
  if paramstr(1)='-d' then ram[dbgf] := 1;
  while ram[ip] <= maxheap do begin
    if ram[ip] = ram[stop] then begin ram[dbgf]:=1; ram[stop] := high(ub4.address) end;
    if ram[dbgf]=1 then begin
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
        'S': pause := false;
        'C': begin pause := false; ram[stop] := ram[ep] end;
        'O': begin pause := false; ram[dbgf]:=0; ram[stop] := step_over_addr end;
        'R': ram[dbgf] := 0;
        'Q': halt;
        '0': ram[ep] := 0;
        'D': dump_dict;
      end;
      if ram[ep] < 0 then ram[ep] := maxcell;
      if ram[ep] > maxcell then ram[ep] := 0;
    end; { if ram[debug] }
    if not (pause and (ram[dbgf]=1)) then step;
  end; { while }
  close(disk);
  {$IFDEF pauseafter} { for turbo pascal }
  writeln('done. press any key to continue');
  repeat until keypressed;
  {$ENDIF}
end.
