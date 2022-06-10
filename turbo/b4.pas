{$mode delphi}{$i xpc}
{ this is the main entry point for b4 }
program b4;
uses xpc, ub4, ub4asm, ub4ops, kvm, kbd,
 sysutils; // for format

const pgsz = 8 * 8; { should be multiple of 8 to come out even }

var opli, oplb, opjm, opj0, opcl : value;

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
  var x, y, oldattr: word; i, pg: value; skip: byte=0;
      literal, target: boolean; id: ub4asm.ident;
  begin
    x := wherex; y := wherey; oldattr := textattr;
    id := 'call';
    { draw the data and return stacks }
    draw_stack(0, 13, 'd', reg_dp^, maxdata);
    draw_stack(0, 14, 'r', reg_rp^, maxretn);

    { draw some important registers }
    gotoxy(0, 15); bg('K'); clreol;
    wv('ip', reg_ip^); wv('dp',reg_dp^);  wv('rp', reg_rp^);
    wv('hp', reg_hp^); wv('ep',reg_ep^);  wv('lp', reg_lp^);

    { draw ram }
    gotoxy(0,16); pg := pgsz * (reg_ep^ div pgsz);
    literal := false; target := false; { next cell is literal or jump target }
    for i := pg to pg + pgsz-1 do begin
      if (i mod 8 = 0) then begin
        bg('k'); if (i>pg) then writeln; clreol;
        fg('m'); write(hex(i,4));
      end;
      { color cell based on ip / editor cursor positions }
      if (i=reg_ip^) and (i=reg_ep^) then bg('m')
      else if i=reg_ip^ then bg('c')
      else if i=reg_ep^ then bg('r')
      else bg('k');
      { literal numbers (after si/li) }
      if skip>0 then begin dec(skip); target := false; literal := false end
      else if literal or (i < 32) then begin fg('y'); write(hex(ram[i],2):4); literal := false end
      else if target then { target adress for jump/etc }
          begin if i = reg_ep^ then fg('k') else fg('r');
                write(hex(ram[i],2):4); target := false
          end
      { past end of memory }
      else if i > high(ram) then begin fg('K'); write('xx') end
      { opcodes }
      else if ram[i] in [$80 .. $BF] then
        begin
          if ram[i] in [opli,oplb] then literal := true;
          if ram[i] in [opjm,opj0,opcl] then target := true;
          if (ram[i] = opli) or ((ram[i] in [opcl]) and (rdval(i+1) < high(address))) then
            begin
              if ram[i] = opcl
                then begin skip := 4; find_ident(rdval(i+1), id) end
                else begin skip := 1; find_ident(ram[i+1], id) end;
              write('  ');
              if ram[i] = opcl then fg('W') else begin fg('K'); write('$') end;
              write(format('%-6s',[id]))
            end
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
    repeat if reg_ep^ < ub4.maxcell then inc(reg_ep^)
    until (ram[reg_ep^] = ub4.maxcell)
       or (ram[reg_ep^] in [low(ub4.opcodes)..high(ub4.opcodes)]);
    result := reg_ep^;
  end;

var ch: ansichar; pause: boolean = false;
begin
  opli := b4opc('li');  oplb := b4opc('lb');
  opjm := b4opc('jm'); opj0 := b4opc('j0'); opcl := b4opc('cl');
  open('disk.b4'); boot; clrscr;
  assign(input, 'bios.b4a');
  reset(input); b4as;
  if paramstr(1)='-d' then reg_db^ := 1;
  while reg_ip^ <= maxheap do begin
    if reg_ip^ = reg_bp^ then begin reg_db^:=1; reg_bp^ := high(ub4.address) end;
    if reg_db^=1 then begin
      if not pause then reg_ep^ := reg_ip^;
      pause := true;
      dump;
      ch := readkey;
      case upcase(ch) of
        ^N : inc(reg_ep^,8);
        ^P : if reg_ep^ >= 8 then dec(reg_ep^,8);
        ^F : inc(reg_ep^);
        ^B : dec(reg_ep^);
        ^V : inc(reg_ep^, pgsz);
        'I': reg_ep^ := reg_ip^;
        'S': pause := false;
        'C': begin pause := false; reg_bp^ := reg_ep^ end;
        'O': begin pause := false; reg_db^:=0; reg_bp^ := step_over_addr end;
        'R': reg_db^ := 0;
        'Q': halt;
        '0': reg_ep^ := 0;
        'D': dump_dict;
      end;
      if reg_ep^ < 0 then reg_ep^ := maxcell;
      if reg_ep^ > maxcell then reg_ep^ := 0;
    end; { if ram[debug] }
    if not (pause and (reg_db^=1)) then step;
  end; { while }
  close(disk);
  {$IFDEF pauseafter} { for turbo pascal }
  writeln('done. press any key to continue');
  repeat until keypressed;
  {$ENDIF}
end.
