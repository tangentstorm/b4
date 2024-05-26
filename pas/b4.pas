{$mode delphi}{$i xpc}
{ this is the main entry point for b4 }
program b4;
uses xpc, ub4, ub4asm, ub4ops, kvm, kbd, uhw_vt,
 sysutils; // for format

const pgsz = 16 * 8; { should be multiple of 8 to come out even }

var opli, oplb, opjm, opcl, opnx, ophp, oph0 : value;

procedure draw_stack(x,y : byte; id:char; var s:stack; n:value);
  var i : value;
  begin
    gotoxy(x,y); bg('k'); clreol; fg('w'); write(id,':');
    fg('Y'); write(' <'); fg('y'); write(n+1); fg('Y');
    write('> '); fg('y');
    for i := 0 to n do write(hex(s[i],1),' ');
  end;

procedure wv(k: string; v:value); { write value }
  begin fg('w'); write(k); fg('k'); write(': ');
    fg('W'); write(hex(v,4)); write(' ')
  end;


procedure draw_help(x,y : byte);
  begin gotoxy(x,y); write('keys. (0)top (I)P (s)tep (o)ver, (r)un, to (c)ursor, (q)uit ')
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
    draw_stack(0, 13, 'd', ds^, rg[RDS]);
    draw_stack(0, 14, 'c', cs^, rg[RCS]);

    { draw some important registers }
    gotoxy(0, 15); bg('K'); clreol;
    wv('ip', rg[RIP]); wv('@_', rg[RHP]); wv('ep',rg[RED]);

    { draw ram }
    gotoxy(0,16); pg := pgsz * (rg[RED] div pgsz);
    literal := false; target := false; { next cell is literal or jump target }
    for i := pg to pg + pgsz-1 do begin
      if (i mod 16 = 0) then begin
        bg('k'); if (i>pg) then writeln; clreol;
        fg('m'); write(hex(i,4));
      end;
      { color cell based on ip / editor cursor positions }
      if (i=rg[RIP]) and (i=rg[RED]) then bg('m')
      else if i=rg[RIP] then bg('c')
      else if i=rg[RED] then bg('r')
      else bg('k');
      { literal numbers (after si/li) }
      if skip>0 then begin dec(skip); target := false; literal := false end
      else if literal or (i < 32) then begin fg('y'); write(hex(ram[i],2):4); literal := false end
      else if target then { target adress for jump/etc }
          begin if i = rg[RED] then fg('k') else fg('r');
                write(hex(ram[i],2):4); target := false
          end
      { past end of memory }
      else if i > high(ram) then begin fg('K'); write('xx') end
      { opcodes }
      else if ram[i] in [$80 .. $BF] then
        begin
          if ram[i] in [opli,oplb] then literal := true;
          if ram[i] in [opjm,opcl] then target := true;
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


function step_over: boolean;
  { what this does is fast-forward the editor pointer.
    result = true if stepping over a call }
  var skip : byte = 0;
  begin
    result := ram[rg[RED]] = opcl;
    if ram[rg[RED]] in [opcl,opjm,opli,opnx] then skip := 4
    else if ram[rg[RED]] in [oplb,ophp,oph0] then skip := 1;
    inc(rg[RED]);
    inc(rg[RED], skip);
  end;

var ch: ansichar; pause: boolean = false;
begin
  ub4.term := uhw_vt.TB4KVMTerm.Create;
  opli := b4opc('li'); oplb := b4opc('lb');
  opjm := b4opc('jm');
  ophp := b4opc('hp'); oph0 := b4opc('h0');
  opcl := b4opc('cl'); opnx := b4opc('nx');
  open('disk.b4'); boot; clrscr;
  assign(input, '../bios/bios.b4a');
  reset(input); b4as;
  if rg[RGO]<>0 then rg[RIP] := rg[RGO]; { jump to address in @\ (or default=$100) }
  if paramstr(1)='-d' then rg[RDB] := 1;
  while rg[RIP] <= maxheap do begin
    if rg[RIP] = rg[RBP] then begin rg[RDB]:=1; rg[RBP] := high(ub4.address) end;
    if rg[RDB]=1 then begin
      if not pause then rg[RED] := rg[RIP];
      pause := true;
      dump;
      ch := readkey;
      case upcase(ch) of
        ^N : inc(rg[RED],8);
        ^P : if rg[RED] >= 8 then dec(rg[RED],8);
        ^F : inc(rg[RED]);
        ^B : dec(rg[RED]);
        ^V : inc(rg[RED], pgsz);
        'I': rg[RED] := rg[RIP];
        'S': pause := false;
        'C': begin pause := false; rg[RBP] := rg[RED] end;
        'O': begin pause := false;
               if step_over then begin rg[RDB]:=0; rg[RBP] := rg[RED] end
             end;
        'R': rg[RDB] := 0;
        'Q': halt;
        '0': rg[RED] := 0;
        'D': dump_dict;
      end;
      if rg[RED] < 0 then rg[RED] := maxcell;
      if rg[RED] > maxcell then rg[RED] := 0;
    end; { if ram[debug] }
    if not (pause and (rg[RDB]=1)) then step;
  end; { while }
  close(disk);
  {$IFDEF pauseafter} { for turbo pascal }
  writeln('done. press any key to continue');
  repeat until keypressed;
  {$ENDIF}
end.
