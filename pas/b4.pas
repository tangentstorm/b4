{$mode delphi}
{ this is the main entry point for b4 }
program b4;
uses ub4, ub4asm, ub4ops, crt, uhw_vt, ub4i,
 sysutils; // for format

const pgsz = 16 * 8; { should be multiple of 8 to come out even }

var opli, oplb, opjm, opcl, opnx, ophp, oph0 : value;

procedure draw_stack(x,y : byte; id:char; var s:stack; n:value);
  var i : value;
  begin
    gotoxy(x,y); bg('k'); clreol; fg('w'); write(id,':');
    fg('Y'); write(' <'); fg('y'); write(n); fg('Y');
    write('> '); fg('y');
    for i := 0 to n-1 do write(hexstr(s[i],1),' ');
  end;

procedure wv(k: string; v:value); { write value }
  begin fg('w'); write(k); fg('k'); write(': ');
    fg('W'); write(hexstr(v,4)); write(' ')
  end;


procedure draw_help(x,y : byte);
  begin gotoxy(x,y); write('keys: (0)top, i(p), (s)tep (o)ver, (r)un, to (c)ursor, (q)uit, b4(i)')
  end;

procedure dump_dict;
  var i : byte;
  begin
    for i := 0 to ub4asm.ents do begin
      write(dict[i].id:8, ': ', hexstr(dict[i].adr,4) );
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

    { draw memory }
    gotoxy(0,16); pg := pgsz * (rg[RED] div pgsz);
    literal := false; target := false; { next cell is literal or jump target }
    for i := pg to pg + pgsz-1 do begin
      if (i mod 16 = 0) then begin
        bg('k'); if (i>pg) then writeln; clreol;
        fg('m'); write(hexstr(i,4));
      end;
      { color cell based on ip / editor cursor positions }
      if (i=rg[RIP]) and (i=rg[RED]) then bg('m')
      else if i=rg[RIP] then bg('c')
      else if i=rg[RED] then bg('r')
      else bg('k');
      { literal numbers (after si/li) }
      if skip>0 then begin dec(skip); target := false; literal := false end
      else if literal or (i < 32) then begin fg('y'); write(hexstr(mem[i],2):4); literal := false end
      else if target then { target adress for jump/etc }
          begin if i = rg[RED] then fg('k') else fg('r');
                write(hexstr(mem[i],2):4); target := false
          end
      { past end of memory }
      else if i > high(mem) then begin fg('K'); write('xx') end
      { opcodes }
      else if mem[i] in [$80 .. $BF] then
        begin
          if mem[i] in [opli,oplb] then literal := true;
          if mem[i] in [opjm,opcl] then target := true;
          if (mem[i] = opli) or ((mem[i] in [opcl]) and (rdval(i+1) < high(address))) then
            begin
              if mem[i] = opcl
                then begin skip := 4; find_ident(rdval(i+1), id) end
                else begin skip := 1; find_ident(mem[i+1], id) end;
              write('  ');
              if mem[i] = opcl then fg('W') else begin fg('K'); write('$') end;
              write(format('%-6s',[id]))
            end
          else begin fg('C'); write(optbl[byte(mem[i])] :4) end;
        end
      { ascii characters }
      else if (mem[i] >= 32) and (mem[i] < 128) then
        begin fg('g'); write('  '''); write(chr(mem[i])) end
      { anything else }
      else begin fg('b'); write(hexstr(mem[i],2):4) end
    end;
    { for ui debugging, draw line numbers on the right: }
    bg('K'); fg('k'); for i := 0 to 24 do begin gotoxy(xMax-1,i); write(i:2) end;
    { help text }
    draw_help(0, 24); clreol;
    { restore cursor position and color so we don't break the vm }
    gotoxy(x,y); textattr := oldattr;
  end;


function step_over: boolean;
  { what this does is fast-forward the editor pointer.
    result = true if stepping over a call }
  var skip : byte = 0;
  begin
    result := mem[rg[RED]] = opcl;
    if mem[rg[RED]] in [opcl,opjm,opli,opnx] then skip := 4
    else if mem[rg[RED]] in [oplb,ophp,oph0] then skip := 1;
    inc(rg[RED]);
    inc(rg[RED], skip);
  end;

function readstr:string;
  // after reassigning input in the main program,
  // readln no longer echos. weird, but this is
  // a temporary ui anyway until i migrate over
  // to kvm.pas in the xpl repo.
  var c:char=^@;
  begin
    result := '';
    while true do begin
      c:=readkey;
      if c in [^J,^M] then exit
      else if c = ^H then begin
        if length(result)>0 then begin
          result:=leftstr(result,length(result)-1);
          gotoxy(wherex-1,wherey); write(' ');
          gotoxy(wherex-1,wherey);
        end end
      else begin
        result += c;
        write(c)
      end
    end
  end;
procedure b4i_loop;
  var line:string; done:boolean=false;
  begin
    clrscr;
    repeat
      textattr := $0A;
      write('b4i> ');
      textattr := $07;
      line:=readstr;
      writeln;
      done := (line='') or ub4i.b4i(line);
    until done;
  end;

var ch: ansichar; pause: boolean = false; oldinput : text;
begin
  ub4.term := uhw_vt.TB4KVMTerm.Create;
  opli := b4opc('li'); oplb := b4opc('lb');
  opjm := b4opc('jm');
  ophp := b4opc('hp'); oph0 := b4opc('h0');
  opcl := b4opc('cl'); opnx := b4opc('nx');
  open('disk.b4'); boot; clrscr;
  oldinput := input;
  assign(input, '../bios/bios.b4a');
  reset(input); b4as;
  input := oldinput; reset(input);
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
        ^I : b4i_loop;
        'P': rg[RED] := rg[RIP];
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
    end; { if mem[debug] }
    if not (pause and (rg[RDB]=1)) then
      try step
      except on e:EB4Exception do begin
        rg[RDB]:=1; pause := true end end;
  end; { while }
  close(disk);
  {$IFDEF pauseafter} { for turbo pascal }
  writeln('done. press any key to continue');
  repeat until keypressed;
  {$ENDIF}
end.
