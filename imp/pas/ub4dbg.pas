{$mode delphi}
unit ub4dbg; { visual debugger }
interface uses ub4;

  procedure debug_step;
  procedure draw_state;
  var paused : boolean = false;


implementation
uses crt, sysutils, ub4asm, uhw_vt, ub4ops;

var pgsz : dword = 16 * 10; { should be multiple of 8 to come out even }
    opli, oplb, opls, opjm, opcl, opnx, ophp, oph0 : value;
    ScreenMaxX, ScreenMaxY : dword;

procedure wv(fgc,bgc:char; k: string; v:value); { write value in 'special regs' line }
begin
  fg(fgc); bg(bgc); write(k); bg('k'); fg('w');write(': ');
  fg('w'); write(hexstr(v,4)); write(' ')
end;

procedure draw_stack(x,y : byte; id:char; var s:stack; n:value);
  var i : value;
begin
  goxy(x,y); bg('k'); clreol; fg('w'); write(id,'s:');  fg('y');
  for i := 0 to n-1 do write(' ', b4mat(s[i]));
  fg('c'); write(' <'); fg('C'); write(n); fg('c');
  write('>');
end;


function step_over: boolean;
  { what this does is fast-forward the editor pointer.
    result = true if stepping over a call }
  var skip : byte = 0;
begin
  result := mem[rg[ub4.RED]] = opcl;
  if mem[rg[ub4.RED]] in [opcl,opjm,opli,opnx] then skip := 4
  else if mem[rg[ub4.RED]] in [oplb,opls,ophp,oph0] then skip := 1;
  inc(rg[ub4.RED]);
  inc(rg[ub4.RED], skip);
end;


var oldx, oldy, oldattr: word;
procedure savecrt;
begin oldx := wherex; oldy := wherey; oldattr := getTextAttr;
end;

procedure loadcrt;
begin goxy(oldx,oldy); setTextAttr(oldattr);
end;

procedure fullwin;
begin
  Window(1,1,ScreenMaxX,ScreenMaxY);
end;
procedure debugwin;
begin
  Window(1,16,ScreenMaxX,(ScreenMaxY-16)-2);
  TextBackground(Black);
end;

procedure draw_state;
  { this displays the visual debugger }
  var i, pg, a: value; skip: byte=0;
      literal, target, ishop, found: boolean;
      id: ub4asm.ident;
begin
  id := 'call';

  { draw the data and return stacks }
  draw_stack(0, 0, 'd', ds^, rg[ub4.RDS]);
  draw_stack(0, 1, 'c', cs^, rg[ub4.RCS]);

  { draw some important registers }
  goxy(0, 2); bg('k'); clreol;
  wv('c', 'k', 'ip', rg[ub4.RIP]);
  wv('K', 'k', '@_', rg[ub4.RHP]);
  wv('r', 'k', 'ep', rg[ub4.RED]);

  { draw memory }
  goxy(0,3); pg := pgsz * (rg[ub4.RED] div pgsz);
  bg('b'); fg('W'); writeln('addr +0 +1 +2 +3 +4 +5 +6 +7 +8 +9 +A +B +C +D +E +F ');
  literal := false; target := false; ishop := false; { next cell is literal or jump target }
  i := pg;
  while i < pg + pgsz-1 do begin
    if (i mod 16 = 0) then begin { line headers }
      bg('k'); if (i>pg) then writeln; clreol;
      fg('k'); bg('w'); write(hexstr(i,4)); bg('k'); fg('K');
      if skip>0 then for a := 0 to skip-1 do write(' ~~');
    end;
    { skip one at a time so we always hit the newlines }
    if skip>0 then begin
      dec(skip); target := false; literal := false; inc(i);continue
    end;
    bg('k'); write(' ');
    { color cell based on ip / editor cursor positions }
    if (i=rg[ub4.RIP]) and (i=rg[ub4.RED]) then bg('m')
    else if i=rg[ub4.RIP] then begin fg('k'); bg('c') end
    else if i=rg[ub4.RED] then begin fg('k'); bg('r') end
    else begin fg('C'); bg('k') end;

    { literals but also first 32*4=128 bytes are the registers }
    if literal or (i < 128) then begin
      fg('y'); write(hexstr(mem[i],2)); literal := false end
    else if target then begin { target adress for jump/etc }
      if i = rg[ub4.RED] then fg('k') else fg('r');
      write(hexstr(mem[i],2)); target := false end
    else if ishop then begin { hop to next instruction }
      if i = rg[ub4.RED] then fg('k') else fg('r');
      write(b4mat(mem[i]):2); ishop := false end
    { past end of memory }
    else if i > high(mem) then begin fg('K'); write('xx') end
    { opcodes }
    else begin //if mem[i] in [$0, $80 .. $FF] then begin
      if mem[i] in [opli,oplb,opls] then literal := true;
      if mem[i] in [opjm,opcl] then target := true;
      if mem[i] in [ophp,oph0] then ishop := true;
      write(optbl[byte(mem[i])]);
      if (mem[i] in [opcl,opli,opjm]) then begin
        a := rdval(i+1);
        skip := 4; found := find_ident(rdval(i+1), id);
        if found then if literal then fg('Y') else fg('W')
        else begin id := hexstr(a,8) + '   '; fg('K') end;
        if length(id) > 11 then id := LeftStr(id, 11);
        Write(' ', format('%-11s',[id])); // 4*2 + 3 spaces
      end
    end;
    // { ascii characters }
    // else if (mem[i] >= 32) and (mem[i] < 128) then begin
    //   fg('g'); write(''''); write(chr(mem[i])) end
    // { anything else }
    // else begin fg('B'); write(hexstr(mem[i],2)) end;
    inc(i);
  end;
end; { draw_state }

procedure dump_dict;
  var i : byte;
begin
  for i := 0 to ub4asm.ents do begin
    write(dict[i].id:8, ': ', hexstr(dict[i].adr,4) );
    if (i>0) and (i mod 4 = 0) then writeln;
  end;
  repeat until keypressed;
end; { dump_dict }

procedure debug_step;
  var ch: ansichar;
begin
  if not paused then rg[ub4.RED] := rg[ub4.RIP];
  paused := true;
  savecrt; debugwin; draw_state;
    { help text }
  writeln;bg('k'); clreol; fg('w');
  write(' keys: (0)top, i(p), (s)tep (o)ver, (r)un, to (c)ursor, (q)uit');

  fullwin; loadcrt;
  ch := readkey;
  case upcase(ch) of
    ^N  : inc(rg[ub4.RED],8);
    ^P  : if rg[ub4.RED] >= 8 then dec(rg[ub4.RED],8);
    ^F  : inc(rg[ub4.RED]);
    ^B  : dec(rg[ub4.RED]);
    ^V  : inc(rg[ub4.RED], pgsz);
    // ^I : b4i_loop;
    'P' : rg[ub4.RED] := rg[ub4.RIP];
    'S' : paused := false;
    'C' : begin paused := false; rg[ub4.RBP] := rg[ub4.RED] end;
    'O' : begin paused := false;
           if step_over then begin rg[ub4.RDB]:=0; rg[ub4.RBP] := rg[ub4.RED] end
         end;
    'R' : rg[ub4.RDB] := 0;
    'Q' : halt;
    '0' : rg[ub4.RED] := 0;
    'D' : dump_dict;
  end;
  if rg[ub4.RED] < 0 then rg[ub4.RED] := maxheap;
  if rg[ub4.RED] > maxheap then rg[ub4.RED] := minheap;
end; { debug }

begin
  ScreenMaxX := WindMaxX; ScreenMaxY := WindMaxY;
  opli := b4opc('li'); oplb := b4opc('lb'); opls := b4opc('ls');
  opjm := b4opc('jm');
  ophp := b4opc('hp'); oph0 := b4opc('h0');
  opcl := b4opc('cl'); opnx := b4opc('nx');
end.