{$mode objfpc}

// b4i interactive assembler - extended edition
// this TUI version shows a continuously-updated
// view of the internal state and provides a dedicated
// area to display the 64/16 b4 terminal.

program b4ix(input,output);
uses ub4i, ub4, crt, sysutils, uled;

var
  ScreenMaxX, ScreenMaxY : dword;
  StateHeight            : byte = 13;
  TermHeight             : byte = 17;
  StartView              : dword;

procedure DrawHeadings;
begin
  GotoXY(1,1); TextAttr := $70;
  Write('B4 Terminal'); ClrEol;
  GotoXY(1,TermHeight+1); TextAttr := $70; TextBackground(Yellow);
  Write('Interactive Assembler'); ClrEol;
  GotoXY(1,ScreenMaxY-StateHeight-1); TextAttr := $70;
  Write('Machine State'); ClrEol;
end;

procedure TermWindow;
begin
  Window(1,2,64,TermHeight+1);
end;

procedure FullScreen;
begin
  Window(1,1,ScreenMaxX,ScreenMaxY);
end;

procedure InitTerm;
begin
  Window(1,2,ScreenMaxX,TermHeight);
  TextBackground(Blue); ClrScr;
  TermWindow; TextAttr := $07; ClrScr;
end;

const TermAddr : ub4.address = $0100;
const AttrAddr : ub4.address = $0500;
const hexit = '0123456789ABCDEF';
procedure DrawTerm;
  var x,y: word; a:byte; c:char;
begin
  TermWindow; TextAttr := $07;
  for y := 0 to 15 do begin
    GotoXY(1,y+1);
    for x := 0 to 63 do begin
      a := mem[AttrAddr + y*64+x];
      textColor(lo(a)); textBackground(hi(a));
      c := char(mem[TermAddr + y*64+x]);
      if c < ' ' then c := ' ';
      write(c);
    end;
  end;
end;

procedure MainWindow;
begin
  Window(1,TermHeight+2,ScreenMaxX,(ScreenMaxY-StateHeight)-2);
  TextBackground(Black);
end;

procedure StateWindow;
begin
  Window(1,ScreenMaxY-StateHeight,
         ScreenMaxX, ScreenMaxY);
  TextBackground(Blue);
end;

var oldX, oldY : dword;
procedure StoreXY;
begin
  OldX := WhereX;
  OldY := WhereY;
end;

procedure LoadXY;
begin
  GotoXY(OldX,OldY);
end;

procedure ShowState;
  var i, a : word;
begin
  StateWindow;
  TextColor(White);
  WriteStack('ds: ', ds, rg^[RDS]); ClrEol; WriteLn;
  WriteStack('cs: ', cs, rg^[RCS]); ClrEol; WriteLn;
  TextBackground(Blue);
  TextAttr := $70;
  Write('addr +0 +1 +2 +3 +4 +5 +6 +7 +8 +9 +A +B +C +D +E +F ');
  WriteLn;
  for i := 0 to 9 do begin
    a := StartView + i * $10;
    TextAttr := $70; Write(HexStr(a,4));
    TextAttr := $07; Write(' '); ShowMem(a);
  end;
end;

procedure DrawPrompt;
begin
  TextColor(Cyan); Write('b4i');
  TextColor(LightBlue); Write('> ');
  TextAttr := $07;
end;

procedure redraw;
begin
  StoreXY; DrawTerm; ShowState;
  FullScreen; DrawHeadings;
  MainWindow; GoToXY(OldX,OldY); DrawPrompt;
end;

var line: string; done:boolean=false; histPath:string;
const START = $2000;
begin
  rg^[RIP] := START; rg^[regn('_')] := START; StartView := START;
  histPath := GetUserDir + '/' + '.b4ix'; uled.loadHist(histPath);
  ScreenMaxX := WindMaxX; ScreenMaxY := WindMaxY;
  ClrScr; DrawHeadings; InitTerm;
  StateWindow; ClrScr;
  MainWindow; ClrScr; GoToXY(1, WindMaxY); TextColor(DarkGray);
  writeln('b4ix [',{$i %date%},'] ',
          'type \h for help, \q to quit');
  if not b4i_args then
    repeat
      redraw;
      uled.input(line); writeln;
      uled.saveHist(histPath);
    until ub4i.b4i(line);
  // clean up screen at end:
  Window(1,1,ScreenMaxX,ScreenMaxY);
  GotoXY(1,ScreenMaxY); TextAttr := $07; WriteLn;
end.
