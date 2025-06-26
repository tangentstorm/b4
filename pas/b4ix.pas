{$mode objfpc}

// b4i interactive assembler - extended edition
// this TUI version shows a continuously-updated
// view of the internal state and provides a dedicated
// area to display the 64/16 b4 terminal.

program b4ix(input,output);
uses ub4i, ub4, crt, sysutils;

var
  ScreenMaxX, ScreenMaxY : dword;
  StateHeight            : byte = 13;
  TermHeight             : byte = 16;

procedure DrawHeadings;
begin
  GotoXY(1,1); TextAttr := $70;
  Write('B4 Terminal'); ClrEol;
  GotoXY(1,TermHeight+1); TextAttr := $70;
  Write('Interactive Assembler'); ClrEol;
  GotoXY(1,ScreenMaxY-StateHeight-1); TextAttr := $70;
  Write('Internal State'); ClrEol;
end;

procedure TermWindow;
begin
  Window(1,2,64,TermHeight);
end;

procedure InitTerm;
begin
  Window(1,2,ScreenMaxX,TermHeight);
  TextBackground(Blue); ClrScr;
  TermWindow; TextAttr := $07; ClrScr;
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

procedure ShowState;
  var oldX, oldY : dword; i, a : word;
begin
  OldX := WhereX;
  OldY := WhereY;
  StateWindow;
  TextColor(Black);
  WriteStack('ds: ', ds, rg^[RDS]);
  WriteStack('cs: ', cs, rg^[RCS]);
  TextBackground(Blue);
  TextAttr := $70;
  Write('addr +0 +1 +2 +3 +4 +5 +6 +7 +8 +9 +A +B +C +D +E +F ');
  WriteLn;
  for i := 0 to 9 do begin
    a := $100 + i * $10;
    TextAttr := $70; Write(HexStr(a,4));
    TextAttr := $07; Write(' '); ShowMem(a);
  end;
  MainWindow;
  GoToXY(OldX,OldY);
end;

procedure DrawPrompt;
begin
  TextColor(Cyan); Write('b4i');
  TextColor(LightBlue); Write('> ');
  TextColor(White);
end;

var prompt,line: string; done:boolean=false;
begin
  ScreenMaxX := WindMaxX; ScreenMaxY := WindMaxY;
  ClrScr; DrawHeadings; InitTerm;
  StateWindow; ClrScr;
  MainWindow; ClrScr;
  GoToXY(1, WindMaxY); TextColor(DarkGray);
  writeln('b4ix [',{$i %date%},'] ',
          'type \h for help, \q to quit');
  rg^[RIP] := $100; rg^[regn('_')] := $100;
  ShowState; DrawPrompt;
  repeat
    ReadLn(line);
    done := ub4i.b4i(line);
    if not done then begin
      ShowState; DrawPrompt;
    end
  until done or eof;
  // clean up screen at end:
  Window(1,1,ScreenMaxX,ScreenMaxY);
  GotoXY(1,ScreenMaxY); TextAttr := $07; WriteLn;
end.
