{$mode objfpc}
program b4i(input,output);
uses ub4i, ub4, crt, sysutils;

var
  ScreenMaxX, ScreenMaxY : dword;
  StateHeight            : byte = 13;

procedure MainWindow;
begin
  Window(1,1,ScreenMaxX,(ScreenMaxY-StateHeight)-1);
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
  GotoXY(1,1);
  TextColor(Black);
  WriteStack('ds: ', ds, rg^[RDS]);
  WriteStack('cs: ', cs, rg^[RCS]);
  TextAttr := $70;
  Write('addr +0 +1 +2 +3 +4 +5 +6 +7 +8 +9 +A +B +C +D +E +F ');
  WriteLn;
  for i := 0 to 9 do begin
    a := $100 + i * $10;
    TextAttr := $70; Write(HexStr(a,4), ' ');
    TextAttr := $07; ShowMem(a);
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
  ScreenMaxX := WindMaxX;
  ScreenMaxY := WindMaxY;
  StateWindow; ClrScr;
  MainWindow; ClrScr;
  GoToXY(1, WindMaxY);
  TextColor(DarkGray);
  writeln('b4i.pas [',{$i %date%},'] ',
          'type \h for help, \q to quit.');
  rg^[RIP] := $100; rg^[regn('_')] := $100;
  ShowState; DrawPrompt;
  repeat
    ReadLn(line);
    done := ub4i.b4i(line);
    if not done then begin
      ShowState; DrawPrompt;
    end
  until done or eof;
end.
