{$mode objfpc}
program b4i(input,output);
uses ub4i, ub4;

var line: string; done:boolean=false;
begin
  rg^[RIP] := $100; rg^[regn('_')] := $100;
  repeat
    readln(line);
    done := ub4i.b4i(line);
  until done or eof;
end.
