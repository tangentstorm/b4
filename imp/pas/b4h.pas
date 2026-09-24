{$mode objfpc}
program b4h(input,output);
uses ub4i, ub4, sysutils;

var
  line: string;
  done: boolean = false;
begin
  ub4i.b4h_mode := true;
  rg^[RIP] := $100; rg^[regn('_')] := $100;
  done := b4i_args;
  while not (done or eof) do begin
    readln(line);
    done := ub4i.b4i(line)
  end
end.
