{$mode objfpc}
program b4i(input,output);
uses ub4i, ub4;

var prompt,line: string; done:boolean=false;
begin
  if paramstr(1) = '-q' then prompt:=''
  else begin
    writeln('b4i.pas [',{$i %date%},'] ',
            'type \h for help, \q to quit.');
    prompt:='b4i> ';
  end;
  rg^[RIP] := $100; rg^[regn('_')] := $100;
  write(prompt);
  repeat
    readln(line);
    done := ub4i.b4i(line);
    if not done then write(prompt);
  until done or eof;
end.
