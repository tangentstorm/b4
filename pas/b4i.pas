{$mode objfpc}
program b4i(input,output);
uses ub4i, ub4, ub4asm, sysutils;

var
  line: string;
  done: boolean = false;
  i: integer = 1;
begin
  rg^[RIP] := $100; rg^[regn('_')] := $100;

  while (i <= ParamCount) and (not done) do begin
    case ParamStr(i) of
      '-i': begin
        inc(i);
        if i <= ParamCount then ub4i.b4i_file(ParamStr(i))
        else writeln('error: -i requires a filename');
      end;
      '-a': begin
        inc(i);
        if i <= ParamCount then ub4asm.b4a_file(ParamStr(i))
        else writeln('error: -a requires a filename');
      end;
      '-q': done := true;
    end;
    inc(i);
  end;

  while not (done or eof) do begin
    readln(line);
    done := ub4i.b4i(line);
  end;
end.
