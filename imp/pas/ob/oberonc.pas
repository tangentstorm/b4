{$mode objfpc}{$H+}
program oberonc;
uses ors, orb, org, orp;
begin
  if ParamCount < 1 then begin
    writeln('Usage: oberonc <source.ob>');
    halt(1);
  end;
  ors.Init(ParamStr(1));
  orp.Module;
  if ors.errcnt = 0 then
    writeln('  compilation successful')
  else begin
    writeln('  errors: ', ors.errcnt);
    halt(1);
  end;
end.
