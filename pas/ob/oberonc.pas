{$mode objfpc}{$H+}
program oberonc;
{ Oberon-07 compiler for b4vm }
uses ors, orb, org, orp;
begin
  if ParamCount < 1 then begin
    writeln('Oberon-07 compiler for b4vm');
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
