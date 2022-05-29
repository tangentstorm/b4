program b4a; { b4 assembler }
uses ub4, ub4asm;

var
  ifn, ofn : string;
  out : file of ub4.value;
  i : ub4.value;
begin
  if paramstr(1) = '' then
    begin writeln('usage: b4a input.b4a [output.b4]'); halt end
  else ifn := paramstr(1);

  assign(input, ifn);
  reset(input); ub4asm.b4as;
  close(input);

  ofn := 'b4a.out';
  assign(out, ofn);
  rewrite(out);
  for i := 0 to ub4.maxcell do write(out, ub4.ram[i]);
  close(out);
end.
