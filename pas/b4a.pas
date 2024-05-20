program b4a; { b4 assembler }
uses ub4, ub4asm, sysutils {leftstr};

type
  tfmt = (json, b4x);
var
  i : ub4.value;

procedure ParseParams(var ifn, ofn: string; var fmt: tfmt);
  var a : byte = 0;
  begin
    fmt := b4x;
    for i := 1 to paramcount do begin
      if paramstr(i) = '-j' then fmt := json
      else case a of
        0: begin ifn := paramstr(i); inc(a) end;
        1: begin ofn := paramstr(i); inc(a) end;
        otherwise begin writeln('too many arguments'); halt end
      end
    end;
    if a = 0 then begin writeln('usage: b4a [-j] infile.b4a [outfile]'); halt end
    else if a = 1 then begin
      a := lastdelimiter('.', ifn);
      if a > 0 then ofn := leftstr(ifn, a) else ofn := ifn + '.';
      if fmt = json then ofn += 'json' else ofn += 'b4x';
    end
  end;

procedure emit_b4x(ofn: string);
  var out : file of byte;
  begin assign(out, ofn); rewrite(out);
    for i := 0 to ub4.maxcell do write(out, ub4.ram[i]);
    close(out)
  end;

procedure emit_json(ofn: string);
  var out : text;
  begin assign(out, ofn); rewrite(out); i:= 0;
    write(out, '[');
    while i < ub4.maxcell do begin
      if i mod 10 = 0 then writeln(out);
      write(out, ' ', ub4.ram[i] : 3);
      inc(i); if i < ub4.maxcell then write(out, ',');
    end;
    writeln(out, ']');
    close(out)
  end;

var ifn, ofn : string; fmt: tfmt;
begin
  parseParams(ifn, ofn, fmt);

  assign(input, ifn); reset(input);
  boot; ub4asm.b4as;
  close(input);

  if fmt = b4x then emit_b4x(ofn)
  else emit_json(ofn)
end.
