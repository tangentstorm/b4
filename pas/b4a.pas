program b4a; { b4 assembler }
uses ub4, ub4asm, sysutils, classes {leftstr};

type
  tfmt = (json, b4x);
var
  i : ub4.value;

procedure ParseParams(var inputs: TStringList; var ofn: string; var fmt: tfmt);
  var
    a : byte = 0;
    idx: integer;
    arg: string;
    have_output: boolean = false;
  begin
    fmt := b4x;
    inputs.Clear;
    idx := 1;
    while idx <= paramcount do begin
      arg := paramstr(idx);
      if arg = '-j' then fmt := json
      else if arg = '-o' then begin
        inc(idx);
        if idx > paramcount then begin writeln('-o requires a filename'); halt end;
        ofn := paramstr(idx);
        have_output := true;
      end
      else if (arg = '-a') or (arg = '-i') then begin
        inc(idx);
        if idx > paramcount then begin writeln(arg, ' requires a filename'); halt end;
        inputs.Add(paramstr(idx));
      end
      else case a of
        0: begin inputs.Add(arg); inc(a) end;
        1: if have_output then begin writeln('too many arguments'); halt end
           else begin ofn := arg; have_output := true; inc(a) end;
        otherwise begin writeln('too many arguments'); halt end
      end;
      inc(idx);
    end;
    if inputs.Count = 0 then begin
      writeln('usage: b4a [-j] [-o outfile] [-a file] [-i file] [infile.b4a] [outfile]');
      halt
    end
    else if not have_output then begin
      a := lastdelimiter('.', inputs[0]);
      if a > 0 then ofn := leftstr(inputs[0], a) else ofn := inputs[0] + '.';
      if fmt = json then ofn += 'json' else ofn += 'b4x';
    end
  end;

procedure emit_b4x(ofn: string);
  var out : file of byte;
  begin assign(out, ofn); rewrite(out);
    for i := 0 to ub4.maxcell do write(out, ub4.mem[i]);
    close(out)
  end;

procedure emit_json(ofn: string);
  var out : text;
  begin assign(out, ofn); rewrite(out); i:= 0;
    write(out, '[');
    while i < ub4.maxcell do begin
      if i mod 10 = 0 then writeln(out);
      write(out, ' ', ub4.mem[i] : 3);
      inc(i); if i < ub4.maxcell then write(out, ',');
    end;
    writeln(out, ']');
    close(out)
  end;

var
  ofn : string;
  fmt: tfmt;
  inputs: TStringList;
begin
  inputs := TStringList.Create;
  try
    parseParams(inputs, ofn, fmt);
    boot;
    for i := 0 to inputs.Count - 1 do ub4asm.b4a_file(inputs[i]);

    if fmt = b4x then emit_b4x(ofn)
    else emit_json(ofn)
  finally
    inputs.Free;
  end
end.
