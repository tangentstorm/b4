{ b4 debugger/interpreter }
program b4i(input, output);
  uses sysutils, strutils, ub4, ub4asm;

// format as hex in b4 style
function b4mat(v : value):string;
begin
  if v < 0 then result := Format('$-%x',[-v])
  else result := Format('$%x',[v])
end;

type pstack = ^stack;
procedure WriteStack(pre : string; s:pstack; count:integer);
  var v: ub4.value; i:integer=0;
begin
  Write(pre);
  Write('[');
  if count > 0 then for i := 1 to count do begin
    if i>1 then Write(' ');
    v := s^[i];
    Write(b4mat(v));
  end;
  WriteLn(']');
end;

function PutHex(tok : string): boolean;
begin
  try dput(Hex2Dec(tok)); result:= true;
  except on EConvertError do result := false; end
end;

procedure ok; begin end;

procedure err(s : string);
begin
  writeln(s);
  halt;
end;

var str, tok : string; done: boolean = false; op:byte;
begin
  reg_ip^ := $100;
  while not (done or eof) do begin
    readln(str);
    for tok in SplitString(str, ' ') do begin
      if tok = '' then continue;
      if ub4asm.b4op(tok, op) then runop(op)
      else case tok of
        '%q' : done := true;
        '?d' : WriteStack('ds: ', ds, reg_dp^);
        '?c' : WriteStack('cs: ', rs, reg_rp^);
        '?i' : WriteLn('ip: ', b4mat(reg_ip^));
        else case tok[1] of
          '''' : if length(tok)=1 then dput(32) // space
                 else dput(ord(tok[2])); // char literals
          '`'  : if length(tok)=1 then err('invalid ctrl char')
                   // TODO test this against chars out of range
                 else dput(4 * (ord(upcase(tok[2])) - ord('@')))
          else if PutHex(tok) then ok
          else Writeln('what does "', tok, '" mean?');
        end
      end
    end;
  end
end.