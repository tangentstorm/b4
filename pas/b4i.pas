{ b4 debugger/interpreter }
program b4i(input, output);
  uses strutils;

var str, tok: string; done: boolean = false;
begin
  while not (done or eof) do begin
    readln(str);
    for tok in SplitString(str, ' ') do begin
      case tok of
        '%q' : done := true;
        '?d' : WriteLn('ds: []');
        '?c' : WriteLn('cs: []');
      end
    end;
  end
end.