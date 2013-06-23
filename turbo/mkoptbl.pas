program mkoptbl;

{
  this reads the source of the vm
  and generates an optable unit.

  kind of a lot of work to avoid
  copying and pasting. :)
}

const
  ipath = 'ub4.pas';
  opath = 'ub4ops.pas';

var
 line  : string;
 optbl : array[0..63] of string[4];

function match(have, want:string) : boolean;
  var i : byte; sofar : boolean;
  begin
    i := 1;
    sofar := length(want) <= length(have);
    while sofar and (i < length(want)) do
      begin
        sofar := have[i] = want[i];
        inc(i);
      end;
    match := sofar;
  end;

procedure readops;
  var i, op : byte;
  begin
    fillchar(optbl[0], sizeof(optbl), 0);
    assign(input, ipath);
    reset(input);
    repeat readln(line) until match(line, 'function step');
    repeat readln(line) until match(line, '    case');
    repeat
      readln(line);
      { lines we want look like: }
      { '      00 : (code) ... '  }
      { #123456789012             }
      if line[7] in ['0'..'9'] then
        begin
          op := 10 * (ord(line[7]) - ord('0'))
                   + (ord(line[8]) - ord('0'));
          for i := 1 to 4 do
            if line[12+i] <> ' ' then
              begin
                optbl[op][i] := line[12+i];
                inc(optbl[op][0]); { length byte }
              end
        end
    until match(line, '  end');
end;

var i : byte; out : text;
begin
  readops;
  assign(out, opath);
  rewrite(out);
  writeln(out, '{-- do not edit! regenerate with mkoptbl.pas --}');
  writeln(out, 'unit ub4ops;');
  writeln(out, 'interface');
  writeln(out, '  type opstring = string[4];');
  writeln(out, '  var optbl : array[ 0 .. 63 ] of opstring;');
  writeln(out, 'implementation');
  writeln(out, 'begin');
  for i := 0 to 63 do
    writeln(out, '  optbl[', i:2, '] := ''', optbl[ i ], ''';');
  writeln(out, 'end.');
  close(out);
end.
