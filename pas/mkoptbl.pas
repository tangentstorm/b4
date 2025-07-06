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
 optbl : array[$80..$FF] of string[2];

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



function unhex(c : char) : byte;
  const hexits = '0123456789ABCDEF';
  var i : byte;
  begin
    unhex := 0;
    for i := 0 to 15 do if c = hexits[i+1] then unhex := i
  end;

procedure readops;
  var i, op : byte;
  begin
    fillchar(optbl, sizeof(optbl), 0);
    assign(input, ipath);
    reset(input);
    repeat readln(line) until match(line, 'procedure runop');
    repeat readln(line) until match(line, '    else case');
    repeat
      readln(line);
      { lines we want look like: }
      { '      $00 : (xx) ... ' }
      { #123456789012             }
      if line[7] = '$' then
        begin
          op := 16 * unhex(line[8]) + unhex(line[9]);
          for i := 1 to 2 do
            if line[13+i] <> ' ' then
              begin
                optbl[op][i] := line[13+i];
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
  // we make it string[3] just so we can test for trailing characters
  writeln(out, '  type opstring = string[3];');
  writeln(out, '  var optbl : array[ $80 .. $FF ] of opstring;');
  writeln(out, 'implementation');
  writeln(out, 'begin');
  for i := $80 to $FF do
    writeln(out, '  optbl[', i:2, '] := ''', optbl[ i ], ''';');
  writeln(out, 'end.');
  close(out);
end.
