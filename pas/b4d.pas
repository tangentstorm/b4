{$mode delphi}{$i xpc}
program b4d; { b4 dis-assembler }
uses ub4, ub4asm, ub4ops, sysutils;

procedure ParseParams(var ifn: string);
  begin
    if paramcount = 0 then begin writeln('usage: b4d infile.b4x'); halt end
    else ifn := paramstr(1)
  end;

function dis(b:byte; hexonly:boolean):string;
  var tok: string;
begin
  if hexonly then tok := format('%.2x', [b])
  else case b of
    0 : tok := '..';
    $01..$1F : tok := '^' + chr(ord('@')+b);
    $20..$3F : tok := '@' + chr(ord('@')+b-$20);
    $40..$5F : tok := '!' + chr(ord('@')+b-$40);
    $60..$7F : tok := '+' + chr(ord('@')+b-$60);
    $80..$FF : begin tok := optbl[b]; if length(tok)=0 then tok:=format('%02x', [b]) end
    else tok := format('%.2x', [b])
  end;
  dis := tok;
end;

var ifn : string; b4x:file of byte; b: byte; i:ub4.value; ascii:string=' # ';
begin
  ParseParams(ifn);
  assign(b4x, ifn); reset(b4x);
  for i := 0 to ub4.maxcell do begin
    if (i>0) then
      if i mod 16 = 0 then begin writeln(ascii); ascii := ' # '; end
      else if i mod 4 = 0 then write(' ');
    if i mod 256 = 0 then begin
      if i > 0 then writeln;
      writeln(format(':%.4x',[i]))
    end;
    read(b4x,b);
    if (b > 31) and (b<127) then ascii+=chr(b) else ascii+='.';
    write(dis(b, i<64*4),' ');
  end;
  if i mod 16 <> 0 then writeln(ascii);
  close(b4x);
end.
