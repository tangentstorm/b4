{ b4 debugger/interpreter }
{$mode objfpc}{$i xpc}
unit ub4i;
interface uses sysutils, strutils, ub4, ub4asm, ub4ops;

  procedure ShowMem(addr:integer);
  function b4i(line:string):boolean; { returns 'done' flag }

implementation

procedure ShowMem(addr :integer );
  var i: integer; v:byte; tok: string;
begin
  for i := 0 to 15 do begin
    v := mem[(addr + i)];
    case v of
      0 : tok := '..';
      $01..$1F : tok := '^' + chr(ord('@')+v);
      $20..$3F : tok := '@' + chr(ord('@')+v-$20);
      $40..$5F : tok := '!' + chr(ord('@')+v-$40);
      $60..$7F : tok := '+' + chr(ord('@')+v-$60);
      $80..$FF : begin tok := optbl[v]; if tok='' then tok:=format('%02x', [v]) end
      else tok := format('%02x', [v]);
    end;
    write(tok,' ')
  end;
  writeln;
end;

function unhex(tok:string): integer;
begin
  if length(tok) = 0 then raise EConvertError.Create('unhex of empty string?');
  if tok[1]='-' then result := -Hex2Dec(copy(tok,2,length(tok)-1))
  else result := Hex2Dec(tok);
end;

function PutHex(tok : string): boolean;
begin
  try dput(unhex(tok)); result := true
  except on EConvertError do result := false; end
end;

procedure ok; begin end;

procedure err(s : string);
begin
  writeln(s);
  halt;
end;

function ParseAddress(tok:string; out isHERE:boolean; out a:address):boolean;
begin
  tok := RightStr(tok, length(tok)-1); isHERE:=false; a:=0; result:=true;
  if length(tok)=1 then begin
    isHERE := true;
    if tok[1]<>':' then rg^[regn(tok[1])]:=rg^[RHP];
    a := rg^[RHP] end
  else try a := hex2dec(tok)
  except on EConvertError do result:=false end
end;

procedure PutMem(str:string);
  var a:address; i:integer; v:byte; tok: string; ishere:boolean;
begin
  i := -1;
  for tok in SplitString(str, ' ') do begin
    if i = -1 then
      if ParseAddress(tok,isHere,a) then ok
      else writeln(format('bad: putmem(%s)', [str]))
    else begin
      // TODO: handle integers instead of just bytes
      if not ub4asm.b4op(tok, v) then
        if tok = '..' then v := 0
        else if tok[1]='''' then v := byte(tok[2])
        else if tok[1]='-' then v := byte(-unhex(tok[2]))
        else v := unhex(tok);
      mem[a+i] := byte(v)
    end;
    inc(i);
  end;
  if isHere then rg^[RHP]:=a+i;
end;

procedure ShowRegs;
var r:char; i:byte=0;
begin
  for r in '@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_' do begin
    Write('  ',r,': ',format('%.8x', [rg^[regn(r)]]));
    inc(i); if 0=i mod 4 then writeln;
  end
end;


procedure help;
begin
  writeln('-- b4i: interactive interpreter for b4 virtual machine');
  writeln;
  writeln('enter b4a assembly language tokens to execute immediately');
  writeln;
  writeln('use :$hex-addr (ex: ":100") to assemble rest of line');
  writeln('use :$reg-name (ex: ":E") to assemble line and assign register');
  writeln;
  writeln('example:');
  writeln;
  writeln('  :E ''e io rt                       # emit character');
  writeln('  :W !R vb +R rv .f +R rv ^E .n rt  # write a counted string');
  writeln('  :S 05 ''h ''e ''l ''l ''o              # counted string');
  writeln('  @S ^W                             # (W)rite the (S)tring');
  writeln;
  writeln('for help, see https://github.com/tangentstorm/b4/tree/main/doc');
  writeln;
end;

function b4i(line:string):boolean;
  type tstate = (st_cmt,st_asm,st_imm);
  var tok : string; done: boolean = false; op:byte; st:tstate=st_imm;
  _b:boolean; a:address;
begin
  st:=st_imm;
  if length(line)=0 then exit(false);
  if line[1] = ':' then PutMem(line)
  else for tok in SplitString(line, ' ') do begin
    if length(tok)=0 then continue;
    if tok[1] = '#' then st:=st_cmt;
    if (st=st_cmt) or (tok='') then continue;
    if ub4asm.b4op(tok, op) then begin
      if op > $20 then runop(op)
      else ub4.runa(4*op) end // immediately invoke ^R
    else case tok of
      '%C' : boot;
      '%q' : done := true;
      '%s' : ub4.step;
      '%h' : help;
      '?d' : WriteStack('ds: ', ds, rg^[RDS]);
      '?c' : WriteStack('cs: ', cs, rg^[RCS]);
      '?i' : WriteLn('ip: ', b4mat(rg^[RIP]));
      '?r' : ShowRegs;
    else case tok[1] of
      '''' : if length(tok)=1 then dput(32) // space
             else dput(ord(tok[2])); // char literals
      '`'  : if length(tok)=1 then err('invalid ctrl char')
               // TODO test this against chars out of range
             else dput(rega(tok[2]));
      '?'  : if length(tok)=2 then writeln(format('%.8x',[rg^[regn(tok[2])]]))
             else if parseAddress(tok,_b,a) then ShowMem(a)
             else writeln('bad address:', tok);
    else if PutHex(tok) then ok
    else Writeln('what does "', tok, '" mean?') end;
    end
  end;
  if ub4.ob <> '' then begin writeln(ub4.ob); ub4.ob := '' end;
  result := done;
end;

end.
