{ b4 debugger/interpreter }
{$mode objfpc}{$i xpc}
program b4i(input, output);
  uses sysutils, strutils, ub4, ub4asm, ub4ops;

// format as hex in b4 style
function b4mat(v : value):string;
begin
  if v < 0 then result := Format('-%x',[-v])
  else result := Format('%x',[v])
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

procedure ShowMem(addr :integer );
  var i: integer; v:byte; tok: string;
begin
  for i := 0 to 15 do begin
    v := ram[(addr + i)];
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

function ParseAddress(tok:string; out isHERE:boolean):integer;
begin
  tok := RightStr(tok, length(tok)-1); isHERE:=false;
  if length(tok)=1 then begin
    isHERE := true;
    if tok[1]<>':' then rg^[regn(tok[1])]:=rg^[RHP];
    result := rg^[RHP] end
  else result := hex2dec(tok)
end;

procedure PutMem(str:string);
  var a,i:integer; v:byte; tok: string; ishere:boolean;
begin
  i := -1;
  for tok in SplitString(str, ' ') do begin
    if i = -1 then a := ParseAddress(tok,isHere)
    else begin
      // TODO: handle integers instead of just bytes
      if not ub4asm.b4op(tok, v) then
        if tok = '..' then v := 0
        else if tok[1]='''' then v := byte(tok[2])
        else if tok[1]='-' then v := byte(-unhex(tok[2]))
        else v := unhex(tok);
      ram[a+i] := byte(v)
    end;
    inc(i);
  end;
  if isHere then rg^[RHP]:=a+i;
end;

type tstate = (st_cmt,st_asm,st_imm);
var str, tok : string; done: boolean = false; op:byte; st:tstate=st_imm; _b:boolean;
begin
  rg^[RIP] := $100; rg^[regn('_')] := $100;
  while not (done or eof) do begin
    readln(str); st:=st_imm;
    if str[1] = ':' then PutMem(str)
    else for tok in SplitString(str, ' ') do begin
      if tok[1] = '#' then st:=st_cmt;
      if (st=st_cmt) or (tok='') then continue;
      if ub4asm.b4op(tok, op) then begin
        if op > $20 then runop(op)
        else ub4.runa(4*op) // immediately invoke ^R
        end
      else case tok of
        '%C' : boot;
        '%q' : done := true;
        '%s' : ub4.step;
        '?d' : WriteStack('ds: ', ds, rg^[RDS]);
        '?c' : WriteStack('cs: ', cs, rg^[RCS]);
        '?i' : WriteLn('ip: ', b4mat(rg^[RIP]));
        else case tok[1] of
          '''' : if length(tok)=1 then dput(32) // space
                 else dput(ord(tok[2])); // char literals
          '`'  : if length(tok)=1 then err('invalid ctrl char')
                   // TODO test this against chars out of range
                 else dput(rega(tok[2]));
          '?'  : if length(tok)=2 then writeln(format('%.8x',[rg^[regn(tok[2])]]))
                 else ShowMem(parseAddress(tok, _b));
          else if PutHex(tok) then ok
          else Writeln('what does "', tok, '" mean?');
        end
      end
    end;
    if ub4.ob <> '' then begin writeln(ub4.ob); ub4.ob := '' end;
  end
end.
