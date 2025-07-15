{ b4 debugger/interpreter }
{$mode objfpc}
unit ub4i;
interface uses sysutils, strutils, ub4, ub4asm, ub4ops;

  procedure ShowMem(addr:integer);
  procedure ShowOpcodes;
  procedure b4i_file(path:string);
  function b4i(line:string):boolean; { returns 'done' flag }
  function b4i_args:boolean; { returns true if should quit }

var
  logging_enabled: boolean = false;

implementation

procedure LogState(op_str: string);
  var a:integer; ident: string;
begin
  if logging_enabled then begin
    a:=ub4.rg^[ub4.RIP];
    Write(format('| IP: %s ', [ub4.b4mat(a)]));
    if ub4asm.find_ident(a, ident) then write('{ ', ident, ' } ');
    Write('OP: ',op_str);
    WriteStack('  ds: ', ub4.ds, ub4.rg^[ub4.RDS]);
    WriteStack('  cs: ', ub4.cs, ub4.rg^[ub4.RCS]);
    WriteLn;
  end;
end;

procedure wrapOp(op: byte);
begin
  LogState(ub4ops.optbl[op]);
  ub4.runop(op);
end;

function step_wrapped : value;
begin
  try wrapOp(ub4.mem[ub4.rg^[ub4.RIP]]);
  except on e: EAccessViolation do begin
    writeln('IP:', hexstr(ub4.rg^[ub4.RIP], 8), ' ',
            'is outside ram. (max: ', hexstr(high(ub4.mem), 8), ')');
    halt end;
  end;
  inc(ub4.rg^[ub4.RIP]);
  step_wrapped := ub4.rg^[ub4.RIP];
end;

procedure run_wrapped;
begin
  ub4.rg^[ub4.RST]:=1; while ub4.rg^[ub4.RST]=1 do step_wrapped
end;

procedure wrapCall(addr: ub4.address);
begin
  ub4.cput(ub4.rg^[ub4.RIP]);
  ub4.cput(0);
  ub4.go(addr+1);
  run_wrapped;
  ub4.rg^[ub4.RIP]:=ub4.cpop;
end;

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

function PutHex(tok : string): boolean;
begin
  try dput(ub4asm.unhex(tok)); result := true
  except on EConvertError do result := false; end
end;

procedure ok; begin end;

procedure err(s : string);
begin
  writeln(s);
  halt;
end;

procedure reset_vm;
begin
  rg^[RCS] := 0;
  rg^[RDS] := 0;
  rg^[RIP] := rg^[RHP];
end;

procedure PutMem(str:string);
  var r: byte; tok, w_tok : string; a : integer;
      toks : TStringArray;
      cmt_pos: integer;
begin
  cmt_pos := Pos('#', str);
  if cmt_pos > 0 then str := copy(str, 1, cmt_pos - 1);
  toks := SplitString(trim(str), ' ');
  for tok in toks do begin
    if length(tok) = 0 then continue;
    if tok[1] = ':' then begin
      w_tok := copy(tok, 2, length(tok));
      if length(w_tok) = 0 then continue;
      if (length(w_tok) = 1) and ub4asm.isRegChar(w_tok[1], r) then
        rg^[r] := rg^[RHP]
      else if tryHex(w_tok, a) then rg^[RHP] := a
      else ub4asm.b4a(tok) // let b4a add ':name'
    end else ub4asm.b4a(tok)
  end
end;

procedure ShowRegs;
var r:char; i:byte=0;
begin
  for r in '@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_' do begin
    Write('  ',r,': ',format('%.8x', [rg^[regn(r)]]));
    inc(i); if 0=i mod 4 then writeln;
  end
end;

procedure PrintWords;
var i: integer;
begin
  for i := 0 to ub4asm.ents - 1 do
  begin
    write(hexstr(ub4asm.dict[i].adr, 4), ':', format('%-11s', [ub4asm.dict[i].id]));
    if (i > 0) and (i mod 5 = 4) then writeln;
  end;
  writeln;
end;

procedure ShowOpcodes;
var
  row, col, op: integer;
  tok: string;
begin
  write('    ');
  for col := 0 to 15 do
    write(' +', Uppercase(hexstr(col, 1)));
  writeln;
  writeln('   +------------------------------------------------');
  for row := 0 to 15 do begin
    write('$', Uppercase(hexstr(row * 16, 2)), '| ');
    for col := 0 to 15 do begin
      op := row * 16 + col;
      case op of
        0 : tok := '..';
        $01..$1F : tok := '^' + chr(ord('@')+op);
        $20..$3F : tok := '@' + chr(ord('@')+op-$20);
        $40..$5F : tok := '!' + chr(ord('@')+op-$40);
        $60..$7F : tok := '+' + chr(ord('@')+op-$60);
        else begin
          tok := optbl[op];
          if tok = '' then tok := '  ';
        end;
      end;
      write(format('%-3s', [tok]));
    end;
    writeln;
  end;
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
  writeln('  :E lb ''e io rt                        # emit character');
  writeln('  :W !R c1 +R rb .f c1 +R rb ^E .n rt   # write counted string');
  writeln('  :S 05 ''h ''e ''l ''l ''o                  # counted string');
  writeln('  @S ^W                                 # (W)rite the (S)tring');
  writeln;
  writeln('for help, see https://github.com/tangentstorm/b4/tree/main/doc');
  writeln;
end;

procedure b4i_file(path:string);
var f: textfile;
    line: string;
    done: boolean;
begin
  if not FileExists(path) then begin
    writeln('file not found: ', path);
    exit;
  end;
  assign(f, path);
  reset(f);
  done := false;
  while not eof(f) and not done do begin
    readln(f, line);
    done := b4i(line);
  end;
  close(f);
end;

function b4i(line:string):boolean;
  type tstate = (st_cmt,st_asm,st_imm);
  var tok : string; done: boolean = false; r,op:byte; st:tstate=st_imm;
  toks: TStringArray; i: integer = -1; addr: ub4.address; a: integer;
begin
  st:=st_imm;
  if length(line)=0 then exit(false);
  if line[1] = ':' then begin PutMem(line); exit(false); end;
  toks := SplitString(line, ' ');
  while i < High(toks) do begin
    inc(i); tok := toks[i]; if tok='' then continue;
    // skip comments, but note ':' has its own comment handler :/
    if tok[1] = '#' then st:=st_cmt; if st=st_cmt then continue;
    case tok of
      '\a' : if i < High(toks) then begin inc(i); ub4asm.b4a_file(toks[i]) end
             else writeln('usage: \a <filename>');
      '\C', '%C' : begin boot; ub4asm.clear_dict; end;
      '\d' : if (i < High(toks)) then begin
               inc(i); ChDir(toks[i]); if IoResult<>0
                 then writeln('error changing directory to ', toks[i]) end
             else writeln(GetCurrentDir);
      '\f' : for fw in fwds do writeln(hexstr(fw.at, 4), '>', fw.key);
      '\g' : logging_enabled := not logging_enabled;
      '\j' : if i < High(toks) then begin inc(i);
               if tryhex(toks[i], a) then ub4.rg^[ub4.RIP]:=a end
             else writeln('usage: \j <address>');
      '\h', '%h' : help;
      '\i' : if i < High(toks) then begin inc(i); ub4i.b4i_file(toks[i]) end
             else writeln('usage: \i <filename>');
      '\o', '-o' : ShowOpcodes;
      '\p', '%p' : PrintWords;
      '\q', '%q' : done := true;
      '\R', '%R' : reset_vm;
      '\s', '%s' : ub4.step;
      '?d' : begin WriteStack('ds: ', ds, rg^[RDS]); WriteLn end;
      '?c' : begin WriteStack('cs: ', cs, rg^[RCS]); WriteLn end;
      '?i' : WriteLn('ip: ', b4mat(rg^[RIP]));
      '?r' : ShowRegs;
      else case tok[1] of
        '''' : if length(tok)=1 then dput(32) // space
               else dput(ord(tok[2])); // char literals
        '`'  : begin
                 if (length(tok)=2) and IsRegChar(tok[2],r)
                   then dput(rega(tok[2]))
                 else if ub4asm.find(copy(tok,2,length(tok)-1), addr)
                   then dput(addr)
                 else err('unknown `word')
               end;
        '@' : if (length(tok)=2) and isRegChar(tok[2],r) then dput(rg^[r])
              else if ub4asm.find(copy(tok,2,length(tok)-1), addr) then dput(rdval(addr))
              else err('unknown @word');
        '!' : if (length(tok)=2) and isRegChar(tok[2],r) then rg^[r] := dpop
              else if ub4asm.find(copy(tok,2,length(tok)-1), addr) then wrval(addr, dpop)
              else err('unknown !word');
        '?'  : if length(tok)=2 then begin
                 WriteLn(Format('%.8x',[rg^[regn(tok[2])]])); end
               else if (length(tok)=3) and (tok[2]='@') then begin
                 a := rdval(rega(tok[3]));
                 write(format('%.8x ',[a])); ShowMem(a); end
               else begin
                 tok := copy(tok,2,length(tok)-1);
                 if tryHex(tok, a) then showmem(a)
                 else if ub4asm.find(tok, addr) then begin
                   a:=longint(addr);
                   write(format('%.8x ',[a])); ShowMem(a); end
                 else writeln('invalid address: ', tok);
               end;
        else
          if tryHex(tok, a) then dput(a)
          else if ub4asm.find(tok, addr) then wrapCall(addr)
          else if ub4asm.b4op(tok, op) then begin
            if op < $20 then wrapCall(ub4.address(rg^[op]))
            else wrapOp(op) end // immediately invoke ^R
          else WriteLn('what does "', tok, '" mean?');
      end;
    end
  end;
  if ub4.ob <> '' then begin writeln(ub4.ob); ub4.ob := '' end;
  result := done;
end;

function b4i_args:boolean;
var
  i: integer = 1;
  done: boolean = false;
begin
  while (i <= ParamCount) and (not done) do
  begin
    case ParamStr(i) of
      '-i': begin
        inc(i);
        if i <= ParamCount then b4i_file(ParamStr(i))
        else writeln('error: -i requires a filename');
      end;
      '-a': begin
        inc(i);
        if i <= ParamCount then ub4asm.b4a_file(ParamStr(i))
        else writeln('error: -a requires a filename');
      end;
      '-o': ShowOpcodes;
      '-p': PrintWords;
      '-q': done := true;
    end;
    inc(i);
  end;
  result := done;
end;

end.
