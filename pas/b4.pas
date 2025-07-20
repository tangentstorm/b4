{$mode delphi}
{ this is the main entry point for b4 }
program b4;
uses sysutils, ub4, ub4asm, ub4dbg, uhw_vt, ub4i;

function readstr:string;
  // after reassigning input in the main program,
  // readln no longer echos. weird, but this is
  // a temporary ui anyway until i migrate over
  // to kvm.pas in the xpl repo.
  var c:char=^@;
  begin
    result := '';
    while true do begin
      c:=readkey;
      if c in [^J,^M] then exit
      else if c = ^H then begin
        if length(result)>0 then begin
          result:=leftstr(result,length(result)-1);
          goxy(wherex-1,wherey); write(' ');
          goxy(wherex-1,wherey);
        end end
      else begin
        result += c;
        write(c)
      end
    end
  end;

procedure b4i_loop;
  var line:string; done:boolean=false;
  begin
    clrscr;
    repeat
      fg('G'); write('b4i'); fg('g'); write('> ');
      fg('w');
      line:=readstr;
      writeln;
      done := (line='') or ub4i.b4i(line);
    until done;
  end;

var oldinput : text;
begin
  ub4.term := uhw_vt.TB4KVMTerm.Create;
  open('disk.b4'); boot; clrscr;
  oldinput := input;
  assign(input, '../bios/b4f.b4a');
  reset(input); b4as;
  input := oldinput; reset(input);
  if rg[RGO]<>0 then rg[RIP] := rg[RGO]; { jump to address in @\ (or default=$100) }
  if paramstr(1)='-d' then rg[RDB] := 1;
  while rg[RIP] <= maxheap do begin
    if rg[RIP] = rg[RBP] then begin rg[RDB]:=1; rg[RBP] := high(ub4.address) end;
    if rg[RDB]=1 then debug_step;
    if not (paused and (rg[RDB]=1)) then
      try step
      except on e:EB4Exception do begin
        rg[RDB]:=1; paused := true end end;
  end; { while }
  close(disk);
  {$IFDEF pauseafter} { for turbo pascal }
  writeln('done. press any key to continue');
  repeat until keypressed;
  {$ENDIF}
end.
