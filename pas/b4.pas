{$mode delphi}
{ this is the main entry point for b4 }
program b4;
uses sysutils, ub4, ub4asm, ub4dbg, uhw_vt, ub4i;

var oldinput : text; done : boolean;
begin
  done := b4i_args;
  if done then halt;
  ub4.term := uhw_vt.TB4KVMTerm.Create;
  clrscr;
  rg[RIP]:=$0B40;
  if rg[RGO]<>0 then rg[RIP] := rg[RGO]; { jump to address in @\ }
  if paramstr(1)='-d' then rg[RDB] := 1;
  while rg[RIP] <= maxheap do begin
    if rg[RIP] = rg[RBP] then begin rg[RDB]:=1; rg[RBP] := high(ub4.address) end;
    if rg[RDB]=1 then ub4dbg.debug_step;
    if not (paused and (rg[RDB]=1)) then
      try ub4.step
      except on e:EB4Exception do begin
        rg[RDB]:=1; paused := true end end;
  end; { while }
  {$IFDEF pauseafter} { for turbo pascal }
  writeln('done. press any key to continue');
  repeat until keypressed;
  {$ENDIF}
end.
