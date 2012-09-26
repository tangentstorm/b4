{ opcodes implementations for ng.pas }
  
{ tn : move data.tos and data.nos into t and n }
var t, n : integer;
{$MACRO ON}
{$DEFINE TN := t :=data.pop; n:=data.pop; }

procedure vm.oNOP; begin end;

{ -- memory ops --------------------------------------------- }
procedure vm.oLIT; begin inc( ip ); data.push( ram[ ip ]) end;
procedure vm.oLOD; begin data.push( ram[ data.pop ]) end;
{ STORE : (na-) - put nos into ram at tos }
procedure vm.oSTO; begin TN ram[ t ] := ram[ n ]; end;

{ -- stack ops ---------------------------------------------- }
procedure vm.oDUP ; begin data.dup end;
procedure vm.oDROP; begin data.drop end;
procedure vm.oSWAP; begin data.swap end;
procedure vm.oPUSH; begin addr.push( data.pop ) end;
procedure vm.oPOP ; begin data.push( addr.pop ) end;

{ -- port ops ----------------------------------------------- }
procedure vm.oIN; { p-n }
begin t := data.pop;
  data.push( port[ t ] );
  port[ t ] := 0;
end; 
procedure vm.oOUT ; { np- } begin port[ data.pop ] := data.pop; end;
procedure vm.oWAIT; { - } begin runio; end;

{ -- arithmetic --------------------------------------------- }
procedure vm.oADD ; begin data.push(  data.pop + data.pop ) end;
procedure vm.oSUB ; begin data.push( -data.pop + data.pop ) end;
procedure vm.oMUL ; begin data.push( -data.pop + data.pop ) end;
procedure vm.oDIVM;
begin
  TN
  data.push( n mod t ); { yep. mod comes first }
  data.push( n div t );
end;
procedure vm.oINC ; begin inc( data.cell[ data.sp ] ) end;
procedure vm.oDEC ; begin dec( data.cell[ data.sp ] ) end;

{ -- logic -------------------------------------------------- }
procedure vm.oAND ; begin data.push( data.pop AND data.pop ) end;
procedure vm.oOR  ; begin data.push( data.pop OR data.pop ) end;
procedure vm.oXOR ; begin data.push( data.pop XOR data.pop ) end;
procedure vm.oSHL ; begin t := data.pop; data.push( data.pop shl t ); end;
procedure vm.oSHR  ; begin t := data.pop; data.push( data.pop shr t ); end;


{ -- jump and conditional jumps ----------------------------- }
procedure vm.oJMP();
begin
  ip := ram[ ip + 1 ];
  while ram[ ip ] = 0 do inc( ip ); { skip over no-ops }
  dec( ip ); { compensating for the post-op inc }
end;
procedure vm.oJLT; begin TN if t <  n then oJMP else inc( ip ) end;
procedure vm.oJGT; begin TN if t >  n then oJMP else inc( ip ) end;
procedure vm.oJNE; begin TN if t <> n then oJMP else inc( ip ) end;
procedure vm.oJEQ; begin TN if t =  n then oJMP else inc( ip ) end;

{ gosub / return }
procedure vm.oRET; begin ip := addr.pop end;

procedure vm.oLOOP;
begin
  dec( data.cell[ data.sp ] );
  if data.cell[ data.sp ] > 0 then
    ip := ram[ ip + 1 ]
  else
  begin
    inc( ip );
    data.pop;
  end
end;

{ zex : exit (return) if TOS = 0 ( sort of like ~assert~ ) }
procedure vm.oZEX;
begin
  if data.cell[ data.sp ] = 0 then
  begin
    { sort of an assert / guard }
    data.pop;
    ip := addr.pop;
  end
end;
