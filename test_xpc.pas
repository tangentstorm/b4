{$i xpc.inc }
program test_xpc;
  uses xpc, math;


  var x : integer;
begin

{  for x := 0 to 8 do
  begin
    writeln( '2^', x, '=', 2**x, ' = ', 1 shl x, ' = $', hex( 1 shl x )); 
  end;
 } 
  
  if hex(1234) <> ansistring('00 00 04 D2') then
    writeln( 'hex broke. says hex(1234) = ', hex(1234), '.' );
  
end.
