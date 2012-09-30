{$i xpc.inc }
program test_xpc;
  uses xpc, math;
  var x : integer;
begin
  if hex(1234) = ansistring('4D2') then writeln('ok.')
  else writeln( 'no: hex says hex(1234) = "', hex(1234), '". Should be "4D2"' );
end.
