program retro;
uses xpc, ng;

  var
    i	    : integer;
    vm	    : ng.vm;
    imgpath : string = 'retroImage';
    debug   : boolean;
begin
  for i := 1 to paramcount do
  begin
    if paramstr( i ) = '-d' then
      debug := true
    else
      imgpath := paramstr( i )
  end;
  {$i-}
  vm.init( imgpath, debug );
  {$i+}
  if ioresult = 0 then vm.loop
  else
    writeln( 'not found: ', imgpath );
end.
