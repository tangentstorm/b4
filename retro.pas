program retro;
uses xpc, ng;

  var
    vm	    : ng.vm;
    imgpath : string = 'retroImage';
begin
  if paramcount = 1 then imgpath := paramstr( 1 );
  {$i-}
  vm.init( imgpath );
  {$i+}
  if ioresult = 0 then vm.loop
  else 
    
end.
