program retro;
uses xpc, ng, sysutils;

  var
    inputs : array of string;
    vm	   : ng.vm;

  { the main logic }
  procedure main;
    var path : string;
  begin
    for path in inputs do vm.enqueue( path );
    vm.loop;
  end;

{ helper routines to prepare the loop }

  var init_failed : boolean = false;
    
  { helper routine for startup errors }
  procedure die( msg : string );
  begin
    writeln;
    writeln( msg );
    init_failed := true;
  end; { die }

  { if file exists, add to inputs array }
  procedure with_file( const path : string );
  begin
    setlength( inputs, length( inputs ) + 1 );
    if sysutils.fileexists( path ) then
      inputs[ length( inputs ) - 1 ] := path
    else die( '"' + path + '" not found' );
  end; { with_file }

{ main code }

var
  i	  : integer;
  imgpath : string = 'retroImage';
  debug	  : boolean;
  p	  : string;

begin
  i := 1;
  while ( i < paramcount ) and not init_failed do
  begin
    p := paramstr( i );
    if p = '-d' then  debug := true
    else if p = '--with' then
      if i + 1 <= paramcount then
      begin
	inc( i );
	with_file( paramstr( i ))
      end else die( 'no filename given for --with' )
    else { no prefix, so expect image name }
    begin
      if imgpath = '' then imgpath := paramstr( i )
      else die( 'error: more than one image path given.' )
    end
  end;
  if not init_failed then
  begin
    {$i-} vm.init( imgpath, debug ); {$i+}
    if ioresult = 0 then main
    else die( 'couldn''t open image file: ' + imgpath );
  end
end.
