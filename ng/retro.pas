program retro;
uses
  xpc, sysutils,
  {$IFDEF GL}
  rxgl in 'ng/rxgl_sdl.pas',
  {$ENDIF}
  ng;
  
  var
    inputs : array of string;
    vm	   : ng.TRetroVM;

{ helper routines for main loop }

  var init_failed : boolean = false;
    
  { helper routine for startup errors }
  procedure die( msg : string );
  begin
    writeln;
    writeln( msg );
    init_failed := true;
  end; { die }

  { if file exists, add to array of inputs to include }
  procedure with_file( const path : string );
  begin
    if sysutils.fileexists( path ) then begin
      { we can't just call include until we initialize the vm, and
	we can'd do that until we're done with args, so we use the
	inputs array as a buffer. }
      setlength( inputs, length( inputs ) + 1 );
      inputs[ length( inputs ) - 1 ] := path;
    end else die( '"' + path + '" not found' );
  end; { with_file }


{ main code }

var
  i	     : integer;
  imgpath    : string = '';
  fallback   : string = 'retroImage';
  debug	     : boolean;
  p	     : string;
  dump_after : boolean = false;
  padsize    : int32   = -1;

begin
  i := 1;
  while ( i <= paramcount ) and not init_failed do begin
    p := paramstr( i );
    if p = '-d' then  debug := true
    else if p = '--dump' then dump_after := true
    else if p = '--with' then
      if i + 1 <= paramcount then begin
	inc( i );
	with_file( paramstr( i ))
      end else die( 'no filename given for --with' )
    else if p = '--pad' then
      if i + 1 <= paramcount then begin
	inc( i );
	padsize := strtoint( paramstr( i ))
      end else die( 'expected a number after --pad' )
    else if p = '--image' then begin { do nothing. this is for ngarotest } end
    else begin { no prefix, so expect image name }
      if imgpath = '' then imgpath := paramstr( i )
      else die( 'error: more than one image path given.' )
    end;
    inc( i )
  end;

  if imgpath = '' then imgpath := fallback;
  if ( imgpath = fallback ) and ( padsize = -1 ) then padsize := 100000;


{ main code , continued }

  if not init_failed then begin
    {$i-} vm := TRetroVM.Create( imgpath, debug, padsize ); {$i+}
    if ioresult <> 0 then die( 'couldn''t open image file: ' + imgpath )
    else begin
      for p in inputs do vm.include( p );
      {$IFDEF GL}
      rxgl.Main(vm);
      {$ELSE}
      vm.Loop;
      {$ENDIF}
      if dump_after then vm.dump
    end
  end
end.
