{$i xpc.inc}
program retro;
uses xpc, ng, ln, sysutils, cx;

  type
    TRetroConfig = record
		     padSize   : int32;
		     imgPath   : string;
		     debug     : boolean;
		     dumpAfter : boolean;
		     lineEdit  : boolean;
		     inputs    : array of string;
		   end;	       

  function configure( var cfg : TRetroConfig ) : boolean;

    procedure fail( msg : string );
    begin
      writeln; writeln( msg );
      result := false;
    end; { fail }

    { if file exists, add to array of inputs to include }
    procedure with_file( const path : string );
    begin
      if sysutils.fileexists( path )
	then with cfg do
	begin
	  { we can't just call include until we initialize the vm, and
	    we can'd do that until we're done with args, so we use the
	    inputs array as a buffer. }
	  setlength( inputs, length( inputs ) + 1 );
	  inputs[ length( inputs ) - 1 ] := path;
	end
      else fail( '"' + path + '" not found' );
    end; { with_file }

  { configure }
  var
    p : string;
    i : integer = 1;

  begin
    with cfg do begin

      { defaults  - would be nice if this were in the record definition... :/ }
      imgPath := '';
      padSize := -1;
      dumpAfter := false;
      lineEdit := false;
      debug := false;

      result := true; { false will indicate failure }

      { parse the command line parameters }
      i := 1;
      while result and ( i <= paramcount ) do begin
	p := paramstr( i );
	if (p = '--debug') or (p = '-d') then debug := true
	else if p = '--dump' then dumpAfter := true
	else if (p = '--editline') or (p = '-e') then lineEdit := true
	else if p = '--with' then
	  if i + 1 <= paramcount then begin
	    inc( i );
	    with_file( paramstr( i ))
	  end else fail( 'no filename given for --with' )
	else if p = '--pad' then
	  if i + 1 <= paramcount then begin
	    inc( i ); padsize := strtoint( paramstr( i ))
	  end else fail( 'expected a number after --pad' )
	else if p = '--image' then begin
	  inc( i ); imgPath := paramstr( i );
	end
	else begin { no prefix, so expect image name }
	  if imgpath = '' then imgpath := paramstr( i )
	  else fail( 'error: more than one image path given.' )
	end;
	inc( i )
      end;
      if imgPath = '' then imgPath := 'retroImage';
      if ( padsize = -1 ) then padsize := 100000;
    end
  end; { configure }

  { optional line editor }

  var
    waiting : boolean; { true when waiting on input }
    buff    : string = '';

  { replacement for ng.vm.handle_keyboard }
  type EdlinDevice = object
    edln    : ln.LineEditor;
    constructor init;
    function handle ( msg : int32 ) : int32;
    procedure step;
  end;

  constructor EdlinDevice.init;
  begin
    edln := ln.ed;
    edln.prompt := 'ok> '
  end;

  procedure EdlinDevice.step;
  begin
    edln.step;
  end;

  function EdlinDevice.handle ( msg : int32 ) : int32;
  begin
    while length( buff ) = 0 do begin
      while not edln.done do edln.step;
      buff := edln.flush + #10;
      writeln;
    end;
    result := ord( buff[ 1 ]);
    delete( buff, 1, 1 );
  end; { handle_keyboard_buffer }
  

{ main code }
var
  cfg	: TRetroConfig;
  path	: string;
  vm	: ng.vm;
  edev	: EdlinDevice;
begin
  if configure( cfg ) then with cfg do
  begin
    {$i-} vm.init( imgpath, debug, padsize ); {$i+}
    if ioresult <> 0 then die( 'couldn''t open image file: ' + imgpath )
    else begin
      for path in inputs do vm.include( path );
      if cfg.lineEdit then begin
	{ devices should probably all be objects, not functions... }
	edev.init;
	vm.devices[1] := @edev.handle;
      end;
      vm.loop;
      if dumpAfter then vm.dump
    end
  end
end.
