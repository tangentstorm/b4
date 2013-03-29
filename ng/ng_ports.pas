{$IFDEF NESTUNITS }
unit ng.ports; implementation
{$ENDIF}

  { the handlers in this file were mostly ported from ngaro.js, then broken into routines }


  { -- port 0 ------------------------------------------------- }

  function vm.handle_syncport( msg :  int32 ): int32;
  begin
    { Nothing to do: port 0 isn't connected to a device.
      It's just used to signal that one side or the other
      has data to transfer. }
    result := 0;
  end;

  { -- port 1 ------------------------------------------------- }
  {

    We implement two separate handlers for port 1.

    The default handler, handle_keyboard reads input from the keyboard.

    handle_input reads input from a text file in the input queue.

    These are toggled dynamically by updating the pointer in the
    vm.devices array.

    The switch happens whenever a file is pushed onto the input
    the stack ( either via --with on the command line or by sending
    msg 2 to port 4 ), and the switch back happens on EOF for the
    last input file on the stack.

    see also : ng.input.pas for managing the input stack

  }

  { keyboard handler }
  function vm.handle_keyboard( msg : int32 ) : int32;
  begin
    result := ord( kbd.readkey );
  end;

  { input file handler }
  function vm.handle_input( msg	: int32 ) : int32;
    var ch : char;
  begin
    if eof( self.input^ ) then begin
      self.next_input;
      result := self.devices[ 1 ]( msg )
    end else begin
      read( self.input^, ch );
      result := ord( ch );
    end
  end;

  { -- port 2 : simple text output ---------------------------- }

  procedure vm.clear;
  begin
    kvm.clrscr;
    kvm.gotoxy( 0, 0 );
  end;

  function vm.handle_write( msg : int32 ) : int32;
    var x : int32;
  begin
    if msg = 1 then begin
      x := self.data.pop;
      if x < 0 then clear
      else if x < 32 then
	case chr( x ) of
	  ^H : write( ^H, ' ', ^H );
	  ^J : writeln;
	  ^M : ;
	  else write( chr( x ))
	end
      else write( chr( x ))
    end;
    result := 0;
  end;


  { -- port 3 : video refresh --------------------------------- }

  function vm.handle_refresh( msg : int32 ) : int32;
  begin
    { Whether I need to do anything here depends on how I implement KVM stuff }
    result := 0;
  end;

  { -- port 4 : file i/o -------------------------------------- }

  { see also ng.files.pas }

  function vm.handle_files( msg : int32 ) : int32;
    var t, n : int32;
  begin
    result := 0;
    case msg of
      +1 : self.save;
      +2 : self.include( rx_getstring( data.pop ));
      { -- }
      -1 : begin data.pop2( t, n );   ng.file_open( t, rx_getstring( n ), result )   end;
      -2 : begin data.pop1( t );      ng.file_read( t, result )                      end;
      -3 : begin data.pop2( t, n );   ng.file_write( t, chr( n ), result )           end;
      -4 : begin data.pop1( t );      ng.file_close( t )                             end;
      -5 : begin data.pop1( t );      ng.file_getpos( t, result )                    end;
      -6 : begin data.pop2( t, n );   ng.file_setpos( t, n, result )                 end;
      -7 : begin data.pop1( t );      ng.file_size( t, result )                      end;
      -8 : begin data.pop1( t );      ng.file_delete( rx_getstring( t ), result )    end;
    end; { case }
  end; { handle_files }

  { -- port 5 : vm query -------------------------------------- }

  function vm.handle_vmquery( msg: int32 ) : int32;
  begin
    result := 0;
    case msg of

      -1  : { memory size } result := length( self.ram );
      -2  : { canvas exists? } if kvm.hascanvas then result := 1;
      -3  : { canvas width } result := kvm.canvas.w;
      -4  : { canvas height } result := kvm.canvas.h;
      -5  : { data stack depth } result := self.data.sp;
      -6  : { address stack depth } result := self.addr.sp;
      -7  : {  TODO: mouse exists? } result := 0;
      -8  : { current time } result := lo( posix.time );
      -9  : { exit the vm } self.ip := length( self.ram ) + 1;
      -10 : {  TODO: environ } result := 0;
      -11 : { console width } result := kvm.term.w;
      -12 : { console height } result := kvm.term.h;
      -13 : { num bits/cell } result := 32;
      -14 : {$IFDEF ENDIAN_BIG} result := 1; {$ELSE} result := 0; {$ENDIF}
      -15 : { extended console? } result := -1;
      -16 : { max depth of data stack } result := self.data.limit;
      -17 : { max depth of address stack } result := self.addr.limit;

      { non-standard extensions }
      64  : self.debugmode := true;

      else
	result := -1
    end
  end;


  { -- port 6 : graphic canvas -------------------------------- }

  function vm.handle_canvas( msg: int32 ) : int32;
  begin
    { see rt_ports.pas for a version that works better. }
    result := -1;
  end;


  { -- port 7 : mouse ----------------------------------------- }

  function vm.handle_mouse( msg : int32 ) : int32;
  begin
    result := 0;
    case msg of
      1 : self.data.push2( kvm.mx, kvm.my );
      2 : self.data.push( toint( kvm.mb ));
      else result := -1;
    end;
  end;

  { -- port 8 : enhanced terminal ----------------------------- }

  function vm.handle_eterm( msg : int32 ) : int32;
  begin
    result := 0;
    case msg of
      1 : { rc- } kvm.gotoxy( data.pop, data.pop );
      2 : { n- } kvm.fg( data.pop );
      3 : { n- } kvm.bg( data.pop );
      else
	result := -1;
    end;
  end;


  { -- the port map ------------------------------------------- }

  procedure vm.init_porthandlers;
    var i : integer;
    const portcount = 9;
  begin
    setlength( self.devices, portcount );
    setlength( self.ports, portcount );
    for i := 0 to portcount - 1 do begin
      ports[ i ] := 0;
    end;
    self.devices[0] := @self.handle_syncport;
    self.devices[1] := @self.handle_keyboard;
    self.devices[2] := @self.handle_write;
    self.devices[3] := @self.handle_refresh;
    self.devices[4] := @self.handle_files;
    self.devices[5] := @self.handle_vmquery;
    self.devices[6] := @self.handle_canvas;
    self.devices[7] := @self.handle_mouse;
    self.devices[8] := @self.handle_eterm;
  end; { init_porthandlers }


{$IFDEF NESTUNITS}
end.
{$ENDIF}
