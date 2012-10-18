{$IFDEF NESTUNITS }
unit ng.ports; implementation
{$ENDIF}

  { these were mostly ported from ngaro.js, then refactored into routines }

  { -- port 0 ------------------------------------------------- }

  function vm.handle_syncport( msg :  int32 ): int32;
  begin
    { Nothing to do: port 0 isn't connected to a device.
      It's just used to signal that one side or the other
      has data to transfer. }
    result := 0;
  end;

  { -- port 1 ------------------------------------------------- }

  { normally, port 1 reads the keyboard }
  function vm.handle_keyboard( msg : int32 ) : int32;
  begin
    result := ord( kvm.readkey );
  end;


  { but we can also fake keyboard input from a file }
  function vm.handle_input( msg	: int32 ) : int32;
    var ch : char;
  begin
    if eof( self.input ) then begin
      self.next_input;
      result := self.devices[ 1 ]( msg )
    end else begin
      read( self.input, ch );
      result := ord( ch );
    end
  end;

  { next_input either switches to the next file or to the keyboard }
  procedure vm.next_input;
  begin
    if self.inptr >= 0 then close( self.input );
    inc( self.inptr );
    if self.inptr >= length( self.inputs ) then
    begin
      self.devices[ 1 ] := @self.handle_keyboard;
    end
    else
    begin
      assign( self.input, self.inputs[ self.inptr ]);
      reset( self.input );
    end
  end;



  { -- port 2 : simple text output ---------------------------- }

  procedure clear;
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

  type
    ngfile = record
	       handle : file of byte;
	       assigned, opened, closed	: boolean;
	     end;
  var
    files   : array of ngfile;

  function vm.handle_files( msg : int32 ) : int32;
    const r = 0; w = 1; a = 2; m = 3;
    procedure nexthandle;
    begin
    end;
  begin
    result := 0;
    case msg of
      +1 : self.save;
      -1 : begin { open :: name -> mode -> handle }
	     { }
	   end;
      -2 : begin { read :: handle -> flag }

	   end;
      -3 : begin { write :: char -> handle -> flag }

	   end;
      -4 : begin { close :: handle -> flag }
	     { 0 on successful close }
	   end;
      -5 : begin { fpos :: handle -> offset }
	   end;
      -6 : begin { seek :: offset -> handle -> flag }
	   end;
      -7 : begin { size :: handle -> size }

	   end;
      -8 : begin { delete :: filename -> flag }
	     { -1 if deleted, else 0 }
	   end
      else
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
      -8  : { current time } result := posix.time;
      -9  : { exit the vm } self.ip := length( self.ram ) + 1;
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
{    var x, y, h, w : int32;}
  begin
    result := 0;
    case msg of
      1 : begin end;
      { kvm.setcolor( data.pop );

      2 : begin
	    data.pop2( y, x );
	    fb.fillRect(x, y, 2, 2);
	  end;
      3 : begin
	    data.pop4( w, h, y, x );
	    fb.strokeRect(x, y, w, h);
	  end;
      4 : begin
	    data.pop4( w, h, y, x );
	    fb.fillRect(x, y, w, h);
	  end;
      5 : begin
	    data.pop3( h, y, x );
	    fb.fillRect(x, y, 2, h);
	  end;
      6 : begin
	    data.pop3( w, y, x );
	    fb.fillRect(x, y, w, 2);
	  end;
      7 : begin
	    data.pop3( w, y, x );
	    fb.beginPath;
	    fb.arc(x, y, w, 0, Math.PI*2, true);
	    fb.closePath();
	    fb.stroke();
	  end;
      8 : begin
	    data.pop3( w, y, x );
	    fb.beginPath;
	    fb.arc(x, y, w, 0, Math.PI*2, true);
	    fb.closePath;
	    fb.fill;
	  end
	}
      else
	result := -1;
    end
  end;


  { -- port 7 : mouse ----------------------------------------- }

  function vm.handle_mouse( msg : int32 ) : int32;
  begin
    result := 0;
    case msg of
      1 : self.data.push2( kvm.mx, kvm.my );
      2 : self.data.push( toint( kvm.mb ));
      else
	result := -1;
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
