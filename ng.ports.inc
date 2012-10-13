{$IFDEF NESTUNITS }
unit ng.ports; implementation
{$ENDIF}

  { these were ported from ngaro.js, except file/io and enhanced text }

  { -- port 0 ------------------------------------------------- }

  function vm.handle_syncport( msg :  int32 ): int32;
  begin
    { Nothing to do: port 0 isn't connected to a device.
      It's just used to signal that one side or the other
      has data to transfer. }
    result := 0;
  end;

  { -- port 1 ------------------------------------------------- }
  var
    direct_input : boolean = false;
    lastKey      : int32 = -1; { TODO : ascii.syn, maybe? }
    tib		 : string  = '';

  function vm.handle_keyboard( msg : int32 ) : int32;
  begin
    { direct input }
    if direct_input then begin
      result := lastKey;
      lastKey := 0;
    end else if length( tib ) > 0 then begin
      result := ord( tib[ 1 ]);
      delete( tib, 1, 1 );
    end else begin
      result := -1; { TODO: or should it wait for a key? }
    end;
  end;

  { -- port 2 : simple text output ---------------------------- }

  function vm.handle_write( msg : int32 ) : int32;
    var x : int32;
  begin
    if msg = 1 then begin
      x := self.data.pop;
      if x < 0 then kvm.clrscr else write( chr( x ));
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
    writeln( 'handle_vmquery' );
    case msg of
      -1	: result := length( self.ram );
      -2	: if kvm.hascanvas then result := 1 else result := 0;
      -3	: result := kvm.canvas.w;
      -4	: result := kvm.canvas.h;
      -5	: result := self.data.sp;
      -6	: result := self.addr.sp;
      -7	: result := -1;
      -8	: begin
		    {
		      var foo = new Date;
		      var unixtime_ms = foo.getTime();
		      var unixtime = parseInt(unixtime_ms / 1000);
		      result : = unixtime;
		      }
		  end;
      -9	: result := 0;
      -11	: result := kvm.term.w;
      -12	: result := kvm.term.h;
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
      1 : begin { * msg 1 : ( rc- ) row col }
	  end;
      2 : begin { * msg 2 : ( n-  ) fg      }
	  end;
      3 : begin { * msg 3 : ( n-  ) bg      }
	  end;
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
