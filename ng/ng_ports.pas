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
    vdp.RenderDisplay;
    result := ord(vdp.PollKeyboard);
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
    var i : longword;
  begin
    for i := 0 to cScnChrSize do vdp.aCharMap[i] := 0;
    for i := 0 to cScnAtrSize do vdp.aAttrMap[i] := 0;
    cx := 0; cy := 0;
  end;

  function vm.handle_write (msg : int32): int32;
    var x:    int32;
        adr:  longword;
        attr: tVDPAttrData;

    procedure cursorRight; inline;
    begin
      if cx < vdp.termW then cx := cx + 1
      else if cy < vdp.termH then begin cy := cy + 1; cx := 0; end
      else begin clear; cy := 0; cx := 0; end;
    end;

    procedure cursorLeft; inline;
    begin
      if cx < vdp.termW then cx := cx-1;
    end;

    procedure cursorBackspace; inline;
    begin
      attr[1] := 0;
      vdp.WriteAttrMap(adr, attr);
      vdp.WriteCharMap(adr, 0);
      if (cx < vdp.termW) and (cx > 0) then cx := cx - 1;
    end;

    procedure cursorReturn; inline;
    begin
      attr[1] := 0;
      vdp.WriteAttrMap(adr, attr);
      cy := cy + 1; cx := 0;
      if cy >= vdp.termH then begin
         clear; cy := 0; cx := 0;
      end;
    end;

  begin
    if msg = 1 then begin
      x := self.data.pop;
      if x < 0 then clear else
         if x < 32 then begin case chr (x) of
                                ^H: cursorBackspace;
                                ^J: cursorReturn;
                                ^M: ; end;
                        end;
      adr := cy * vdp.termW + cx;
      attr[1] := vdp.rBR; vdp.WriteAttrMap(adr, attr);
      if x > 31 then begin
	attr[1] := 0;
	vdp.WriteAttrMap(adr, attr);
	vdp.WriteCharMap(adr, x);
      end;
      if x > 31 then cursorRight;
      if count = refresh then begin
	count := 0; vdp.RenderDisplay; end;
    end;
    count := count + 1;
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
     var x, y, h, w : int32;
         attr: tVdpAttrData;
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
       9:  begin
             data.pop2 (x, y);
	     attr := vdp.ReadAttrMap(cScnXRes * y + x);
             data.push (attr[0]);
             data.push (attr[1]);
           end;
       10: begin
             data.pop2(x, y);
             data.pop2(h, w);
             attr[0] := h; attr[1] := w;
	     vdp.WriteAttrMap(cScnXRes * y + x, attr);
           end;
       11: begin
             data.pop2(x, y);
	     data.push(vdp.ReadCharMap(cScnXRes * y + x));
           end;
       12: data.push(vdp.rBR);
       13: data.pop1(vdp.rBR);
       14: data.push(vdp.rFG);
       15: data.pop1(vdp.rFG);
       16: vdp.RenderDisplay;
       18: begin
             data.pop3 (x, y, h);
	     vdp.PlotPixel(y * cScnXRes + x, h);
           end;
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
