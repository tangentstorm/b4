{$i xpc.inc }
unit ng;
interface uses romVDP, xpc, stacks, kvm, kbd, posix, sysutils;

  type
    pvm	   = ^vm;
    token  = string[ 6 ];
    thunk  = procedure of object;
    device = function( msg : int32 ) : int32 of object;
    oprec  = record
	       go	: thunk;
	       tok, sig	: token;
	       hasarg	: boolean;
	     end;
    image  = file of int32;

{ interface > types }

    vm	   = object
      data, addr : specialize stack< int32 >;
      ram, ports : array of int32;
      devices	 : array of device;
      ip	 : integer;              { instruction pointer }
      done       : boolean;

      inputs     : array of textfile;    { input files - see ng.input.pas }
      input      : ^textfile;

      optbl	 : array of oprec;
      imgfile	 : image;
      imgpath    : string;
      debugmode  : boolean;
      padsize    : int32;

      {terminal emulation}
      cx,cy      : longword;
      count      : longword;
      vdp        : TVDP;
      refresh    : longword;

      constructor init( imagepath : string; debug : boolean; pad: int32 );

      { single-step instructions }
      procedure tick;
      procedure runop( op:int32 );
      procedure runio;

      { main loop(s) }
      procedure loop;

      { input file loader }
      procedure include( path : string );

      { image routines }
      procedure load;
      procedure save;

      { debug / inspect routines }
      procedure trace;
      procedure dump;
      procedure show_debugger( msg : string );

{ continued... }

{ interface > type vm = object ... }

  { opcodes, defined in ng.ops.pas }
      procedure oNOP;  procedure oLIT;  procedure oDUP;  procedure oDROP;
      procedure oSWAP; procedure OPUSH; procedure oPOP;  procedure oLOOP;
      procedure oJMP;  procedure oRET;  procedure oJLT;  procedure oJGT;
      procedure oJNE;  procedure oJEQ;  procedure oLOD;  procedure oSTO;
      procedure oADD;  procedure oSUB;  procedure oMUL;  procedure oDIVM;
      procedure oAND;  procedure oOR;   procedure oXOR;  procedure oSHL;
      procedure oSHR;  procedure oZEX;  procedure oINC;  procedure oDEC;
      procedure oIN;   procedure oOUT;  procedure oWAIT; procedure oIVK;
      procedure oDEBUG; { custom opcode }
      procedure init_optable;

  { port handlers, defined in ng.ports.pas }
      procedure clear;
      function handle_syncport( msg : int32 ) : int32;
      function handle_keyboard( msg : int32 ) : int32;
      function handle_input( msg : int32 ) : int32;
      procedure next_input;
      function handle_write( msg : int32 ) : int32;
      function handle_refresh( msg : int32 ) : int32;
      function handle_files( msg : int32 ) : int32;
      function handle_vmquery( msg : int32 ) : int32;
      function handle_canvas( msg : int32 ) : int32;
      function handle_mouse( msg : int32 ) : int32;
      function handle_eterm( msg : int32 ) : int32;
      procedure init_porthandlers;

  { retro image layout conventions }
      function rx_getstring( start : int32 ) : string;

    end;

implementation

  {$i ng_ops.pas }
  {$i ng_retro.pas }
  {$i ng_input.pas }
  {$i ng_files.pas }
  {$i ng_ports.pas }
  {$i ng_debug.pas }

  constructor vm.init( imagepath : string; debug : boolean; pad : int32 );
  begin
    self.padsize := pad;
    self.init_optable;
    assert( length( self.optbl ) >= 31 );
    self.init_porthandlers;
    self.data.init( 128 );
    self.addr.init( 128 );
    self.ip := 0;
    self.imgpath := imagepath;
    assign( self.imgfile, imagepath );
    self.load;
    self.debugmode := debug;

    {VDP initialisation (textmode: 100x40, 256 colours)}
    vdpInit;
    vdp.Open;
    refresh := cScnRow * 160;
  end; { vm.init }


{ load and save the vm }

  procedure vm.load;
    var size, i : int32;
  begin
    {$i-}
    reset( self.imgfile );
    {$i+}
    if ioresult = 0 then begin
      size := filesize( self.imgfile );
      // log.debug([ 'image size = ', size, ' cells' ]);
      setlength( self.ram, size );
      // log.debug([ 'ram = array [', low( self.ram ),
      //             '..', high( self.ram ), ']' ]);
      for i := 0 to size - 1 do begin
	read( self.imgfile, self.ram[ i ]);
      end;
      close( self.imgfile );
    end else begin
      writeln( 'error: unable to open ', self.imgpath );
      halt;
    end;
    if self.padsize > size then setlength( self.ram, self.padsize )
  end; { vm.load }

  procedure vm.save;
    var size, i : int32;
  begin
    size := length( self.ram );
    rewrite( self.imgfile, 1 );
    for i := 0 to size - 1 do begin
      write( self.imgfile, self.ram[ i ])
    end;
    close( self.imgfile );
  end; { vm.save }


{ step by step run of the vm }

  procedure vm.tick;
  begin
    if self.debugmode then show_debugger( '' );
    if ( ip >= low( ram )) and ( ip <= high( ram )) then
    begin
      runop( ram[ ip ] );
      inc( ip );
    end else done := true
  end;

  procedure vm.trace;
    var log : text;
  begin
    assign( log, 'pascal.log' );
    rewrite( log );
    repeat
      writeln( log, ip, ^_, ram[ ip ], ^_, data.dumps, ^_, addr.dumps );
      tick
    until ip >= length( ram );
  end; { vm.trace }

  procedure vm.runop( op: int32 );
  begin
    { TODO : real breakpoints }
    if false { or ( op = 129 ) } and not debugmode then begin
      debugmode := true; show_debugger( ' break ' );
    end;
    if op >= length( optbl ) then oIVK { invoke a procedure }
    else if op < 0 then begin
       show_debugger( 'bad opcode: ' + inttostr( op )); readln; debugmode := true;
    end
    else optbl[ op ].go;
  end;

{ run io }
  {
  | Ngaro machines connect via ports.                      |
  | A port is just a normal cell that's writable from both |
  | inside and outside the machine, much like a usb port.  |
  |                                                        |
  | The protocol is:                                       |
  |                                                        |
  | - write whatever you want to the ports                 |
  | - set ports[ 0 ] to 0                                  |
  | - invoke the 'wait' instruction                        |
  |                                                        |
  | - the vm pauses until a device sets port[ 0 ] to 1     |
  |                                                        |
  | Note: only one device will trigger on each WAIT, and   |
  | (at least in this vm and the js one) they will always  |
  | be executed in order of ascending port numbers.        |
  |                                                        |
  | A device will only be triggered when you write a       |
  | non-zero values to its port.                           |
  |                                                        |
  }
  procedure vm.runio; { triggered by the oWAIT op }
    var p: int32;
  begin
    if ports[ 0 ] = 0 then begin
      ports[ 0 ] := 1;
      for p := 0 to length( ports )-1 do begin
	if ports[ p ] <> 0 then begin
	  ports[ p ] := devices[ p ]( ports[ p ]);
	end;
      end;
    end;
  end; { vm.runio }


{ the top level routine }

procedure vm.loop;
begin
  repeat tick until done;
end; { vm.loop }


initialization
  { nothing to initialize. }
end.
