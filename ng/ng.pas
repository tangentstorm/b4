{$i xpc.inc }
unit ng;
interface uses xpc, stacks, kvm, kbd, posix, sysutils;

  type
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

    vm = class
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

      constructor create( imagepath : string; debug : boolean; pad: int32 );

      { single-step instructions }
      procedure step;
      procedure runop( op:int32 );

      { input/output }
      procedure runio;
      function waiting : boolean;
      // maybe later: (for now, the retry mechanism in oIN seems to work fine.)
      // procedure send( msg : Int32 );

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
    TRetroVM = vm;
    ENotFinished = class (Exception);

  const
    rxWAITING = 0;
    rxACTIVE  = 1;


implementation

  {$i ng_ops.pas }
  {$i ng_retro.pas }
  {$i ng_input.pas }
  {$i ng_files.pas }
  {$i ng_ports.pas }
  {$i ng_debug.pas }

  constructor vm.Create( imagepath : string; debug : boolean; pad : int32 );
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
    self.ports[0] := rxACTIVE;
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

  procedure vm.step;
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
      step
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

{ IO System
  -------------------------------------------------------------

  The VM communicates with the outside world through a group of
  virtual ports. These are simply slots in an array, associated with a
  particular callback function of type:

  function callback( msg : Int32 ) : Int32;

  - from the VM, use the OUT instruction to send data to a port.
  - [ NOTE: this part is going away ]
       * set ports[ 0 ] to 0, again via the OUT opcode
       * invoke the 'wait' opcode, which triggers =runio=, below.

  A device will only be triggered when you write a non-zero values
  to its port. Only one device will trigger on each WAIT, and they
  will always be executed in order of ascending port number.

  NOTE:Because only one is port is called per step, the WAIT
  opcode isn't necessary, and should be removed.

  <I am only keeping it for the time being because I haven't fully
  tested retro without it.>
  ------------------------------------------------------------- }
procedure vm.runio; { triggered by the oWAIT op }
var p: int32 = 1;
begin
  { find the first message }
  while (p < length( ports )) and (ports[p] = 0) do inc(p);
  if p < length( ports ) then
    try
      ports[ p ] := devices[ p ]( ports[ p ]);
      ports[ 0 ] := rxACTIVE;
    except
      on ENotFinished do ports[ 0 ] := rxWAITING
      else writeln('protocol error on port:', p);
    end;
  { end; // if }
  {TODO: use something besides exceptions for asynchronous
     requests: probably port 0. }
  {TODO: design a mechanism for sending non-stack data. }
end;

{ the top level routine }

procedure vm.loop;
begin
  repeat step until done;
end; { vm.loop }


initialization
  { nothing to initialize. }
end.
