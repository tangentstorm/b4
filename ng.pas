{$i xpc.inc }
unit ng;
interface uses xpc, stacks, sim, kvm;

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
    vm	   = object
      data, addr : stack;
      ip	 : integer;
      ram	 : array of int32;
      ports	 : array of int32;
      devices	 : array of device;
      optbl	 : array of oprec;
      imgfile	 : image;
      imgpath    : string;
      debugmode  : boolean;

      constructor init( imagepath : string; debug : boolean );
      procedure tick; 
      procedure loop;
      procedure dump;
      procedure runop( op:int32 );
      procedure runio;
      procedure load;
      procedure save;

      { these are the opcodes.. agin. having to type out the interface
	in pascal really makes me want to use oberon instead. }
      procedure oNOP;  procedure oLIT;  procedure oDUP;  procedure oDROP;
      procedure oSWAP; procedure OPUSH; procedure oPOP;  procedure oLOOP;
      procedure oJMP;  procedure oRET;  procedure oJLT;  procedure oJGT;
      procedure oJNE;  procedure oJEQ;  procedure oLOD;  procedure oSTO;
      procedure oADD;  procedure oSUB;  procedure oMUL;  procedure oDIVM;
      procedure oAND;  procedure oOR;   procedure oXOR;  procedure oSHL;
      procedure oSHR;  procedure oZEX;  procedure oINC;  procedure oDEC;
      procedure oIN;   procedure oOUT;  procedure oWAIT; procedure oIVK;
      procedure init_optable;

      { and the port handlers }
      function handle_syncport( msg : int32 ) : int32;
      function handle_keyboard( msg : int32 ) : int32;
      function handle_write( msg : int32 ) : int32;
      function handle_refresh( msg : int32 ) : int32;
      function handle_files( msg : int32 ) : int32;
      function handle_vmquery( msg : int32 ) : int32;
      function handle_canvas( msg : int32 ) : int32;
      function handle_mouse( msg : int32 ) : int32;
      function handle_eterm( msg : int32 ) : int32;
      procedure init_porthandlers;
    end;

implementation

  {$i ng.ops.inc }
  {$i ng.ports.inc }
  {$i ng.debug.inc }

  constructor vm.init( imagepath : string; debug : boolean );
  begin
    self.init_optable;
    assert( length( self.optbl ) >= 31 );
    self.init_porthandlers;
    self.data.init( 32 );
    self.addr.init( 32 );
    self.ip := 0;
    self.imgpath := imagepath;
    assign( self.imgfile, imagepath );
    self.load;
    self.debugmode := debug;
  end; { vm.init }


  procedure vm.load;
    var size, i : int32;
  begin
    {$i-}
    reset( self.imgfile );
    {$i+}
    if ioresult = 0 then begin
      size := filesize( self.imgfile );
      log.debug([ 'image size = ', size, ' cells' ]);
      setlength( self.ram, size );
      log.debug([ 'ram = array [', low( self.ram ), '..', high( self.ram ), ']' ]);
      for i := 0 to size - 1 do begin
	read( self.imgfile, self.ram[ i ]);
      end;
      close( self.imgfile );
    end else begin
      writeln( 'error: unable to open ', self.imgpath );
      halt;
    end
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


  procedure vm.tick;
  begin
    if self.debugmode then dump;
    if ( ip >= low( ram )) and ( ip <= high( ram )) then
      runop( ram[ ip ] );
    inc( ip );
  end;


  procedure vm.loop;
  begin
    repeat tick until ip >= length( ram );
  end; { vm.loop }


  procedure vm.runop( op: int32 );
  begin
    { TODO : real breakpoints }
    if false { or ( op = 129 ) } and not debugmode then begin
      debugmode := true; dump;
    end;
    if op >= length( optbl ) then oIVK { invoke a procedure }
    else if op < 0 then begin
      writeln( 'bad opcode: ', op ); readln; dump; debugmode := true;
    end
    else optbl[ op ].go;
  end;

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
    kvm.clrscr;
    if ports[ 0 ] = 0 then begin
      ports[ 0 ] := 1;
      for p := 0 to length( ports )-1 do begin
	if ports[ p ] <> 0 then begin
	  ports[ p ] := devices[ p ]( ports[ p ]);
	end;
      end;
    end;
  end; { vm.runio }
  
begin

end.
