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
      port	 : array [ 0 .. 15 ] of int32;
      device	 : array [ 0 .. 15 ] of device;
      optbl	 : array of oprec;
      imgfile	 : image;
      imgpath    : string;
		 
      constructor init( imagepath : string );
      procedure tick;
      procedure loop;
      procedure dump;
      procedure runop( op:int32 );
      procedure runio;
      procedure load;
      procedure save;
		 
      { these are the opcodes. bleh. this is one part of pascal that
	makes me want to use oberon instead. }
      procedure oNOP;  procedure oLIT;  procedure oDUP; procedure oDROP;
      procedure oSWAP; procedure OPUSH; procedure oPOP; procedure oLOOP;
      procedure oJMP;  procedure oRET;  procedure oJLT; procedure oJGT;
      procedure oJNE;  procedure oJEQ;  procedure oLOD; procedure oSTO;
      procedure oADD;  procedure oSUB;  procedure oMUL; procedure oDIVM;
      procedure oAND;  procedure oOR;   procedure oXOR; procedure oSHL;
      procedure oSHR;  procedure oZEX;  procedure oINC; procedure oDEC;
      procedure oIN;   procedure oOUT; procedure oWAIT;
      procedure init_optable;
      
      { and the port handlers }
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

  constructor vm.init( imagepath : string );
  begin
    self.init_optable;
    assert( length( self.optbl ) >= 31 );
    self.data.init( 32 );
    self.addr.init( 32 );
    self.ip := 0;
    self.imgpath := imagepath;
    assign( self.imgfile, imagepath );
    self.load;
  end; { vm.init }


  procedure vm.load;
    var size, i : int32;
  begin
    {$i-}
    reset( self.imgfile );
    {$i+}
    if ioresult = 0 then begin
      size := filesize( self.imgfile );
      setlength( self.ram, size );
      for i := 0 to size - 1 do begin
	read( self.imgfile, self.ram[ i ])
      end;
      close( self.imgfile );
    end else begin
      writeln( 'error: unable to open ', self.imgpath );
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
    dump;
    runop( ram[ ip ] );
    inc( ip );
  end;
  
  procedure vm.dump;
    var
      i	: int32;
      s	: string[ 4 ];
      r	: oprec;
  begin

    write( 'data :' ); data.dump;
    write( 'addr :' ); addr.dump;
    write( 'port :' );
    for i:= 0 to 15 do
      begin
        str( port[ i ], s );
        write( s, ' ');
      end;
    writeln;

    { mini-debugger }
    i := 0;
    repeat
      if i = ip
      then write( ' -> ' )
      else write( '    ' );

      r := optbl[ ram[ i ]];  { TODO: handle > 31 }
      write( r.tok );
      if r.hasarg then
        begin
          inc( i );
          str( ram[ i ], s );
          write(' ');
          write( s );
        end;
      writeln;
      inc( i );
    until i = length( self.ram );
    readln;
  end;

  procedure vm.loop;
  begin
    repeat tick until ip >= length( ram );
  end; { vm.loop }

  
  procedure vm.runop( op: int32 );
  begin
    if op >= length( optbl ) then
      { TODO : CALL USER OP }
    else
      optbl[ op ].go;
  end;

  {
  | Ngaro machines connect via ports.                      |
  | A port is just a normal cell that's writable from both |
  | inside and outside the machine, much like a usb port.  |
  |                                                        |
  | The protocol is:                                       |
  |                                                        |
  | - write whatever you want to the ports                 |
  | - set port[ 0 ] to 0                                   |
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
    if port[ 0 ] = 0 then
      begin
        port[ 0 ] := 1;
        for p in port do
          begin
            if port[ p ] <> 0 then
              begin
                port[ p ] := device[ p ]( port[ p ]);
              end;
          end;
      end;
  end;

begin

end.
