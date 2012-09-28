{$i xpc.inc }
unit ng;
interface uses xpc, stacks, sim;

  type
    token = string[ 6 ];
    thunk = procedure of object;
    oprec = record
	      go       : thunk;
	      tok, sig : token;
	      hasarg   : boolean;
	    end;

  type vm = object
    data, addr : stack;
    ip     : integer;
    ram    : array [ 0 .. 31 ] of int32;
    port   : array [ 0 .. 15 ] of int32;
    device : array [ 0 .. 15 ] of sim.hardware;
    optbl : array of oprec;
    constructor init;
    procedure tick;
    procedure loop;
    procedure dump;
    procedure runop( op:int32 );
    procedure runio;  

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
    
  end;

implementation

  {$i ng.ops.inc }
  {$i ng.ports.inc }

  constructor vm.init;
    procedure addop(	 
		id	 : int32;
		go	 : thunk;
		tok, sig : token;
		hasarg	 : boolean );
      var rec : oprec;
    begin
      rec.go  := go;
      rec.tok := tok;
      rec.sig := sig;
      rec.hasarg := hasarg;
      optbl[ id ] := rec;
    end; { addop }
    const _ = false; X = true;
  begin
    data.init( 32 );
    addr.init( 32 );
    ip := 0;
    setlength( optbl, 31 );
    addop( 00, @oNOP , 'nop' , '  -  ', _ );
    addop( 01, @oLIT , 'lit' , '  -  ', X );
    addop( 02, @oDUP , 'dup' , ' n-nn', _ );
    addop( 03, @oDROP, 'drop', ' n-  ', _ );
    addop( 04, @oSWAP, 'swap', 'xy-yx', _ );
    addop( 05, @oPUSH, 'push', ' n-^ ', _ );
    addop( 06, @oPOP , 'pop' , ' ^-n ', _ );
    addop( 07, @oLOOP, 'loop', '  -  ', _ );
    addop( 08, @oJMP , 'jmp' , '  -  ', X );
    addop( 09, @oRET , 'ret' , '  -  ', _ );
    addop( 10, @oJLT , 'jlt' , '  -  ', X );
    addop( 11, @oJGT , 'jgt' , '  -  ', X );
    addop( 12, @oJNE , 'jne' , '  -  ', X );
    addop( 13, @oJEQ , 'jeq' , '  -  ', X );
    addop( 14, @oLOD , '@'   , ' a-n ', _ );
    addop( 15, @oSTO , '!'   , 'na-  ', _ );
    addop( 16, @oADD , '+'   , ' y-n ', _ );
    addop( 17, @oSUB , '-'   , 'xy-n ', _ );
    addop( 18, @oMUL , '*'   , 'xy-n ', _ );
    addop( 19, @oDIVM, '/mod', 'xy-rq', _ );
    addop( 20, @oAND , 'and' , 'xy-n ', _ );
    addop( 21, @oOR  , 'or'  , 'xy-n ', _ );
    addop( 22, @oXOR , 'xor' , 'xy-n ', _ );
    addop( 23, @oSHL , '<<'  , 'xy-n ', _ );
    addop( 24, @oSHR , '>>'  , 'xy-n ', _ );
    addop( 25, @oZEX , 'zex' , '  -  ', _ );
    addop( 26, @oINC , '1+'  , ' n-n ', _ );
    addop( 27, @oDEC , '1-'  , ' n-n ', _ );
    addop( 28, @oIN  , 'in'  , ' p-n ', _ );
    addop( 29, @oOUT , 'out' , 'np-  ', _ );
    addop( 30, @oWAIT, 'wait', '  -  ', _ );
  end;
  
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
                port[ p ] := device[ p ].send( port[ p ]);
              end;
          end;
      end;
  end;

begin

end.
