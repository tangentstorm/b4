
{$mode objfpc}
program ngaro;
uses crt;

  type int32 = longint;

  
  const 
    oNOP  = 00;  oLIT  = 01;  oDUP = 02;   oDROP = 03;
    oSWAP = 04;  oPUSH = 05;  oPOP = 06;   oLOOP = 07;
    oJMP  = 08;  oRET  = 09;  oJLT = 10;   oJGT  = 11;
    oJNE  = 12;  oJEQ  = 13;  oLOD = 14;   oSTO = 15;
    oADD  = 16;  oSUB  = 17;  oMUL = 18;   oDIVM = 19;
    oAND  = 20;  oOR   = 21;  oXOR = 22;   oSHL = 23;
    oSHR  = 24;  oZEX  = 25;  oINC = 26;   oDEC = 27;
    oIN   = 28;  oOUT  = 29;  oWAIT = 30;
  
  
  const 
    mnemonic : array[ 0 .. 30 ] of string[ 5 ]
      = ( 'nop', 'lit', 'dup', 'drop',
          'swap', 'push', 'pop', 'loop',
          'jmp', 'ret', 'jlt', 'jgt',
          'jne', 'jeq', 'lod', 'sto',
          'add', 'sub', 'mul', 'divm',
          'and', 'or', 'xor', 'shl',
          'shr', 'zex', 'inc', 'dec',
          'in', 'out', 'wait'
        );
  

  
  type stack = object
    sp   : integer;          // stack pointer
    cell: array of int32;   // a standard forth term
    constructor init( len:word );
    procedure dup;
    procedure swap;
    procedure drop;
    procedure overflow;
    procedure underflow;
    procedure dump;
    procedure push( v: int32 );
    function tos: int32;
    function nos: int32;
    function pop: int32;
  end;
  
  function stack.tos : int32;
  begin
    result := cell[ sp ];
  end;
  function stack.nos : int32;
  begin
    result := cell[ sp - 1 ];
  end;
  procedure stack.dup;
  begin
    push( tos );
  end;
  procedure stack.drop;
  begin
    dec( sp );
    if sp < 0 then underflow;
  end;
  procedure stack.swap;
    var t : int32;
  begin
    if sp >= 2 then
      begin
        t := tos;
        cell[ sp ] := nos;
        cell[ sp - 1 ] := t;
      end
    else underflow;
  end;
  procedure stack.push( v : int32 );
  begin
    inc( sp );
    if sp >= length( cell ) then overflow
    else cell[ sp ] := v;
  end;
  function stack.pop : int32;
  begin
    result := tos;
    drop;
  end;
  procedure stack.underflow;
  begin
    writeln( 'warning: stack underflow' );
    sp := 0;
  end;
  procedure stack.overflow;
  begin
    writeln( 'warning: stack overflow' );
    sp := length( cell ) - 1;
  end;
  procedure stack.dump;
    var s: string;
    var i: int32;
  begin
    if sp > 0 then
      for i := 1 to sp  do
        begin
          str( cell[ i ], s );
          write( s, ' ' );
        end;
    writeln;
  end;
  constructor stack.init( len: word );
  begin
    sp := 0;
    setlength( cell, len );
  end;
  
  type ngarovm = object
    ip    : integer;
    port  : array [ 0 .. 16 ] of int32;
    data, addr : stack;
    
    const ram : array [ 0 .. 25 ] of int32 = (
    
        // set color to red : 4 1 6 out wait ; 
        oLIT, 4,                    //  0  1
        oLIT, 1,                    //  2  3
        oLIT, 6,                    //  4  5
        oOUT, oWAIT,                //  6  7
              
        // draw a pixel
        // : 320 200 2 6 out wait ;
        oLIT, 320,                  //  8  9
        oLIT, 200,                  // 10 11
        oLIT, 2,                    // 12 13
        oLIT, 6,                    // 14 15
        oOUT, oWAIT,                // 16 17
    
        // write 0 to port 3 to force video update. 
        oLIT, 0,                    // 18 19
        oLIT, 3,                    // 20 21
        oOUT, oNOP,                 // 22 23
    
        oNOP, oNOP                  // 24 25
    
    );
    
    constructor init;
    procedure loop;
    procedure dump;
    procedure runop( op:int32 );
    procedure runio;
  end;
  
  
  procedure ngarovm.dump;
  var i: int32;
    var s: string[ 4 ];
  begin
    crt.clrscr;
    
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
      write( mnemonic[ ram[ i ]] );
      if ram[ i ] in [ oLIT, oLOOP, oJMP, oJGT, oJLT, oJNE, oJEQ ] then
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
  procedure ngarovm.loop( );
  begin
    repeat 
      dump;
      runop( ram[ ip ] );
      inc( ip );
    until ip >= length( ram );
  end;
  
  procedure ngarovm.runop( op: int32 );
    var t, n, a : int32;
  begin
    case op of
      oNOP : ; { do nothing }
      oDUP : data.push( data.tos );
      oDROP: data.drop;
      oSWAP: data.swap;
      oPUSH: addr.push( data.pop );
      oPOP : data.push( addr.pop );
      oLOOP: ;
      oJMP : ;
      oRET : ;
      oJLT : ;
      oJGT : ;
      oJNE : ;
      oJEQ : ;
      oZEX : ;
      oLIT: begin
              inc( ip );
              data.push( ram[ ip ]);
            end;
      oLOD: begin { FETCH }
              data.cell[ data.sp ] := ram[ data.tos ];
            end;
      oSTO: begin { STORE : (na-) - put nos into ram at tos }
              ram[ data.tos ] := ram[ data.nos ];
              data.drop; 
              data.drop;
            end;
      oADD : data.push( data.pop + data.pop );
      oSUB : data.push( -data.pop + data.pop );
      oMUL : data.push( -data.pop + data.pop );
      oDIVM: begin
               t := data.pop; 
               n := data.pop; 
               data.push( n div t ); 
               data.push( n mod t ); 
             end;
      oAND : data.push( data.pop AND data.pop );
      oOR  : data.push( data.pop OR data.pop );
      oXOR : data.push( data.pop XOR data.pop );
      oSHL : begin 
               t := data.pop; 
               data.push( data.pop shl t );
             end;
      oSHR : begin
               t := data.pop; 
               data.push( data.pop shr t );
             end;
      oINC : inc( data.cell[ data.sp ] );
      oDEC : dec( data.cell[ data.sp ] );
      oIN  : begin { p-n }
               t := data.pop;
               data.push( port[ t ] );
               port[ t ] := 0;
             end;
      oOUT : begin { np- }
               port[ data.pop ] := data.pop;
             end;
      oWAIT: begin { - }
               runio;
             end;
    else
      writeln( 'don''t know how to handle op:', op );
      readln;
      
      { ... }
      
    end;
  end;
  
  procedure ngarovm.runio;
    var p: int32;
  begin
    for p in port do port[ p ] := 0;
    writeln( 'io' );
  end;
  
  
  constructor ngarovm.init;
  begin
    data.init( 32 );
    addr.init( 32 );
    ip := 0;
  end;
  

var vm : ngarovm;
begin
  vm.init();
  vm.loop();
end.
