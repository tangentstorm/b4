
{$mode objfpc}
  
program ngaro;
uses sdl, crt;

type int32 = longint;


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

type hardware = object
  constructor init; 
  function send( msg:int32 ):int32; virtual;
end;
  
type ngarovm = object
  ip     : integer;
  ram    : array [ 0 .. 31 ] of int32;
  port   : array [ 0 .. 15 ] of int32;
  device : array [ 0 .. 15 ] of hardware;
  data, addr : stack;
  
  constructor init;
  procedure tick;
  procedure loop;
  procedure dump;
  procedure runop( op:int32 );
  procedure runio;
end;
  
type ngarovt = object
  constructor init;
end;


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


constructor stack.init( len: word );
begin
  sp := 0;
  setlength( cell, len );
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

procedure stack.overflow;
begin
  writeln( 'warning: stack overflow' );
  sp := length( cell ) - 1;
end;

procedure stack.underflow;
begin
  writeln( 'warning: stack underflow' );
  sp := 0;
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


constructor ngarovm.init;
begin
  data.init( 32 );
  addr.init( 32 );
  ip := 0;
end;
    
procedure ngarovm.loop;
begin
  repeat tick until ip >= length( ram );
end;

procedure ngarovm.tick;
begin
  dump;
  runop( ram[ ip ] );
  inc( ip );
end;

procedure ngarovm.runop( op: int32 );
  var t, n, tmp : int32;
  
  procedure jump();          
  begin 
    ip := ram[ ip + 1 ];
    while ram[ ip ] = ord( oNOP ) do inc( ip );
    dec( ip ); { compensating for the post-op inc }
  end;
  
  procedure tn();
  begin
    t := data.pop;
    n := data.pop;
  end;
begin
  if ( op > oWAIT ) or ( op < oNOP ) then
    
  else
    case op of
      oNOP : { do nothing } ;
      
      oDUP : data.dup;
      oDROP: data.drop;
      oSWAP: data.swap;
      oPUSH: addr.push( data.pop );
      oPOP : data.push( addr.pop );
      
      
      oJMP : jump();
      oJLT : begin tn(); if t <  n then jump else inc( ip ) end;
      oJGT : begin tn(); if t >  n then jump else inc( ip ) end;
      oJNE : begin tn(); if t <> n then jump else inc( ip ) end;
      oJEQ : begin tn(); if t =  n then jump else inc( ip ) end;
      
      oRET : ip := addr.pop;
      
      oLOOP: begin 
               dec( data.cell[ data.sp ] );
               tmp := ram[ ip ];
               if data.cell[ data.sp ] > 0 then 
                 ip := ram[ ip + 1 ]
               else
                 begin
                   inc( ip );
                   data.pop;
                 end
             end;
      
      oZEX : if data.cell[ data.sp ] = 0 then 
               begin
                 { sort of an assert / guard }
                 data.pop; 
                 ip := addr.pop;
               end;
      
      
      oLIT: begin
              inc( ip );
              data.push( ram[ ip ]);
            end;
      oLOD: begin { FETCH }
              data.push( ram[ data.pop ]);
            end;
      oSTO: begin { STORE : (na-) - put nos into ram at tos }
              tn;
              ram[ t ] := ram[ n ];
            end;
      
      
      oADD : data.push(  data.pop + data.pop );
      oSUB : data.push( -data.pop + data.pop );
      oMUL : data.push( -data.pop + data.pop );
      oDIVM: begin
               tn;
               data.push( n mod t ); { yep. mod comes first }
               data.push( n div t ); 
             end;
      oINC : inc( data.cell[ data.sp ] );
      oDEC : dec( data.cell[ data.sp ] );
      
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
      // TODO: assert()
      writeln('error: this should not happen ');
      readln
    end
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
procedure ngarovm.runio; { triggered by the oWAIT op }
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


constructor ngarovt.init;
begin
end;

constructor hardware.init;
begin
end;

  
function hardware.send( msg: int32 ): int32;
begin
  result := 0;
end;


var vm : ngarovm;
var vt : ngarovt;
begin
  vt.init;
  vm.init;
  vm.loop;
end.
