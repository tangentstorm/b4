{

  Simple stack objects.
  author  : michal j wallace
  license : MIT / ngaro
  
}
{$i xpc.inc }
unit stacks;
interface uses xpc;

  type stack = object
    sp   : integer;           // stack pointer
    cell : array of int32;    // a standard forth term
    constructor init( len:word );
    procedure push( v: int32 );
    function pop: int32;
    procedure push2( a, b : int32 );
    procedure pop2( var a, b :  int32 );
    procedure push3( a, b, c : int32 );
    procedure pop3( var a, b, c : int32 );
    function tos: int32;
    function nos: int32;
    procedure dup;
    procedure swap;
    procedure drop;
    procedure overflow;
    procedure underflow;
    procedure dump;
  end;

implementation

  constructor stack.init( len: word );
  begin
    sp := 0;
    setlength( cell, len );
  end; { stack.init }

  procedure stack.push( v : int32 );
  begin
    inc( sp );
    if sp >= length( cell ) then overflow
    else cell[ sp ] := v;
  end; { stack.push }

  function stack.pop : int32;
  begin
    result := tos;
    drop;
  end; { stack.pop }

  procedure stack.push2( a, b :  int32 );
  begin
    self.push( a );
    self.push( b );
  end; { stack.push2 }
  
  procedure stack.pop2( var a, b :  int32 );
  begin
    a := self.pop;
    b := self.pop;
  end; { stack.pop2 }

  procedure stack.push3( a, b, c :  int32 );
  begin
    self.push( a );
    self.push( b );
    self.push( c );
  end; { stack.push3 }
  
  procedure stack.pop3( var a, b, c :  int32 );
  begin
    a := self.pop;
    b := self.pop;
    c := self.pop
  end; { stack.pop3 }
  
  function stack.tos : int32;
  begin
    result := cell[ sp ];
  end; { stack.tos }

  function stack.nos : int32;
  begin
    result := cell[ sp - 1 ];
  end; { stack.nos }

  procedure stack.dup;
  begin
    push( tos );
  end; { stack.dup }

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
  end; { stack.swap }

  procedure stack.drop;
  begin
    dec( sp );
    if sp < 0 then underflow;
  end; { stack.drop }

  procedure stack.overflow;
  begin
    writeln( 'warning: stack overflow' );
    sp := length( cell ) - 1;
  end; { stack.overflow }

  procedure stack.underflow;
  begin
    writeln( 'warning: stack underflow' );
    sp := 0;
  end; { stack.underflow }


  { ! This might be better off in a debug.pas ? }
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
  end; { stack.dump }

end.
