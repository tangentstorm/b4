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
    overflow, underflow : thunk;
    constructor init( len:word );
    procedure push( t : int32 );
    function pop: int32;
    procedure pop1( t : int32 );
    procedure push2( n, t : int32 );
    procedure pop2( var t, n :  int32 );
    procedure push3( x, n, t : int32 );
    procedure pop3( var t, n, x : int32 );
    function tos: int32;
    function nos: int32;
    procedure dup;
    procedure swap;
    procedure drop;
    procedure default_overflow;
    procedure default_underflow;
    function dumps:string;
    procedure dump;
    function limit : int32;
  end;

implementation

  constructor stack.init( len: word );
  begin
    sp := 0;
    setlength( cell, len );
    overflow := @default_overflow;
    underflow := @default_underflow;
  end; { stack.init }

  procedure stack.push( t : int32 );
  begin
    inc( sp );
    if sp >= length( cell ) then overflow
    else cell[ sp ] := t;
  end; { stack.push }
  
  function stack.pop : int32;
  begin
    result := tos;
    drop;
  end; { stack.pop }
  
  procedure stack.pop1( t : int32 );
  begin
    t := pop
  end; { stack.pop1 }

  procedure stack.push2( n, t :  int32 );
  begin
    self.push( n );
    self.push( t );
  end; { stack.push2 }
  
  procedure stack.pop2( var t, n :  int32 );
  begin
    t := self.pop;
    n := self.pop;
  end; { stack.pop2 }

  procedure stack.push3( x, n, t :  int32 );
  begin
    self.push( x );
    self.push( n );
    self.push( t );
  end; { stack.push3 }
  
  procedure stack.pop3( var t, n, x :  int32 );
  begin
    t := self.pop;
    n := self.pop;
    x := self.pop
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

  procedure stack.default_overflow;
  begin
    writeln( 'error: stack overflow' );
    halt
  end;

  procedure stack.default_underflow;
  begin
    writeln( 'error: stack underflow' );
    halt
  end;

  function stack.dumps : string;
    var s: string;
    var i: int32;
  begin
    result := '';
    if sp > 0 then begin
      for i := 1 to sp - 1 do
        begin
          str( cell[ i ], s );
	  result += s + ' ';
	end;
      str( cell[ sp ], s );
      result += s
    end
  end; { stack.dumps }
  
  procedure stack.dump;
  begin
    writeln( dumps );
  end; { stack.dump }

  function stack.limit : int32;
  begin
    result := length( cell );
  end;

end.
