{$i xpc.inc }
unit xpc; { cross-platform compilation help }
interface

  type set32 = set of 0 .. 31;
  type int32 = longint;
  function toint( s : set32 ) : int32;
  function hex( x :  int32 ) : string;
  function min( a, b : int32 ): int32;
  function max( a, b : int32 ): int32;
		     
  type logger = object
    procedure debug( args : array of const );
  end;
  var log : logger;

implementation

  function toint( s : set32 ) : int32;
    var i, p : byte;
  begin
    result := 0;
    p := 1;
    for i := 0 to 31 do begin
      if i in s then result := result + p;
      p := p * 2;
    end;
  end; { toint }


  procedure logger.debug( args :  array of const );
    var i : integer;
  begin
    write( '(DEBUG: ' );
    for i := 0 to length( args ) - 1 do
    begin
      case args[ i ].vtype of
	vtinteger : write( args[ i ].vinteger );
	vtstring  : write( args[ i ].vstring^ );
	vtansistring  : write( ansistring( args[ i ].vansistring ));
	else
	  write( '??' );
      end; { case }
    end;
    writeln( ')' );
  end;


  function hex( x : int32 ) : string;
    const digits = '0123456789ABCDEF';
      len	=  length( digits );
    var i : int32;
  begin
    result := '0x00000000';
    for i := 0 to 8 do
    begin
      result[ 11 - i ] := digits[ (( x shr i ) mod 16 ) + 1 ];
    end;
  end;


  function min( a, b :  int32 ) : int32;
  begin
    if a < b then result := a else result := b;
  end;

  function max( a, b :  int32 ) : int32;
  begin
    if a > b then result := a else result := b;
  end;
		     
end.		     
