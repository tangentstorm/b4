{$i xpc.inc }
unit xpc; { cross-platform compilation help }
interface

  type set32 = set of 0 .. 31;
  type int32 = longint;
  function toint( s : set32 ) : int32;
  function hex( x :  int32 ) : string;
  function min( a, b : int32 ): int32;
  function max( a, b : int32 ): int32;

 function cLength( s : string ) : byte;                { length - color codes }
 function cstrip( s : string ) : string;
 function normaltext( s : string ) : string;
 function strtrunc( s : string; len : byte ) : string;
 function UpStr( s : string ) : String;
 function DnCase( ch : char ) : Char;
 function DnStr( s : string ) : String;
 function chntimes( c : char; n : byte ) : string;
 function flushrt( s : string; n : byte; ch : char ) : string;
  function padstr( s : string; len : byte; ch : char ) : string;
  function unpadstr( s : string; ch : char ) : string;
  function cpadstr( s : string; len : byte; ch : char ) : string;

  { i don't think the str is necessary }
  function pad( s : string; len : byte; ch : char ) : string;

  
  type logger = object
    procedure debug( args : array of const );
  end;
  var log : logger;

implementation

  {$i xpc.strings.pas }

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
    var i, d : int32; begun :  boolean;
  begin
    result := '';
    begun := false;
    for i := 7 downto 0 do begin
      d := (( x shr ( i * 4 ))  mod 16 );
      if begun or ( d > 0 ) then begin
	result += digits[ d + 1 ];
	begun := true;
      end;
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
