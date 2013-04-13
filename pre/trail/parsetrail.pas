{$i xpc.inc}
unit parsetrail;
interface

  type matchfunc = function ( s : string ) : boolean;

  procedure match_loop( match : matchfunc );

implementation

  procedure match_loop( match : matchfunc );
    var s : string;
  begin
    repeat
      readln( s );
      if match( s ) then writeln( 'ok' )
      else writeln( 'error' );
    until eof
  end;

begin
end.
