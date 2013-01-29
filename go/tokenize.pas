{$mode objfpc}
program tokens( input, output );

  function next( out ch : char ) : char;
  begin
    read( ch );
    result := ch;
  end;

  var
    ch	: char;
    tok	: string;
  type
    charset = set of char;
    onlast  = ( keep, drop );

  function scan( stop : charset; action : onlast ) : string;
  begin
    result := '';
    repeat result := result + ch;
    until next( ch ) in stop;
    if action = keep then result := result + ch;
    writeln( result );
  end;

begin
  repeat
    case next( ch ) of
      #0 .. #32 : ;
      '(' : scan([ ')' ], keep );
      '"' : scan([ '"' ], keep );
      otherwise scan([ #0 .. #32 ], drop )
    end
  until eof
end.
