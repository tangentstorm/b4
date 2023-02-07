{ 0305.2013 : lexer-generator prototype
  was going to use this to generate something like ../ref/dfa-fpc.txt
  but then realized this is not top priority. :) }

// generates pascal code for a lexer
// input must be a sorted list of tokens
program lexer( input, output );
  
  var
    i, j   : integer;
    tokens : array of string;
    line,  
    last   : string;
    stack  : array of integer;

  procedure die( msg : string );
  begin writeln( msg ); halt;
  end;

begin

  setlength( tokens, 1024 );
  setlength( stack, 64 );

  // load the array
  i := 0;
  while not eof do begin
    readln( tokens[ i ]);
    inc( i );
  end;
  
  // double check that they're sorted
  for j := i-1 downto 1 do
    if tokens[ j ] <= tokens[ j - 1 ] then
      die( 'input must be sorted. try `cat tokens.txt | sort | lexer.pas`' );

end.
