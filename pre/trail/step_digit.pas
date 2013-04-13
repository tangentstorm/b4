{$i xpc.inc}
program step_digit;
uses parsetrail;

  const
    digits : set of char = [ '0'..'9' ];

  function match_digit( s : string ) : boolean;
  begin
    result := s[ 0 ] in digits;
  end;
  
begin
  parsetrail.match_loop( @match_digit );
end.
