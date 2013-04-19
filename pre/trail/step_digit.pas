{$i xpc.inc}
program step_digit;
uses parsetrail;

const
  digits : set of char = [ '0'..'9' ];

function match_digit( s : string ) : boolean;
begin
  result := (s[ 1 ] in digits) and (length( s ) = 1);
end;
  
begin
  parsetrail.match_loop( @match_digit );
end.
