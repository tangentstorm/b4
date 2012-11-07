// pre : pattern recognition engine
{$i xpc.inc }
unit pre;
interface uses xpc;


  type
    charset = set of Char;
    pattern = class
      procedure match( var s : string ); virtual; abstract;
    end;
    patlist = array of pattern;

  { primitives }
  function nul : pattern;
  function err( msg : string ): pattern;
  function chr( const c  : char ) : pattern;
  function str( const s  : string ) : pattern;
  function any( const cs : charset ) : pattern;

  { recursion support }
  function def( const iden : string; p : pattern ) : pattern;
  function sub( const iden : string ) : pattern;

  { combinators }
  function alt( const pats : patlist ) : pattern;
  function seq( const pats : patlist ) : pattern;
  function rep( const pat : pattern ) : pattern; // kleene +
  function opt( const pat : pattern ) : pattern; // kleene ?
  function orp( const pat : pattern ) : pattern; // kleene *

  { support for multiple grammars }
  //  todo : procedure new( const iden : string );
  //  todo : procedure use( const iden : string );


implementation

  {$i pre_gen.pas}
  {$i pre_match.pas}

begin
end.
