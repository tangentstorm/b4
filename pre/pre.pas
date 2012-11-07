// pre - pattern recognition engine
{$i xpc.inc }
unit pre;
interface uses xpc;

  type charset = set of Char;
  type pattern = class
    procedure match( var s : string );
  end;

  { primitives }
  function nul : pattern;
  function err( msg : string ): pattern;
  function chr( const c  : char ) : pattern;
  function str( const s  : string ) : pattern;
  function any( const cs : charset ) : pattern;

  { recursion support }
  function def( const iden : string; p : pattern ) : pattern;
  function sub( const iden : string ) : pattern;

  { multiple grammars }
  procedure new( const iden : string );
  procedure use( const iden : string );

  { combinators }
  function alt( const pats : array of pattern ) : pattern;
  function seq( const pats : array of pattern ) : pattern;
  function rep( const pat : pattern ) : pattern; // kleene +
  function opt( const pat : pattern ) : pattern; // kleene ?
  function orp( const pat : pattern ) : pattern; // kleene *


implementation

  {  TODO : $i pre_gen.pas }

begin
end.
