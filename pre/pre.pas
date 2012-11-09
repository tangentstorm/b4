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
  function wld : pattern;
  function err( msg : string ): pattern;
  function lit( const c  : char ) : pattern;
  function str( const s  : string ) : pattern;
  function any( const cs : charset ) : pattern;

  { recursion support }
  function def( const iden : string ; p : pattern ) : pattern;
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

  procedure new_grammar( const name : string ); begin end;
  procedure end_grammar; begin end;

begin

  new_grammar( 'ebnf' );
  def( 'syntax',  orp( sub( 'rule' )));
  def( 'rule',    seq((sub( 'iden' ), lit( '=' ), sub( 'expr' ), lit( '.' ))));
  def( 'expr',    seq((sub( 'term' ), orp((lit( '|' ), sub( 'term' ))))));
  def( 'term',    rep( 'factor'));
  def( 'factor',  alt((sub( 'ident' ),
		       sub( 'string' ),
		       sub( 'braces' ),
		       sub( 'bracks' ),
		       sub( 'parens' ))));
  def( 'iden',   seq( sub( 'alpha' )));
  def( 'strn',     seq( lit( '"' ), orp( wld ), lit( '"' ));
  def( 'alph',   any([ 'a'..'z', 'A'..'Z' ]);
  def( 'numb',   any([ '0'..'9' ]);
  end_grammar;

end.
