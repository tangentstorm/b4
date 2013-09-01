{$i test_pre.def }
implementation uses pre;

  var pat : IPattern;

  procedure should_match( s : string );
  begin
    chk.that( pat.matches( s ), 'false negative:' + s );
  end;

  procedure should_not_match( s : string );
  begin
    chk.that( not pat.matches( s ), 'false positive:' + s );
  end;

  procedure should_consume( part, whole	: string );
    var m : matcher;
  begin
    m := matcher.create( whole );
    pat.match( m );
    chk.equal( m.consumed, part );
  end;


  procedure test_nul;
  begin
    chk.that( nul.matches( '<anything>' ), 'nul should always match.' );
  end;

  procedure test_sym;
  begin
    pat := sym( 'a' );
    should_match( 'apple' );
    should_not_match( 'banana' );
  end;

  procedure test_any;
  begin
    pat := any([ 'a', 'b', 'c' ]);
    should_match( 'apple' );
    should_match( 'banana' );
    should_match( 'cherry' );
    should_not_match( 'durian' );
  end;

  procedure test_lit;
  begin
    pat := lit( 'app' );
    should_match( 'apple' );
    should_match( 'application' );
    should_not_match( 'apropos' );
    should_not_match( 'bumblebee' );
    should_not_match( ' apple' );
  end;


  procedure test_opt;
  begin
    pat := opt( lit( 'app' ));
    should_consume( 'app', 'apple' );
    should_consume( 'app', 'application' );
    should_consume( '',    'apropos' );
    should_consume( '',    'bumblebee' );
    should_consume( '',    ' apple' );
  end;

  procedure test_rep;
  begin
    pat := rep( lit( 'z' ));
    should_consume( 'z',  'zoo' );
    should_consume( 'zz', 'zzt' );
    should_consume( '',   'abc' );

    pat := rep( any([ '0' .. '9' ]));
    should_consume( '123',  '123abc' );
    should_consume( '',     'abc123' );
  end;


  procedure test_seq;
  begin
    pat := seq([ lit( '0' ), any([ 'x', 'b', 'o' ]) ]);
    should_consume( '0x', '0x' );
    should_consume( '0b', '0bx' );
    should_consume( '',   '0-o' );
  end;

  procedure test_seq_2;
  begin
    pat := seq( ps( 7 ));
    p( lit( 'once' ));
    p( lit( ' ' ));
    p( lit( 'upon' ));
    p( lit( ' ' ));
    p( lit( 'a' ));
    p( lit( ' ' ));
    p( lit( 'time' ));
    should_consume( 'once upon a time', 'once upon a time' );
  end;

  procedure test_alt;
  begin
    pat := alt( ps( 2 ));
    p( lit( 'one' ));
    p( lit( 'red' ));
    should_consume( 'one',  'one fish' );
    should_consume( '',     'two fish' );
    should_consume( 'red',  'red fish' );
    should_consume( '',     'blue fish' );
  end;


  procedure test_sub;
  begin
    def( 'lower', any([ 'a'..'z' ]));
    def( 'digit', any([ '0'..'9' ]));
    def( 'upper', any([ 'A'..'Z' ]));
    def( 'alpha', alt( ps( 2 )));
    p( sub( 'upper' ));
    p( sub( 'lower' ));
    def( 'alphas', rep( sub( 'alpha' )));

    pat := sub( 'alphas' );
    should_consume( 'aBc',  'aBc123' );
    should_consume( '',     '123aBc' );
    should_consume( 'zZ',   'zZ3Zz' );
  end;


  procedure test_hex;
  begin
    // regexp to match a hex number in pascal
    pat := seq( ps( 2 ));
    p( lit( '$' ));
    p( rep( any([ '0' .. '9', 'A'..'F', 'a'..'f' ])));
    should_consume( '',         '1a2b3c' );
    should_consume( '$1a2b3c',  '$1a2b3c' );
  end;


end.
