{$i test_pre.def }
implementation uses pre;

  var pat : pattern;

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


end.
