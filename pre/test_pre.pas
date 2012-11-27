{$i test_pre.def }
implementation uses pre;

  var pat : pattern;

  procedure test_nul;
  begin
    chk.that( nul.matches( '<anything>' ),
	     'nul should always match.' );
  end;

  procedure test_lit;
  begin
    pat := lit( 'a' );
    chk.that( pat.matches( 'apple' ), 'lit.match false negative' );
    chk.that( not pat.matches( 'banana' ), 'lit.match false positive' );
  end;

  procedure test_any;
  begin
    pat := any([ 'a', 'b', 'c']);
    chk.that( pat.matches( 'apple' ), 'any.match false negative: apple' );
    chk.that( pat.matches( 'banana' ), 'any.match false negative: apple' );
    chk.that( pat.matches( 'cherry' ), 'any.match false negative: apple' );
    chk.that( not pat.matches( 'durian' ), 'lit.match false positive' );
  end;


end.
