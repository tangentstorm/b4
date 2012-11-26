{$i test_pre.def }
implementation uses pre;

  procedure test_nul;
  begin
    chk.that( nul.matches( '<anything>' ),
	     'nul should always match.' );
  end;

  procedure test_lit;
  begin
    chk.that( lit( 'a' ).matches( 'apple' ),
	     'lit.match false negative' );
    chk.that( not lit( 'a' ).matches( 'banana' ),
	     'lit.match false positive' );
  end;


end.
