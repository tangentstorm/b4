{$i test_pre.def }
implementation uses pre;

  var tok : token;

  procedure setup;
  begin
    tok.create;
  end;

  procedure teardown;
  begin
    tok.create;
  end;


  procedure test_nul;
  begin
    chk.that( nul.matches( '' ));
  end;

  procedure test_lit;
  begin
    chk.that( rx( 'a' ).matches( 'apple' ));
    chk.that( not rx( 'a' ).matches( 'banana' ));
  end;


end.
