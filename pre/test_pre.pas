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


end.
