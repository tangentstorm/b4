{$i test_org.def }
implementation uses org;

  var
    doc	: org_node;


  procedure test_parse;
  begin
    assign( input, 'test.org');
    reset( input );
    chk.that( org.parse( input, doc ),
	      'failed to parse test.org' );
  end;

end.
