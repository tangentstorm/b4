{$i test_gen.def }
implementation
uses tpl_gen, classes, sysutils, StreamIO;

  var
    stream : TStringStream;
    pipe   : Text;

  procedure setup;
  begin
    if Assigned(stream) then stream.Free;
    stream := TStringStream.Create('');
    AssignStream( pipe, stream );
    Rewrite( pipe );
  end;

  { String nodes should just render their contents. }
  procedure test_string;
  begin
    with IOutput(TString.Create( 'hello world' )) do
      Render( nil, pipe );
    chk.equal( stream.datastring, 'hello world' );
  end;

  { Block nodes recursively render their children. }
  procedure test_block;
  begin
    with IBlock(TBlock.Create) do
      Append( TString.Create( '[' ))
      .Append( TBlock.Create
	      .Append( TString.Create( 'a' ))
	      .Append( TString.Create( 'b' ))
	      .Append( TString.Create( 'c' )))
      .Append( TString.Create( ']' ))
      .Render( nil, pipe );
    chk.equal( stream.datastring, '[abc]');
  end;

end.
