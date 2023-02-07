{$i xpc.inc }
program grin;
uses xpc, pre, stacks;

  type
    FileSource = class ( pre.Source )
      constructor create( path : string );
      procedure next( var ch : char ); override;
      procedure mark( var mk : Marker ); override;
      procedure back( var mk : Marker ); override;
      procedure keep;
    private
      hnd : file of char;
    end;

  constructor filesource.create( path : string );
  begin
  end;

  procedure filesource.next( var ch : char );
  begin
    read( self.hnd, ch );
  end;

  procedure filesource.mark( var mk : Marker );
  begin

  end;

  procedure filesource.back( var mk : Marker );
  begin
  end;

  procedure filesource.keep;
  begin
  end;


  procedure dumpAST( m :  matcher );
  begin
  end;


var
  m : matcher;
  f : filesource;
begin
  m := matcher.create;
  f := filesource.create( 'oberon0.ebnf' );
  dumpAST( m );
end.
