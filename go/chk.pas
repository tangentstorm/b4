unit chk;
interface
type int = integer;
type str = string[ 255 ];

procedure fail( const msg : str );
procedure test( pred : boolean; const msg : str );
procedure equal( a, b : int; const msg : str );
procedure report;
  
implementation

var count : int = 0;  

procedure pass;
begin
  write ( '.' );
end;

procedure fail ( const msg : str );
begin
  writeln;
  writeln( msg );
  halt;
end;

procedure test( pred : boolean; const msg : str );
begin
  if pred then pass else fail( msg );
  inc( count );
end;

procedure equal(a, b : int; const msg : str );
begin
  test( a = b, msg );
end;

procedure report;
begin
  writeln;
  writeln;
  write( count );
  writeln( ' tests passed.' );
end;

end.
