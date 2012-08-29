unit chk;
interface
type int = integer;
type str = string[ 255 ];

procedure fail( const msg : str );
procedure test( pred : boolean; const msg : str );
procedure equal( a, b : int; const msg : str );

implementation

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
end;

procedure equal(a, b : int; const msg : str );
begin
  if a <> b then fail( msg );
end;

end.
