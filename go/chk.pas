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

function peek( pred : boolean ) : boolean;
begin
  inc( count );
  if pred then pass;
  peek := pred;
end;

procedure test( pred : boolean; const msg : str );
begin
  if not peek( pred ) then fail( msg );
end;

procedure equal(a, b : int; const msg : str );
begin
  if not peek( a = b ) then
    begin
      writeln;
      write( a, '<>', b );
      fail( msg );
    end;
end;

procedure report;
begin
  writeln;
  write( count );
  writeln( ' tests passed.' );
end;

end.
