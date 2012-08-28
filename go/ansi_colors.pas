program ansi_colors;

type TColor = ( k, r, g, y, b, m, c, w );

procedure fg( color : TColor );
begin
  write( #27, '[00;3' );
  write( ord( color ));
  write( 'm' );
end; { fg }

procedure hi( color : TColor );
{ high intensity foreground }
begin
  write( #27, '[01;3' );
  write( ord( color ));
  write( 'm' );
end; { fg }

procedure colors;
begin
  fg( k ); write( 'k' );
  fg( r ); write( 'r' );
  fg( g ); write( 'g' );
  fg( y ); write( 'y' );
  fg( b ); write( 'b' );
  fg( m ); write( 'm' );
  fg( c ); write( 'c' );
  fg( w ); write( 'w' );

  hi( k ); write( 'K' );
  hi( r ); write( 'R' );
  hi( g ); write( 'G' );
  hi( y ); write( 'Y' );
  hi( b ); write( 'B' );
  hi( m ); write( 'M' );
  hi( c ); write( 'C' );
  hi( w ); write( 'W' );

  fg( w ); writeln;
  
end; { colors }

begin
  colors;
end. { b4i }

