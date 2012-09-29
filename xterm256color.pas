{ mjw ported from 256colors2.pl : }
// Author: Todd Larason <jtl@molehill.org>
// $XFree86: xc/programs/xterm/vttests/256colors2.pl,v 1.2 2002/03/26 01:46:43 dickey Exp $

{$i xpc.inc }
program xterm256color;
  uses xpc, kvm;

  procedure resetcolor;
  begin
    write( #27, '[0m' )
  end;

  function hexbyte( b : byte ) : string;
  begin
    result := pad( hex( b ), 2, '0' )
  end;

  { the original looked a lot nicer because it used two spaces of background
    and thus showed solid blocks... but: i was testing both foreground and   
    background, and the brackets still kind of look cool. :) }
  procedure showcolor( color :  byte );
  begin
    resetcolor;
    kvm.fg( color );
    write('[');
    kvm.bg( color );
    kvm.fg( 0 );
    write(']');
  end;
  
  var red, green, blue, gray, color, level : byte;

begin
  
  { -- looks like he's configuring the palette here --------------------- }
  
  // use the resources for colors 0-15 - usually more-or-less a
  // reproduction of the standard ANSI colors, but possibly more
  // pleasing shades
  for red := 0 to 5 do
    for green := 0 to 5 do
      for blue := 0 to 5 do
      begin
	write( #27, ']4;', 16 + (red * 36) + (green * 6) + blue, 'rgb:' );
	if red <> 0 then write( hexbyte( red * 40 + 55 )) else write( '00' );
	write('/');
	if green <> 0 then write( hexbyte( green * 40 + 55 )) else write( '00' );
	write('/');
	if blue <> 0 then write( hexbyte( blue * 40 + 55 )) else write( '00' );
	write( #27, '\' );
      end;
  
  // colors 232-255 are a grayscale ramp, intentionally leaving out
  // black and white
  for gray := 0 to 23 do
  begin
    level := (gray * 10) + 8;
    write( #27, ']4;', 232 + gray, 'rgb:' );
    write( hexbyte( level ), '/', hexbyte( level ), '/', hexbyte( level ));
    write( #27, '\' );
  end;


  // display the colors
  { by drawing two space characters, with different background colors each time }
  
  // first the system ones:
  writeln( 'System colors:');
  for color := 0 to 7 do showcolor( color );

  resetcolor;
  writeln;

  for color := 8 to 15 do showcolor( color );
  resetcolor;
  writeln;

  // now the color cube
  writeln( 'Color cube, 6x6x6:' );
  for green := 0 to 5 do begin
    for red := 0 to 5 do begin
      for blue := 0 to 5 do begin
	showcolor( 16 + (red * 36) + (green * 6) + blue )
      end;
      resetcolor; { to draw the spaces between layers of the cube }
      write('  ');
    end;
    writeln;
  end;
  
  // now the grayscale ramp
  writeln( 'Grayscale ramp:' );
  for color := 232 to 255 do showcolor( color );

  resetcolor;
  writeln;

end.