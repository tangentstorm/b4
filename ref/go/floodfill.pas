{ pascal implementation of a floodfill algorithm.
----------------------------------------------------------
  The date on this file was 5/31/1994. Given the formatting
  and the use of (a,b) for coordinates in the original, I
  undoubtably typed this by hand.

  It was probably translated from a book on graphics
  programming ( "tricks of the graphics gurus" perhaps? )
  or taken from a textfile or magazine article on the
  subject. In any case, the floodfill algorithm is public
  domain, and as far as I am concerned, so is this
  implementataion.

  --mjw, cleaning it up 1/31/2013
  for translation to retroforth for minesweep.rx
---------------------------------------------------------- }

program floodfill;
uses crt, math;

  type
    TPoint2D  = record
		  x , y	: integer
		end;
    TTriangle = array [ 0..2 ] of TPoint2D;
    TColor    = byte; { just for testing }
    TCanvas   = array [ 0..639, 0..479 ] of TColor;

  var
    canvas   : TCanvas;
    penColor : TColor;

  { --- a very simple abstract canvas ---- }
  { this could eventually become an object, but mostly I'm just
    stubbing out the things that depend on the graph unit. }

  const
    getmaxx = 320-1;
    getmaxy = 200-1;
    black   = 0;
    white   = 15;

  function getpixel( const x, y	: integer ) : TColor;
  begin
    getpixel := canvas[ x, y ];
  end;

  procedure putpixel( const x, y : integer; const color : TColor );
  begin
    if ( x >= 0 ) and ( x <= getmaxx ) and
       ( y >= 0 ) and ( y <= getmaxy )
    then canvas[ x, y ] := color
  end; { putpixel }

  procedure setcolor( const color : TColor );
  begin
    penColor := color
  end;

  procedure assert( fact : boolean; message : string );
  begin
    if fact <> true then begin
      writeln( message );
      halt;
    end;
  end; { assert }

  procedure line( x1, y1, x2, y2 : integer );
    var x, y, oldy, dy : integer; m, b : real;
  begin
    { special cases for vertical and horizontal lines. vertical check is
      critical to avoid division by 0. horizontal is just an optimization. }
    if x1 = x2 then for y := y1 to y2 do putpixel( x1, y, penColor )
    else if y1 = y2 then for x := x1 to x2 do putpixel( x, y1, penColor )
    else if x1 > x2 then line( x2, y2, x1, y1 ) { always go left to right }
    else begin

      {$ifdef debug} crt.textColor( white );
      writeln('( ', x1:2, ', ', y1:2, ' ) -> ', '( ', x2:2, ', ', y2:2, ' ) ' );
      {$endif}

      { slope is dy-dx ( "rise over run" ) }
      m := ( y2 - y1 ) / ( x2 - x1 );

      {$ifdef debug} crt.textColor( green );
      writeln( 'm := ( ', y2, ' - ', y1, ') / ( ', x2, ' - ', x1, ' )' );
      writeln( 'm := ', m:4:2 );
      {$endif}

      { now calculate the base:
	-----------------------------------------
	line formula:  y  = m * x  + b
	therefore:     y1 = m * x1 + b
		  -b + y1 = m * x1
                      -b  = m * x1 - y1
      }                b := -((m * x1) - y1);
      {$ifdef debug}
      crt.textColor( magenta );
      writeln( 'b := ', y1, ' -( ', m, ' * ', x1, ' )' );
      writeln( 'b := ', floor( b ));
      {$endif}

      oldy := floor( m * x1 + b );
      assert( oldy=y1, 'self-check failed' );

      for x := x1 to x2 do begin
	y := floor( m * x + b );
	if abs( y - oldy ) <= 1
	  then putpixel( x, y, penColor )
	  else line( x, y, x, oldy - sign( oldy-y)  );
	oldy := y;
      end
    end
  end;

  procedure fill( a, b : integer; c: TColor );
    var bg : TColor;

    function needsfill( x, y : integer ) : boolean;
    begin
      needsfill := ( x >= 0 ) and ( x <= getmaxx ) and
		   ( y >= 0 ) and ( y <= getmaxy ) and
		   ( getpixel( x, y ) = bg )
    end;

    procedure flood( x, y : integer );
    begin
      if needsfill( x, y ) then
      begin
	putpixel( x, y, c );
	flood( x, y - 1 ); // north
	flood( x, y + 1 ); // south
	flood( x + 1, y ); // east
	flood( x - 1, y ); // west
      end;
    end;

  begin
    bg := getpixel( a, b );
    flood( a, b );
  end;

  type
    pixelproc = procedure(const x, y : integer;
			  const c    : TColor );
  procedure forpixels( callback : pixelproc );
    var x, y, x1, y1, x2, y2 : integer;
  begin
    { crt's coordinates start at 1, per ansi, so i'm
      just ignoring the leftmost column and topmost
      row of pixels }
    x1 := 1;
    y1 := 1;

    x2 := min( getmaxx, crt.windmaxx );
    y2 := min( getmaxy, crt.windmaxy -3 ); { so it doesn't scroll }

    for y := 1 to y2 do
      for x := x1 to x2 do
	callback( x, y, getpixel( x, y ));
  end;

  procedure rand( const x, y : integer; const c : TColor );
  begin
    putpixel( x, y, floor( random * 256 ))
  end;

  procedure clear( const x, y : integer; const c : TColor );
  begin
    putpixel( x, y, black )
  end;

  { this just renders the "pixels" as characters in the terminal }
  procedure show( const x, y : integer; const c : TColor );
  begin
    crt.textcolor( c mod $F );
    crt.gotoxy( x, y );
    write( '0' );
  end;

  function rnd( x : integer ) : integer;
  begin
    rnd := floor( random * x )
  end;

  { bisect the screen with various lines }
  procedure randlines;
    var i : byte;
  begin
    for i := 0 to 255 do begin
      setcolor( i mod $e + 1 );
      if odd( i )
	then line( 0, rnd( crt.windmaxy ), crt.windmaxx, rnd( crt.windmaxy ))
      else line( rnd( crt.windmaxx ), 0, rnd( crt.windmaxx ), crt.windmaxy )
    end
  end; { randlines }


  procedure triangle( abc : TTriangle );
  var i : byte; p1, p2 : TPoint2D;
  const
    colors : array[ 0..2 ] of byte
	      = ( red, green, blue );
  begin
    p2 := abc[ 2 ];     { leading point }
    for i := 0 to 2 do
    begin
      setcolor( colors[ i ]);
      p1 := p2;         { trailing point}
      p2 := abc[ i ];
      line( p1.x, p1.y, p2.x, p2.y )
    end
  end;

 const
   ABC : TTriangle = // Fact: 93% of all triangles in the US are named ABC.
         (( x : 25; y: 42 ),
	  ( x : 50; y:  3 ),
	  ( x : 75; y: 16 ));

  var p : TPoint2D;
begin
  crt.clrscr;
  randomize;
  forpixels( @clear );

  { various experiments: }
  // forpixels( @rand );
  // randlines;
  // putpixel( 10, 10, red );

  for p in ABC do putpixel( p.x, p.y, yellow );
  triangle( ABC );

  fill( 5, 5, magenta );
  forpixels( @show );
end.
