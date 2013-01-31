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
    triangle : TTriangle =
	      (( x : 50;  y: 100),
	       ( x : 100; y: 100),
	       ( x : 150; y: 150));

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
    canvas[ x, y ] := color
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

  procedure line( const x1, y1, x2, y2 : integer );
    var i : integer;
  begin
    assert( y1 = y2, 'horizontal lines only. sorry.' );
    assert( x1 <= x2, 'line must go left to right.' );
    for i := x1 to x2 do putpixel( i, y1, penColor );
  end;

  procedure fill( a, b : integer; c: byte );
    var
      r	       : array[ 1 .. 100 ] of TPoint2D;
      back     : TColor;
      rcounter : byte;
      up, down : boolean;
	       
    procedure checkup;
    begin
      if (b < 0) then exit;
      if (getpixel( a, b - 1 ) = back) then
	if not up then begin
	  up   := true;
	  inc( rcounter );
	  with r[rcounter] do begin
	    x := a;
	    y := b-1;
	  end
	end else up := true
      else up := false;
    end; { checkup }
	       
    procedure checkdown;
    begin      
      if (b > getmaxy) then exit;
      if (getpixel( a, b + 1 ) = back) then
	if not down then begin
	  down := true;
	  inc( rcounter );
	  with r[rcounter] do begin
	    x  := a;
	    y  := b+1;
	  end
	end
	else down := true
      else down := false;
    end; { checkdown }

    procedure fillone;
      var x, x1: integer;
    begin
      if getpixel( a, b ) <> back then exit;
      x := a;
      up := false; down := false;
      while (a <= getmaxx) and (getpixel( a, b) = back) do
      begin
	checkup;
	checkdown;
	inc( a );
      end;
      x1 := a-1;
      a := x-1;
      while (a >= 0) and (getpixel( a, b) = back) do
      begin
	checkup;
	checkdown;
	dec( a );
      end;
      line( a+1, b, x1, b );
    end; { fillone }

  begin
    back := getpixel( a, b );
    setcolor( c );
    if (a < 0) or (a > getmaxx) or
       (b < 0) or (b > getmaxy) or
       (back = c)
    then exit
    else begin
      rcounter := 0;
      fillone;
      while rcounter <> 0 do
      begin
	a := r[ rcounter ].x;
	b := r[ rcounter ].y;
	rcounter := rcounter-1;
	fillone;
      end
    end
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

begin
  crt.clrscr;
  //forpixels( @rand );
  forpixels( @clear );
  putpixel( 10, 10, red );
  setcolor( white );
  // fill( 50, 50, 15 );
  forpixels( @show );
end.
