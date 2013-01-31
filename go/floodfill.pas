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

program fillroutine;
uses
  Graph,bgistuff;
const
  Triangle: array[1..4] of PointType =
   ((x:  50; y: 100),
    (x: 100; y: 100),
    (x: 150; y: 150),
    (x:  50; y: 100));

procedure fill( a, b : integer; c: byte );
 var
  r : array[ 1 .. 100] of pointtype;
  back, rcounter : byte;
  up, down : boolean;
 procedure checkup;
  begin
   if (b < 0) then exit;
   if (getpixel( a, b - 1 ) = back) then
    if not up then
     begin
      up := true;
      inc( rcounter );
      with r[rcounter] do
       begin
        x := a;
        y := b-1;
       end;
     end
    else
     up := true
   else
    up := false;
  end;
 procedure checkdown;
  begin
   if (b > getmaxy) then exit;
   if (getpixel( a, b + 1 ) = back) then
    if not down then
     begin
      down := true;
      inc( rcounter );
      with r[rcounter] do
       begin
        x := a;
        y := b+1;
       end;
     end
    else
     down := true
   else
    down := false;
  end;
 procedure fillone;
  var
   x,x1 : integer;
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
   graph.line( a+1, b, x1, b );
  end;
 begin
  back := getpixel( a, b );
  setcolor( c );
  if
   (a < 0) or (a > getmaxx) or
   (b < 0) or (b > getmaxy) or
   (back = c)
  then
   exit;
  rcounter := 0;
  fillone;
  while rcounter <> 0 do
   begin
    a := r[ rcounter ].x;
    b := r[ rcounter ].y;
    rcounter := rcounter-1;
    fillone;
   end;
 end;

begin
  initgrafx;
  DrawPoly(SizeOf(Triangle) div
           SizeOf(PointType), Triangle);
  setfillstyle( 1, red );
  floodfill(50,50,15);
  fill( 175, 105, blue );
  ReadLn;
  CloseGraph;
end.
