{
| b4 editor
|
|   a colorforth - style editor
|
| -------------------------------------------------
| copyright (c) 2012 michal j. wallace
| see LICENSE.org for usage information
}
{$gnu-pascal}
program bed;
uses crt, gpcutil;

const
  kw = 64;
  kh = 32;
  ks = kw * kh;

var
  gbuf  : array [ 0 .. ks ] of char;
  gx, gy : byte;
  gdone : boolean;

procedure handle_key;
  var key : crt.tkey;
begin
  key := crt.readkeyword;
  case key of
    
    crt.kbLEFT : dec( gx );
    crt.kbUP   : dec( gy );
    crt.kbDOWN : inc( gy );
    crt.kbRIGHT: inc( gx );
    crt.kbESC  : gdone := true;

    13         : begin
                   gx := 1;
                   inc( gy );                   
                 end;
    
    { the 16x make option-x act like fx on OSX panther }
    crt.kbf1, 161   : crt.textcolor( crt.blue );
    crt.kbf2, 162   : crt.textcolor( crt.green );
    crt.kbf3, 163   : crt.textcolor( crt.cyan );
    crt.kbf4, 164   : crt.textcolor( crt.red );
    crt.kbf5, 165   : crt.textcolor( crt.magenta );
    crt.kbf6, 166   : crt.textcolor( crt.brown );
    crt.kbf7, 167   : crt.textcolor( crt.lightgray );
  else
    begin
      crt.writechar( crt.key2char( key ));
      inc( gx );
      crt.writestring( '     ', 1 , 1 );
      crt.writestring( int2str( key ), 1 , 1 );
    end;
  end;
  
  if crt.gettextcolor = crt.black
  then crt.textbackground( crt.lightgray )
  else crt.textbackground( crt.black );
  
  gx := min( kw, max( gx, 1 ));
  gy := min( kh, max( gy, 1 ));
end;

procedure load( buf : array of char );
  var x, y : byte;
begin
  crt.textbackground( crt.black );
  crt.textcolor( crt.lightgray );
  for y := 0 to kh do
  begin
    crt.gotoxy( 4, y + 2 );
    for x := 0 to kw do
    begin
      write( ' ' );
    end;
  end;
end;

procedure banner;
begin
  crt.gotoxy( 1, 1 );
  crt.textbackground( crt.blue );
  crt.clrscr;
  crt.textbackground( crt.lightgray );
  crt.clreol;
end;


begin
  crt.SetCRTUpdate( crt.UpdateAlways );
  banner();
  
  load( gbuf );
  gx := 1; gy := 1;
  repeat
     crt.gotoxy( gx +3, gy+1 );
     if crt.keypressed then handle_key;
   until gdone;
end.
