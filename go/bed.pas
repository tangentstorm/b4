{
| b4 editor
|
|   a colorforth - style editor
|
| -------------------------------------------------
| copyright (c) 2012 michal j. wallace
| see LICENSE.org for usage information
}

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

    crt.kbCR   : begin
                   gx := 1;
                   inc( gy );                   
                 end;
    
    crt.kbBkSp : begin
                   dec( gx );
                   crt.gotoxy( gx, gy );
                   crt.writechar( ' ' );
                 end;
    
    crt.kbf1   : crt.textcolor( crt.blue );
    crt.kbf2   : crt.textcolor( crt.green );
    crt.kbf3   : crt.textcolor( crt.cyan );
    crt.kbf4   : crt.textcolor( crt.red );
    crt.kbf5   : crt.textcolor( crt.magenta );
    crt.kbf6   : crt.textcolor( crt.brown );
    crt.kbf7   : crt.textcolor( crt.lightgray );
  else
    begin
      crt.writechar( crt.key2char( key ));
      inc( gx );
      crt.writestring( '     ', 1 , 65 );
      crt.writestring( int2str( key ), 1, 65 );
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
    crt.gotoxy( 1, y);
    crt.clreol;
  end;
end;


begin
  crt.SetCRTUpdate( crt.UpdateAlways );
  
  load( gbuf );
  gx := 1; gy := 1;
  repeat
     crt.gotoxy( gx, gy );
     if crt.keypressed then handle_key;
   until gdone;
end.
