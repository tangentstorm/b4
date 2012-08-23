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
var
  key : char;
begin
  key := crt.readkey;
  
  case key of
    
    #0 : begin
           { capture key again so we can debug the codes }
           key := crt.readkey;
           case key of
             { option-f1 through option-f5 is all
               i could get to work on panther... }
             #64 : crt.textattr := $07;
             #65 : crt.textcolor( crt.red );
             #66 : crt.textcolor( crt.green );
             #67 : crt.textcolor( crt.yellow );
             #68 : crt.textcolor( crt.cyan );
             crt.ksLEFT : dec( gx );
             crt.ksUP   : dec( gy );
             crt.ksDOWN : inc( gy );
             crt.ksRIGHT: inc( gx );
           else
           end
         end;
    
    crt.chESC  : gdone := true;
    crt.chCR   : begin
                   gx := 1;
                   inc( gy );                   
                 end;
    crt.chBkSp : begin
                   dec( gx );
                   crt.gotoxy( gx, gy );
                   crt.writechar( ' ' );
                 end;
  else
    begin
      crt.writechar( key );
      inc( gx );
    end;
  end;
  
  if crt.gettextcolor = crt.black
  then crt.textbackground( crt.lightgray )
  else crt.textbackground( crt.black );

  { debug display of keypress / color: }
  crt.gotoxy( 67, 1 );
  write( '[    ]');
  crt.gotoxy( 68, 1 );
  write( ord( key ));
  
  gx := min( kw, max( gx, 1 ));
  gy := min( kh, max( gy, 1 ));
end;

procedure load( buf : array of char );
var x, y : byte;
begin
end;

procedure init;
var y : byte;
begin
  crt.textattr := $17;
  crt.clrscr;
  for y := 0 to kh do
    begin
      crt.textattr := $07;
      crt.gotoxy( 1, y);
      crt.clreol;
      crt.gotoxy( 65, y);
      crt.textattr := $13;
      write( '|' );
      crt.clreol;
    end;
  crt.textattr := $07;
end;


begin
  init;
  load( gbuf );
  gx := 1; gy := 1;
  repeat
     crt.gotoxy( gx, gy );
     if crt.keypressed then handle_key;
   until gdone;
end.
