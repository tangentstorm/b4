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
uses crt, gpcutil, sd, log;

const
  kw = 64;
  kh = 16;
  ks = kw * kh;

var
  gDrive : sd.tDrive;
  gBuf : sd.tBlock;
  gx, gy : byte;
  gDone	 : boolean;

procedure handle_key;
var
  key : char;
begin
  key := crt.readkey;
  
  case key of
    
    #0 :
      begin
        key := crt.readkey;
        case key of
          crt.ksLEFT  : dec( gx );
          crt.ksUP    : dec( gy );
          crt.ksDOWN  : inc( gy );
          crt.ksRIGHT : inc( gx );
          
          { colors : $08 indicates bright }
          crt.ksF1 : crt.textattr := crt.red      and not 8;
          crt.ksF2 : crt.textattr := crt.green    and not 8;
          crt.ksF3 : crt.textattr := crt.yellow   and not 8;
          crt.ksF4 : crt.textattr := crt.blue     and not 8;
          crt.ksF5 : crt.textattr := crt.magenta  and not 8;
          crt.ksF6 : crt.textattr := crt.cyan     and not 8;
          crt.ksF7 : crt.textattr := crt.white    and not 8;
          crt.ksF8 : crt.textattr := crt.black    and not 8; 
          { f9 toggles the brightness bit. }
          crt.ksF9 : crt.textattr := crt.textattr xor 8;
        else
          { KLUDGE! these are option-f1 through option-f7 on my   }
          { old mac - at least in terminal.app. I think my config }
          { may be screwy. I will probably just switch over to an }
          { SDL term later, and then I can just delete this junk. }
          if key in [ #64 .. #71 ] then crt.textattr := ord( key ) - 64
        end;
        { black text is dark gray, OR black on light gray, depending on bit 8 }
        if crt.gettextcolor = crt.black
        then crt.textbackground( crt.lightgray )
        else crt.textbackground( crt.black );
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
                   gBuf[ (( gy - 1 ) * 64 ) + ( gx - 1 ) ] := 32; 
                 end;
  else
    begin
      gBuf[ (( gy - 1 ) * 64 ) + ( gx - 1 ) ] := ord( key ); 
      crt.writechar( key );
      inc( gx );
    end;
  end;
  
  { debug display of keypress / color: }
  crt.gotoxy( 67, 1 );
  write( '[    ]');
  crt.gotoxy( 68, 1 );
  write( ord( key ));
  
  gx := min( kw, max( gx, 0 ));
  gy := min( kh, max( gy, 1 )); { 2 because of title bar }
end;


procedure write_line( lineno : integer; txtcolor, symcolor : byte );
var i, b : byte;
begin
  for i := 0 to kw-1 do
  begin
    b := gBuf[ ( lineno * kw ) + i ];
    if b < 32 then begin
      crt.textattr := symcolor;
      write( ' ' );
    end
    else begin
      crt.textattr := txtcolor;
      write( chr( b ) );
    end;
  end;
end;


procedure load_data;
begin
  log.level := log.lvl_debug;
  gDrive.init( 'blocks.sd' );
  if gDrive.block_count = 0 then gDrive.grow( 1 );
  gDrive.load( 0, gBuf );
end;

procedure save_data;
begin
  gDrive.save( 0, gBuf );
end;

procedure init_view;
var y : byte;
begin

  crt.textattr := $17;
  crt.clrscr;
  
  for y := 0 to 15 do
    begin
      crt.gotoxy( 1, y+1 );
      if y = 0 then write_line( y, $70, $60 )
      else write_line( y, $07, $40 );
      crt.textattr := $13;
      write( '|' );
      crt.clreol;
    end;
  crt.textattr := $07;
  gx := 1; gy := 2;
end;


begin
  load_data;
  init_view;
  repeat
     crt.gotoxy( gx, gy );
     if crt.keypressed then handle_key;
  until gdone;
  save_data;
end.
