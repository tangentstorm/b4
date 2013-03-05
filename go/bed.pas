{
| b4 editor
|
|   a colorforth - style editor
|
| -------------------------------------------------
| copyright (c) 2012 michal j. wallace
| see LICENSE.org for usage information
}
{$i xpc.inc }
program bed;
uses xpc, vt, sd, log, kbd, ascii {$ifdef gpc}, gpcutil{$endif};

const
  kw = 64;
  kh = 16;
  ks = kw * kh;

var
  gDrive     : sd.tDrive;
  gBuf	     : sd.tBlock;
  gx, gy     : byte;
  gDone	     : boolean;
  brightflag : byte = 0;

procedure handle_key;
var
  key : char;
begin
  key := kbd.readkey;
  
  case key of
    
    #0 :
      begin
        key := kbd.readkey;
        case key of
	  kbd.LEFT  : dec( gx );
	  kbd.UP    : dec( gy );
	  kbd.DOWN  : inc( gy );
	  kbd.RIGHT : inc( gx );
          
          { colors : $08 indicates bright }
	  kbd.F1 : vt.fg( ord(vt.red) + brightflag );
	  kbd.F2 : vt.fg( ord(vt.grn) + brightflag );
	  kbd.F3 : vt.fg( ord(vt.yel) + brightflag );
	  kbd.F4 : vt.fg( ord(vt.blu) + brightflag );
	  kbd.F5 : vt.fg( ord(vt.mgn) + brightflag );
	  kbd.F6 : vt.fg( ord(vt.cyn) + brightflag );
	  kbd.F7 : vt.fg( ord(vt.wht) + brightflag );
	  kbd.F8 : vt.fg( ord(vt.blk) + brightflag );
          { f9 toggles the brightness bit. }
	  kbd.F9 : begin
		     brightflag := brightflag xor 8;
		     vt.fg( vt.textattr xor 8 );
		   end
        end;
        { black text is dark gray, OR black on light gray, depending on bit 8 }
	if lo(vt.textattr) = ord( vt.blk )
	  then vt.bg( ord( vt.blk ) + 8 )
          else vt.bg( ord( vt.blk ))
      end;
    
    ascii.ESC  : gdone := true;
    ascii.CR   : begin
                   gx := 1;
                   inc( gy );
                 end;
    ascii.BS   : begin
                   dec( gx );
                   vt.gotoxy( gx, gy );
		   write(' ');
                   gBuf[ (( gy - 1 ) * 64 ) + ( gx - 1 ) ] := 32;
                 end;
  else
    begin
      gBuf[ (( gy - 1 ) * 64 ) + ( gx - 1 ) ] := ord( key );
      write( key );
      inc( gx );
    end;
  end;
  
  { debug display of keypress / color: }
  vt.gotoxy( 67, 1 );
  write( '[    ]');
  vt.gotoxy( 68, 1 );
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
      vt.fg( symcolor );
      write( ' ' );
    end
    else begin
      vt.fg( txtcolor );
      write( chr( b ) );
    end;
  end;
end;


procedure load_data;
begin
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

  vt.fg( $17 );
  vt.clrscr;
  
  for y := 0 to 15 do
    begin
      vt.gotoxy( 1, y+1 );
      if y = 0 then write_line( y, $70, $60 )
      else write_line( y, $07, $40 );
      vt.fg( $13 );
      write( '|' );
      vt.clreol;
    end;
  vt.fg( $07 );
  gx := 1; gy := 2;
end;


begin
  log.level := log.lvl_debug;
  load_data;
  init_view;
  repeat
     vt.gotoxy( gx, gy );
     if kbd.keypressed then handle_key;
  until gdone;
  save_data;
end.
