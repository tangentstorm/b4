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
uses xpc, kvm, sd, log, kbd, ascii {$ifdef gpc}, gpcutil{$endif};

const
  kw = 64;
  kh = 16;
  ks = kw * kh;

var
  gDrive     : sd.tDrive;
  gBuf	     : sd.tBlock;
  gx, gy     : byte;
  gDone	     : boolean;
  brightFlag : boolean = false;
  color	     : char;

  const infoColumn = 67;

procedure fg( ch : char );
  begin
    color := ch; kvm.fg( ch );
  end;

procedure ToggleBright;
  begin
    if brightFlag then Fg( LowerCase( color ))
    else Fg( UpCase( color ));
    brightFlag := not brightFlag;
  end;

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
	  ^C : gDone := true;
	  ^B, kbd.LEFT  : if gx > 0 then dec( gx );
	  ^P, kbd.UP    : if gy > 0 then dec( gy );
	  ^N, kbd.DOWN  : inc( gy );
	  ^F, kbd.RIGHT : inc( gx );

          { colors }
	  kbd.F1 : Fg( 'r' );
	  kbd.F2 : Fg( 'g' );
	  kbd.F3 : Fg( 'y' );
	  kbd.F4 : Fg( 'b' );
	  kbd.F5 : Fg( 'm' );
	  kbd.F6 : Fg( 'c' );
	  kbd.F7 : Fg( 'w' );
	  kbd.F8 : Fg( 'k' );

          { f9 toggles the brightness bit. }
	  kbd.F9 : ToggleBright;
	end;
        { black text is dark gray, OR black on gray, depending on bit 8 }
	if color = 'k' then kvm.bg( 'K' ) else kvm.bg( 'k' );
      end;

    ^A : gx := 0;
    ^C,
    ascii.ESC  : gdone := true;
    ^E : gx := 63;
    ^B : if gx > 0 then dec( gx );
    ^P : if gy > 0 then dec( gy );
    ^N : inc( gy );
    ^F : inc( gx );
    ascii.CR   : begin
                   gx := 0; inc( gy );
                 end;
    ascii.BS   : if gx > 0 then
		   begin
		     dec( gx );
		     kvm.gotoxy( gx, gy );
		     write(' ');
		     gBuf[ gy * 64 + gx ] := 32;
	           end;
  else
    begin
      gBuf[ gy * 64 + gx ] := ord( key );
      write( key );
      inc( gx );
    end;
  end;

  { debug display of keypress / color: }
  kvm.gotoxy( infoColumn, 0 );
  write( '[    ]');
  kvm.gotoxy( infoColumn + 1, 0 );
  write( ord( key ));

  { bounds checking }
  gx := min( kw-1, max( gx, 0 ));
  gy := min( kh-1, max( gy, 0 ));
end;


procedure write_line( lineno : integer; txtcolor, symcolor : byte );
var i, b : byte;
begin
  for i := 0 to kw-1 do
  begin
    b := gBuf[ ( lineno * kw ) + i ];
    if b < 32 then begin
      kvm.fg( symcolor );
      write( ' ' );
    end
    else begin
      kvm.fg( txtcolor );
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

  kvm.bg( 0 );
  fg( 'w' );
  kvm.clrscr;

  for y := 0 to 15 do
    begin
      kvm.gotoxy( 0, y );
      if y = 0 then write_line( y, $70, $60 )
      else write_line( y, $07, $40 );
      kvm.fg( $13 );
      write( '|' );
      kvm.clreol;
    end;
  kvm.fg( $07 );
  gx := 0; gy := 1;
end;


begin
  log.level := log.lvl_debug;
  load_data;
  init_view;
  repeat
     kvm.gotoxy( gx, gy );
     if kbd.keypressed then handle_key;
  until gdone;
  save_data;
end.
