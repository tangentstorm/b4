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
  gBlockNum  : word = 0;
  gx, gy     : byte;
  gDone	     : boolean;
  brightFlag : boolean = false;
  color	     : char;

  const infoColumn = 67;


procedure make_block;
  begin
    gDrive.grow( 1 );
    fillchar(gBuf, sizeOf(gBuf), $87);
    // go back and make first line reversed:
    fillchar(gBuf, 64, $88);
  end;

procedure load_data;
  begin
    gDrive.init( 'blocks.sd' );
    if gDrive.block_count = 0 then
      make_block
    else gDrive.load( 0, gBuf );
  end;

procedure save_data;
  begin
    gDrive.save( 0, gBuf );
    write('SAVING');
  end;

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

procedure emit( ch : char );
  begin
    if ch = #$88 then
      begin
	kvm.fg( 'k' ); kvm.bg( 'w' );
	write( '.' )
      end
    else if ch > #$80 then
      begin
	kvm.bg( 'k' );
	kvm.fg( ord(ch) - $80 );
	write( '.' );
      end
    else write( ch )
  end;

procedure addChar( ch : char );
  begin
    gBuf[ gy * 64 + gx ] := ord( ch );
    emit( ch );
    inc( gx );
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
	  kbd.LEFT  : if gx > 0 then dec( gx );
	  kbd.UP    : if gy > 0 then dec( gy );
	  kbd.DOWN  : inc( gy );
	  kbd.RIGHT : inc( gx );

          { colors }
	  kbd.F1 : fg('r');
	  kbd.F2 : fg('g');
	  kbd.F3 : fg('y');
	  kbd.F4 : fg('b');
	  kbd.F5 : fg('m');
	  kbd.F6 : fg('c');
	  kbd.F7 : fg('w');
	  kbd.F8 : fg('k');

          { f9 toggles the brightness bit. }
	  kbd.F9 : ToggleBright;
	end;
        { black text is dark gray, OR black on gray, depending on bit 8 }
	if color = 'k' then kvm.bg( 'w' ) else kvm.bg( 'k' );
      end;

    ^A : gx := 0;
    ^C,
    ascii.ESC  : gdone := true;
    ^E : gx := 63;
    ^B : if gx > 0 then dec( gx );
    ^P : if gy > 0 then dec( gy );
    ^N : inc( gy );
    ^F : inc( gx );
    ^S : save_data;
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
  else if key in [ #32 .. #127 ] then
    addchar( key )
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


procedure write_line( lineno : integer );
var x, b : byte;
begin
  for x := 0 to kw-1 do
  begin
    b := gBuf[ ( lineno * kw ) + x ];
    emit( chr( b ));
  end;
end;


procedure init_view;
var y : byte;
begin

  kvm.bg( 0 );
  fg( 'w' );
  kvm.clrscr;

  for y := 0 to 15 do
    begin
      if y = 0 then begin fg('k'); bg('w') end
      else begin bg('k'); fg('w') end;
      kvm.gotoxy( 0, y );
      write_line( y );
      kvm.fg( 'b' ); bg('k'); write( '|' );
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
