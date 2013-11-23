{
| block editor
|
|   a colorforth - style editor
|
| -------------------------------------------------
| copyright (c) 2012 michal j. wallace
| see LICENSE.org for usage information
}
{$mode delphi}{$i xpc.inc}
program bed;
uses xpc, classes, kvm, ublockdrive, log, kbd, ascii, cw;

const
  kw = 64;
  kh = 16;
  ks = kw * kh;

type
  TBlockEditor = class (TComponent)
  private
    _drive     : TBlockDrive;
    _block     : TBlock;
    _blockNum  : word;
    _x, _y     : byte;
    _done      : boolean;
    _isBright : boolean;
    _color      : char;
  public
    constructor Create(aOwner : TComponent); override;
    procedure MakeBlock;
    procedure LoadData;
    procedure SaveData;
    procedure Fg(ch : char );
    procedure ToggleBright;
    procedure Emit(ch : char);
    procedure AddChar(ch : char);
    procedure HandleKey;
    procedure WriteLine( lineno : integer );
    procedure InitView;
    procedure Run;
  published
    property curx : byte read _x;
    property cury : byte read _y;
    property isdone : boolean read _done;
  end;

const infoColumn = 67;


constructor TBlockEditor.Create(aOwner : TComponent);
  begin
    inherited Create(aOwner);
    _blockNum := 0;
    _isBright := false;
  end;

procedure TBlockEditor.MakeBlock;
  begin
    _drive.grow( 1 );
    fillchar(_block, sizeOf(_block), $87);
    // go back and make first line reversed:
    fillchar(_block, 64, $88);
  end;

procedure TBlockEditor.LoadData;
  begin
    _drive := TBlockDrive.Create( 'blocks.sd' );
    if _drive.BlockCount = 0 then MakeBlock
    else _drive.load( 0, _block );
  end;

procedure TBlockEditor.SaveData;
  begin
    _drive.save( 0, _block );
    cw.cxy(7, infoColumn, 2, 'SAVING');
  end;

procedure TBlockEditor.fg( ch : char );
  begin
    _color := ch; kvm.fg( ch );
  end;

procedure TBlockEditor.ToggleBright;
  begin
    if _isBright then Fg( LowerCase( _color ))
    else Fg( UpCase( _color ));
    _isBright := not _isBright;
  end;

procedure TBlockEditor.Emit( ch : char );
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

procedure TBlockEditor.AddChar( ch : char );
  begin
    _block[ _y * 64 + _x ] := ord( ch );
    emit( ch );
    inc( _x );
  end;

procedure TBlockEditor.HandleKey;
var
  key : char;
begin
  key := kbd.readkey;

  case key of

    #0 :
      begin
        key := kbd.readkey;
        case key of
	  kbd.LEFT  : if _x > 0 then dec( _x );
	  kbd.UP    : if _y > 0 then dec( _y );
	  kbd.DOWN  : inc( _y );
	  kbd.RIGHT : inc( _x );

          { _colors }
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
	if _color = 'k' then kvm.bg( 'w' ) else kvm.bg( 'k' );
      end;

    ^A : _x := 0;
    ^C,
    ascii.ESC  : _done := true;
    ^E : _x := 63;
    ^B : if _x > 0 then dec( _x );
    ^P : if _y > 0 then dec( _y );
    ^N : inc( _y );
    ^F : inc( _x );
    ^S : SaveData;
    ascii.CR   : begin
                   _x := 0; inc( _y );
                 end;
    ascii.BS   : if _x > 0 then
		   begin
		     dec( _x );
		     kvm.gotoxy( _x, _y );
		     write(' ');
		     _block[ _y * 64 + _x ] := 32;
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
  _x := min( kw-1, max( _x, 0 ));
  _y := min( kh-1, max( _y, 0 ));
end;


procedure TBlockEditor.WriteLine( lineno : integer );
var x, b : byte;
begin
  for x := 0 to kw-1 do
  begin
    b := _block[ ( lineno * kw ) + x ];
    emit( chr( b ));
  end;
end;


procedure TBlockEditor.InitView;
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
      WriteLine( y );
      kvm.fg( 'b' ); bg('k'); write( '|' );
      kvm.clreol;
    end;
  kvm.fg( $07 );
  _x := 0; _y := 1;
end;


procedure TBlockEditor.Run;
  begin
    LoadData;
    InitView;
    repeat
      kvm.gotoxy( curx, cury );
      if kbd.keypressed then HandleKey;
    until isDone;
    SaveData;
  end;

var ed : TBlockEditor;
begin
  log.level := log.lvl_debug;
  ed := TBlockEditor.Create(Nil);
  ed.Run;
  ed.Free;
end.
