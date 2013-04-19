{ retroterm: retro extended terminal }
{$i xpc.inc}
unit rt_term;
interface uses xpc, grids, romFont, SysUtils, ng, ascii, agg2d;

const
  canvas_w  = 800;
  canvas_h  = 600;
  bitdepth  = 32;
  glyph_w   = 8;
  glyph_h   = 14;

  console_w = canvas_w div glyph_w;
  cScnHLine = $2BC0;

type

  tVDPAttrData = array [0..1] of byte;

  TCharCell = record
    bg : byte;
    fg : byte;
  case boolean of
    true  : (ch : widechar);
    false : (val : word);
  end;

  TCharGrid = class (specialize TGrid<TCharCell>)
    procedure Clear;
    function GetAttr( const i : cardinal ) : TVDPAttrData;
    procedure SetAttr( const i : cardinal; const value : tVDPAttrData );
    function GetChar( const i : cardinal ) : WideChar;
    procedure SetChar( const i : cardinal; const value : WideChar );
    property attr[ i : cardinal ] : TVDPAttrData read GetAttr write SetAttr;
    property char[ i : cardinal ] : WideChar read GetChar write SetChar;
  end;


  TRGBA = record
    r, g, b, a : byte
  end;

  TPalette = array[ byte ] of TRGBA;

  TRxCanvas = class( specialize TGrid<UInt32> )
    agg     : TAgg2D;
    color   : TRGBA;
    palette : TPalette;
    constructor Create( width, height : Int32; bitmap : Pointer );
    procedure SetColor( c : Int32 );
    procedure PutPixel( x, y : Int32 );
    procedure rect( x, y, width, height : Int32 );
    procedure FillRect( x, y, width, height : Int32 );
    procedure VLine( x, y, height : Int32 );
    procedure HLine( x, y, width : Int32 );
    procedure Circle( x, y, radius : Int32 );
    procedure FillCircle( x, y, radius : Int32 );
  end;

  TScreen = class
    rFG: Int32;
    rBG: Int32;
    rBR: Int32;
    rHStart: Int32;
    rVStart: Int32;
    termW : Int32;
    termH : Int32;
    buffer  : TCharGrid;
    fError: boolean;
    length  : integer;
    offsets : array of Int32;
    canvas : TRxCanvas;

    constructor Create;
    procedure Clear;

    { abstract drawing interface }
    procedure CreateCanvas; virtual; abstract;
    procedure PlotPixel(adr: Int32; Value: byte); virtual; abstract;
    procedure Display; virtual; abstract;

    { character-generator specific }
    procedure RenderChar(adr: Int32; Value: byte); virtual;
    procedure RenderDisplay;
  end;

  TKeyboard = class
    buffer : UnicodeString;
    needKey : boolean;
    constructor Create;
    procedure SendKey( ch : WideChar );
    function KeyPressed : Boolean;
    function ReadKey : WideChar;
  end;

  TConsole = class (TScreen)
    keyboard : TKeyboard;
    constructor Create;
  end;

  TRxConsole = class( TConsole )
  protected
    vm      : ng.TRetroVM;
    procedure Attach( retrovm : ng.TRetroVM ); virtual;
    function handle_keyboard( msg : int32 ) : int32;
    function handle_write (msg : int32 ) : int32;
    function handle_canvas (msg : int32 ) : int32;

    function ReadAttrMap(adr: Int32): tVDPAttrData;
    procedure WriteAttrMap(adr: Int32; Value: tVDPAttrData);
    function ReadCharMap(adr: Int32): byte;
    procedure WriteCharMap(adr: Int32; Value: byte);
    procedure DrawGlyphTable;
  private
    _cx, _cy  : Int32;
    _adr      : longword;
    procedure resetadr;
    procedure set_cx(x:int32);
    procedure set_cy(y:int32);
    procedure Fw;
    procedure Bw;
    procedure Bs;
    procedure Cr;
  public
    count     : Int32; {  what does this do? }
    refresh   : Int32;
    attr: tVDPAttrData;
    constructor Create;
    procedure Clear;
    procedure Emit( x	:  int32 );
    property cx:int32 read _cx write set_cx;
    property cy:int32 read _cy write set_cy;
  end;

implementation

const
  kTermW = 99;
  kTermH = 40;
var
  kXTPal : TPalette; { xterm color palette }

  constructor TConsole.Create;
  begin
    inherited Create;
    self.keyboard := TKeyboard.Create;
  end;

  constructor TRxConsole.Create;
  begin
    inherited Create;
    self.refresh := self.termW * 160;
  end;

  procedure TRxConsole.DrawGlyphTable;
    var x, y : byte; cc:TCharCell;
    const xo = console_w - $11; yo = 0; // offsets
  begin
    for y := $0 to $F do for x := $0 to $F do
    begin
      cc := self.buffer[x+xo, y+yo];
      cc.ch := chr( y * $10 + x );
      cc.bg := 1;
      self.buffer[x+xo, y+yo] := cc;
    end
  end;

  procedure TRxConsole.clear; inline;
  begin
    inherited Clear; cx := 0; cy := 0;
    RenderDisplay; { actually wipe the screen }
    DrawGlyphTable; { temp code for debugging }
  end;

  procedure TRxConsole.resetadr; inline;
  begin
    _adr := cy * termW + cx;
  end;

  procedure TRxConsole.set_cx(x:int32); inline;
  begin
    _cx := x; resetadr;
  end;

  procedure TRxConsole.set_cy(y:int32);
  begin
    _cy := y; resetadr;
  end;

  procedure TRxConsole.fw; inline;
  begin
    if cx < termW then cx := cx + 1
    else if cy < termH then begin cy := cy + 1; cx := 0; end
    else begin clear; cy := 0; cx := 0; end;
  end;

  procedure TRxConsole.bw; inline;
  begin
    if cx < termW then cx := cx-1;
  end;

  procedure TRxConsole.bs; inline;
  begin
    attr[1] := 0;
    WriteAttrMap(_adr, attr);
    WriteCharMap(_adr, 0);
    if (cx < termW) and (cx > 0) then cx := cx - 1;
  end;

  procedure TRxConsole.cr; inline;
  begin
    attr[1] := 0;
    WriteAttrMap(_adr, attr);
    cy := cy + 1; cx := 0;
    if cy >= termH then begin
      clear; cy := 0; cx := 0;
    end;
  end;

  procedure TRxConsole.emit( x : int32 );
  begin
    if x < 0 then clear else
      if x < 32 then case chr (x) of
	^H : bs;
	^J : cr;
	^M : cr;
      end;
    attr[1] := rBR; WriteAttrMap(_adr, attr);
    if x > 31 then begin
      attr[1] := 0;
      WriteAttrMap(_adr, attr);
      WriteCharMap(_adr, x);
      RenderChar(_adr, lo(buffer.at[_adr].val));
      fw;
    end
  end;

{ transformation table bitmap address -> character offset.
  This just caches the offsets of the upper left corner of
  each character, so the arithmetic doesn't have to be
  done repeatedly for each character when drawing the
  screen. }
procedure CacheOffsets(self : TScreen);
  var i, j, n, m: Int32;
begin
  SetLength(self.offsets, self.length);
  m := 0;
  n := 0;
  for i := 1 to kTermH do
  begin
    for j := 1 to kTermW do
    begin
      self.offsets[m] := n;
      n := n + 8;
      m := m + 1;
    end;
    n := i * cScnHLine;
  end;
end;



constructor TScreen.Create;
begin
  self.termW := kTermW;
  self.termH := kTermH;

  self.rFG := 200;
  self.rBG := 32;
  self.rBR := 128;
  self.fError := False;

  self.buffer := TCharGrid.Create( termW, termH );
  self.length := self.buffer.count;
  CacheOffsets(self);

  self.Clear;
  self.rHStart := 4;
  self.rVStart := 18;
end;


procedure TScreen.Clear; inline;
begin
  self.buffer.Clear;
end;

procedure TCharGrid.Clear;
begin
  FillDWord(self._data[0], self.ramSize div 4, 0);
end;


function TCharGrid.GetAttr( const i: cardinal ) : TVDPAttrData;
begin
  result[0] := _data[i].fg;
  result[1] := _data[i].bg;
end;

procedure TCharGrid.SetAttr( const i : cardinal; const value : tVDPAttrData );
begin
  with _data[i] do
  begin
    bg := value[0];
    fg := value[1];
  end
end;

function TCharGrid.GetChar( const i : cardinal ) : WideChar;
begin
   result := _data[i].ch;
end;

procedure TCharGrid.SetChar( const i : cardinal; const value : WideChar );
begin
  _data[i].ch := value;
end;


function TRxConsole.ReadAttrMap(adr: Int32): tVDPAttrData;
begin
  self.fError := False;
  if adr >= length then self.fError := True
  else result := self.buffer.attr[ adr ]
end;

procedure TRxConsole.WriteAttrMap(adr: Int32; Value: tVDPAttrData);
begin
  if adr >= length then self.fError := True
  else self.buffer.attr[ adr ] := value;
end;

function TRxConsole.ReadCharMap(adr: Int32): byte;
begin
  self.fError := False;
  if adr >= length then self.fError := True
  else result := ord(self.buffer.char[adr]);
end;

procedure TRxConsole.WriteCharMap(adr: Int32; Value: byte);
begin
  self.fError := False;
  if adr >= length then self.fError := True
  else self.buffer.char[adr] := chr(value)
end;

procedure TScreen.RenderChar(adr: Int32; Value: byte);
var
  attr: tVDPAttrData;
  chr: taChar;
  ofs: Int32;
  i, j: Int32;
  fg, bg: Int32;
begin
  if adr < length then
  begin
    attr := self.buffer.attr[adr];
    chr := romFontReadChar(self.buffer.at[adr].val);
    if attr[0] = 0 then
      fg := self.rFG
    else
      fg := attr[0];
    if attr[1] = 0 then
      bg := self.rBG
    else
      bg := attr[1];
    ofs := offsets[adr];
    for i := 0 to glyph_h - 1 do
    begin
      for j := 0 to glyph_w - 1 do
      begin
        if ((chr[i] shr 7) and 1) = 1 then
	  self.plotPixel(ofs, fg)
        else
	  self.plotPixel(ofs, bg);
	ofs := ofs + 1;
	{$RANGECHECKS OFF}
        chr[i] := chr[i] shl 1;
	{$RANGECHECKS ON}
      end;
      ofs := (ofs + canvas_w) - glyph_w;
    end;
  end
  else
    self.fError := True;
end;

procedure TScreen.RenderDisplay;
var
  i: Int32;
begin
  for i := 0 to pred(length) do
    self.RenderChar(i, lo(buffer.at[i].val));
  self.Display;
end;


{ keyboard buffer }

constructor TKeyboard.Create;
begin
  buffer := '';
  needKey := false;
end;

procedure TKeyboard.SendKey( ch : WideChar );
begin
  buffer += ch;
  needKey := false;
end;

function TKeyboard.KeyPressed : boolean; inline;
begin
  result := Length(buffer) > 0;
end;

function TKeyboard.ReadKey : WideChar;
begin
  if KeyPressed then
    begin
      result := buffer[1];
      Delete(buffer, 1, 1)
    end
  else begin
    needKey := true;
    result := ascii.SYN; { synchronous idle }
  end;
end;

{ canvas device }

constructor TRxCanvas.Create( width, height : int32; bitmap : Pointer );
begin
  if bitmap = nil then raise EObjectCheck.Create( 'bitmap pointer is nil' );
  inherited CreateAt( width, height, 0, bitmap );
  writeln( 'TRxCavanvas.Create( ', width, ', ', height, ', <$', HexStr( bitmap ), '>)' );
  writeln( self[ 10, 20 ] );
  agg := TAgg2d.Create( bitmap, width, height, {stride=}width * 4, agg2d.pfRGBA );
  palette := kXtPal;
  SetColor( $0F ); // ansi white
end;

procedure TRxCanvas.SetColor( c : int32 );
begin
  if c < 256 then color := palette[ c ]
  else color := TRGBA( Int32( c shl 8 + $ff ));
  WriteLn( 'r:', color.r, ' g:', color.g, ' b:', color.b, ' a:', color.a );
  agg.SetLineColor( color.r, color.g, color.b, color.a );
  agg.SetFillColor( color.r, color.g, color.b, color.a );
end;

procedure TRxCanvas.PutPixel( x, y : int32 );
begin
  self[ x, y ] := UInt32( self.color );
end;

procedure TRxCanvas.rect( x, y, width, height : int32 );
begin
  agg.NoFill;
  agg.Rectangle( x, y, x + width, y + height );
end;

procedure TRxCanvas.FillRect( x, y, width, height : int32 );
begin
  agg.Rectangle( x, y, x + width, y + height );
end;

procedure TRxCanvas.VLine( x, y, height : int32 );
begin
  agg.Line( x, y, x, y + height );
end;

procedure TRxCanvas.HLine( x, y, width : int32 );
begin
  agg.Line( x, y, x + width, y );
end;

procedure TRxCanvas.Circle( x, y, radius : int32 );
begin
  agg.NoFill;
  agg.Ellipse( x, y, radius, radius );
end;

procedure TRxCanvas.FillCircle( x, y, radius : int32 );
begin
  agg.Ellipse( x, y, radius, radius );
end;


{ vm integration }

procedure TRxConsole.Attach( retrovm : ng.TretroVM );
begin
  self.vm := retrovm;
  self.vm.devices[1] := @self.handle_keyboard;
  self.vm.devices[2] := @self.handle_write;
  self.vm.devices[6] := @self.handle_canvas;
end;

function TRxConsole.handle_keyboard( msg : int32 ) : int32;
begin
  { ensure a refresh of the screen wehn it's time to input text,
    but only if we're just waiting. }
  if keyboard.needKey then pass else self.Display;
  result := ord(keyboard.ReadKey);
  if keyboard.needKey then
    raise ENotFinished.Create('ReadKey');
end;

function TRxConsole.handle_write( msg : int32 ) : int32;
begin
  if msg = 1 then Emit(vm.data.pop);
  result := 0;
end;

function TRxConsole.handle_canvas( msg : int32 ) : int32;
  var x, y, w, h : int32;
begin
  case msg of
    1 : canvas.SetColor( vm.data.pop );
    2 : begin
	  vm.data.pop2( y, x );
	  canvas.PutPixel( x, y );
	end;
    3 : begin
	  vm.data.pop2( h, w );
	  vm.data.pop2( y, x );
	  Writeln('canvas.Rect(', x, ', ', y, ', ', w, ', ', h, ')');
	  canvas.Rect( x, y, w, h )
	end;
    4 : begin
	  vm.data.pop2( h, w );
	  vm.data.pop2( y, x );
          Writeln('canvas.FillRect(', x, ', ', y, ', ', w, ', ', h, ')');
	  canvas.FillRect( x, y, w, h )
	end;
    5 : begin
	  vm.data.pop1( h );
	  vm.data.pop2( y, x );
	  canvas.VLine( x, y, h );
          Writeln('canvas.VLine(', x, ', ', y, ', ', h, ')');
	end;
    6 : begin
	  vm.data.pop1( w );
	  vm.data.pop2( y, x );
          Writeln('canvas.HLine(', x, ', ', y, ', ', w, ')');
	  canvas.HLine( x, y, w )
	end;
    7 : begin
	  vm.data.pop1( w );
	  vm.data.pop2( y, x );
          Writeln('canvas.Circle(', x, ', ', y, ', ', w div 2, ')');
	  canvas.Circle( x, y, w div 2 )
	end;
    8 : begin
	  vm.data.pop1( w );
	  vm.data.pop2( y, x );
          Writeln('canvas.FillCircle(', x, ', ', y, ', ', w div 2, ')');
	  canvas.FillCircle( x, y, w div 2 )
	end;
  end;
  self.Display;
  result := 0;
end;

procedure setup_xterm_colors( var xtc : TPalette );
  const
    ansi : array[ 0 .. 15 ] of UInt32
	   =( $ff000000, // black
	      $ff0000aa, // red
	      $ff00aa00, // green
	      $ff00aaaa, // dark yellow ( note : not vga brown! )
	      $ffaa0000, // blue
	      $ffaa00aa, // magenta
	      $ffaaaa00, // cyan
	      $ffaaaaaa, // gray
	      $ff555555, // dark gray
	      $ff5555ff, // light red
	      $ff55ff55, // light green
	      $ff55ffff, // yellow
	      $ffff5555, // light blue
	      $ffff55ff, // light magenta
	      $ffffff55, // light cyan
	      $ffffffff  // white
	      );
    ramp : array[ 0 .. 5 ] of UInt32
	   = ( $00, $5f, $87, $AF, $D7, $FF );
  var i, r, g, b, c : byte;
begin

  // 0..15 are the ansi colors:
  for i := 0 to 15 do xtc[ i ] := TRGBA( ansi[ i ]);

  // 16..231 are a color cube (6^3 = 216 colors)
  i := 16;
  for r := $0 to $5 do
    for g := $0 to $5 do
      for b := $0 to $5 do
      begin
	xtc[ i ] := TRGBA( UInt32(
		       ramp[ r ] shl 24
		     + ramp[ g ] shl 16
		     + ramp[ b ] shl 8
		     + $ff ));
	inc( i );
      end;

  // 232 .. 255 are a grayscale ramp
  for c in [ $00, $12, $1C, $26, $30, $3A, $44, $4E,
             $58, $62, $6C, $76, $80, $8A, $94, $9E,
	     $A8, $B2, $BC, $C6, $D0, $DA, $E4, $EE ] do
  begin
    xtc[ i ] := TRGBA( UInt32( c shl 24 + c shl 16 + c shl 8 + $ff ));
    if i < $ff then inc( i ); // skip very last increment to avoid range error
  end
end;

  var i : byte;
initialization
  setup_xterm_colors( kXtPal );
  for i in byte do with kXtPal[ i ] do
    WriteLn( hex(i, 2), ' = rgba: $', hex(r,2), hex(g,2), hex(b,2), hex(a,2));
end.
