{ retroterm: retro extended terminal }
{$i xpc.inc}
unit rt_term;
interface uses xpc, grids, romFont, SysUtils, ng;


const
  cScnXRes = 800;
  cScnYRes = 600;
  cScnCRes = 32;
  cChrXRes = 8;
  cChrYRes = 14;
  cScnHLine = $2BC0;
  cScnAtrMap = $FA0;
  cScnFntDta = $2EE0;

  kScnChrSize = $FA0 - $28;
  kScnAtrSize = $1F40;
  kScnFntSize = $E00;

type
  tVDPAttrData = array [0..1] of byte;

  TCharCell = record
    fg : byte;
    bg : byte;
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
    constructor Create;
    procedure Clear;

    { abstract drawing interface }
    procedure PlotPixel(adr: Int32; Value: byte); virtual; abstract;
    procedure Display; virtual; abstract;

    { character-generator specific }
    procedure RenderChar(adr: Int32; Value: byte);
    procedure RenderDisplay;
  end;

  TConsole = class (TScreen)
    function PollKeyboard : char; virtual; abstract;
  end;
  
  TRxConsole = class( TConsole )
    vm      : ng.TRetroVM;
    procedure Attach( retrovm : ng.TRetroVM ); virtual;
    function handle_keyboard( msg : int32 ) : int32;
    function handle_write (msg : int32 ) : int32;

    function ReadAttrMap(adr: Int32): tVDPAttrData;
    procedure WriteAttrMap(adr: Int32; Value: tVDPAttrData);
    function ReadCharMap(adr: Int32): byte;
    procedure WriteCharMap(adr: Int32; Value: byte);
    private
      _cx, _cy  : Int32;
      adr	: longword;
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

  constructor TRxConsole.Create;
  begin
    inherited Create;
    self.refresh := self.termW * 160;
  end;

  procedure TRxConsole.clear; inline;
  begin
    inherited Clear; cx := 0; cy := 0;
  end;

  procedure TRxConsole.resetadr; inline;
  begin
    adr := cy * termW + cx;
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
    WriteAttrMap(adr, attr);
    WriteCharMap(adr, 0);
    if (cx < termW) and (cx > 0) then cx := cx - 1;
  end;

  procedure TRxConsole.cr; inline;
  begin
    attr[1] := 0;
    WriteAttrMap(adr, attr);
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
    attr[1] := rBR; WriteAttrMap(adr, attr);
    if x > 31 then begin
      attr[1] := 0;
      WriteAttrMap(adr, attr);
      WriteCharMap(adr, x);
    end;
    if x > 31 then fw;

    count := count + 1;
    if count = refresh then begin
      count := 0; RenderDisplay;
    end;
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
  self.length := self.buffer.size;
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
  FillDWord(self.data[0], self.size, 0);
end;

function TCharGrid.GetAttr( const i : cardinal ) : TVDPAttrData;
begin
  result[0] := data[i].bg;
  result[1] := data[i].fg;
end;

procedure TCharGrid.SetAttr( const i : cardinal; const value : tVDPAttrData );
begin
  with data[i] do
  begin
    bg := value[0];
    fg := value[1];
  end
end;

function TCharGrid.GetChar( const i : cardinal ) : WideChar;
begin
   result := data[i].ch;
end;

procedure TCharGrid.SetChar( const i : cardinal; const value : WideChar );
begin
  self.data[i].ch := value;
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
    for i := 0 to cChrYRes - 1 do
    begin
      for j := 0 to cChrXRes - 1 do
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
      ofs := (ofs + cScnXRes) - cChrXRes;
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


{ vm integration }

procedure TRxConsole.Attach( retrovm : ng.TretroVM );
begin
  self.vm := retrovm;
  self.vm.devices[1] := @self.handle_keyboard;
  self.vm.devices[2] := @self.handle_write;
end;

function TRxConsole.handle_keyboard( msg : int32 ) : int32;
begin
  self.RenderDisplay;
  result := ord(self.PollKeyboard);
end;
  
function TRxConsole.handle_write (msg : int32): int32;
begin
  if msg = 1 then Emit(vm.data.pop);
  result := 0;
end;

  
begin
end.
