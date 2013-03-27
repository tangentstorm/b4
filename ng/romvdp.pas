{$mode objfpc}
unit romVDP;
interface
uses grids, romFont, SDL, SysUtils;

{This is a simple soft-core of a text-display processor. It features a
 resolution of 99 columns x 40 rows and 256 colours. There exist three
 memory areas, the character, attribute and font-data map:

   character map (4000 byte)
   attribute map (8000 byte)
   font data     (3584 byte)

 A character is composed of 14 scan-lines of 8 pixel (8x14) whereby the
 colour information for each character is stored in two bytes of the
 attribute map start from offset zero:

  character map 0000: 65       'A'
  attribute map 0000: 128      foreground
                0001: 32       background colour

 Colour 0 is specially handled. This colour value is replaced with two
 internal colour registers for global fore and background colours so
 clearing the attribute map to zero enabled a mode where the fore and
 background colour is selected by these two internal registers for the
 whole screen!

 All three areas can either be mapped into an unified address space or
 handled seperatly. If the first option is choosen, the character map
 should begin at address 0, followed by the attribute map at FA0 and
 the font data at 2EE0. Beware the address mapping in combination with
 handling values though the abstract tVDPData type cost performance.
 The aternative way is to handle all maps as seperate memory areas.
 The choice is by you !}

const
  cScnXRes = 800;
  cScnYRes = 600;
  cScnCRes = 8;
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

  TVDP = class
    rFG: Int32;
    rBG: Int32;
    rBR: Int32;
    rHStart: Int32;
    rVStart: Int32;
    termW : Int32;
    termH : Int32;

    length  : integer;
    buffer  : TCharGrid;
    offsets : array of Int32;
    pBitmap: pSDL_SURFACE;

    fError: boolean;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function ReadAttrMap(adr: Int32): tVDPAttrData;
    procedure WriteAttrMap(adr: Int32; Value: tVDPAttrData);
    function ReadCharMap(adr: Int32): byte;
    procedure WriteCharMap(adr: Int32; Value: byte);

    procedure PlotPixel(adr: Int32; Value: byte);
    procedure RenderChar(adr: Int32; Value: byte);
    procedure RenderDisplay;
    procedure Display;
    function PollKeyboard: char;
  end;
  procedure vdpInit;

implementation

const
  kTermW = 99;
  kTermH = 40;

procedure TVDP.PlotPixel(adr: Int32; Value: byte); inline;
  var rBitmap: ^Int32;
begin
  pBitmap := self.pBitmap;
  rBitmap := self.rVStart * cScnXRes + self.rHStart +
    pBitmap^.pixels + adr;
  rBitmap^ := Value;
end;

procedure vdpInit;
begin
  SDL_INIT(SDL_INIT_VIDEO);
  SDL_EnableUnicode(1);
  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL);
end;


{ transformation table bitmap address -> character offset.
  This just caches the offsets of the upper left corner of
  each character, so the arithmetic doesn't have to be
  done repeatedly for each character when drawing the
  screen. }

procedure CacheOffsets(self : TVDP);
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


constructor TVDP.Create;
begin
  self.termW := kTermW;
  self.termH := kTermH;
  self.pBitmap := SDL_SETVIDEOMODE(cScnXRes, cScnYRes, cScnCRes, SDL_HWSURFACE);
  if self.pBitmap = nil then raise Exception.Create('Failed to create SDL bitmap');

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

destructor TVDP.Destroy;
begin
  SDL_FREESURFACE(self.pBitmap);
  SDL_QUIT;
end;

procedure TVDP.Clear; inline;
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


function TVDP.ReadAttrMap(adr: Int32): tVDPAttrData;
begin
  self.fError := False;
  if adr >= length then self.fError := True
  else result := self.buffer.attr[ adr ]
end;

procedure TVDP.WriteAttrMap(adr: Int32; Value: tVDPAttrData);
begin
  if adr >= length then self.fError := True
  else self.buffer.attr[ adr ] := value;
end;

function TVDP.ReadCharMap(adr: Int32): byte;
begin
  self.fError := False;
  if adr >= length then self.fError := True
  else result := ord(self.buffer.char[adr]);
end;

procedure TVDP.WriteCharMap(adr: Int32; Value: byte);
begin
  self.fError := False;
  if adr >= length then self.fError := True
  else self.buffer.char[adr] := chr(value)
end;

procedure TVDP.RenderChar(adr: Int32; Value: byte);
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
        chr[i] := chr[i] shl 1;
      end;
      ofs := (ofs + cScnXRes) - cChrXRes;
    end;
  end
  else
    self.fError := True;
end;

procedure TVDP.RenderDisplay;
var
  i: Int32;
begin
  for i := 0 to pred(length) do
    self.RenderChar(i, lo(buffer.at[i].val));
  SDL_FLIP(self.pBitmap);
end;

procedure TVDP.Display; inline;
begin
  SDL_FLIP(self.pBitmap);
end;

function TVDP.PollKeyboard: char;
var
  done: boolean;
  evt: pSDL_Event;
  key: TSDLKey;
  ch: char;
begin
  done := False;
  NEW(evt);

  repeat
    if SDL_PollEvent(evt) = 1 then
      case evt^.type_ of
        SDL_KEYDOWN:
        begin
          key := evt^.key.keysym.unicode;
          if key in [1 .. 255] then
          begin
            ch := chr(key);
            done := True;
            result := ch;
          end;
        end;
        SDL_QUITEV: halt;
      end;
  until done;
end;

begin
end.
