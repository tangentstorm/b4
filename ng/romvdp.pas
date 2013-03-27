{$mode objfpc}
unit romVDP;
interface
uses romFont, SDL, SysUtils;

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
  cScnCol = 100 - 1;
  cScnRow = 40;
  cScnHLine = $2BC0;
  cScnAtrMap = $FA0;
  cScnFntDta = $2EE0;
  cScnChrSize = $FA0 - $28;
  cScnAtrSize = $1F40;
  cScnFntSize = $E00;

type
  tVDPAttrData = array [0..1] of byte;

  TVDP = class
    rFG: Int32;
    rBG: Int32;
    rBR: Int32;
    rHStart: Int32;
    rVStart: Int32;

    aCharMap: array [0..cScnChrSize] of byte;
    aAttrMap: array [0..cScnAtrSize] of byte;
    pBitmap: pSDL_SURFACE;

    fError: boolean;

    constructor Create;
    destructor Destroy; override;

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

var
  cScnOfsTab: array [0..cScnChrSize] of Int32;

procedure TVDP.PlotPixel(adr: Int32; Value: byte); inline;
  var rBitmap: ^Int32;
begin
  pBitmap := self.pBitmap;
  rBitmap := self.rVStart * cScnXRes + self.rHStart +
    pBitmap^.pixels + adr;
  rBitmap^ := Value;
end;

procedure vdpInit;
var
  i, j, n, m: Int32;
begin
  SDL_INIT(SDL_INIT_VIDEO);
  SDL_EnableUnicode(1);
  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL);

  {transformation table bitmap address -> character offset}

  m := 0;
  n := 0;
  for i := 1 to cScnRow do
  begin
    for j := 1 to cScnCol do
    begin
      cScnOfsTab[m] := n;
      n := n + 8;
      m := m + 1;
    end;
    n := i * cScnHLine;
  end;

end;

constructor TVDP.Create;
var
  i: Int32;
begin
  self.pBitmap := SDL_SETVIDEOMODE(cScnXRes, cScnYRes, cScnCRes, SDL_HWSURFACE);
  if self.pBitmap = nil then raise Exception.Create('Failed to create SDL bitmap');

  self.rFG := 200;
  self.rBG := 32;
  self.rBR := 128;
  self.fError := False;

  for i := 0 to cScnChrSize do
    self.aCharMap[i] := 0;
  for i := 0 to cScnAtrSize do
    self.aAttrMap[i] := 0;

  self.rHStart := 4;
  self.rVStart := 18;
end;

destructor TVDP.Destroy;
begin
  SDL_FREESURFACE(self.pBitmap);
  SDL_QUIT;
end;

function TVDP.ReadAttrMap(adr: Int32): tVDPAttrData;
begin
  if adr > cScnAtrSize then
    self.fError := True
  else
  begin
    result[0] := self.aAttrMap[adr];
    result[1] := self.aAttrMap[adr + 1];
  end;
end;

procedure TVDP.WriteAttrMap(adr: Int32; Value: tVDPAttrData);
begin
  adr := adr * 2;
  if adr > cScnAtrSize then
    self.fError := True
  else
  begin
    self.aAttrMap[adr] := Value[0];
    self.aAttrMap[adr + 1] := Value[1];
  end;
end;

function TVDP.ReadCharMap(adr: Int32): byte;
begin
  self.fError := False;
  if adr < cScnChrSize then
    result := self.aCharMap[adr]
  else
    self.fError := True;
end;

procedure TVDP.WriteCharMap(adr: Int32; Value: byte);
begin
  self.fError := False;
  if adr < cScnChrSize then
    self.aCharMap[adr] := Value
  else
    self.fError := True;
end;

procedure TVDP.RenderChar(adr: Int32; Value: byte);
var
  attr: tVDPAttrData;
  chr: taChar;
  ofs: Int32;
  i, j: Int32;
  fg, bg: Int32;
begin
  if adr < cScnChrSize then
  begin
    attr := self.ReadAttrMap(adr * 2);
    chr := romFontReadChar(self.aCharMap[adr]);
    if attr[0] = 0 then
      fg := self.rFG
    else
      fg := attr[0];
    if attr[1] = 0 then
      bg := self.rBG
    else
      bg := attr[1];
    ofs := cScnOfsTab[adr];

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
  for i := 0 to cScnChrSize do
    self.RenderChar(i, self.aCharMap[i]);
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
