{$mode objfpc}
unit romVDP;

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

interface

uses romFont, SDL;

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
  tKeymap = array [0..9] of array [0..1] of char;

  tVDPData = record
    fontData: taChar;
    chrMapData: byte;
    atrMapData: array [0..1] of byte;
  end;

  tVDPAttrData = array [0..1] of byte;

  TVDP = object
    rFG: longword;
    rBG: longword;
    rBR: longword;
    rHStart: longword;
    rVStart: longword;

    aCharMap: array [0..cScnChrSize] of byte;
    aAttrMap: array [0..cScnAtrSize] of byte;
    pBitmap: pSDL_SURFACE;

    fError: boolean;

    function Open: boolean;
    procedure Close;
    function ReadBrReg: longword;
    procedure WriteBrReg(Value: longword);
    function ReadVStartReg: longword;
    procedure WriteVStartReg(Value: longword);
    function ReadHStartReg: longword;
    procedure WriteHStartReg(Value: longword);
    function ReadFgReg: longword;
    procedure WriteFgReg(Value: longword);
    function ReadBgReg: longword;
    procedure WriteBgReg(Value: longword);
    function ReadAttrMap(adr: longword): tVDPAttrData;
    procedure WriteAttrMap(adr: longword; Value: tVDPAttrData);
    function ReadCharMap(adr: longword): byte;
    procedure WriteCharMap(adr: longword; Value: byte);
    procedure PlotPixel(adr: longword; Value: byte);
    procedure RenderChar(adr: longword; Value: byte);
    procedure RenderDisplay;
    procedure Display;
    function PollKeyboard: char;
  end;
  procedure vdpInit;

implementation

var
  pBitmap: pSDL_SURFACE;
  rBitmap: ^longword;

  cScnColPal: array [0..255] of array [0..2] of byte;
  cScnOfsTab: array [0..cScnChrSize] of longword;

procedure TVDP.PlotPixel(adr: longword; Value: byte); inline;
begin
  pBitmap := self.pBitmap;
  rBitmap := self.rVStart * cScnXRes + self.rHStart +
    pBitmap^.pixels + adr;
  rBitmap^ := Value;
end;

procedure vdpInit;
var
  i, j, n, m: longword;
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

  {init linear grayscale palette}

  for i := 0 to 255 do
  begin
    cScnColPal[i][0] := i;
    cScnColPal[i][1] := i;
    cScnColPal[i][2] := i;
  end;
end;

function TVDP.Open: boolean;
var
  i: longword;
begin
  self.pBitmap := SDL_SETVIDEOMODE(cScnXRes, cScnYRes, cScnCRes, SDL_HWSURFACE);
  if self.pBitmap = nil then
    result := False
  else
    result := True;

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

procedure TVDP.Close;
begin
  SDL_FREESURFACE(self.pBitmap);
  SDL_QUIT;
end;

function TVDP.ReadBrReg: longword; inline;
begin
  result := self.rBR;
end;

procedure TVDP.WriteBrReg(value : longword); inline;
begin
  self.rBR := Value;
end;

function TVDP.ReadVStartReg: longword; inline;
begin
  result := self.rVStart;
end;

procedure TVDP.WriteVStartReg(value : longword); inline;
begin
  self.rVStart := Value;
end;

function TVDP.ReadHStartReg: longword; inline;
begin
  result := self.rHStart;
end;

procedure TVDP.WriteHStartReg(value : longword); inline;
begin
  self.rHStart := Value;
end;

function TVDP.ReadFgReg: longword; inline;
begin
  result := self.rFG;
end;

procedure TVDP.WriteFgReg(value : longword); inline;
begin
  self.rFG := Value;
end;

function TVDP.ReadBgReg: longword; inline;
begin
  result := self.rBG;
end;

procedure TVDP.WriteBgReg(value : longword); inline;
begin
  self.rBG := Value;
end;

function TVDP.ReadAttrMap(adr: longword): tVDPAttrData;
begin
  if adr > cScnAtrSize then
    self.fError := True
  else
  begin
    result[0] := self.aAttrMap[adr];
    result[1] := self.aAttrMap[adr + 1];
  end;
end;

procedure TVDP.WriteAttrMap(adr: longword; Value: tVDPAttrData);
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

function TVDP.ReadCharMap(adr: longword): byte;
begin
  self.fError := False;
  if adr < cScnChrSize then
    result := self.aCharMap[adr]
  else
    self.fError := True;
end;

procedure TVDP.WriteCharMap(adr: longword; Value: byte);
begin
  self.fError := False;
  if adr < cScnChrSize then
    self.aCharMap[adr] := Value
  else
    self.fError := True;
end;

procedure TVDP.RenderChar(adr: longword; Value: byte);
var
  attr: tVDPAttrData;
  chr: taChar;
  ofs: longword;
  i, j: longword;
  fg, bg: longword;
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
  i: longword;
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
