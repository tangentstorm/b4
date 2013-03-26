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

  tVDP = record
    rFG: longword;
    rBG: longword;
    rBR: longword;
    rHStart: longword;
    rVStart: longword;

    aCharMap: array [0..cScnChrSize] of byte;
    aAttrMap: array [0..cScnAtrSize] of byte;
    pBitmap: pSDL_SURFACE;

    fError: boolean;
  end;

procedure vdpInit;
function vdpOpen(var handle: tVDP): boolean;
procedure vdpClose(handle: tVDP);

function vdpReadBrReg
  (var handle: tVDP): longword;
procedure vdpWriteBrReg
  (var handle: tVDP; Value: longword);
function vdpReadVStartReg
  (var handle: tVDP): longword;
procedure vdpWriteVStartReg
  (var handle: tVDP; Value: longword);
function vdpReadHStartReg
  (var handle: tVDP): longword;
procedure vdpWriteHStartReg
  (var handle: tVDP; Value: longword);
function vdpReadFgReg
  (var handle: tVDP): longword;
procedure vdpWriteFgReg
  (var handle: tVDP; Value: longword);
function vdpReadBgReg
  (var handle: tVDP): longword;
procedure vdpWriteBgReg
  (var handle: tVDP; Value: longword);
function vdpReadAttrMap
  (var handle: tVDP; adr: longword): tVDPAttrData;
procedure vdpWriteAttrMap
  (var handle: tVDP; adr: longword; Value: tVDPAttrData);
function vdpReadCharMap
  (var handle: tVDP; adr: longword): byte;
procedure vdpWriteCharMap
  (var handle: tVDP; adr: longword; Value: byte);
procedure vdpPlotPixel
  (var handle: tVDP; adr: longword; Value: byte);

procedure vdpRenderChar
  (var handle: tVDP; adr: longword; Value: byte);
procedure vdpRenderDisplay
  (handle: tVDP);

procedure vdpDisplay
  (handle: tVDP);

function vdpPollKeyboard
  (handle: tVDP): char;

implementation

var
  pBitmap: pSDL_SURFACE;
  rBitmap: ^longword;

  cScnColPal: array [0..255] of array [0..2] of byte;
  cScnOfsTab: array [0..cScnChrSize] of longword;

procedure plotPixel(handle: tVDP; adr: longword; Value: byte); inline;
begin
  pBitmap := handle.pBitmap;
  rBitmap := handle.rVStart * cScnXRes + handle.rHStart +
    pBitmap^.pixels + adr;
  rBitmap^ := Value;
end;

procedure vdpPlotPixel(var handle: tVDP; adr: longword; Value: byte); inline;
begin
  plotPixel(handle, adr, Value);
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

function vdpOpen(var handle: tVDP): boolean;
var
  i: longword;
begin
  handle.pBitmap := SDL_SETVIDEOMODE(cScnXRes, cScnYRes, cScnCRes, SDL_HWSURFACE);
  if handle.pBitmap = nil then
    vdpOpen := False
  else
    vdpOpen := True;

  handle.rFG := 200;
  handle.rBG := 32;
  handle.rBR := 128;
  handle.fError := False;

  for i := 0 to cScnChrSize do
    handle.aCharMap[i] := 0;
  for i := 0 to cScnAtrSize do
    handle.aAttrMap[i] := 0;

  handle.rHStart := 4;
  handle.rVStart := 18;
end;

procedure vdpClose(handle: tVDP);
begin
  SDL_FREESURFACE(handle.pBitmap);
  SDL_QUIT;
end;

function vdpReadBrReg
  (var handle: tVDP): longword; inline;
begin
  vdpReadBrReg := handle.rBR;
end;

procedure vdpWriteBrReg
  (var handle: tVDP; Value: longword); inline;
begin
  handle.rBR := Value;
end;

function vdpReadVStartReg
  (var handle: tVDP): longword; inline;
begin
  vdpReadVStartReg := handle.rVStart;
end;

procedure vdpWriteVStartReg
  (var handle: tVDP; Value: longword); inline;
begin
  handle.rVStart := Value;
end;

function vdpReadHStartReg
  (var handle: tVDP): longword; inline;
begin
  vdpReadHStartReg := handle.rHStart;
end;

procedure vdpWriteHStartReg
  (var handle: tVDP; Value: longword); inline;
begin
  handle.rHStart := Value;
end;

function vdpReadFgReg
  (var handle: tVDP): longword; inline;
begin
  vdpReadFgReg := handle.rFG;
end;

procedure vdpWriteFgReg
  (var handle: tVDP; Value: longword); inline;
begin
  handle.rFG := Value;
end;

function vdpReadBgReg
  (var handle: tVDP): longword; inline;
begin
  vdpReadBgReg := handle.rBG;
end;

procedure vdpWriteBgReg
  (var handle: tVDP; Value: longword); inline;
begin
  handle.rBG := Value;
end;

function vdpReadAttrMap(var handle: tVDP; adr: longword): tVDPAttrData;
var
  ret: tVDPAttrData;
begin
  if adr > cScnAtrSize then
    handle.fError := True
  else
  begin
    ret[0] := handle.aAttrMap[adr];
    ret[1] := handle.aAttrMap[adr + 1];
    vdpReadAttrMap := ret;
  end;
end;

procedure vdpWriteAttrMap(var handle: tVDP; adr: longword; Value: tVDPAttrData);
begin
  adr := adr * 2;
  if adr > cScnAtrSize then
    handle.fError := True
  else
  begin
    handle.aAttrMap[adr] := Value[0];
    handle.aAttrMap[adr + 1] := Value[1];
  end;
end;

function vdpReadCharMap(var handle: tVDP; adr: longword): byte;
begin
  handle.fError := False;
  if adr < cScnChrSize then
    vdpReadCharMap := handle.aCharMap[adr]
  else
    handle.fError := True;
end;

procedure vdpWriteCharMap(var handle: tVDP; adr: longword; Value: byte);
begin
  handle.fError := False;
  if adr < cScnChrSize then
    handle.aCharMap[adr] := Value
  else
    handle.fError := True;
end;

procedure vdpRenderChar(var handle: tVDP; adr: longword; Value: byte);
var
  attr: tVDPAttrData;
  chr: taChar;
  ofs: longword;
  i, j: longword;
  fg, bg: longword;
begin
  if adr < cScnChrSize then
  begin
    attr := vdpReadAttrMap(handle, adr * 2);
    chr := romFontReadChar(handle.aCharMap[adr]);
    if attr[0] = 0 then
      fg := handle.rFG
    else
      fg := attr[0];
    if attr[1] = 0 then
      bg := handle.rBG
    else
      bg := attr[1];
    ofs := cScnOfsTab[adr];

    for i := 0 to cChrYRes - 1 do
    begin
      for j := 0 to cChrXRes - 1 do
      begin
        if ((chr[i] shr 7) and 1) = 1 then
          plotPixel(handle, ofs, fg)
        else
          plotPixel(handle, ofs, bg);
        ofs := ofs + 1;
        chr[i] := chr[i] shl 1;
      end;
      ofs := (ofs + cScnXRes) - cChrXRes;
    end;
  end
  else
    handle.fError := True;
end;

procedure vdpRenderDisplay(handle: tVDP);
var
  i: longword;
begin
  for i := 0 to cScnChrSize do
    vdpRenderChar(handle, i, handle.aCharMap[i]);
  SDL_FLIP(handle.pBitmap);
end;

procedure vdpDisplay(handle: tVDP); inline;
begin
  SDL_FLIP(handle.pBitmap);
end;

function vdpPollKeyboard(handle: tVDP): char;
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
            vdpPollKeyboard := ch;
          end;
        end;
        SDL_QUITEV: halt;
      end;
  until done;
end;

begin
end.
