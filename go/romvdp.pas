UNIT romVDP;

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

INTERFACE USES romFont,SDL;

CONST cScnXRes    = 800;
      cScnYRes    = 600;
      cScnCRes    = 8;
      cChrXRes    = 8;
      cChrYRes    = 14;
      cScnCol     = 100-1;
      cScnRow     = 40;
      cScnHLine   = $2BC0;
      cScnAtrMap  = $FA0;
      cScnFntDta  = $2EE0;
      cScnChrSize = $FA0-$28;
      cScnAtrSize = $1F40;
      cScnFntSize = $E00;

TYPE  tKeymap = ARRAY [0..9] OF ARRAY [0..1] OF CHAR;

      tVDPData = RECORD
                   fontData   : taChar;
                   chrMapData : BYTE;
                   atrMapData : ARRAY [0..1] OF BYTE;
                 END;

      tVDPAttrData = ARRAY [0..1] OF BYTE;

      tVDP = RECORD
              rFG     : LONGWORD;
              rBG     : LONGWORD;
              rBR     : LONGWORD;
              rHStart : LONGWORD;
              rVStart : LONGWORD;
              
              aCharMap : ARRAY [0..cScnChrSize] OF BYTE;
              aAttrMap : ARRAY [0..cScnAtrSize] OF BYTE;
              pBitmap  : pSDL_SURFACE;
              
              fError : BOOLEAN;
            END;

PROCEDURE vdpInit;         
FUNCTION  vdpOpen  (VAR handle : tVDP) : BOOLEAN;
PROCEDURE vdpClose (handle : tVDP);

FUNCTION  vdpReadBrReg
  (VAR handle : tVDP) : LONGWORD;
PROCEDURE vdpWriteBrReg
  (VAR handle : tVDP; value : LONGWORD);
FUNCTION  vdpReadVStartReg
  (VAR handle : tVDP) : LONGWORD;
PROCEDURE vdpWriteVStartReg
  (VAR handle : tVDP; value : LONGWORD);
FUNCTION  vdpReadHStartReg
  (VAR handle : tVDP) : LONGWORD;
PROCEDURE vdpWriteHStartReg
  (VAR handle : tVDP; value : LONGWORD);
FUNCTION  vdpReadFgReg
  (VAR handle : tVDP) : LONGWORD;
PROCEDURE vdpWriteFgReg
  (VAR handle : tVDP; value : LONGWORD);
FUNCTION  vdpReadBgReg
  (VAR handle : tVDP) : LONGWORD;
PROCEDURE vdpWriteBgReg
  (VAR handle : tVDP; value : LONGWORD);
FUNCTION  vdpReadAttrMap
  (VAR handle : tVDP; adr : LONGWORD) : tVDPAttrData;
PROCEDURE vdpWriteAttrMap
  (VAR handle : tVDP; adr : LONGWORD; value : tVDPAttrData);
FUNCTION  vdpReadCharMap
  (VAR handle : tVDP; adr : LONGWORD) : BYTE;
PROCEDURE vdpWriteCharMap
  (VAR handle : tVDP; adr : LONGWORD; value : BYTE);
PROCEDURE vdpPlotPixel
  (VAR handle : tVDP; adr : LONGWORD; value : BYTE);

PROCEDURE vdpRenderChar
  (VAR handle : tVDP; adr : LONGWORD; value : BYTE);
PROCEDURE vdpRenderDisplay
  (handle : tVDP);
  
PROCEDURE vdpDisplay
  (handle : tVDP);

FUNCTION vdpPollKeyboard
  (handle : tVDP) : CHAR;
  
IMPLEMENTATION

VAR pBitmap : pSDL_SURFACE;
    rBitmap : ^LONGWORD;
        
    cScnColPal : ARRAY [0..255] OF ARRAY [0..2] OF BYTE;
    cScnOfsTab : ARRAY [0..cScnChrSize] OF LONGWORD;
              
PROCEDURE plotPixel
(handle : tVDP; adr : LONGWORD; value : BYTE); INLINE;
BEGIN
  pBitmap  := handle.pBitmap;
  rBitmap  := handle.rVStart  * cScnXRes + handle.rHStart + 
              pBitmap^.pixels + adr;
  rBitmap^ := value;
END;

PROCEDURE vdpPlotPixel
(VAR handle : tVDP; adr : LONGWORD; value : BYTE); INLINE;
BEGIN
  plotPixel (handle, adr, value);
END;

PROCEDURE vdpInit;
  VAR i,j,n,m : LONGWORD;
BEGIN
  SDL_INIT(SDL_INIT_VIDEO);
  SDL_EnableUnicode(1);
  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL);

  {transformation table bitmap address -> character offset}
  
  m := 0; n := 0;
  FOR i := 1 TO cScnRow DO BEGIN
    FOR j := 1 TO cScnCol DO BEGIN cScnOfsTab[m] := n;
      n := n + 8; m := m + 1; END;
    n := i * cScnHLine; END;
  
  {init linear grayscale palette}
  
  FOR i := 0 TO 255 DO BEGIN
    cScnColPal[i][0] := i; cScnColPal[i][1] := i;
    cScnColPal[i][2] := i; END;
END;

FUNCTION vdpOpen
(VAR handle : tVDP) : BOOLEAN;
  VAR i : LONGWORD;
BEGIN
  handle.pBitmap := SDL_SETVIDEOMODE (cScnXRes, cScnYRes, cScnCRes, SDL_HWSURFACE);
  IF handle.pBitmap = NIL THEN vdpOpen := FALSE ELSE vdpOpen := TRUE;

  handle.rFG := 200; handle.rBG := 32; handle.rBR := 128;
  handle.fError := FALSE;
  
  FOR i := 0 TO cScnChrSize DO handle.aCharMap[i] := 0;
  FOR i := 0 TO cScnAtrSize DO handle.aAttrMap[i] := 0;
  
  handle.rHStart := 4; handle.rVStart := 18;  
END;

PROCEDURE vdpClose
(handle : tVDP);
BEGIN
  SDL_FREESURFACE (handle.pBitmap);
  SDL_QUIT;
END;

FUNCTION  vdpReadBrReg
  (VAR handle : tVDP) : LONGWORD; INLINE;
BEGIN
  vdpReadBrReg := handle.rBR;
END;

PROCEDURE vdpWriteBrReg
  (VAR handle : tVDP; value : LONGWORD); INLINE;
BEGIN
  handle.rBR := value;
END;

FUNCTION  vdpReadVStartReg
  (VAR handle : tVDP) : LONGWORD; INLINE;
BEGIN
  vdpReadVStartReg := handle.rVStart;
END;

PROCEDURE vdpWriteVStartReg
  (VAR handle : tVDP; value : LONGWORD); INLINE;
BEGIN
  handle.rVStart := value;
END;

FUNCTION  vdpReadHStartReg
  (VAR handle : tVDP) : LONGWORD; INLINE;
BEGIN
  vdpReadHStartReg := handle.rHStart;
END;

PROCEDURE vdpWriteHStartReg
  (VAR handle : tVDP; value : LONGWORD); INLINE;
BEGIN
  handle.rHStart := value;
END;

FUNCTION  vdpReadFgReg
  (VAR handle : tVDP) : LONGWORD; INLINE;
BEGIN
  vdpReadFgReg := handle.rFG;
END;

PROCEDURE vdpWriteFgReg
  (VAR handle : tVDP; value : LONGWORD); INLINE;
BEGIN
  handle.rFG := value;
END;

FUNCTION  vdpReadBgReg
  (VAR handle : tVDP) : LONGWORD; INLINE;
BEGIN
  vdpReadBgReg := handle.rBG;
END;

PROCEDURE vdpWriteBgReg
  (VAR handle : tVDP; value : LONGWORD); INLINE;
BEGIN
  handle.rBG := value;
END;

FUNCTION vdpReadAttrMap
(VAR handle : tVDP; adr : LONGWORD) : tVDPAttrData;
  VAR ret : tVDPAttrData;
BEGIN
  IF adr > cScnAtrSize THEN handle.fError := TRUE ELSE BEGIN
     ret[0] := handle.aAttrMap[adr]; ret[1] := handle.aAttrMap[adr+1];
     vdpReadAttrMap := ret;
  END;
END;

PROCEDURE vdpWriteAttrMap
(VAR handle : tVDP; adr : LONGWORD; value : tVDPAttrData);
BEGIN
  adr := adr * 2;
  IF adr > cScnAtrSize THEN handle.fError := TRUE ELSE BEGIN
     handle.aAttrMap[adr] := value[0]; handle.aAttrMap[adr+1] := value[1];
  END;
END;

FUNCTION vdpReadCharMap
(VAR handle : tVDP; adr : LONGWORD) : BYTE;
BEGIN
  handle.fError := FALSE;
  IF adr < cScnChrSize THEN vdpReadCharMap := handle.aCharMap[adr] ELSE
     handle.fError := TRUE;
END;

PROCEDURE vdpWriteCharMap
(VAR handle : tVDP; adr : LONGWORD; value : BYTE);
BEGIN
  handle.fError := FALSE;
  IF adr < cScnChrSize THEN handle.aCharMap[adr] := value ELSE
    handle.fError := TRUE;
END;

PROCEDURE vdpRenderChar
(VAR handle : tVDP; adr : LONGWORD; value : BYTE);
  VAR attr  : tVDPAttrData;
      chr   : taChar;
      ofs   : LONGWORD;
      i,j   : LONGWORD;
      fg,bg : LONGWORD;
BEGIN
  IF adr < cScnChrSize THEN BEGIN
    attr := vdpReadAttrMap  (handle, adr * 2);
    chr  := romFontReadChar (handle.aCharMap[adr]);
    IF attr[0] = 0 THEN fg := handle.rFG ELSE fg := attr[0];
    IF attr[1] = 0 THEN bg := handle.rBG ELSE bg := attr[1];
    ofs := cScnOfsTab[adr];
    
    FOR i := 0 TO cChrYRes-1 DO BEGIN
      FOR j := 0 TO cChrXRes-1 DO BEGIN 
        IF ((chr[i] SHR 7) AND 1) = 1 THEN plotPixel (handle, ofs, fg)
                                      ELSE plotPixel (handle, ofs, bg);
        ofs := ofs + 1;
        chr[i] := chr[i] SHL 1;
      END;
      ofs := (ofs + cScnXRes) - cChrXRes;
    END;
  END ELSE handle.fError := TRUE;
END;

PROCEDURE vdpRenderDisplay
(handle : tVDP);
  VAR i : LONGWORD;
BEGIN
  FOR i := 0 TO cScnChrSize DO vdpRenderChar (handle, i, handle.aCharMap[i]);
  SDL_FLIP (handle.pBitmap);
END;
  
PROCEDURE vdpDisplay
(handle : tVDP); INLINE;
BEGIN
  SDL_FLIP (handle.pBitmap);
END;

FUNCTION vdpPollKeyboard
(handle : tVDP) : CHAR;
  VAR done : BOOLEAN;
      evt  : pSDL_Event;
      key  : TSDLKey;
      ch   : char;
BEGIN
  done := false;
  NEW( evt );

  REPEAT IF SDL_PollEvent(evt) = 1 THEN
    CASE evt^.type_ OF
      SDL_KEYDOWN  : BEGIN key := evt^.key.keysym.unicode;
                           IF key IN [ 1 .. 255 ] THEN BEGIN ch := chr(key);
                              done := true; vdpPollKeyboard := ch; END;
                     END;
      SDL_QUITEV   : halt; END;
  UNTIL done;
END;

BEGIN
END.
