{$mode objfpc}
unit romVDP;
interface
uses grids, romFont, SysUtils;

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
  
  TVDP = class (TConsole)
    function ReadAttrMap(adr: Int32): tVDPAttrData;
    procedure WriteAttrMap(adr: Int32; Value: tVDPAttrData);
    function ReadCharMap(adr: Int32): byte;
    procedure WriteCharMap(adr: Int32; Value: byte);
  end;


implementation

const
  kTermW = 99;
  kTermH = 40;



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
        chr[i] := chr[i] shl 1;
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

begin
end.
