{ virtual terminal device for b4 }
{$mode objfpc}
unit uhw_vt;

interface
uses uhw, crt;

type
  TB4KVMTerm = class (TB4Device)
    procedure invoke(op : char); override;
  end;

function xmax : byte;
function ymax : byte;
procedure fg(ch : char);
procedure bg(ch : char);
procedure goxy( x, y : byte );
function wherex: byte;
function wherey: byte;
procedure clrscr;
procedure clreol;
function keypressed:boolean;
function readkey:char;
function getTextAttr:word;
procedure setTextAttr(a : word);

implementation
uses ub4;

function wherex: byte; begin result := crt.wherex-1 end;
function wherey: byte; begin result := crt.wherey-1 end;
procedure clrscr; begin crt.clrscr end;
procedure clreol; begin crt.clreol end;
function keypressed:boolean; begin result := crt.keypressed end;
function readkey:char; begin result := crt.readkey end;
function getTextAttr:word; begin result := crt.textattr end;
procedure setTextAttr(a : word); begin crt.textattr := a end;

var _fg : byte = 7; _bg:byte = 0; _hi:boolean = false;

function tc(ch : char): byte;
  begin
    // result := pos(ch, 'krgybmcwKRGYBMCW');
    // crt uses a different ordering:
    result := pos(ch, 'kbgcrmywKBGCRMYW');
    if result = 0 then result := 8;
    dec(result);
  end;

procedure fg(ch: char);
  begin
    _fg := tc(ch);
    if _hi then crt.TextColor(_fg + crt.blink)
    else crt.TextColor(_fg);
  end;

procedure bg(ch: char);
  begin
    _bg := tc(ch);
    { "blink" doesn't actually work in crt, but can be used for bright backgrounds }
    if _bg > 7
      then if not _hi then begin _hi := true; crt.TextColor(_fg + crt.blink) end
      else if _hi then begin _hi := false; crt.TextColor(_fg) end;
    crt.TextBackground(_bg);
  end;

function xmax : byte;
  begin
    xmax := crt.WindMaxX;
  end;

function ymax : byte;
  begin
    ymax := crt.WindMaxY;
  end;

procedure goxy( x, y : byte );
  begin
    crt.gotoxy( x+1, y+1 )
  end;

procedure getc;
  begin dput(value(crt.readkey));
    { we have 32 bits & only need 16. we can store extended keys in one cell }
    if tos = 0 then begin zap(dpop); dput(value(crt.readkey) shl 8) end
  end;

const
  AnsiToCrtColor : array[0..15] of byte
    = ( 0, 4, 2, 6, 1, 5, 3, 7, 8, 12, 10, 14, 9, 13, 11, 15 );

procedure TB4KVMTerm.invoke(op : char);
begin
  case op of
    'g' : begin dswp; goxy(dpop mod (xMax+1), dpop mod (yMax+1)) end;
    'a' : crt.textattr := AnsiToCrtColor[dpop];
    'e' : if tos in [$00..$ff] then write(chr(dpop)) else write('[',dpop,']');
    'r' : getc;
    'k' : if keypressed then dput(-1) else dput(0);
    's' : crt.clrscr;
    'l' : crt.clreol;
    'c' : begin dput(crt.wherex); dput(crt.wherey) end;
  end
end;

begin
end.
