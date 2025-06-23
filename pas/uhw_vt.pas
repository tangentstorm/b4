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

implementation
uses ub4;

function tc(ch: char): byte;
  begin
    // result := pos(ch, 'krgybmcwKRGYBMCW');
    // crt uses a different ordering:
    result := pos(ch, 'kbgcrmywKBGCRMYW');
    if result = 0 then result := 8;
    dec(result);
  end;

procedure fg(ch: char);
  begin
    crt.TextColor(tc(ch));
  end;

procedure bg(ch: char);
  begin
    crt.TextBackground(tc(ch));
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
