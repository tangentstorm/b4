{ virtual terminal device for b4 }
unit uhw_vt;

interface
uses uhw, kvm, kbd;

type
  TB4KVMTerm = class (TB4Device)
    procedure invoke(op : char); override;
  end;

implementation
uses ub4;

{$IFDEF TURBO}
function xmax : byte;
  begin
    xmax := lo( kvm.windmax ) - lo( kvm.windmin )
  end;

function ymax : byte;
  begin
    ymax := hi( kvm.windmax ) - hi( kvm.windmin )
  end;

procedure goxy( x, y : byte );
  begin
    kvm.gotoxy( x+1, y+1 )
  end;
{$ENDIF}

procedure getc;
  begin dput(value(kbd.readkey));
    { we have 32 bits & only need 16. we can store extended keys in one cell }
    if tos = 0 then begin zap(dpop); dput(value(kbd.readkey) shl 8) end
  end;

procedure TB4KVMTerm.invoke(op : char);
begin
  case op of
    'g' : begin dswp; kvm.gotoxy(dpop mod (xMax+1), dpop mod (yMax+1)) end;
    'a' : kvm.textattr := dpop;
    'e' : if tos in [$00..$ff] then write(chr(dpop)) else write('[',dpop,']');
    'r' : getc;
    'k' : if keypressed then dput(-1) else dput(0);
    's' : kvm.clrscr;
    'l' : kvm.clreol;
    'c' : begin dput(kvm.wherex); dput(kvm.wherey) end;
  end
end;

begin
end.
