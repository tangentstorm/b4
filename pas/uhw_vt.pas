{ virtual terminal device for b4 }
unit uhw_vt;

interface
uses uhw, kvm, kbd;

type
  TB4KVMTerm = class (TB4Term)
    procedure invoke(op : TTermOp); override;
  end;

implementation
uses ub4;

{$IFDEF TURBO}
function xmax : byte;
  begin
    xmax := lo( kvm.windmax ) - lo( kvm.windmin )
  end;

function maxy : byte;
  begin
    ymax := hi( kvm.windmax ) - hi( kvm.windmin )
  end;
{$ENDIF}

procedure goxy( x, y : byte );
  begin
    kvm.gotoxy( x+1, y+1 )
  end;

procedure getc;
  begin dput(value(kbd.readkey));
    { we have 32 bits & only need 16. we can store extended keys in one cell }
    if tos = 0 then begin zap(dpop); dput(value(kbd.readkey) shl 8) end
  end;

procedure TB4KVMTerm.invoke(op : TTermOp);
begin
  case op of
    vtTG : begin dswp; kvm.gotoxy(dpop mod (xMax+1), dpop mod (yMax+1)) end;
    vtTA : kvm.textattr := dpop;
    vtTW : if tos in [$00..$ff] then write(chr(dpop)) else write('[',dpop,']');
    vtTR : getc;
    vtTK : if keypressed then dput(-1) else dput(0);
    vtTS : kvm.clrscr;
    vtTL : kvm.clreol;
    vtTC : begin dput(kvm.wherex); dput(kvm.wherey) end;
  end
end;

begin
end.
