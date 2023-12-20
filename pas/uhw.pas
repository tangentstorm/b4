// b4 virtual hardware devices
{$mode objfpc }
unit uhw;

interface

type
  TTermOp = (vtTG, vtTA, vtTW, vtTR, vtTK, vtTS, vtTL, vtTC);
  TB4Term = class
    constructor create;
    procedure invoke(op : TTermOp); virtual;
  end;

implementation
uses ub4;

constructor TB4Term.create;
begin
end;

procedure ok; begin end;

procedure TB4Term.invoke(op : TTermOp);
begin
  case op of
    vtTG : ok; // begin dswp; kvm.gotoxy(dpop mod (xMax+1), dpop mod (yMax+1))
    vtTA : ok; // kvm.textattr := args[0];
    vtTW : if tos in [$00..$ff] then write(chr(dpop)) else write('[',dpop,']');
    vtTR : ok; // getc;
    vtTK : ok; // if keypressed then dput(-1) else dput(0)
    vtTS : ok; // kvm.clrscr;
    vtTL : ok; // kvm.clreol;
    vtTC : ok; // begin dput(kvm.wherex); dput(kvm.wherey) end;
  end
end;

begin
end.