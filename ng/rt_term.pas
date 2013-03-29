{ retroterm: retro extended terminal }
{$mode objfpc}
unit rt_term;
interface uses romVDP;

  type
    TRetroTerm = class
    private
      _cx, _cy  : Int32;
      adr	: longword;
      procedure resetadr;
      procedure set_cx(x:int32);
      procedure set_cy(y:int32);
      procedure Fw;
      procedure Bw;
      procedure Bs;
      procedure Cr;
    public
      vdp       : TVDP;
      count     : Int32; {  what does this do? }
      refresh   : Int32;
      attr: tVDPAttrData;
      constructor Create( aVDP : TVDP );
      procedure Clear;
      procedure Emit( x	:  int32 );
      property cx:int32 read _cx write set_cx;
      property cy:int32 read _cy write set_cy;
    end;

implementation

  constructor TRetroTerm.Create( aVDP : TVDP );
  begin
    vdp := aVDP;
    self.refresh := vdp.termW * 160;
  end;

  procedure TRetroTerm.clear; inline;
  begin
    vdp.Clear; cx := 0; cy := 0;
  end;

  procedure TRetroTerm.resetadr; inline;
  begin
    adr := cy * vdp.termW + cx;
  end;

  procedure TRetroTerm.set_cx(x:int32); inline;
  begin
    _cx := x; resetadr;
  end;

  procedure TRetroTerm.set_cy(y:int32);
  begin
    _cy := y; resetadr;
  end;

  procedure TRetroTerm.fw; inline;
  begin
    if cx < vdp.termW then cx := cx + 1
    else if cy < vdp.termH then begin cy := cy + 1; cx := 0; end
    else begin clear; cy := 0; cx := 0; end;
  end;

  procedure TRetroTerm.bw; inline;
  begin
    if cx < vdp.termW then cx := cx-1;
  end;

  procedure TRetroTerm.bs; inline;
  begin
    attr[1] := 0;
    vdp.WriteAttrMap(adr, attr);
    vdp.WriteCharMap(adr, 0);
    if (cx < vdp.termW) and (cx > 0) then cx := cx - 1;
  end;

  procedure TRetroTerm.cr; inline;
  begin
    attr[1] := 0;
    vdp.WriteAttrMap(adr, attr);
    cy := cy + 1; cx := 0;
    if cy >= vdp.termH then begin
      clear; cy := 0; cx := 0;
    end;
  end;

  procedure TRetroTerm.emit( x : int32 );
  begin
    if x < 0 then clear else
      if x < 32 then case chr (x) of
	^H : bs;
	^J : cr;
	^M : cr;
      end;
    attr[1] := vdp.rBR; vdp.WriteAttrMap(adr, attr);
    if x > 31 then begin
      attr[1] := 0;
      vdp.WriteAttrMap(adr, attr);
      vdp.WriteCharMap(adr, x);
    end;
    if x > 31 then fw;

    count := count + 1;
    if count = refresh then begin
      count := 0; vdp.RenderDisplay;
    end;
  end;

begin
end.
