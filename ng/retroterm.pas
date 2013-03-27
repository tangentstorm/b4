{ retroterm: retro extended terminal }
{$mode objfpc}
unit retroterm;
interface uses romVDP;

  type
    TRetroTerm = class
      cx, cy  : Int32;
      count   : Int32; {  what does this do? }
      refresh : Int32;
      vdp     : TVDP;
      attr: tVDPAttrData;
      constructor Create;
      procedure Clear;
      procedure Fw;
      procedure Bw;
      procedure Bs;
      procedure Cr;
      procedure emit( x	:  int32 );
    end;

implementation
  var
    adr	: longword;

  constructor TRetroTerm.Create;
  begin
    vdp := TVDP.Create;
    self.refresh := vdp.termW * 160;
  end;

  procedure TRetroTerm.clear; inline;
    var i : longword;
  begin
    for i := 0 to cScnChrSize do vdp.aCharMap[i] := 0;
    for i := 0 to cScnAtrSize do vdp.aAttrMap[i] := 0;
    cx := 0; cy := 0;
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
    adr := cy * vdp.termW + cx;
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
