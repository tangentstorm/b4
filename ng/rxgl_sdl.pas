{$i xpc.inc}
unit rxgl_sdl;
interface uses xpc, ng, SDL, sysutils, rt_term, cw;

  type
    TSDLVDP = class (rt_term.TRxConsole)
      pBitmap : pSDL_SURFACE;
      constructor Create;
      destructor Destroy; override;
      {$IFDEF WITH_AGG}
      procedure CreateCanvas; override;
      {$ENDIF}
      procedure PlotPixel(adr: Int32; Value: byte); override;
      procedure Display; override;
    end;

  procedure Main( rxvm : ng.TRetroVM );
  procedure step_sdl(vdp : TSDLVDP);

implementation

constructor TSDLVDP.Create;
begin

  SDL_INIT(SDL_INIT_VIDEO);
  SDL_EnableUnicode(1);
  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL);

  self.pBitmap := SDL_SETVIDEOMODE(canvas_w, canvas_h, bitdepth, SDL_HWSURFACE);
  if self.pBitmap = nil then
    raise Exception.Create('Failed to create SDL bitmap');
  inherited Create;
  {$IFDEF WITH_AGG}
  self.CreateCanvas;
  {$ENDIF}
end;

destructor TSDLVDP.Destroy;
begin
  SDL_FREESURFACE(self.pBitmap);
  SDL_QUIT;
end;

{$IFDEF WITH_AGG}
procedure TSDLVDP.CreateCanvas;
begin
  self.canvas := TRxCanvas.Create( canvas_w, canvas_h, pBitmap^.pixels );
end;
{$ENDIF}

procedure TSDLVDP.PlotPixel(adr: Int32; Value: byte); inline;
  var rBitmap: ^TRGBA;
begin
  if not assigned(self.pBitmap) then begin writeln('pBitmap is nil!'); halt end;
  rBitmap := pBitmap^.pixels + 4 * (self.rVStart * canvas_w + self.rHStart + adr);
  with rBitmap^ do
  begin
    r := Value;
    g := Value;
    b := Value;
    a := $ff;
  end
end;

procedure TSDLVDP.Display; inline;
begin
  SDL_FLIP(self.pBitmap);
end;

procedure step_sdl(vdp : TSDLVDP); inline;
var
  evt : pSDL_Event;
  key : TSDLKey;
  ch  : widechar;
begin
  New(evt);
  if SDL_PollEvent(evt) = 1 then
    case evt^.type_ of
      SDL_KEYDOWN :
	begin
	  key := evt^.key.keysym.unicode;
	  vdp.keyboard.SendKey(widechar(key));
	end;
      SDL_MOUSEMOTION :
	begin
	  vdp.mouse.x := evt^.motion.x;
	  vdp.mouse.y := evt^.motion.y;
	end;
      SDL_MOUSEBUTTONDOWN : vdp.mouse.buttons += [ evt^.button.button ];
      SDL_MOUSEBUTTONUp : vdp.mouse.buttons -= [ evt^.button.button ];
      SDL_QUITEV : Halt;
    end; { case }
  Dispose(evt)
end;


{ TODO: Mat2 Provided these routines for administering the terminal,
  but they were attached to the canvas device, and as far as I can
  tell were never actually used from retro. These probably make more
  sense on the extended terminal device.

function handle_termcolors( msg: int32 ) : int32;
  var x, y, h, w : int32; attr : tVdpAttrData;
begin
  result := 0;
  case msg of
    1	  : begin end;
    9	  : begin
	  data.pop2 (x, y);
	  attr := vdp.ReadAttrMap(canvas_w * y + x);
	  data.push (attr[0]);
	  data.push (attr[1]);
	end;
    10	  : begin
	   data.pop2(x, y);
	   data.pop2(h, w);
	   attr[0] := h; attr[1] := w;
	   vdp.WriteAttrMap(canvas_w * y + x, attr);
	 end;
       11 : begin
	     data.pop2(x, y);
	     data.push(vdp.ReadCharMap(canvas_w * y + x));
	   end;
    12	  : data.push(vdp.rBR);
    13	  : data.pop1(vdp.rBR);
    14	  : data.push(vdp.rFG);
    15	  : data.pop1(vdp.rFG);
    16	  : vdp.RenderDisplay;
    18	  : begin
	  data.pop3 (x, y, h);
	  vdp.PlotPixel(y * canvas_w + x, h);
	end;
    else
      result := -1;
  end
end;
}
  
procedure Main( rxvm : ng.TRetroVM );
  var vdp : TSDLVDP;
begin
  vdp := TSDLVDP.Create;
  vdp.Attach( rxvm );
  repeat
    rxvm.step; step_sdl(vdp)
  until rxvm.done;
  vdp.Destroy;
end;

initialization
end.
