{$mode objfpc}
unit rxgl_sdl;
interface uses ng, SDL, sysutils, rt_term;

  procedure Main( rxvm : ng.TRetroVM );

type
  TSDLVDP = class (rt_term.TRxConsole)
    pBitmap : pSDL_SURFACE;
    constructor Create;
    destructor Destroy; override;
    function PollKeyboard: char; override;
    procedure PlotPixel(adr: Int32; Value: byte); override;
    procedure Display; override;
  end;

implementation

constructor TSDLVDP.Create;
begin
  inherited Create;
  SDL_INIT(SDL_INIT_VIDEO);
  SDL_EnableUnicode(1);
  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL);

  self.pBitmap := SDL_SETVIDEOMODE(cScnXRes, cScnYRes, cScnCRes, SDL_HWSURFACE);
  if self.pBitmap = nil then
    raise Exception.Create('Failed to create SDL bitmap');
end;

destructor TSDLVDP.Destroy;
begin
  SDL_FREESURFACE(self.pBitmap);
  SDL_QUIT;
end;

procedure TSDLVDP.PlotPixel(adr: Int32; Value: byte); inline;
  var rBitmap: ^Int32;
begin
  pBitmap := self.pBitmap;
  rBitmap := self.rVStart * cScnXRes + self.rHStart +
    pBitmap^.pixels + adr;
  rBitmap^ := Value;
end;

procedure TSDLVDP.Display; inline;
begin
  SDL_FLIP(self.pBitmap);
end;

function TSDLVDP.PollKeyboard: char;
var
  done: boolean;
  evt: pSDL_Event;
  key: TSDLKey;
  ch: char;
begin
  done := False;
  NEW(evt);

  repeat
    if SDL_PollEvent(evt) = 1 then
      case evt^.type_ of
        SDL_KEYDOWN:
        begin
          key := evt^.key.keysym.unicode;
          if key in [1 .. 255] then
          begin
            ch := chr(key);
            done := True;
            result := ch;
          end;
        end;
        SDL_QUITEV: halt;
      end;
  until done;
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
	  attr := vdp.ReadAttrMap(cScnXRes * y + x);
	  data.push (attr[0]);
	  data.push (attr[1]);
	end;
    10	  : begin
	   data.pop2(x, y);
	   data.pop2(h, w);
	   attr[0] := h; attr[1] := w;
	   vdp.WriteAttrMap(cScnXRes * y + x, attr);
	 end;
       11 : begin
	     data.pop2(x, y);
	     data.push(vdp.ReadCharMap(cScnXRes * y + x));
	   end;
    12	  : data.push(vdp.rBR);
    13	  : data.pop1(vdp.rBR);
    14	  : data.push(vdp.rFG);
    15	  : data.pop1(vdp.rFG);
    16	  : vdp.RenderDisplay;
    18	  : begin
	  data.pop3 (x, y, h);
	  vdp.PlotPixel(y * cScnXRes + x, h);
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
  while not rxvm.done do rxvm.tick;
  vdp.Destroy;
end;

initialization
end.
