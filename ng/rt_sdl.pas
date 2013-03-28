{$mode objfpc}
unit rt_sdl;
interface uses SDL, romvdp, sysutils;

  {TODO: extract interface IVDP for rt_*.CreateVDP }
  function CreateVDP : romvdp.TVDP;

implementation

  type
    TSDLVDP = class (romvdp.TVDP)
      pBitmap : pSDL_SURFACE;
      constructor Create;
      destructor Destroy; override;
      function PollKeyboard: char; override;
      procedure PlotPixel(adr: Int32; Value: byte); override;
      procedure Display; override;
    end;

function CreateVDP : romvdp.TVDP;
begin
  result := TSDLVDP.Create;
end;

constructor TSDLVDP.Create;
begin
  inherited Create;
  self.pBitmap := SDL_SETVIDEOMODE(cScnXRes, cScnYRes, cScnCRes, SDL_HWSURFACE);
  if self.pBitmap = nil then raise Exception.Create('Failed to create SDL bitmap');
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


initialization
  SDL_INIT(SDL_INIT_VIDEO);
  SDL_EnableUnicode(1);
  SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL);
end.
