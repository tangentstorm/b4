{$mode objfpc}
unit rxgl_sdl;
interface uses
  gqueue,
  ng, SDL, sysutils, rt_term, math,
  zgl_main,
  zgl_types,
  zgl_window,
  zgl_screen,
  zgl_textures,
  zgl_keyboard,
  zgl_sprite_2d,
  zgl_timers,
  zgl_utils;

  procedure Main( vm : ng.TRetroVM );

implementation
type
  TZenGLVDP = class (rt_term.TRxConsole)
    function PollKeyboard: char; override;
    procedure PlotPixel(adr: Int32; Value: byte); override;
    procedure Display; override;
  end;

const
  canvas_w = 800;
  canvas_h = 600;

var
  pBitmap : pSDL_SURFACE;
  rxvm	  : ng.TRetroVM;
  rxvt	  : TZenGLVDP;
  texture : zglPTexture;

procedure OnCreate;
begin
  { Set up keyboard reporting. }
  key_beginReadText({initial buffer:}'', {buffer size:} 32);

  SDL_INIT(SDL_INIT_VIDEO);
  SDL_EnableUnicode(1);
  //SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL);
  pBitmap := SDL_SETVIDEOMODE(cScnXRes, cScnYRes, cScnCRes, SDL_HWSURFACE);


  { Create the Texture }
  texture := tex_CreateZero( canvas_w, canvas_h );
  tex_SetFrameSize(texture, canvas_w, canvas_w);
  WriteLn( 'Created OpenGL texture #', texture^.ID );
end;

procedure OnExit;
begin
  SDL_FREESURFACE(pBitmap);
  SDL_QUIT;
end;

procedure TZenGLVDP.PlotPixel(adr: Int32; Value: byte); inline;
  var rBitmap: ^Int32; x, y :word; color: uint32;
begin
  rBitmap := self.rVStart * cScnXRes + self.rHStart + pBitmap^.pixels + adr;
  rBitmap^ := Value;
  {
  DivMod(adr, canvas_w, y,x);
  color := value;
  tex_setData(texture, @color, x, y, 1, 1, 0 );
    }
end;

procedure TZenGLVDP.Display; inline;
begin
  SDL_FLIP(pBitmap);
end;

procedure OnRender;
begin
  //Write('<RENDER>');
  asprite2d_Draw( texture, 0, 0, canvas_w, canvas_h, {angle:}0, {frame:}0);
  //Writeln('</RENDER>');
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

type
  TKeyBuffer =  specialize TQueue<WideChar>;
var
  KeyBuffer : WideChar = #0; // TKeyBuffer;
  evt	    : pSDL_EVent;

function TZenGLVDP.PollKeyboard: char;
begin
  if KeyBuffer = #0 then
    raise ng.ENotFinished.Create('ReadKey')
  else begin
    Result := KeyBuffer;
    KeyBuffer := #0;
  end
end;
    {
  WriteLn('<ReadKey>');
  
  if KeyBuffer.IsEmpty then
    begin
      Writeln('<WAIT>');

    end
  else begin
    result := KeyBuffer.Front; KeyBuffer.Pop; // TODO: combine these!
  end
end;
      }

procedure OnKeyChar( Symbol : UTF8String );
  var ch : WideChar;
begin
  KeyBuffer := Symbol[1]; {TODO : decode utf8. }
{  ch := 
  write('[ ', ch, ']');
  KeyBuffer.Push(ch);}
end;



procedure OnUpdate( dt : double );
  var i: word = 0;
begin
  write('.');
  SDL_PollEvent(evt);
  repeat
    rxvm.tick; inc(i)
  until (i = 1024) or rxvm.done or rxvm.waiting;
  if rxvm.done then zgl_exit
end;

procedure FPSTimer;
begin
  wnd_SetCaption( 'retrogl : ' + u_IntToStr( zgl_Get( RENDER_FPS )) + ' FPS');
end;

procedure Main( vm : ng.TRetroVM );
begin
{  KeyBuffer := TKeyBuffer.Create;}
  new(Evt);
  rxvm := vm;
  rxvt := TZenGLVDP.Create;
  rxvt.Attach( rxvm );
  Randomize;
  wnd_SetCaption( 'retrogl' );
  wnd_ShowCursor( true );
  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, {fullscreen=}false, {vsync=}true );
  timer_add( @FPSTimer, 1000 );
  zgl_reg( SYS_LOAD,   @OnCreate);
  zgl_reg( SYS_UPDATE, @OnUpdate);
  zgl_reg( SYS_DRAW,   @OnRender);
  zgl_reg( SYS_EXIT,   @OnExit);
  zgl_reg( INPUT_KEY_CHAR, @OnKeyChar );
  zgl_init;
end;

initialization
end.
