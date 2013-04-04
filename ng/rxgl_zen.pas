{$i xpc.inc}
unit rxgl_zen;
interface uses xpc, ng, sysutils, rt_term, rxgl_sdl,
  zgl_main,
  zgl_types,
  zgl_window,
  zgl_screen,
  zgl_textures,
  zgl_keyboard,
  zgl_sprite_2d,
  zgl_fx,
  zgl_timers,
  zgl_utils;


  procedure Main( rxvm : ng.TRetroVM );

  type
    TZenGLVDP = class (TSDLVDP)
      procedure Display; override;
    end;

implementation

var
  vm : ng.TRetroVM;
  vt : TSDLVDP;
  texture : zglPTexture;


procedure TZenGLVDP.Display;
begin
  inherited Display;
  tex_setData( texture, vt.pBitmap^.pixels, 0, 0, canvas_w, canvas_h);
end;

procedure OnLoad;
begin
  vt.Attach( vm );
  texture := tex_CreateZero( canvas_w, canvas_h );
  tex_SetFrameSize(texture, canvas_w, canvas_w);
  { Set up keyboard reporting. }
  key_beginReadText({initial buffer:}'', {buffer size:} 32);
end;

procedure OnStep;
  var i : integer = 0;
  const opsPerStep = 65535; { arbitrary number }
begin
  repeat vm.step until vm.waiting or vm.done;
  step_sdl(vt);
  if vm.done then zgl_exit
end;

procedure OnDraw;
begin
  { SDL images are upside down from what ZenGL expects, hence FX2D_FLIPY. }
  ssprite2d_Draw( texture, 0, 0, canvas_w, canvas_h,
		 {angle:} 0, {alpha:} $ff, FX2D_FLIPY );
end;

procedure OnChar( Symbol : UTF8String );
begin
  vt.keyboard.SendKey(Symbol[1]); {TODO : decode utf8. }
end;

procedure OnTick; { for the fps timer }
begin
  wnd_SetCaption( 'retrogl : ' + u_IntToStr( zgl_Get( RENDER_FPS )) + ' FPS');
end;

procedure OnExit;
begin
  vt.Destroy;
end;

procedure Main( rxvm : ng.TRetroVM );
begin
  vt := TZenGLVDP.Create;
  vm := rxvm;
  zgl_init;
end;

initialization
  zgl_Disable( APP_USE_LOG );
  zgl_Disable( APP_USE_AUTOPAUSE );
  zgl_reg( SYS_LOAD,   @OnLoad);
  zgl_reg( SYS_UPDATE, @OnStep);
  zgl_reg( SYS_DRAW,   @OnDraw);
  zgl_reg( INPUT_KEY_CHAR, @OnChar );
  zgl_reg( SYS_EXIT,   @OnExit);
  timer_add( @OnTick, 1000 );
  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, {fullscreen=}false, {vsync=}true );
end.
