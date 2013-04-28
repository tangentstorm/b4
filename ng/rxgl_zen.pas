{$i xpc.inc}
unit rxgl_zen;
interface uses xpc, ng, sysutils, rt_term,
  math,
  zgl_main,
  zgl_types,
  zgl_window,
  zgl_screen,
  zgl_textures,
  zgl_keyboard,
  zgl_sprite_2d,
  zgl_render_target,
  zgl_primitives_2d,
  zgl_fx,
  zgl_timers,
  zgl_utils;

  procedure Main( rxvm : ng.TRetroVM );

  type
    TZenGLVDP = class (rt_term.TRxConsole)
      target : zglPRenderTarget;
      constructor Create;
      destructor Destroy; override;
      procedure PlotPixel( offset : Int32; value: byte ); override;
      procedure Display; override;
    end;

implementation

var
  vm : ng.TRetroVM;
  vt : TZenGLVDP;

constructor TZenGLVDP.Create;
begin
  inherited Create;
  self.target := rtarget_Add( tex_CreateZero( canvas_w, canvas_h ), RT_DEFAULT );
end;

destructor TZenGLVDP.Destroy;
begin
  inherited Destroy;
end;

procedure TZenGLVDP.PlotPixel( offset : Int32; value : byte );
  var x, y : int32;
begin
  DivMod( offset, canvas_w, y, x );
  pr2d_Pixel( x, y, (value shl 16) + (value shl 8) + value, $FF );
end;

procedure TZenGLVDP.Display;
begin
end;

procedure OnLoad;
begin
  vt := TZenGLVDP.Create;
  vt.Attach( vm );
  { Set up keyboard reporting. }
  key_beginReadText({initial buffer:}'', {buffer size:} 32);
end;

procedure OnStep;
  var i : cardinal = 1;
  const opsPerStep = 1 shl 19; { arbitrary number }
begin
  rtarget_Set( vt.target );
  if vm.done then zgl_exit
  else if vt.keyboard.needKey then pass
  else
    repeat inc(i); vm.step
    until (i = opsPerStep) or vm.done or vt.keyboard.needKey;
  rtarget_Set( nil );
end;

procedure OnDraw;
  const x = 4; y = 20;
begin
//  divmod( cScnHLine, canvas_w, y, x );
  ssprite2d_Draw( vt.target^.surface, x, y, canvas_w, canvas_h,
		 {angle:} 0, {alpha:} $ff );
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
