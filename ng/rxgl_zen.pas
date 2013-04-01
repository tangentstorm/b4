{$mode objfpc}
unit rxgl_zen;
interface uses ng, sysutils, rt_term, rxgl_sdl,
  zgl_main,
  zgl_types,
  zgl_window,
  zgl_screen,
  zgl_textures,
  zgl_keyboard,
  zgl_sprite_2d,
  zgl_timers,
  zgl_utils;

  procedure Main( rxvm : ng.TRetroVM );

implementation

var
  vm : ng.TRetroVM;
  vt : TSDLVDP;

procedure OnLoad;
begin
  vt.Attach( vm );
end;

procedure OnStep;
  var i : integer = 0;
  const opsPerStep = 65535; { arbitrary number }
begin
  repeat
    inc(i); vm.tick {todo: raname to step }
  until (i = opsPerStep) or vm.done;
  if vm.done then zgl_exit
end;

procedure OnExit;
begin
  vt.Destroy;
end;

procedure Main( rxvm : ng.TRetroVM );
begin
  vt := TSDLVDP.Create;
  vm := rxvm;
  zgl_init;
end;

initialization
  zgl_Disable( APP_USE_LOG );
  zgl_Disable( APP_USE_AUTOPAUSE );
  zgl_reg( SYS_LOAD,   @OnLoad);
  zgl_reg( SYS_UPDATE, @OnStep);
//  zgl_reg( SYS_DRAW,   @OnDraw);
//  zgl_reg( INPUT_KEY_CHAR, @OnChar );
  zgl_reg( SYS_EXIT,   @OnExit);
  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, {fullscreen=}false, {vsync=}true );
end.
