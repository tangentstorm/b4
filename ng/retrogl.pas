program retrogl;
uses
  zgl_main,
  zgl_types,
  zgl_window,
  zgl_screen,
  zgl_textures,
  zgl_keyboard,
  zgl_sprite_2d,
  zgl_timers,
  zgl_utils;

const
  canvas_w = 800;
  canvas_h = 600;
  tile_w   = 10;
  tile_h   = 10;

var
  texture : zglPTexture;
  tile : array [0 .. tile_w * tile_h - 1] of DWord;

procedure OnCreate;
  begin
    { Set up keyboard reporting. }
    key_beginReadText({initial buffer:}'', {buffer size:} 32);

    { Create the Texture }
    texture := tex_CreateZero( canvas_w, canvas_h );
    tex_SetFrameSize(texture, canvas_w, canvas_w);
    WriteLn( 'Created OpenGL texture #', texture^.ID );
  end;

procedure OnUpdate( dt : double );
  var x, y : word;
  begin
    if key_Down(K_ESCAPE) then zgl_exit
    else begin
      FillDWord(tile[0], length(tile), random($FFFFFFFF));
      x := random( canvas_w div tile_w );
      y := random( canvas_h div tile_h );
      tex_setData(texture, @tile[0],
		  x * tile_w, y * tile_h, tile_w, tile_h, 0 );
    end
  end;

procedure OnRender;
  begin
    asprite2d_Draw( texture, 0, 0, canvas_w, canvas_h,
		   {angle:}0, {frame:}0);
  end;

procedure OnKeyChar( Symbol : UTF8String );
  begin
    writeln( '  KeyChar: ', Symbol );
  end;

procedure FPSTimer;
  begin
    wnd_SetCaption( 'retrogl : ' +
		   u_IntToStr( zgl_Get( RENDER_FPS )) + ' FPS');
  end;

begin
  Randomize;
  wnd_SetCaption( 'retrogl' );
  wnd_ShowCursor( true );
  scr_SetOptions( 800, 600, REFRESH_MAXIMUM,
		 {fullscreen=}false, {vsync=}true );
  timer_add( @FPSTimer, 1000 );
  zgl_reg( SYS_LOAD,   @OnCreate);
  zgl_reg( SYS_UPDATE, @OnUpdate);
  zgl_reg( SYS_DRAW,   @OnRender);
  zgl_reg( INPUT_KEY_CHAR, @OnKeyChar );
  zgl_init;
end.
