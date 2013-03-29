program retrogl;
  uses
    aggbasics,
    agg2d,
    zgl_main,
    zgl_types,
    zgl_window,
    zgl_screen,
    zgl_textures,
    zgl_keyboard,
    zgl_sprite_2d ;


  const
    canvas_w	 = 300;	
    canvas_h	 = 100;	
    sprite_w	 = canvas_w div 3;
    sprite_h	 = canvas_h;

    { Some ARGB colors to work with. }
    kOpaque	 = $ff000000;
    kTranslucent = $88000000;
    rgb_blk	 = $000000;
    rgb_red	 = $ff0000;
    rgb_grn	 = $00ff00;
    rgb_blu	 = $0000ff;
    rgb_wht	 = $ffffff;

  type
    { This is just some arbitrary place in ram to store
      the image data. The actual type doesn't matter,
      since ZenGL and Agg will only see a pointer. }
    TPixelBuf = array of Int32;

    { This is just a stub for a real sprite class. }
    TSprite   = record
      x, y, dx, dy : integer;
    end;

  var
    pixels  : TPixelBuf;
    address : Pointer;
    texture : zglPTexture;
    canvas  : agg2d.TAgg2D;
    i       : integer;  // standard loop variables
    sprites : array [ 0 .. 255 ] of TSprite;
    desk_w,
    desk_h  : integer;


  { This shows the basic usago for AggPas. What this code does is
    draw three translucent circles: one red, one blue, one green.
    We will use this as a tiny sprite sheet. }
  procedure DrawSomething(canvas : TAgg2D);
    var
      r	: array[ 0 .. 2 ] of byte = ( $ff, $00, $00 );
      g	: array[ 0 .. 2 ] of byte = ( $00, $ff, $00 );
      b	: array[ 0 .. 2 ] of byte = ( $00, $00, $bb );
  begin
    { The high-level API for AggPas is documented here:
      http://www.crossgl.com/aggpas/documentation/index.html
      Though there seem to be a few undocumented ones, like circle. :) }
    canvas.ClearAll(0,0,0,0);

    { three translucent circles, for the three frames: }
    for i := 0 to 2 do
    begin
      canvas.LineWidth := 5;
      canvas.LineColor.Initialize($00, $00, $00, $FF); { black }
      canvas.FillColor.Initialize(r[i], g[i], b[i], $88 ); { translucent fill }
      canvas.Circle( sprite_w div 2 + i * sprite_w, sprite_h div 2, {radius=}32);
      
      canvas.FillColor.Initialize(0, 0, 0, 0); { no fill }
      canvas.LineWidth := 2;
      canvas.LineColor.Initialize($ff, $ff, $ff, $ff); { white }
      canvas.Circle( sprite_w div 2 + i * sprite_w, sprite_h div 2, {radius=}32);
    end
  end; { DrawSomething }


  procedure OnCreate;
  begin
    
    key_beginReadText({initial buffer value :=} '',
		      {arbitrary buffer limit := } 16 );
    
    desk_w := zgl_get(desktop_width);
    desk_h := zgl_get(desktop_height);

    { Allocate a buffer in ram for the bitmap. }
    setlength(pixels, canvas_w * canvas_h);
    address := @pixels[0];

    { Create the TAgg2D canvas and tell it where the buffer is. }
    canvas := TAgg2D.Create(agg2d.pfRGBA);
    canvas.Attach(address, canvas_w, canvas_h, {stride=} canvas_w * 4);

    { Draw whatever you want on the canvas. }
    DrawSomething(canvas);

    { Now we need to create a texture. ZenGL passes this data to OpenGL, which
      makes its own copy of the data, presumably on-board the graphics card. }
    texture := tex_Create( PByteArray( address ), canvas_w, canvas_h,
			  TEX_FORMAT_RGBA, {Flags=} 0 );

    { zengl supports spritesheets, so we will divide the 300x100 image into
      three 100x100 frames. This only affects the meta-data. We will choose
      which frame to display }
    tex_SetFrameSize(texture, sprite_w, sprite_h);
    
    { The zglPTexture object we get back contains only meta-data. The only link
      to OpenGL's internal data is through a sequential identifier, stored in
      texture.ID. ZenGL seems to create one texture for its own nefarious
      purposes, so the first one we create ourselves should have ID=2. }
    WriteLn( 'Created OpenGL texture #', texture^.ID );

    { Finally, we'll set up some random points and velocities for
      drawing these things. The velocities are random numbers between
      -5 and +5. Note that each sprite has just under a 1% chance
      of being stationary (1/11*11), so that's perfectly normal. }
    for i := low( sprites ) to high( sprites ) do
      with sprites[i] do
      begin
	x  := random( desk_w - sprite_w );
	y  := random( desk_h - sprite_h );
	dx := random( 11 ) - 5;
	dy := random( 11 ) - 5;
      end
  end; { create }


  { This routine gets called on every tick, after Update. }
  { All it's doing is moving the sprites and making them
    bounce off the walls. }
  procedure OnUpdate( dt : double );
  begin
    if key_Down(K_ESCAPE) then zgl_exit
    else for i := low( sprites ) to high( sprites ) do
      with sprites[i] do
      begin
	x := x + dx;
	y := y + dy;
	if (x < 0) or (x > desk_w - sprite_w) then dx := -dx;
	if (y < 0) or (y > desk_h - sprite_h) then dy := -dy;
      end;
  end; { Update }


  { This routine gets called on every tick, after Update. }
  procedure OnRender;
  begin
    { zgl_sprite_2d.asprite2d_draw is for "animated" sprites,
      where a single draw the image as a sprite. We pick the
      frame based on the index of the sprite in the array.
      Animated sprites work on the same principle, they'd
      just need a counter. }
    for i := low( sprites ) to high( sprites ) do
      asprite2d_Draw(texture, sprites[i].x, sprites[i].y,
		     sprite_w, sprite_h, {angle:=} 0, {frame:=}i mod 3 );

  end;

  procedure OnKeyChar( Symbol : UTF8String );
  begin
    writeln( '  KeyChar: ', Symbol );
  end;

begin
  wnd_SetCaption('retrogl');
  scr_SetOptions(800, 600, refresh_default, {fullscreen=}false, {vsync=}true);
  zgl_reg( SYS_LOAD,   @OnCreate);
  zgl_reg( SYS_UPDATE, @OnUpdate);
  zgl_reg( SYS_DRAW,   @OnRender);
  zgl_reg( INPUT_KEY_CHAR, @OnKeyChar );
  zgl_init;
end.
