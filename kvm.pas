{

  wrapper for keyboard, video, mouse

  copyright (c) 2012 michal j. wallace. all rights reserved.
  license: mit / isc

}
{$i xpc.inc }
unit kvm;
interface
  uses xpc, keyboard, video, mouse, crt;


  type
    keycode = (
      key_f1, key_f2, key_f3, key_f4, key_f5, key_f6, key_f7, key_f8, key_f9, key_f10, key_f11, key_f12,
      key_print, key_scroll, key_pause,
      key_0, key_1, key_2, key_3, key_4, key_5, key_6, key_7, key_8, key_9,
      key_backquote, key_esc, key_lbrack, key_rbrack, key_bs, key_tab, key_apos, key_comma, key_period,
      key_p, key_y, key_f, key_g, key_c, key_r, key_l, key_fslash, key_equal, key_bslash,
      key_a, key_o, key_e, key_u, key_i, key_d, key_h, key_t, key_n, key_s, key_minus, key_enter,
      key_semi, key_q, key_j, key_k, key_x, key_b, key_m, key_w, key_v, key_z,
      key_ctrl, key_os, key_alt, key_space, key_menu, key_shift,
      key_left, key_right, key_up, key_down, key_ins, key_del, key_home, key_end, key_pgup, key_pgdn
    );
    tKeyState = set of keycode;

    { buttons : for mouse / gamepad }
    vect2    = record
		 x, y : int32;
	       end;   

    color    = record
		 case separate : boolean of
		 true	       : ( r, g, b, a : byte );
		 false	       : ( c : int32 );
	       end;	       

    glyph    = record
		 codepoint : int32;
		 w, h	   : byte;
	       end;	   

    bmpfont  = record
		 size	: vect2;
		 glyphs	: array of glyph;
	       end;	

  { mouse stuff }
  function havemouse : boolean;
  function mx : int32;
  function my : int32;
  function mb : set32;

  { keyboard stuff }
  function readkey : int32;
  function kbstate : tKeyState;

  { text stuff }
  procedure clrscr;
  procedure gotoxy( x, y : int32 );
  procedure setfont( font :  bmpfont );

  { graphics stuff }
  function havegraphics : boolean;

implementation

  { -- mouse stuff ------------------------------------------ }

  function havemouse : boolean;
  begin
    result := false;
  end; { havemouse }

  function mx : int32;
  begin
    result := 0;
  end; { mx }

  function my : int32;
  begin
    result := 0;
  end; { my }

  function mb : set32;
  begin
    result := [];
  end; { mbtn }


  { -- keyboard stuff --------------------------------------- }

  function readkey : int32;
  begin
    result := 0;
  end; { readkey }

  function kbstate : tKeyState;
  begin
    result := [];
  end; { kbstate }


  { -- text stuff ------------------------------------------- }

  procedure clrscr;
  begin
    crt.clrscr;
  end; { clrscr }

  procedure gotoxy( x, y : integer );
  begin
    crt.gotoxy( x, y );
  end; { gotoxy }

  procedure write( args : array of const );
  begin
  end; { write }

  procedure setfont( font : bmpfont );
  begin
  end; { write }


  { -- graphics stuff --------------------------------------- }

  function havegraphics : boolean;
  begin
    result := false;
  end; { havegraphics }

end.
