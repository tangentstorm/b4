{  kvm : wrapper for keyboard, video, and mouse units
   copyright (c) 2012 michal j. wallace. all rights reserved.
   license: mit / isc
}
{$i xpc.inc }
unit kvm;
interface uses xpc, keyboard, video, mouse;

{  this should probably get moved into its own class? }
type
  vector2d = record
	       case kind : ( asize, apoint, avec2d ) of
		 asize	: ( w, h : int32 );
		 apoint	: ( x, y : int32 );
		 avec2d	: ( v : array[ 0 .. 1 ] of int32 );
	     end;

{-- interface > keyboard --}
type
  { a set of symbols for the common physical keys ( on USA keyboards ) }
  keycode     = (
    key_unknown, { just in case something is missing }
    key_f1, key_f2, key_f3, key_f4, key_f5, key_f6, key_f7, key_f8,
    key_f9, key_f10, key_f11, key_f12, key_print, key_scroll,
    key_pause, key_0, key_1, key_2, key_3, key_4, key_5, key_6,
    key_7, key_8, key_9, key_backquote, key_esc, key_lbrack,
    key_rbrack, key_bs, key_tab, key_apos, key_comma, key_period,
    key_p, key_y, key_f, key_g, key_c, key_r, key_l, key_fslash,
    key_equal, key_bslash, key_a, key_o, key_e, key_u, key_i, key_d,
    key_h, key_t, key_n, key_s, key_minus, key_enter, key_semi,
    key_q, key_j, key_k, key_x, key_b, key_m, key_w, key_v, key_z,
    key_ctrl, key_os, key_alt, key_space, key_menu, key_shift,
    key_left, key_right, key_up, key_down, key_ins, key_del,
    key_home, key_end, key_pgup, key_pgdn );

  tKeyState = set of keycode;

function readkey : char;
function kbstate : tKeyState;


{-- interface > video > graphics --}
type
  color	  = record
	      case separate : boolean of
		true  : ( r, g, b, a : byte );
		false : ( c : int32 );
	    end;

  surface = record
	      w, h : int32;
	      data : array of int32;
	    end;

function hascanvas : boolean;
var canvas : surface;

{-- interface > video > terminal --}
type
  glyph	  = record
	      codepoint	: int32;
	      w, h	: int32;
	    end;

  bmpfont = record
	      size   : vector2d;
	      glyphs : array of glyph;
	    end;

procedure clrscr;
procedure gotoxy( x, y : int32 );
procedure fg( c : char );  procedure fg( b : byte );
procedure bg( c : char );  procedure bg( b : byte );
procedure setfont( font :  bmpfont );
var term : surface;

{-- interface > mouse --}

{  TODO type buttons = (??) for mouse / gamepad }

function hasmouse : boolean;
function mx : int32;
function my : int32;
function mb : set32;

implementation

{ -- implementation > mouse ------------------------------ }

{  mouse routines are just stubs at the moment }

function hasmouse : boolean;
begin
  result := false;
end; { hasmouse }

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

{ -- implementation > keyboard --------------------------- }

var
  have_cached : boolean;
  cached_key  : char;
  
function readkey : char;
  var Key: keyboard.TKeyEvent;
begin
  if have_cached then begin
    have_cached := false;
    result := cached_key;
  end else begin
    key := TranslateKeyEvent(GetKeyEvent);
    case GetKeyEventFlags(Key) of
      kbASCII : result := GetKeyEventChar(Key);
      else
      begin
	cached_key := chr( GetKeyEventCode(Key));
	have_cached := true;
	result := #0;
      end
    end
  end;
end; { readkey }

function kbstate : tKeyState;
begin
  result := [];
end; { kbstate }


{ -- implementation > video > graphics -------------------- }

function hascanvas : boolean;
begin
  result := false;
end; { hascanvas }


{ -- implementation > video > text > general ------------- }

procedure clrscr;
begin
  write( #27, '[2J' );
  { crt.clrscr; }
end; { clrscr }


procedure ansi_reset( i :  byte );
begin
  write( #27, '[0m' );
end; { ansi_reset }


procedure gotoxy( x, y : integer );
begin
  write( #27, '[', y + 1, ';', x + 1, 'H' );
  { crt.gotoxy( x + 1, y + 1 ); }
end; { gotoxy }

  
procedure setfont( font : bmpfont );
begin
end; { write }


{ -- implementation > video > text > fg color ------------ }

procedure ansi_fg( i : byte );
begin
  if i < 8 then write( #27, '[0;3', i , 'm' )           // ansi dim
  else if i < 17 then write( #27, '[01;3', i-8 , 'm' ); // ansi bold
  // else do nothing
end; { ansi_fg }

{ xterm 256-color extensions }
procedure xterm_fg( i	:  byte );
begin
  write( #27, '[38;5;', i , 'm' );
end;

{ --- public --- }

procedure fg( c :  char );
  var i : byte;
begin
  {  crt.textcolor( i - 1 ); }
  i := pos( c, 'krgybmcwKRGYBMCW' );
  if i > 0 then ansi_fg( i - 1 ) ;
end; { fg }

procedure fg( b : byte );
begin
  xterm_fg( b );
end; { fg }

{ -- implementation > video > text > bg color ------------ }

{  implement ansi_bg  -- below is only a copy/paste of ansi_fg }
{procedure ansi_bg( i : byte );
begin
  if i < 8 then write( #27, '[0;3', i , 'm' )           // ansi dim
  else if i < 17 then write( #27, '[01;3', i-8 , 'm' ); // ansi bold
  // else do nothing
end; }
  
procedure xterm_bg( i	:  byte );
begin
  write( #27, '[48;5;', i , 'm' );
end;

procedure bg( c :  char );
var i : byte;
begin
  {  crt.textbackground( i - 1 ); }
  i := pos( 'krgybmcwKRGYBMCW', c );
  if i > 7 then xterm_bg( i - 1  ) else xterm_bg( 7 );
end;

procedure bg( b : byte );
begin
  xterm_bg( b );
end; { fg }
  

initialization
  keyboard.initkeyboard;
finalization
  keyboard.donekeyboard;
end.
