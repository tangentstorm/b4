NB. ----------------------------------------------
NB. j virtual terminal support.
NB. supports windows and linux (and osx??)
NB. ----------------------------------------------
cocurrent 'vt'

NB. obtain a pointer to stdin, so we can ungetc()
uh_libc =: >0{'libdl.so dlopen * *c i' cd 'libc.so.6';1
uh_stdi0 =. >0{'libdl.so dlsym * x *c'  cd uh_libc;'stdin'
uh_stdi =: memr uh_stdi0,0,1,4

u_fcntl =: 'libc.so.6 fcntl i i i i'&cd
u_ungetc =: 'libc.so.6 ungetc i i x'&cd
u_usleep =: 'libc.so.6 usleep i i'&cd

sleep =: (0 0 $ [: u_usleep 1000&*)
noblock =: {{ u_fcntl 0 4 2048 }}
keyp=:{{ noblock raw 1 if. _1=k=.>rkey'' do. 0 else. 1: u_ungetc k,uh_stdi end. }}

help =: 0 : 0

  'vt' locale: j virtual terminal support
  ----------------------------------------
  raw b     -> enter(b=1)/leave(b=0) raw mode
  keyp''    -> bit. is key pressed?
  rkey''    -> read key from keyboard
  wfc c     -> wait for character c to be pressed

  cscr''    -> clear screen
  ceol''    -> clear to end of line

  gethw''   -> get video height/width
  xmax''    -> get max x coordinate
  ymax''    -> get max y coordinate

  curs b    -> hide(b=0) or show(b=1) cursor
  curxy''   -> get cursor xy
  goxy x,y  -> set cursor xy

  puts s    -> emit string
  putc c    -> emit character

  fgc n     -> set foreground color
  bgc n     -> set background color
  reset''   -> reset to default colors

  sleep n   -> sleep for n milliseconds
)



NB. ANSI/VT-100/xterm escape codes
NB. -------------------------------------------------------
NB. these all-caps names indicate string constants
NB. or verbs that return strings. This is so you can
NB. build up your own sequences in bulk.

ESC  =: u:27               NB. ANSI escape character
CSI  =: ESC,'['            NB. vt100 'command sequence introduce'
CSCR =: ,CSI,"1 0'HJ'      NB. clear screen
CEOL =: CSI,'0K'           NB. clear to end of line
RESET =: CSI,'0m'          NB. reset color to gray on black
CURSU =: CSI,'A'           NB. cursor up
CURSD =: CSI,'B'           NB. cursor down
CURSL =: CSI,'D'           NB. cursor left
CURSR =: CSI,'C'           NB. cursor right

NB. xterm 256-color codes
NB. Most modern terminals seem to support 256 colors.
NB. FG256 y ->str.
FG256=: CSI,'38;5;', 'm',~ ":   NB. fg (numeric)
BG256=: CSI,'48;5;', 'm',~ ":   NB. bg (numeric)

NB. GOXY[X,Y]->str: code to set cursor position
GOXY =: CSI, ":@{:, ';', 'f',~ ":@{.

NB. CURS [0/1]->str: code to show/hide cursor
CURS =: CSI,'?25','lh'{~]


NB. ----------------------------------------------------
NB. windows terminal:
NB. ----------------------------------------------------

NB. https://docs.microsoft.com/en-us/windows/console/console-functions
ReadConsoleInput =: 'kernel32 ReadConsoleInputW b x *s s *s'&cd
GetConsoleScreenBufferInfo =: 'kernel32 GetConsoleScreenBufferInfo c x *s'&cd
GetStdHandle =: 'kernel32 GetStdHandle *c s'&cd

w_rkey =: {{ for.i.y do.
  RBS =. 10 NB. read buffer size
  'r h b n nr' =. ReadConsoleInput w_stdi;(*#~RBS);1;(,0)
  'FOCUS KEY MENU MOUSE SIZE' =.  16b10 16b01 16b08 16b02 16b04
  select. 0 { b
    case. FOCUS do. echo 'FOCUS ',":b
    case. KEY do.
      dn=. 2{b
      'rc kc sc ch' =. 4{.4}.b
      ch =. u:ch
      mod =. '.x'{~_32{.!.0#: 65536 65536 #. _2{.b
      smoutput 'dn:';dn;'rc:';rc;'kc:';kc;'ch:';ch;'mod:';mod
    case. MENU do.  echo 'MENU: ',":b
    case. MOUSE do. echo 'MOUSE: ',":b
    case. SIZE do. echo 'SIZE: ',":b
    case. do. 'UNKNOWN EVENT!' throw.
  end.
end. }}

w_gethw =: {{
  'l t r b'=. 4{.5}.>{: GetConsoleScreenBufferInfo w_stdo;22#0
  (b-t),(r-l) }}

NB. ----------------------------------------------------
NB. vt-100 terminal queries (must be done in raw mode)
NB. ----------------------------------------------------
NB. TODO: verify that this works on windows, or re-implement
v_gcxy =: {{ NB. get current xy. must be done in raw_mode
  puts CSI,'6n'
  wfc"0 CSI
  r=.'' while.'R'~:a.{~>c=.rkey''do. r=.r,c end.
  |.0".>';' splitstring a.{~>r }}



init =: {{
  NB. define (raw, rkey, gethw, putc) + platform-specific helpers
  if. IFWIN do.
    w_stdi =: >{. GetStdHandle _10
    w_stdo =: >{. GetStdHandle _11
    raw  =: ]
    rkey =: w_rkey
    gethw =: w_gethw
    putc =: w_putc
  elseif. IFUNIX do.
    u_raw1 =: '' [ [: 2!:0 'stty raw -echo' []
    u_raw0 =: ][ [: 2!:0 'stty -raw echo' []
    NB.u_libc =: >0{ (LF cut 2!:0) ::(a:"_) 'locate libc.so'
    u_libc =: 'libc.so.6'
    raw  =: {{ if. y do. u_raw1'' else. u_raw0'' end. }}
    rkey =: (u_libc,' getchar l') & cd
    gethw =: {{ _".}: 2!:0 'stty size' }}
    putc =: 0 0 $ (u_libc,' putchar  n c') & cd
  else.
    'vt.ijs only supports windows and unix platforms.'
  end. >a: }}

wfc =: {{ while.y~:r=.a.{~>rkey'' do. end. }}

cscr =: puts@CSCR
ceol =: puts@CEOL

xmax =: <:@{:@gethw
ymax =: <:@{.@gethw
curxy =: v_gcxy

puts =: putc"0
goxy =: puts@GOXY
curs =: puts@CURS

fgc =: puts@FG256
bgc =: puts@BG256
reset=: puts@RESET

NB. these two are just handy to type when your screen gets messy:
cls =: CSCR_vt_
bw  =: RESET_vt_

init''
coinsert_base_'vt'


NB. just a demo to show colors and waiting for a keypress.
demo =: {{
  cscr@'' reset''
  puts '256 terminal colors:',CR,LF
  (puts@(CR,LF) ([: puts ' ',~hfd [ fgc)"0)"1  i.16 16
  reset''
  puts^:2 CR,LF
  puts 'press a key!'
  xy=.curxy'' [ s =. ' _.,oO( )Oo,._ ' [ raw 1
  while. -. keyp'' do.
    goxy xy [ bgc 4 [ fgc 9
    puts '['
    fgc 15
    puts 4{. s=.1|.s
    fgc 9
    puts']'
    sleep 150
  end.
  echo a.{~>rkey'' [ reset''
  puts^:2 CR,LF
}}
