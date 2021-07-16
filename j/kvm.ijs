NB. terminal mode keyboard/video/mouse driver
NB. supports windows and linux.

cocurrent 'z'
putc =: putc_kvm_
puts =: putc_kvm_"0
with_kbd =: with_kbd_kvm_
kvm =: loop_kvm_

cocurrent 'kvm'
NB. ----------------------------------------------------

NB. wfk y: wait for key code y
wfk =: {{ while.y~:>r=.rkey'' do. end. }}

NB. wait for character
wfc =: {{ while.y~:r=.a.{~>rkey'' do. end. }}


NB. with_kbd: run v on keypress.
NB. (u is meant to be an event loop, but it will have to wait
NB. until i get keyp'' test working)
with_kbd =: {{y[ v'' [ raw 1  while. 1-:v rkey'' do. u'' end. raw 0 }}

break=:0

onkey =: {{
  if. y-:'' do. 1 return. end.
  select. c =. 1 27 28 31 126 I. k =. {.>y
  case. 0 do. vnm =. 'k_nul'
  case. 1 do. vnm =. 'kc_',a.{~97+<:k  NB. ascii ctrl+letter ^C-> kc_c
  case. 2 do. vnm =. 'k_esc'
  NB. case. 3 do. NB. TODO ^\, ^], ^^, ^_ (FS,GS,RS,US)
  case. 4 do. vnm =. 'k_',a.{~k
  case.   do. vnm =. 'kx_',hfd k
    NB. hex code catchall (k=255)-> kc_ff
    NB. ^? = KDEL, other alt chars
  end.
  NB. ask for more keys unless break=1
  if. 3=4!:0<vnm do. (vnm~) a.{~>y
  elseif. (c=4)*.3=4!:0<'k_asc' do. 1[k_asc a.{~>y
  elseif. 3=4!:0<'k_any' do. 1[k_any a.{~>y
  elseif. k e. 3 0 do. break_kvm_ =: 1
  end.
  -. break_kvm_ +. k=3 }}

loop =: {{
  coinsert'kvm'
  break_kvm_ =: 0
  u with_kbd onkey }}


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

getxy =: {{
  puts csi,'6n'
  wfc"0 csi
  r=.'' while.'R'~:a.{~>c=.rkey''do. r=.r,c end.
  |.0".>';' splitstring a.{~>r }}


NB. ----------------------------------------------------

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
    u_libc =: >0{ (LF cut 2!:0) ::(a:"_) 'locate libc.so'
    raw  =: {{ if. y do. u_raw1'' else. u_raw0'' end. }}
    rkey =: (u_libc,' getchar l') & cd
    gethw =: {{ _".}: 2!:0 'stty size' }}
    putc =: 0 0 $ (u_libc,' putchar  n c') & cd
  else.
    'kvm only supports windows and unix platforms.'
  end. >a: }}

init''
