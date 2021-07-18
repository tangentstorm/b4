NB.
NB. kvm: keyboard/video/mouse* for terminal apps in j
NB.
NB. * but no mouse stuff yet
NB. ----------------------------------------------------
cocurrent 'kvm'
coinsert 'vt' [ require 'vt.ijs'

ticks=: 0 NB. milliseconds between calling v
break=: 0

NB. with_kbd: run v on keypress, u every 'ticks' milliseconds.
NB. until i get keyp'' test working)
with_kbd =: {{
  u'' [ raw 1 [ y
  while. -. break_kvm_ do.
    if. keyp'' do. v rkey''
    else. sleep ticks end.
    u''
  end. raw 0 [ curs 1 }}


onkey =: {{
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
  elseif. (c=4)*.3=4!:0<'k_asc' do. k_asc k{a.
  elseif. 3=4!:0<'k_any' do. 1[k_any a.{~>y
  elseif. k e. 3 0 do. break_kvm_ =: 1
  end.
  NB.sp =: putc@' '
  NB. reset @ ceol (puts ":>coname'') sp puts ": vnm [ sp puts ":y [ goxy 0 9
  }}

loop =: {{ u with_kbd onkey break_kvm_ =: 0 [ cocurrent y }}
