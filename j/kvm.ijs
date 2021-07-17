NB. ----------------------------------------------------

NB. wfk y: wait for key code y
wfk =: {{ while.y~:>r=.rkey'' do. end. }}



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

