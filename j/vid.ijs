NB. video buffers
NB.
NB. these are virtual console windows with separate
NB. foreground color, background corol, and character
NB. buffers.
NB.
NB. writing to the buffers is presumably much faster
NB. than actually sending output, and they can be
NB. composed to allow drawing multiple text-mode
NB. windows onto a main buffer, which can then be
NB. rendered to the screen using vt escape codes.
NB.

require 'vt.ijs'

NB. by default, just use vt directly
term =: <'vt'
cscr =: {{ cscr__term y }}
ceol =: {{ ceol__term y }}
putc =: {{ putc__term y }}
goxy =: {{ goxy__term y }}
go00 =: goxy@0 0
puts =: putc"0
fgc  =: {{ fgc__term y }}
bgc  =: {{ bgc__term y }}
reset=: {{ reset__term y }}

prev =. ([ coclass@'vid') coname''
create =: init@|.

fgc   =: {{ FG =: y }}
bgc   =: {{ BG =: y }}
goxy  =: {{ XY =: y }}
go00  =: goxy@0 0
reset =: fgc@7@bgc@0

fill  =: {{ 0 0$ CHB=:HW$y }}
cscr  =: {{ fill ' ' [ FGB=:HW$FG [ BGB=:HW$BG }}
sethw =: {{ cscr go00 reset WH =: |. HW =: y }}
init  =: {{ sethw gethw_vt_^:(-.*#y) y }}

peek =: {{ (<y) { m~ }}
poke =: {{ 0 0 $ (m)=: x (<y) } m~ }}
pepo =: {{ ([: m peek ]) : (m poke) }}

NB. peek/poke various buffers
fgxy =: 'FGB' pepo
bgxy =: 'BGB' pepo
chxy =: 'CHB' pepo

NB. write to ram
NB. putc =: {{ (y chxy])`(FG fgxy ])`(BG bgxy ])`:0 XY }}
putc =: {{
  y chxy XY [ FG fgxy XY [ BG bgxy XY
  if. {. XY =: XY + 1 0 do. XY =: 0,1+{:XY end. }}
puts =: putc"0

rnd =: {{
  CHB =: u:a.{~97+?HW$26
  FGB =: ?HW$256
  BGB =: HW$0 95 0 4 18
  coname'' }}

cocurrent prev

render =: {{ NB. render to vt
  echo@''^:(h =. 0{HW__y) c =. curxy''
  raw@0 goxy c+h [ c render y [ goxy c=.0 10 -~ curxy''
:
  goxy x [ reset''
  f =. FG256_vt_ each FGB__y
  b =. BG256_vt_ each BGB__y
  j =. ,&.>
  s =. f j b j CHB__y
  for_row. s do.
    goxy x + 0, row_index
    reset@'' puts_vt_ 8 u: ;row
  end. }}

rndscr =: {{
  x render rnd__vid [ vid =. 32 10 conew'vid'
  codestroy__vid''
  echo ''[reset''[ raw 0}}


demo =: {{
  curs 0 [ b =. 64 10 conew 'vid'
  for_c. 20$ '/-+\|' do.
    10 5 render b [ fill__b c [ sleep 50
  end. curs@1 codestroy__b''
  10 5 rndscr^:25''}}
