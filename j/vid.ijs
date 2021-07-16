NB. video buffers
cocurrent'vid'
require'vt.ijs'

GS =: adverb :('m~';':';'(m)=:x')  NB. get/set

fg =: 'FG'GS NB. current foreground color
bg =: 'BG'GS NB. current background color
xy =: 'XY'GS NB. current cursor coordinates

reset =: {{ 0 0$ FG=:7 [ BG =:0 }}
go00  =: {{ 0 0$ XY=:0 0 }}
fill  =: {{ 0 0$ CHB=:HW$y }}
cls   =: {{ fill ' ' [ FGB=:HW$FG [ BGB=:WH$BG }}
sethw =: {{ cls go00 reset WH =: |. HW =: y }}
init  =: {{ sethw gethw_kvm_^:(-.*#y) y }}

peek =: {{ (<y) { m~ }}
poke =: {{ 0 0 $ (m)=: x (<y) } m~ }}
pepo =: {{ ([: m peek ]) : (m poke) }}

NB. peek/poke various buffers
fgxy =: 'FGB' pepo
bgxy =: 'BGB' pepo
chxy =: 'CHB' pepo

NB. write to ram
NB. putc =: {{ (y chxy])`(FG fgxy ])`(BG bgxy ])`:0 XY }}
putc =: {{ y chxy xy [ FG fgxy xy [ BG bgxy XY }}
puts =: putc"0

draw_vid =: {{
 f =. fgn_vt_ each FGB
 b =. bgn_vt_ each BGB
 j =. ,&.>
 s =. f j b j CHB
 (([: echo ''[ [: puts_mje_ reset_vt_ ,~ ])@;)"1 s
 0$0}}

rnd =: {{
  CHB =: a.{~97+?HW$26
  FGB =: ?HW$256
  BGB =: HW$0 95 0 4 18 }}

rndscr =: {{
  puts_mje_ goxy_vt_ 0 0
  echo reset_vt_ [ draw_vid rnd init 10 32 [ puts_mje_ reset_vt_ }}

rndscr^:25''
