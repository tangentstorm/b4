[mn,mx]=[Math.min,Math.max];win=window;con=console;doc=document
next=requestAnimationFrame; defer=(f)->setTimeout(f,50)
[js,sj,id]=[JSON.stringify,JSON.parse,(s)->doc.getElementById(s)]

# cell/grid width/height/spacing
ch=cw=25; cs=1; gh=16; gw=24; [n,s,e,w]=[-gw,gw,1,-1] 
[tox,toy]=[((i)->i%gw),((i)->Math.floor(i/gw))]
names = 'empty wall door hero baddie gold'.split ' '
[empty,wall,door,hero,baddie,gold]=[0..5]
room = ()->[0 for [0..gw*gh-1]][0]
win.cur={t:1, go:0, tool:(()->), at:0, lv:-1, rm:room()}
move = (i)-> cur.rm[cur.at]=0; cur.at = i; cur.rm[cur.at]=hero
into = (i)-> if cur.rm[i]==1 then 0 else 1
nudge= (d)-> j = cur.at+d; if into(j) then move(j) 
step = ( )-> if toy(cur.at)+1<gh then nudge(s)

pal='#666 #999 #6c9 #9cf #e24 #fc4'.split ' '
brush=(c)->(i)->cur.rm[i]=c
win.tools=(brush(c) for c in [0..pal.length-1])
usetool=(i)->cur.t=i;cur.tool=tools[i];tb.attr(hl)
tools[hero]=(i)->move(i)
layout=
  width : cw, height: ch,
  x : (i)-> (cw + cs) * tox(i)
  y : (i)-> (ch + cs) * toy(i)
hl=({'stroke-width':(i)->if cur.t==i then 5 else 1})
tb=d3.select("#tbar").selectAll("rect")
  .data(d3.range(pal.length))
  .enter().append('rect').attr(layout)
  .attr('fill', (i)->pal[i]).attr(hl)
  .on('click', usetool)
gm = d3.select("#game").selectAll("rect").data(d3.range(gh*gw))
gm.enter().append('rect').attr(layout).attr
  'stroke-width': 0
gm.on
   mousedown: (i)-> cur.tool(i); cur.go=1
   mouseover: (i)-> if cur.go then cur.tool(i)
   mouseup:   (i)-> cur.go = 0
draw = ()-> gm.attr {fill: (i)-> pal[cur.rm[i]]}
frame = ()-> step(); draw(); defer(()->next frame)
usetool(1); frame()

lbar = ->
  d3.select('nav').selectAll('a').data(sj ls['levs'])
    .enter().append('a').text((d)->d).on(click:(d)->ld(d))
ls = localStorage; ls['levs']||='[]'; b='button'; lbar()
ld = (i)-> cur.rm=(sj ls['lv:'+i])||room(); lvtxt(i)
sv = (i)-> ls['lv:'+lvtxt()]=js cur.rm
nu = -> cur.rm = room(); lvnew(); lbar()
lvtxt = (_)-> el=id('lev'); if _? then el.value=_ else el.value 
lvnew = ()->
  levs = sj ls['levs']; i=0; i++ while i in levs; levs.push(i) 
  ls['levs'] = js levs.sort(); ls['lv:'+i]=js room(); lvtxt(i)    
lbl='load save new'.split ' '; acts=[ld,sv,nu]
d3.select('form').selectAll(b).data(acts)
  .enter().append(b).attr('type',b).text((d,i)->lbl[i]).on
    click : (d)-> d lvtxt()
