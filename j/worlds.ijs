NB. This implements something like the 'world' concept by Allesandro Warth
NB. (used in his OMeta system).
NB.
NB. A world is just a locale (chained namespace), but when a line of code is executed
NB. through the 'exec' function, /and/ that line contains an assignment, then a new
NB. world is created on the fly to hold the results of the assignment.
NB.
NB. The result is that you can 'time travel' back and forth between states.
NB.
NB. Limitations:
NB.  - does not track global assignments that happen inside verbs
NB.  - assigmnents using locatives operate independently of the 'timeline'
NB.
NB. Basically, this is just enough magic to power my presentation tool and probably
NB. should not be used for anything else without careful consideration.

cocurrent 'world'


ii =: 0       NB. world counter
ihist =: ''   NB. input history
ehist =: ''   NB. echo history

NB. m in_world_ : name builder: append locative for current world to m
in =: {{ m,'_',(this_world_''),'_' }}

lines =: {{
 if. 1 = # $ y do. y =. ,:y end.
 ,(LF cut CR -.~ ])"1 y }}

echo =: {{ for_line. lines y do. ehist =: ehist,line end.
  ('HISTL1'in_world_) =: #ehist_world_  }}

this =: {{ 'WORLD',":ii_world_ }}
next =: {{
  ni =. ii_world_ + 1
  cocurrent nw =. 'WORLD',":ni
  coinsert this_world_''
  if. HISTL1 ~: HISTL0 do. HISTL0 =: HISTL1 end.
  ii_world_ =: ni
  this_world_'' }}

init =: {{
  coerase (#~ #@('WORLD\d+'&rxmatches)&>) conl''
  HISTL0_WORLD0_=: HISTL1_WORLD0_=: ii_world_=:0 }}

exec =: {{ NB. run code in the current 'world'
  try.
    mut =. ('=:';'=.')&(+./@e.) tok0 =. ;: y
    new =. mut +. 1  NB. TODO: allow disabling new worlds
    if. new do.
      tok1 =.  ]`('=:'"_)@.('=.'&-:)L:0 tok0
      cocurrent next_world_''
      NB. echo_world_'running: (',(;tok1),') in: ',this_world_''
      echo_world_ '   ',y
      res =. ": ('do'in_world_)~ ' ' joinstring tok1
    else.
      cocurrent this_world_''
      NB. echo_world_'running: (',(y),') in: ',this_world_''
      echo_world_ '  ',y
      res =. ": ". y
    end.
    if. # res do. echo_world_ res end.
  catch.
    echo_world_ dberm''
  end. }}
