NB. token-centric editor component.
NB. (currently this file is not a standalone application,
NB. but is called by mje.ijs)
load 'tangentstorm/j-kvm/ui tangentstorm/j-lex'

cocurrent 'tok' extends 'vt'

jcolor =: {{
  select. tag [ 'tag tok'=. y
  case. ''  do. 16b999999  NB. ??
  case. 'S' do. 16b999999  NB. space
  case. 'A' do. 16b008b8b  NB. assignment
  case. 'i' do. 16bffffff  NB. identifier
  case. 'v' do. 16bffd700  NB. verb
  case. 'a' do. 16bc78243  NB. adverb
  case. 'D' do. 16bf95c54  NB. def braces
  case. 'c' do. 16bff4500  NB. conjunction
  case. 'P' do. 16b555555  NB. parens
  case. 'p' do. 16b32cd32  NB. proname
  case. 'N' do. 16bf984e5  NB. numeric
  case. 'L' do. 16b2dabfc  NB. literal
  case. 'C' do. 16b666666  NB. comment
  case. 'K' do. 16bf95c54  NB. control word
  case. do. 16b0000cd  end. }}

vtcolor =: {{  NB. colorize a line of j using vt escape codes
  res =. ''
  for_tt. {.>{.> jlex y do.
    if. -.*#tt do. continue. end.
    res =. res,(FG24B jcolor tt),,>}.tt
  end. res }}

coclass 'TokEd' extends 'UiEditWidget';'tok'

create =: {{
  create_UiEditWidget_ f. 0$a:
  BG =: _1
  NB. ced = character editor for new tokens
  ced =: '' conew 'UiEditWidget'
  EX__ced =: 0 [ FG__ced =: _7 [ BG__ced =: _234 }}


kvm_init =: {{ curs 0 }}
kvm_done =: {{ curs 1 }}

run =:  draw loop_kvm_
jcut =: jcut_jlex_
save =: curxy@raw@1
rest =: raw@0 @ reset@'' @ goxy
kpxy =: {{
  NB. execute u and then restore the cursor position.
  res [ rest xy [ res =. u y [ xy =. save''
:
  res [ rest xy [ res =. x u y [ xy =. save'' }}


put_tok =: {{
  if. -.*#y do. return. end.
  fg jcolor 'tag tok' =. y
  puts tok }}

jtype =: jtype_jlex_ &.>

render =: {{
  cscr'' [ bgc 0
  for_tok. C {. B do. put_tok (jtype,]) tok end.
  if. MODE e. 'iq' do. puts B__ced [ fg FG__ced [ bg BG__ced end.
  reset''
  for_tok. C }. B do. put_tok (jtype,]) tok end. }}

emit =: {{
   ins jcut B__ced
   C__ced =: 0 [ B__ced =: '' [ XY__ced =: 0,~4++/# S:0 C {. B }}

eval =: {{
  try.   err =. 0 [ res =. ":".;B
  catch. err =. 1 [ res =. 'error'  end. NB. how to get error message?
  reset''
  for_i. 1+i.#res do. ceol goxy 0,i end.
  goxy 0,1
  puts res ,"1 CRLF }}


vid =: conew'vid'

