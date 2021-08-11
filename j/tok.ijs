NB. token-centric editor component.
NB. (currently this file is not a standalone application,
NB. but is called by mje.ijs)

load 'tangentstorm/j-kvm/ui tangentstorm/j-lex'
load 'b4.ijs' [ cocurrent 'b4'
cocurrent 'tok'
coinsert 'vt';'ed'

NB. token and character editors
ted =: (0$a:) conew 'UiField'
ced =: '' conew 'UiField'

kvm_init =: {{ curs 0 }}
kvm_done =: {{ curs 1 }}

run =:  draw loop_kvm_
jcut =: jcut_jlex_
save =: curxy@raw@1
rest =: raw@0 @ reset@'' goxy
kpxy =: {{
  NB. execute u and then restore the cursor position.
  res [ rest xy [ res =. u y [ xy =. save''
:
  res [ rest xy [ res =. x u y [ xy =. save'' }}

MODE =: 'n' NB. MODE e. 'niq'  : navigate, insert, quote

color =: {{
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
    res =. res,(FG24B color tt),,>}.tt
  end. res }}

put_tok =: {{
  if. -.*#y do. return. end.
  fgx color 'tag tok' =. y
  puts tok }}

jtype =: jtype_jlex_ &.>

draw_toked =: {{
  goxy XY__ted
  for_tok. C__ted {. B__ted do. put_tok (jtype,]) tok end.
  if. MODE e. 'iq' do. puts B__ced [ fg FG__ced [ bg BG__ced end.
  reset''
  for_tok. C__ted }. B__ted do. put_tok (jtype,]) tok end.
  ceol'' }} kpxy

emit =: {{
   ins__ted jcut B__ced
   C__ced =: 0 [ B__ced =: '' [ XY__ced =: 0,~4++/# S:0 C__ted {. B__ted }}

eval =: {{
  try.   err =. 0 [ res =. ":".;B__ted
  catch. err =. 1 [ res =. 'error'  end. NB. how to get error message?
  reset''
  for_i. 1+i.#res do. ceol goxy 0,i end.
  goxy 0,1
  puts res ,"1 CRLF }}


do =: {{
  NB. this provides a little language for animating the editors.
  NB. execute a series of actions on the token editor
  i=.0 [ q =. '?'  NB. quote char. '?' is probably the least used symbol in j
  draw_toked''
  while. i < #y do. i=.i+1 [ c=.i{y
    select. MODE
    fcase. 'q' do.
      if. c = q do. ins__ced q [ MODE =: 'i' continue.
      else. MODE=:'n' [ emit'' end. NB. and fall through
    case. 'n' do.
      select. c
      case. '?' do. MODE =: 'i'
      case. 'b' do. bak__ted''
      case. '!' do. eval''
      end.
    case. 'i' do.
      if. c = q do. MODE =: 'q'
      else. ins__ced c end.
    end.
    sleep 15+?20 [ echo@'' [  draw_toked''
  end.
  if. MODE = 'q' do. MODE =: 'n' [ emit'' end.
  draw_toked''
  0 0 $ 0}} kpxy

EX__ced =: 0 [ BG__ced =: 234
