NB. token editor
load 'tangentstorm/j-kvm tangentstorm/j-lex data/sqlite ed.ijs'
load 'b4.ijs' [ cocurrent 'b4'
cocurrent 'base'
coinsert 'vt';'ed'
load 'app.ijs'

NB. token and character editors
ted =: (0$a:) conew 'ed'
ced =: '' conew 'ed'

cmds =: 'one';'two'

kvm_init =: {{ curs 0 }}
kvm_done =: {{ curs 1 }}

db =: sqlopen_psqlite_'~/b4/sql/syndir.sdb'
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

put_tok =: {{
  select. t=.jtype_jlex_>y
  case. ''  do.        NB. ??
  case. 'S' do.        NB. space
  case. 'A' do. fg'B'  NB. assignment
  case. 'i' do. fg'W'  NB. identifier
  case. 'v' do. fg'r'  NB. verb
  case. 'a' do. fg'y'  NB. adverb
  case. 'D' do. fg'R'  NB. def braces
  case. 'c' do. fg'r'  NB. conjunction
  case. 'P' do. fg'B'  NB. parens
  case. 'p' do. fg'Y'  NB. proname
  case. 'N' do. fg'M'  NB. numeric
  case. 'L' do. fg'c'  NB. literal
  case. 'C' do. fg'K'  NB. comment
  case. 'K' do. fg'B'  NB. control word
  case. do. reset [ puts '[',t,']' [ bg'b'
  end.
  puts >y }}


draw =: {{
  goxy 4 0 [  puts '   ' [ goxy 0 0 [ reset''
  for_tok. C__ted {. B__ted do. put_tok tok end.
  if. MODE e. 'iq' do. puts B__ced [ fg FG__ced [ bg BG__ced end.
  reset''
  for_tok. C__ted }. B__ted do. put_tok tok end.
  ceol'' }} kpxy

emit =: {{
   ins__ted jcut B__ced
   C__ced =: 0 [ B__ced =: '' [ XY__ced =: 1,~4++/# S:0 C__ted {. B__ted }}

eval =: {{
  try.   err =. 0 [ res =. ":".;B__ted
  catch. err =. 1 [ res =. 'error'  end. NB. how to get error message?
  reset''
  for_i. 2+i.#res do. ceol goxy 0,i end.
  goxy 0,2
  puts res ,"1 CR, LF
}}


do =: {{
  NB. this provides a little language for animating the editors.
  NB. execute a series of actions on the token editor
  i=.0 [ q =. '?'  NB. quote char. '?' is probably the least used symbol in j
  draw''
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
    sleep 15+?20 [ echo@'' [  draw''
  end.
  if. MODE = 'q' do. MODE =: 'n' [ emit'' end.
  draw''
  0 0 $ 0}} kpxy

EX__ced =: 0 [ BG__ced =: 234
