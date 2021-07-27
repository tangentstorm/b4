NB. token editor
load 'tangentstorm/j-kvm data/sqlite ed.ijs'
load 'b4.ijs' [ cocurrent 'b4'
cocurrent 'base'
coinsert 'vt';'ed'

NB. token and character editors
ted =: a: conew 'ed'
ced =: '' conew 'ed'

cmds =: 'one';'two'

kvm_init =: {{ curs 0 }}
kvm_done =: {{ curs 1 }}

db =: sqlopen_psqlite_'~/b4/sql/syndir.sdb'
run =:  draw loop_kvm_
run>ced
