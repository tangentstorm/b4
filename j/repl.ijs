load 'tangentstorm/j-kvm tangentstorm/j-kvm/ui tangentstorm/j-lex'
load 'tok.ijs'
coinsert 'kvm'

ed =: conew 'UiEditWidget'
ted_z_ =: '' conew 'TokEd'  NB. for syntax highlighting
app_z_ =: (,ed) conew 'UiApp'

XY__ed =: 3 0
XY__ted =: 3 0
B__ed =: '{{ i. y }}"0 ] 5'


render__ed =: {{
  cscr'' [ bgx BG [ fgx FG
  B__ted =: jcut_jlex_ B
  render__ted''
  render_cursor^:y'' }}


render__app loop_kvm_ >ed
