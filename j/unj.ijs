NB. un-parser for grammar combinators.

NIL =: >ar'nil'
ANY =: >ar'any'
LIT =: (1;0){::>ar'lit'
ONE =: (1;0){::>ar'one'
SEQ =: >ar'seq'
REP =: >ar'rep'
ALT =: >ar'alt'
IFU =: >ar'ifu'
TOK =: (1;1){::>ar'tok'
ZAP =: (1;1){::>ar'zap'
TRC =: >ar'trace'

unj =: {{'unj' assert. 2 32 e.~ t =. 3!:0 y ['unj expects an atomic representation'
  NB. https://code.jsoftware.com/wiki/Vocabulary/GerundsAndAtomicRepresentation
  NB. AR = name:str | tag:str;<values
  if. t = 2 do. y NB. nothing to do if string
  elseif. y -: ANY do. <s:' any'
  elseif. y -: NIL do. <s:' nil'
  elseif. y -: TOK do. s:' tok'
  elseif. y -: ZAP do. s:' zap'
  elseif. y -: SEQ do. s:' seq'
  elseif. 2~:#y  do.
   smoutput 'unxpected len: ',":#y
   smoutput y
  else.
    smoutput 'unj ' ; 'H:'; H; 'T:';<T ['H T' =. y
    if.     H-: LIT do. ((0;1){::T);s:' lit'
    elseif. H-: ONE do. ((0;1){::T);s:' one'
    elseif. H-: SEQ do. (unj 0{::T);s:' seq'
    elseif. H-: REP do. (unj 0{::T);s:' rep'
    elseif. H-: ALT do. (unj 0{::T);s:' alt'
    elseif. H-: IFU do. (unj each T)
  elseif. H-:TRC do. unj T
    NB. elseif. H-: ANY do. <s:' any'
    else.
      select. H
      case. ,'0' do. unj each T  NB. nouns. (could be names)
      case. ,'3' do. 'verb:';<T
      case. '::' do. >unj each 0{T   NB. :: indicates 'try', so just delete it
      case. ':' do. >unj each T
      case. do.
        smoutput'FAIL'
        y
      end.
    end.
  end.}}

mLit =: 'mLIT' lit
mOne =: 'mONE' one
mSeq =: any`any seq
unj >ar'mSeq'

cheq =: {{ NB. check equality
  if. -. x -: y do.
    smoutput 'expected x -: y, got: '
    smoutput 'x:'
    smoutput x
    smoutput 'y:'
    smoutput y
    throw.
  end. }}

{{ NB. test suite for unj
  assert. (unj >ar'mLit') cheq 'mLIT';s:' lit'
  assert. (unj >ar'mOne') cheq 'mONE';s:' one'
  assert. (unj >ar'mSeq') cheq (;~'any');s:' seq'
  assert. (unj >ar'any') cheq <s:' any'
  'ok!'
}}''

unj >ar'll'
unj >ar'se'
