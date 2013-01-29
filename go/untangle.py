"""
analyze and highlight the code for retro's kernel.rx
( yes, the code is a complete mess. it's just something
  i'm throwing together for experimentation / exploration )
"""
import os, sys, functools
emit = sys.stdout.write

last = None
docs = {}
deps = {}
defs = {}
vars = []
dups = []
kind = {}
data = []
seen = {}

ng_ops =\
    """
    nop lit dup drop swap push pop
    loop jump ; >jump <jump !jump =jump 
    @ ! + - * /mod and or xor << >> 0;
    1+ 1- in out wait
    """.split()

prims = {
   'nop' :'nop',    'lit' :'lit',    'dup' :'dup',    'drop':'del',
   'swap':'swp',    'push':'psh',    'pop' :'pop',    'loop':'nxt',
   'jump':'jmp',    ';'   :'ret',    'jump':'jmp',    '<'   :'lt ',
   '<>'  :'ne ',    '='   :'eq ',    '@'   :'get',    '!'   :'put',
   '+'   :'add',    '-'   :'sub',    '*'   :'mul',    '/mod':'dvm',
   'and' :'and',    'or'  :'or ',    'xor' :'xor',    '<<'  :'shl',
   '>>'  :'shr',    '0;'  :'zrt',    '1+'  :'inc',    '1-'  :'dec',
   '>'   :'gt' ,    '<='  :'ge',     '=='  :'eq' ,    '>='  :'ge'
}

STR, NUM, WORD, VAR, DEF, MACRO, PRIM, NEW, REM, SPACE  = range( 10 )

theme = 'gYwCWMyrBw'
ansi  = 'krgybmcwKRGYBMCW'

for item in [ 
    ":", ";", "t:", "w:", "m:", "p:", "i:", ":doc", "data:",
    "[", "]", "label:", "variable", "variable:",
    "constant", "#", ",", "'", "`", "elements", "$,"
    "if", "!if", "then", ".word", "$,", "setClass"
              
    ]: kind[ item ] = MACRO
for item in [ 
    "last", "fb", "fw", "fh", "cw", "ch", "memory",
    "heap", "which", "remapping", "eatLeading?", "base",
    ]: kind[ item ] = VAR


class var:
    @classmethod
    def set( cls, slot, value ):
        setattr( cls, slot, value )

var.fg = 7
var.bg = 0

def color( c ):
    if type( c ) is str:
        return 234 if c == 'K' else ansi.index( c )
    else: return c

def fg( c ):
    var.fg = color( c )
    return "[38;5;%im" % var.fg

def bg( c ):
    var.bg = color( c )
    return "[48;5;%im" % var.bg


def isint( tok ):
    try: int( tok )
    except: return False
    return True

var.thisWord = None
def compile( tok ):
    defs.setdefault( var.thisWord, [ ]).append( tok )

class MakeWord( Exception ): pass

def compileOn( tok ): emit( bg( 'K' ))
def compileOff( tok ): emit( bg( 'k' ))

var.compWord = None
def defineWord( tok ):
    kind[ tok ] = {
        'variable'  : VAR,
        'variable:' : VAR,
        'label:' : VAR,
        'p:' : PRIM,
        'w:' : WORD,
        'm:' : MACRO,
        ':'  : WORD,
        # not sure about these yet:
        't:' : NEW,
        'i:' : NEW,
    }[ var.compWord ]
    compileOn( tok )
    var.thisWord = tok
    raise MakeWord

callback = {}
for item in [ "[" , "]]" ]:
    callback[ item ] = compileOn


immediate = {}
donothing = lambda : ()
for item in [ ":", "t:", "w:", "m:", "i:", "p:",
              "label:", "variable", "variable:" ]:
    callback[ item ] = defineWord
    immediate[ item ] = functools.partial(
        lambda item : var.set( 'compWord', item ),
        item )
for item in [ ";", "]", "[[" ]:
    callback[ item ] = compileOff

compiler = False
def tokens():
    withnext = None
    for tok in os.popen('./tok').read().split(chr(31)):

        if all( ord( ch ) <= 32 for ch in tok ):
            yield tok, SPACE
            continue
        
        try:
            if withnext: withnext( tok )
            elif compiler: compile( tok )
            lastword = tok
            withnext = callback.get( tok )
        except MakeWord:
            yield tok, DEF
            withnext = None
            continue

        if tok.startswith( '(' ): yield tok, REM
        elif tok.startswith( '"' ): yield tok, STR
        elif isint( tok ): yield tok, NUM
        elif tok in prims: yield tok, PRIM
        elif tok.endswith(',') and tok[:-1] in prims:
            yield tok[:-1], PRIM
        else : yield tok, kind.get( tok, NEW )

def main():
    for tok, typ in tokens() :
        immediate.get( tok, donothing )()
        if DEF == typ: 
            if tok in defs: dups.append(( tok, defs[ tok ] ))
            defs[ tok ] = []
        elif REM == typ: pass
        map( emit, ( fg( theme[ typ ]), tok ))
        pass
    print
    print fg("B") + "{0} words defined:".format( len( defs ))
    print fg("w")
    for item in defs.keys():
        emit( fg( theme[ kind.get( item, NEW )]))
        emit( item )
        emit( ' ' )
        pass
    print fg("w")
    print

if __name__=="__main__":
    print
    main()
