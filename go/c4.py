#!/bin/env python3.0
"""
c4 : a bootstrap compiler / interpreter for b4
"""
import sys
import shlex
import logging

## type definitions #############################################
# ( these are purely for exposition at the moment )

class PType(object):
    """
    dummy class for defining haskell-style parameterized types
    """
    def __init__(self, *args):
        args = args

Str = str
class IO( PType ): "IO monad from haskell"
class Maybe( PType ): "maybe monad from haskell"
class Def( PType ): "a type marker for definitions"
Int = int
Tok = int # a word id..  token as in "token-threaded code"
Nil = type( None )
Kind= str

## global variables ############################################

g_data = []
g_addr = []
g_dict = {}
g_defs = []
g_keyw = []
g_this = None # the current definition


## the compiler ################################################

def new_word( word:Str, kind:Maybe( Kind ), data:[ Tok ] ) -> IO( Def ):
    """
    returns a new definition object, saving it to the dictionary as a side effect
    """
    assert type( data ) 
    data = { 'id': len( g_defs ), 'word': word, 'kind': kind, 'data': data }
    g_defs.append( data )
    g_dict[ word ] = data
    return data

def set_kind( word:Str, kind:Maybe( Kind )) -> IO( Nil ):
    if not word in g_dict:
        g_dict[ word ] = new_word( word, kind, [] )
    else:
        data = g_dict[ word ]
        if data[ 'kind' ] is not None:
            assert kind == data['kind'], "kind of %s is %s but comment said %s" \
                % ( word, g_dict['kind'], kind )

def execute( word:Str ) -> IO( Nil ):
    logging.error( 'cannot execute unknown word: %s' % word )

def compile( word:Str ) -> IO( Nil ):
    wid = get_id( word )
    g_this[ 'data' ].append( wid )

def get_id( word ) -> IO( Tok ):
    if not word in g_dict:
        g_dict[ word ] = new_word( word, None, [] )
    return g_dict[ word ][ 'id' ]

def run( line_gen ) -> IO( Nil ):
    global g_this
    for line in line_gen:
        words = shlex.split( line, comments=True )
        if not words: continue
        j = 0 ; prev = None
        while j < len( words ):
            word = words[ j ]
            logging.debug( 'found word: %s' % word  );
            if word == '!def':
                j += 1
                g_this = new_word( words[ j ], None, [] )
                continue
            elif word == '::':
                assert prev is not None, ':: must follow a word'
                set_kind( prev, words[ j : ] )
                prev = None
                break
            elif word[ 0 ].isdigit( ):
                literal( word )
            elif word[ 0 ] == '"':
                string( word )
            elif g_this is None or word[0] in [ '!', ':', ';' ]:
                execute( word )
            else:
                compile( word )
            prev = word
            j += 1

def main() -> IO( Nil ):
    if len( sys.argv ) == 1:
        run( sys.stdin )
    else:
       for filename in sys.argv[ 1 : ]:
           run( open( filename ))

if __name__ == '__main__':
    main()
