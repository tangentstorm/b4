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

class IO( PType ): "IO monad from haskell"
class Maybe( PType ): "maybe monad from haskell"
class Def( PType ): "a type marker for definitions"
class Seq( PType ): "a type marker for sequences (list / array)"
Str = str
Int = int
Tok = int # a word id..  token as in "token-threaded code"
Nil = type( None )
Sig = Seq( str )

## global variables ############################################

g_defs = [] # the list of definitions
g_dict = {} # index of g_dict for speedy lookups
g_data = [] # runtime data stack
g_addr = [] # runtime address / return stack

## the compiler ################################################

def new_word( word:Str, sig:Maybe( Sig ), data:[ Tok ] ) -> IO( Def ):
    """
    returns a new definition object, saving it to the dictionary as a side effect
    """
    assert type( data ) 
    data = { 'id': len( g_defs ), 'word': word, 'sig': sig, 'data': data }
    g_defs.append( data )
    g_dict[ word ] = data
    return data

def set_sig( word:Str, sig:Maybe( Sig )) -> IO( Def ):
    if not word in g_dict:
        g_dict[ word ] = new_word( word, sig, [] )
    else:
        data = g_dict[ word ]
        if data[ 'sig' ] is not None:
            assert sig == data['sig'], "sig of %s is %s but comment said %s" \
                % ( word, g_dict[ 'sig' ], sig )
    return g_dict[ word ]

def execute( word:Str, word_gen ) -> IO( Nil ):
    logging.error( 'cannot execute unknown word: %s' % word )

def get_id( word ) -> IO( Tok ):
    if not word in g_dict:
        g_dict[ word ] = new_word( word, None, [] )
    return g_dict[ word ][ 'id' ]

def tokenize( line_gen ) -> IO( Nil ):
    for line in line_gen:
        for word in shlex.split( line, comments=True ):
            yield word

def read_sig( prev, word_gen ) -> IO( ):
    assert prev is not None, ':: must follow a word'
    sig = []
    for word in word_gen:
        if word == ';:' :
            set_sig( prev, sig )
            break
        else: sig.append( word )
        
def read_def( word_gen ) -> IO( ):
    iden = next( word_gen )
    this = new_word( iden, None, [] )
    prev = iden
    for word in word_gen:
        # compile
        if word == '::': read_sig( prev, word_gen )
        elif word[ 0 ] in [ ':', ';' ]: execute( word, word_gen )
        else: this[ 'data' ].append( get_id( word ))
        prev = word # so we can create signatures inline for unknown words

def eval_loop( word_gen ) -> IO( ):
    for word in word_gen:
        logging.debug( 'found word: %s' % word  );
        if   word == ':def' : read_def( word_gen )
        elif word[ 0 ].isdigit( ) : literal( word )
        elif word[ 0 ] == '"' : string( word )
        else : execute( word )

def main( ) -> IO( Nil ):
    if len( sys.argv ) == 1: source = sys.stdin
    else: source = ( open( sys.argv[ 1 ]))
    eval_loop( tokenize( source ))
    
if __name__ == '__main__':
    main( )
