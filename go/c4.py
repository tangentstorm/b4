#!/bin/env python3.0
"""
c4 : a bootstrap compiler / interpreter for b4
"""
import sys
import shlex
import logging
from collections import namedtuple as ntup

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
class Entry( PType ): "a type marker for dictionary entry"
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
g_jump = [] # compile-time jump stack ( for control structures )

## the compiler ################################################

def new_entry( word:Str, sig:Maybe( Sig ), data:[ Tok ] ) -> IO( Entry ):
    """
    returns a new entry, saving it to the dictionary as a side effect
    """
    assert type( data ) 
    data = { 'id': len( g_defs ), 'word': word, 'sig': sig, 'data': data }
    g_defs.append( data )
    g_dict[ word ] = data
    return data

def set_sig( word:Str, sig:Maybe( Sig )) -> IO( Entry ):
    if not word in g_dict:
        g_dict[ word ] = new_entry( word, sig, [] )
    else:
        data = g_dict[ word ]
        if data[ 'sig' ] is not None:
            assert sig == data['sig'], "sig of %s is %s but comment said %s" \
                % ( word, g_dict[ 'sig' ], sig )
    return g_dict[ word ]

def execute( word:Str, lexer ) -> IO( Nil ):
    logging.error( 'cannot execute unknown word: %s' % word )

# compiler macros
# ----------------------------------------------------
# ':' isn't actually a special prefix, except that by
# convention, we're using it to mark compiler words.
# This just makes it easy to bootstrap.

def todo( entry, lexer ): 
    logging.error( 'todo: %s' % entry.word )


class OP( object ):
    def __init__( self, arg ):
        self.arg = arg
    def __call__( self, entry, lexer ):
        raise NotImplementedError

class LIT( OP ):
    def __call__( self, entry, lexer ):
        entry['data'].append( eval( next( lexer )))
        

class Jump( object ):
    def __init__( self ):
        self.to = None
        self.froms = []

class Box( object ):
    """
    Sort of an anonymous variable.
    """
    def __init__(self):
        self.value = None
    
class Stacks( object ):
    def __init__(self): 
        self.main = []
        self.side = []

    # normal forth stack operations:
    def swap( self ): self.main[ -2 : ] = self.main[ -1 ], self.main[ -2 ]
    def push( self ): self.side.push( self.main.pop( ))
    def pop( self ): self.main.push( self.side.pop( ))
    def val( self, x ): self.main.push( x )
    def rot( self ): self.main[ -3 : ] = self.main[ -2 ], self.main[ -1 ], self.main[ -3 ]
    def dup( self ): self.main.push( self.main[ -1 ])
    def over( self ): self.main.push( self.main[ -2 ])
    def drop( self ): self.main.pop()



g_jump = Stacks()
def AHEAD( word, lexer ): g_jump.val( WRITE_PTR  )
def JUMP_WHEN_FALSE():
    if not data.tos(): self.jump()
def JUMP(): pass
def SWAP_AHEADS(): pass
def LAND_HERE(): pass
def PUSH_LOOP(): pass
def POP_LOOP(): pass

macros = {
    
    # :def is what initiates the compiler, so it's not a macro
    ';def'  : [ todo ],

    ':if'   : [ ],
    ':then' : [ AHEAD, JUMP_WHEN_FALSE ],
    ':else' : [ AHEAD, JUMP, SWAP_AHEADS, LAND_HERE ], 
    ';if'   : [ LAND_HERE ],

    ':loop' : [ PUSH_LOOP ],
    ':done' : [ AHEAD, JUMP ],
    ';loop' : [ POP_LOOP, JUMP, LAND_HERE ],
}

def immediate( word:Str, lexer ) -> IO( Nil ):
    if word in macros:
        for func in macros[ word ]: func( word, lexer )
    else: logging.error( 'unknown immediate word: %s' % word )
    

def get_id( word ) -> IO( Tok ):
    if not word in g_dict: g_dict[ word ] = new_entry( word, None, [] )
    return g_dict[ word ][ 'id' ]

def tokenize( line_gen ) -> IO( Nil ):
    for line in line_gen:
        for word in shlex.split( line, comments=True ):
            yield word
        
def read_sig( prev, lexer ) -> IO( ):
    assert prev is not None, ':: must follow a word'
    sig = []
    for word in lexer:
        if word == ';:' : break
        else: sig.append( word )
    set_sig( prev, sig )
    
def read_def( lexer ) -> IO( ):
    iden = next( lexer )
    this = new_entry( iden, None, [] )
    prev = iden
    for word in lexer:
        # compile
        if word == '::': read_sig( prev, lexer )
        elif word[ 0 ] in [ ':', ';' ]: immediate( word, lexer )
        else: this[ 'data' ].append( get_id( word ))
        prev = word # so we can create signatures inline for unknown words

def eval_loop( lexer ) -> IO( ):
    for word in lexer:
        logging.debug( 'found word: %s' % word  );
        if   word == ':def' : read_def( lexer )
        elif word[ 0 ].isdigit( ) : literal( word )
        elif word[ 0 ] == '"' : string( word )
        else : execute( word )

def main( ) -> IO( Nil ):
    if len( sys.argv ) == 1: source = sys.stdin
    else: source = ( open( sys.argv[ 1 ]))
    eval_loop( tokenize( source ))
    
if __name__ == '__main__':
    main( )
