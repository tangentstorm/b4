"""
generate boilerplate code for pre.pas, to make using the
haskell-style algebraic data types more convenient in pascal.

Also contains a cute little parser for pascal function definitions.
"""
import sys, doctest, string, itertools

# a little stack / dequeue machine to help with parsing:
class the :      # just an arbitrary namespace
    result = [ ] # top level result. set once per call
    focus = [ ]  # set this to the list to build ( can change for nesting )
    input = None # set this to the input list ( tokens, below )
def keep( v ): the.focus.append( v )
def next( index = 0 ):
    the.token = the.input.pop( index )
    return the.token
def last( ): return next( -1 ) # <- dequeue :)
def drop( v ): pass


def extract_sig( line ):
    """
    :: pascal function declaration
    -> ( name:str, type:str, args:[( kwd:str, [ name:str ], type:str )])

    Doesn't support default values yet.

    >>> extract_sig("function abc( a, b : int; var c : array of str ) : obj;")
    ('abc', 'obj', [('', ['a', 'b'], 'int'), ('var', ['c'], 'array of str')])
    """
    the.focus = the.result = [ ]
    the.input = [ tok.strip( ) for tok in
                  line.lower( )
                  .replace( ';', ' ' )
                  .replace( '(', ' ' )
                  .replace( ',', ' ' )
                  .replace( ')', ' ' )
                  .strip( ).split( )]

    # extract the name and return type, leaving just the args:
    if next( ) == 'function':
        keep( next( ))  # function name
        keep( last( ))  # return type
        drop( last( ))  # the ":" before the return type
    else:
        keep( next( ))  # procedure name
        keep( None )    # no return type for procedures

    # at this point, tokens is either empty,
    # or it contains the argument list
    _extract_arglist( )
    return tuple( the.result )


def _extract_arglist( ):
    """
    This is just the second part of extract_sig, that parses
    the argument list inside the parentheses.
    """
    args = [ ]
    keep( args )
    while the.input : # i.e., while not empty

        the.focus = arg_group = [ ] # start a new list for keep( )
        if next( ) in ( 'var', 'const' ):
            keep( the.token )
            next( )
        else: keep( '' )  # no var/const keyword

        the.focus = names = [ ]
        keep( the.token ) # just keep the name

        while next( ) != ':' :
            keep( the.token ) # names in the list

        # now the.token == : , next up is the type for the group
        drop( the.token ) # drop the ":"
        the.focus = arg_group
        keep( names ) # leave it as a list just to alternate the brackets

        keep( next( )) # type name
        if ( the.token ) in ( 'array', 'set' ):
            container = the.token
            drop( next( ) ) # "of"
            the.focus[ -1 ] = "%s of %s" % ( container, next( ))

        the.focus = args
        keep( tuple( arg_group ))


def trim(s): # from my handy.py library
    """
    strips leading indentation from a multi-line string.
    for saving bandwith while making code look nice
    """
    lines = string.split(s, "\n")

    # strip leading blank line
    if lines[0] == "":
        lines = lines[1:]

    # strip indentation
    indent = len(lines[0]) - len(string.lstrip(lines[0]))
    for i in range(len(lines)):
        lines[i] = lines[i][indent:]

    return string.join(lines, "\n")


def gen_code_for( line ):
    """
    This generates the actual code.
    """
    # print extract_sig( line )
    name, type, args = extract_sig( line )

    def show_group( group, suffix ):
        """generate code for one group of arg defs"""
        kw, name_list, type = group
        names = ", ".join( n+suffix for n in name_list )
        return '{kw} {names} : {type}'.format( **locals( ))

    # now for the list of arg groups. since pascal doesn't allow duplicate
    # identifiers, we'll add an "_in" suffix in the constructor
    ctr_args = ";".join( show_group( group, '_in' ) for group in args )
    fun_args = ";".join( show_group( group, '') for group in args )

    arg_names = [] # flat list of all names
    arg_types = {} # name -> type mapping
    for ( kw, names, gtype ) in args:
        for arg in names:
            arg_types[ arg ] = gtype
            arg_names.append( arg )

    call = ", ".join( arg_names )
    if arg_names:
        assigns = '\n  ' + ';\n  '.join(
            "self.{0} := {0}_in".format( a ) for a in arg_names )
        decls = '\n  ' + ';\n  '.join(
            "{0} : {1}".format( a, t ) for a, t in arg_types.items( ))
    else: assigns = decls = ''

    klass = "%sPattern" % name.title( )
    print( trim(
        """
        type {klass} = class ( pattern )
          constructor create( {ctr_args} );
        private{decls}
        end;
        constructor {klass}.create( {ctr_args} );
        begin{assigns}
        end;
        {line}
        begin
          result := {klass}.create( {call} )
        end;
        """).format( **locals( )))

if __name__=="__main__":
    if "--test" in sys.argv: doctest.testmod()
    else:
        print "// ---- generated file! do not edit! ----"
        print "// this file was generated by {0}".format( __file__ )
        print "// --------------------------------------"
        lines = open( "pre.pas" )
        line  = ""
        while not line.startswith( "implementation" ):
            line = lines.next( ).strip( )
            if line.count( '//' ):
                line, _ = line.split( '//', 1 )
                line = line.strip( )
            if ( line.startswith( "function" )
                 and line.endswith( "pattern;" )):
                gen_code_for( line )
