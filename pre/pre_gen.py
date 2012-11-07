import sys, doctest

# generate boilerplate code for pre.pas

def extract_sig( line ):
    """
    :: pascal function declaration
    -> ( name:str, type:str, args:[( kwd:str, [ name:str ], type:str )])

    Doesn't support default values yet.

    >>> extract_sig("function abc( a, b : int; var c : array of str ) : obj;")
    ('abc', 'obj', [('', ['a', 'b'], 'int'), ('var', ['c'], 'array of str')])
    """
    res = [ ]
    tokens = [ tok.strip( ) for tok in line.lower( )
              .replace( ';', ' ' )
              .replace( '(', ' ' )
              .replace( ',', ' ' )
              .replace( ')', ' ' )
              .strip( )
              .split( )]

    # little stack machine to help with parsing:
    class the : list = res # just an arbitrary namespace
    def keep( v ): the.list.append( v )
    def next( index = 0 ):
        the.token = tokens.pop( index )
        return the.token
    def last( ): return next( -1 )
    def drop( v ): pass

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
    args = [ ]
    keep( args )
    while tokens:

        the.list = arg_group = [ ] # start a new list for keep( )
        if next( ) in ( 'var', 'const' ):
            keep( the.token )
            next( )
        else: keep( '' )  # no var/const keyword

        the.list = names = [ ]
        keep( the.token ) # just keep the name

        while next( ) != ':' :
            keep( the.token ) # names in the list

        # now the.token == : , next up is the type for the group
        drop( the.token ) # drop the ":"
        the.list = arg_group
        keep( names ) # leave it as a list just to alternate the brackets

        keep( next( )) # type name
        if ( the.token ) in ( 'array', 'set' ):
            container = the.token
            drop( next( ) ) # "of"
            the.list[ -1 ] = "%s of %s" % ( container, next( ))

        the.list = args
        keep( tuple( arg_group ))

    return tuple( res )

def gen_code_for( line ):
    print line
    print extract_sig( line )
    #  TODO : actually generate the code ;)
    name, type, args = extract_sig( line )

if __name__=="__main__":
    if "--test" in sys.argv: doctest.testmod()
    else:
        lines = open( "pre.pas" )
        line  = ""
        while not line.startswith( "implementation" ):
            line = lines.next( ).strip( )
            if ( line.startswith( "function" )
                 and line.endswith( "pattern;" )):
                gen_code_for( line )
