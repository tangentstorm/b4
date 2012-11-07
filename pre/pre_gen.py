import sys, doctest

# generate boilerplate code for pre.pas

def extract_sig( line ):
    """
    :: pascal function declaration
    -> ( name:str, type:str, args:[( name:str, type:str )])

    Limited to simple types and doesn't support arglists with
    the same type ( ie, this fails on "a, b: integer" )

    >>> extract_sig( "function err( msg : string ) : pattern;" )
    ('err', 'pattern', [('msg','string')])

    """

def gen_code_for( line ):
    name, args = extract_sig( line )
    #  TODO : actually generate the code ;)

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
