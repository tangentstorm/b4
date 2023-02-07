
"""
tangentforth ...?
"""
class VM( object ):
    """
    A small virtual machine inspired by ngaro.

    The instruction set is quite similar. however:

    - conditinals push a boolean to stack rather than jumping
    - added LE = [ 2DUP LT EQ OR ]
    - added GE = [ 2DUP GT EQ OR ]

    - added generalized conditional: ELS ( f - )
      this simply skips one instruction, so can be used to
      make any opcode conditional. note that it performs a
      call rather than a jump.

    - IN/OUT/WAIT replaced with opcode -1: IRQ ( md - ? )
      passes message m to device d. blocks for response.

    """

    language = (
        ' NOP/0  LIT:0  DUP/1  DEL/1  SWP/2  PSH/0  POP/1  INC/1 '
        ' DEC/1  ADD/2  SUB/2  MUL/2  DVM/2  SHL/2  SHR/1  NOT/1 '
        ' AND/2   OR/2  XOR/2   EQ/2   NE/2   LT/2   GT/2   LE/2 '
        '  GE/2  RET/0  JMP:0  ELS:1  NXT:1  ZRT/1  GET/1  PUT/2 '
        ' IRQ/2 ' )

    def __init__( self ):

        MEM = [ 0 for _ in range( 65536 )]
        IP  = 0
        DAT, ADR, BUF = [], [], [] # data, addr, input buffer
        MOD, DEF, DOC = [], [], [] # index to modules and definitions
        DEV = [] # devices

        # NOP/0 -> [ 'NOP', '0' ]
        parsed = (op.split(op[-2]) for op in language.split())

        def makeop( meth, argc ):
            return lambda : meth( self.DAT.pop() for i in range( argc ))

        ops = [ makeop( getattr( self, name ), argc )
                for name, argc in parsed ]

    # helper methods for the opcodes:

    def ext( self, *s )   : self.DAT.extend( s )
    def dat( self, n  )   : self.DAT.append( n )
    def arg( self,   )    : return self.MEM[ self.IP + 1 ]

    # Opcodes:
    # ------------------------------------------
    # args are always in order: top, nos, etc...

    def NOP( self ) : return

    # data stack
    def LIT( self )       : self.dat( self.arg( ))
    def DUP( self, x )    : self.ext( x , x )
    def DEL( self, x )    : return
    def SWP( self, x, y ) : self.ext( y , x )

    # return stack
    def PSH( self, n ) : self.RET.append( n )
    def POP( self,   ) : self.dat( self.RET.pop( ))

    # arithmetic
    def INC( self, x )    : self.dat( x + 1 )
    def DEC( self, x )    : self.dat( x - 1 )
    def ADD( self, x, y ) : self.dat( x + y )
    def SUB( self, x, y ) : self.dat( x - y )
    def MUL( self, x, y ) : self.dat( x * y )
    def DVM( self, x, y ) : self.ext( x / y, x % y )
    def SHL( self, x, n ) : self.dat( x << n ) # * 2
    def SHR( self, x, n ) : self.dat( x >> n ) # / 2
    
    # logic
    def AND( self, x, y ) : self.dat( x & y )
    def  OR( self, x, y ) : self.dat( x | y )
    def XOR( self, x, y ) : self.dat( x ^ y )
    def NOT( self, x )    : self.dat( ~ x )
    
    # conditionals
    def EQ( self, t, n ) : self.dat( -1 if t == n else 0 )
    def NE( self, t, n ) : self.dat( -1 if t != n else 0 )
    def LT( self, t, n ) : self.dat( -1 if t <  n else 0 )
    def GT( self, t, n ) : self.dat( -1 if t >  n else 0 )
    def LE( self, t, n ) : self.dat( -1 if t <= n else 0 )
    def GE( self, t, n ) : self.dat( -1 if t >= n else 0 )
    
    # control flow
    def RET( self ) : self.JMP( self.POP( ))
    def JMP( self ) : self.IP = self.arg( )

    # not exactly an opcode, but instructions > 32 are treated as gosub
    def gosub( self ) :
        self.PSH( self.IP ); self.JMP( self.MEM[ self.IP ])
    def handle( self, callback ) :
        self.PSH( self.IP ); self.JMP( callback )


    # conditional control flow
    def skp( self,   ) : self.JMP( self.IP + 1 ) # jump to ip+1
    def hop( self,   ) : self.JMP( self.arg( ) ) # jump to mem[ip+1]
    def ELS( self, f ) :
        if f : self.skp( )
    def ZRT( self, f ) :
        if f : self.dat( f ); self.skp( )
        else : self.RET( )
    def NXT( self, n ) : 
        if n > 0 : self.skp() 
        else : ( self.dat( n-1 ), self.hop())

    # memory
    def GET( self, a )    : self.dat( self.MEM[ a ])
    def PUT( self, n, a ) : self.MEM[ a ] = n
    
    # device i/o
    def IRQ( self, t , n ): self.interrup( t, n )


    # simulator
    # ------------------------------------------

    def run( self, start = 0 ):
        end = len( self.mem )
        opc = len( self.ops )
        self.IP = start
        while self.IP < end :
            code = self.MEM[ self.IP ]
            if code < opc : self.ops[ code ]( )
            else : self.gosub()
            callback = yield
            if callback = 0 : self.IP += 1
            else : self.handle( callback )
        raise StopIteration
