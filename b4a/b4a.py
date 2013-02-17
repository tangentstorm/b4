import unittest, os, array, sys, subprocess

# opcode list and helper functions

ops = {
  "nop"   : 0,   "lit"   : 1,   "dup"   : 2,    "drop"  : 3,
  "swap"  : 4,   "push"  : 5,   "pop"   : 6,    "loop"  : 7,
  "jump"  : 8,   ";"     : 9,   "<jump" : 10,   ">jump" : 11,
  "!jump" : 12,  "=jump" : 13,  "@"     : 14,   "!"     : 15 ,
  "+"     : 16,  "-"     : 17,  "*"     : 18,   "/mod"  : 19,
  "and"   : 20,  "or"    : 21,  "xor"   : 22,   "<<"    : 23,
  ">>"    : 24,  "0;"    : 25,  "1+"    : 26,   "1-"    : 27,
  "in"    : 28,  "out"   : 29,  "wait"  : 30,
}

def is_int( x ):
  res = True
  try: int( x )
  except ValueError: res = False
  return res

def trim( s ):
  """
  strips leading indentation from a multi-line string.
  for saving bandwidth while making code look nice
  """
  lines = string.split( s, "\n" )
  # strip leading blank line
  if lines[0] == "": lines = lines[ 1: ]
  # strip indentation
  indent = len( lines[ 0 ]) - len( lines[ 0 ].lstrip( ))
  for i in range( len( lines )): lines[ i ] = lines[ i ][ indent: ]
  return '\n'.join( lines )


# assembler

def assemble( src ):
  """
  :: str -> [ int ] or raise ValueError
  --- example program ------------------

    # count until stack overflows
      0           # emits 1 0   ( 1 is an implicit 'lit' opcode )
    :COUNTER
      dup
      inc
      jump COUNTER

  --- end example ----------------------
  """
  labels = {}
  res  = []
  # we do two passes so that we can use labels before they're defined
  for phase in [ 1 , 2 ]:
    here = 0
    for ln, line in enumerate( src.split( "\n" )):
      for word in line.split( ):
        if word.startswith( "#" ): break
        if word.startswith( ":" ): labels[ word[ 1: ]] = here
        else:
          here += 1
          if is_int( word ) :
            here += 1 # add another because of the implicit 'lit' op
            if phase == 2 : res.extend([ ops[ 'lit' ], int( word ) ])
          # the rest are all emitters and we only do that in phase 2:
          elif phase == 1 : pass
          elif word in labels : res.append( labels[ word ])
          elif word in ops : res.append( ops[ word ])
          else : raise ValueError( "unresolved symbol '%s' on line %i"
                                   % ( word, ln + 1 ))
  return res


# save_image ( and 32-bit int encoding )

INT32_FMT = ''
for ch in 'hil':
  if array.array( ch ).itemsize == 4: INT32_FMT = ch
if not INT32_FMT:
  print "couldn't find a 32-bit int in your python's array module."
  print "sorry -- you'll need to update the save_image function or"
  print "use a differnt python version."
  print
  print "see http://docs.python.org/library/array.html#module-array"
  sys.exit()


def save_image( ints, path ):
  """
  write a list of ints as 32-bit binary values
  """
  assert len( ints ), "no instructions given"
  arr = array.array( INT32_FMT, ints )
  img = open( path, 'wb' )
  arr.tofile( img )
  img.close( )



# show control codes

ESC = chr( 27 )
USE_COLOR = True

def maybecolor( string, fg=5, bg=0 ):
  # http://en.wikipedia.org/wiki/ANSI_escape_code#Colors
  if USE_COLOR: return "\x1B[0;3%i;4%im%s\x1B[0m" % ( fg, bg, string )
  else: return string


def catv( string, prefix='^' ):
  """show control characters, like unix cat -v"""
  result = []
  for ch in string:
    if ord( ch ) < 32 and ch not in "\t\n":
      result.append( maybecolor( prefix + '@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_'[ ord( ch )]))
    else: result.append( ch )
  return ''.join( result )

# run

FSEP, GSEP = chr( 28 ), chr( 29 ) # ascii file and group separators

def run( src, imgpath = NGARO_IMG ):
  """
  run the ngaro interpreter and investigate its state.
  see module docstring at top of file for dump format
  """

  save_image( assemble( src ), imgpath )

# test suite

class AssemblerTest( unittest.TestCase ):
  """self-tests for the assembler"""
  def test_labels( self ):
    self.assertEquals( [ 0 ], assemble( ':zero zero' ))
    self.assertEquals( [ 1 ], assemble( 'one :one' ))
  def test_literal( self ):
    """make sure literals increment label counter by 2"""
    self.assertEquals( [ 0, 1, 2, 3 ], assemble( ':zero zero 2 :three three' ))

if __name__=="__main__":
  for arg in sys.argv:
    if os.path.exists( arg ):
      print( arg )
