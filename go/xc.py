"""
xc : xterm colors

The xterm terminal emulator extends the ANSI escape code
sequences, providing a 256-color palette.

"""
__AUTHOR__  = "Michal J Wallace"
__LICENSE__ = "UNLICENSE"

_ansi = [ 
        0x000000, # black
        0xaa0000, # red
        0x00aa00, # green
        0xaaaa00, # dark yellow ( note: not vga brown! )
        0x0000aa, # blue
        0xaa00aa, # magenta
        0x00aaaa, # cyan
        0xaaaaaa, # gray
        0x555555, # dark gray
        0xff5555, # light red
        0x55ff55, # light green
        0xffff55, # yellow
        0x5555ff, # light blue
        0xff55ff, # light magenta
        0x55ffff, # light cyan
        0xffffff ]# white

# colors 16..231 are a color cube:
_ramp = [ 0x00, 0x5F, 0x87, 0xAF, 0xD7, 0xFF ]
_cube = [ ( r << 16 ) + ( g << 8 ) + b 
         for r in _ramp for g in _ramp for b in _ramp ]

# 232..255 are a black to gray gradiant:
_ramp = [ 0x00, 0x12, 0x1C, 0x26, 0x30, 0x3A, 0x44, 0x4E, 
          0x58, 0x62, 0x6C, 0x76, 0x80, 0x8A, 0x94, 0x9E, 
          0xA8, 0xB2, 0xBC, 0xC6, 0xD0, 0xDA, 0xE4, 0xEE ]
_grad = [ ( v << 16 ) + ( v << 8 ) + v for v in _ramp ]

pal = _ansi + _cube + _grad

