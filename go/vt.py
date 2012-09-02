#!/bin/env python2.4
import sys
import pygame as pg
import dosfont

pg.init()

size = scr_w, scr_h = 1024, 768
scr = pg.display.set_mode( size, pg.SWSURFACE | pg.FULLSCREEN )

# standard vga colors:
pal = [ pg.Color( x ) for x in [
        '#000000', # black
        '#aa0000', # red
        '#00aa00', # green
        '#aa5500', # brown ("dark yellow")
        '#0000aa', # blue
        '#aa00aa', # magenta
        '#00aaaa', # cyan
        '#aaaaaa', # gray
        '#555555', # dark gray
        '#ff5555', # light red
        '#55ff55', # light green
        '#ffff55', # yellow
        '#5555ff', # light blue
        '#ff55ff', # light magenta
        '#55ffff', # light cyan
        '#ffffff', # white
        ]]

font = dosfont.data
chbuf = pg.Surface(( 8 , 16 ))
fg = 7
bg = 0
cx = 0
cy = 0

def render( ch, fg, bg ):
    rows = font[ ord( ch ) % 255  ]
    for y, row in enumerate( rows ):
        for x in range( 8 ):
            if row & (1 << x): px = fg
            else: px = bg
            chbuf.set_at(( 7-x, y ), px )
    scr.blit( chbuf, pg.Rect( cx * 8, cy * 8, 8, 16 ))


fg = 0
for ch in 'krgybmcwKRGYBMCW':
    render( ch, pal[ fg ], pal[ bg ])
    fg = ( fg + 1 ) % 16
    cx += 1

cx = 64
cy = 0
for ch in 'hello world':
    render( ch, pal[ 0 ], pal[ 7 ])
    cx += 1
            
done = False
while not done :
    for e in pg.event.get( ) :
        if e.type == pg.QUIT : done = True
        if e.type == pg.KEYDOWN : done = True
    pg.display.flip( )
pg.quit()
