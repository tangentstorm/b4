#!/bin/env python2.4
import sys
import pygame as pg
import dosfont
import xc

fw, fh = ( 8, 16 )     # font width, height
tw, th = ( 128, 48   ) # terminal width, height
vw, vh = ( 1024, 768 ) # video width, height

pg.init()

scr = pg.display.set_mode(( vw, vh ), pg.SWSURFACE | pg.FULLSCREEN )
pal = [ pg.Color( hex( v )) for v in xc.pal ]

font = dosfont.data
chbuf = pg.Surface(( fw , fh ))
fg = 7
bg = 0
cx = 0
cy = 0

def draw_char( ch, fg, bg ):
    rows = font[ ord( ch ) % 255  ]
    for y, row in enumerate( rows ):
        for x in range( 8 ):
            if row & (1 << x): px = fg
            else: px = bg
            chbuf.set_at(( 7-x, y ), px )
    scr.blit( chbuf, pg.Rect( cx * fw, cy * fh, 8, 16 ))

def cx_inc( ):
    global cx, cy
    cx += 1
    if cx > tw : cx, cy = 0, cy + 1
    if cy > th : pass # TODO scroll window up

def write( s ):
    for ch in s:
        draw_char( ch, xc.pal[ fg ], xc.pal[ bg ])
        cx_inc()

def xyc_write( x, y, c, s ):
    global cx; cx = x
    global cy; cy = y
    global fg; fg = c
    write( s )

def color_test( ):
    global cx, cy, bg, fg

    write( "cwrite() color codes: " )
    fg = 0
    for ch in 'krgybmcwKRGYBMCW':
        write( ch )
        fg = fg + 1

    cx, cy = 64, 0
    bg =   8; xyc_write( 64, 0, 0, "black on 0x08" )
    bg =   7; xyc_write( 64, 1, 0, "black on 0x07" )
    bg = 255; xyc_write( 64, 2, 0, "black on 0xff" )
    bg =  15; xyc_write( 64, 3, 0, "black on 0x0f" )
    bg = 0

    for x in range( 16 ):
        for y in range( 16 ):
            cx = 10 + x * 3
            cy = 10 + y
            fg = y * 16 + x
            write( "%02X" % fg )

def main( ):            
    done = False
    while not done :
        for e in pg.event.get( ) :
            if e.type == pg.QUIT : done = True
            if e.type == pg.KEYDOWN : done = True
        pg.display.flip( )
    pg.quit()

if __name__=="__main__":
    color_test()
    main()
