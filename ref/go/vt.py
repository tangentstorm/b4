#!/bin/env python2.4
import sys
import pygame as pg
import dosfont
import xc

fw, fh = ( 8, 16 )     # font width, height
tw, th = ( 128, 48   ) # terminal width, height
vw, vh = ( 1024, 768 ) # video width, height

pg.init()

scr = pg.display.set_mode(( vw, vh ), pg.SWSURFACE )# | pg.FULLSCREEN )
pal = [ pg.Color( v ) for v in xc.pal ]
cwk = dict( zip ( 'krgybmcwKRGYBMCW', range( 16 ))) # codes for cwrite routine
font = dosfont.data
chbuf = pg.Surface(( fw , fh ))

class g:
    # module namespace
    fg = 7
    bg = 0
    cx = 0
    cy = 0
    xy_stack = []

def draw_char( ch, fg, bg ):
    rows = font[ ord( ch ) % 255  ]
    for y, row in enumerate( rows ):
        for x in range( 8 ):
            if row & (1 << x): px = fg
            else: px = bg
            chbuf.set_at(( 7-x, y ), px )
    scr.blit( chbuf, pg.Rect( g.cx * fw, g.cy * fh, 8, 16 ))

def inc_cx( ):
    g.cx += 1
    if g.cx > tw : g.cx, g.cy = 0, g.cy + 1
    if g.cy > th : pass # TODO scroll window up

def write( s ):
    for ch in s:
        draw_char( ch, xc.pal[ g.fg ], xc.pal[ g.bg ])
        inc_cx()

def fg( c ):
    g.fg = c

def xyc_write( x, y, c, s ):
    g.cx = x
    g.cy = y
    g.fg = c
    write( s )

def push_xy( ):
    g.xy_stack.append(( g.cx, g.cy ))

def pop_xy( ):
    ( g.cx, g.cy ) = g.xy_stack.pop( )


###############

def color_test( ):

    write( "cwrite() color codes: " )
    for ch in 'krgybmcwKRGYBMCW':
        fg( cwk[ ch ])
        write( ch )
        g.fg = g.fg + 1

    g.cx, g.cy = 64, 0
    g.bg =   8; xyc_write( 64, 0, 0, "black on 0x08" )
    g.bg =   7; xyc_write( 64, 1, 0, "black on 0x07" )
    g.bg = 255; xyc_write( 64, 2, 0, "black on 0xff" )
    g.bg =  15; xyc_write( 64, 3, 0, "black on 0x0f" )
    g.bg = 0

    for x in range( 16 ):
        for y in range( 16 ):
            g.cx = 10 + x * 3
            g.cy = 10 + y
            g.fg = y * 16 + x
            write( "%02X" % g.fg )


kbstate = [
    ( pg.KMOD_SHIFT  , 'shf' , 'SHF' ),
    ( pg.KMOD_CTRL   , 'ctl' , 'CTL' ),
    ( pg.KMOD_ALT    , 'alt' , 'ALT' ),
    ( pg.KMOD_META   , 'mta' , 'MTA' ),
    ] 

def test( ):
    color_test()
    done = False
    while not done :
        mods = pg.key.get_mods()
        for e in pg.event.get( ) :
            if e.type == pg.QUIT : done = True
            if e.type == pg.KEYDOWN :
                if e.key == pg.K_ESCAPE: done = True
                elif e.key == pg.K_c and mods & pg.KMOD_CTRL: done = True
                else:
                    xyc_write( 0, 4, cwk[ 'K' ], 'last keypress: ' )
                    push_xy( )
                    fg( cwk[ 'w' ])
                    write( ' ' * 16 )
                    pop_xy( )
                    write( pg.key.name( e.key ))

        xyc_write( 0, 5, cwk[ 'K' ], 'keyboard state: ' )

        for k in kbstate:
            fg( cwk['w'])
            if mods & k[ 0 ] :
                fg( cwk['G'])
                write( k[ 2 ])
            else: write( k[ 1 ])
            write( ' ' )
        
        pg.display.flip( )
    pg.quit()

if __name__=="__main__":
    test()
