cocurrent 'vt'

NB. VT ESCAPE CODES
NB. -------------------------------------------------------
esc    =: u:27               NB. ANSI escape character
csi    =: esc,'['            NB. vt100 'command sequence introduce'
clrscr =: ,csi,"1 0'HJ'      NB. clear screen
cls    =: clrscr
clreol =: csi,'0K'           NB. clear to end of line
reset  =: csi,'0m'           NB. reset color to gray on black
cU =: csi,'A'                NB. cursor up
cD =: csi,'B'                NB. cursor down
cL =: csi,'D'                NB. cursor left
cR =: csi,'C'                NB. cursor right


NB. xterm 256-color codes
NB. Most modern terminals seem to support 256 colors.
fgn=: csi,'38;5;', 'm',~ ":   NB. fg (numeric)
bgn=: csi,'48;5;', 'm',~ ":   NB. bg (numeric)

NB. 'colorwrite' character
cwc=. [: [:^:(16=]) 'krgybmcwKRGYBMCW' i. ]
ischr=. 2 = 3!:0
fg=: ([: fgn cwc^:ischr) f.
bg=: ([: bgn cwc^:ischr) f.

NB. set cursor position
goxy =: csi, ":@{:, ';', 'f',~ ":@{.

NB. show/hide cursor
curs =: csi,'?25','lh'{~]

cocurrent 'z'
fg =: fg_vt_
bg =: bg_vt_
cls =: cls_vt_
reset =: reset_vt_
curs =: curs_vt_
goxy =: goxy_vt_
