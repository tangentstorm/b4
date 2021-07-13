cocurrent 'vt'

NB. VT ESCAPE CODES
NB. -------------------------------------------------------
esc =: u:27
cls =: esc,'[H',esc,'[J'
cle =: esc,'[0K' NB. clear to end of line
reset =: esc,'[0m'

NB. cwcolorb → c b (color, bold) where (c e. i.8) ∧ (b e. 0 1)
cwcolorb  =. 8 (|,>:) 'krgybmcwKRGYBMCW' i. ]

NB. xterm 256-color codes
NB. Most modern terminals seem to support 256 colors.
fgn=: esc, '[38;5;', 'm',~ ":
bgn=: esc, '[48;5;', 'm',~ ":
cwc=: [: [:^:(16=]) 'krgybmcwKRGYBMCW' i. ]
istxt=. 131072 2048 2 e.~ 3!:0
fg=: ([: fgn cwc^:istxt) f.
bg=: ([: bgn cwc^:istxt) f.

goxy =: esc, '[', ":@{. , ';',  'f',~ ":@(1&{)

goxy =: esc, '[', ":@(1&{) , ';',  'f',~ ":@{.

cocurrent 'z'
fg =: fg_vt_
bg =: bg_vt_
esc =: esc_vt_
cls =: cls_vt_
cle =: cle_vt_
reset =: reset_vt_
cursor =: cursor_vt_
goxy =: goxy_vt_

