#!/usr/bin/env python3
"""
b3a0.py: a bootstrap b3 assembler in python

This is a simplfied version of b3a.b3a.

It does not provide the ability to create macros
(since that would require a b3 bytecode interpreter).
Instead, it has hard-coded support for the specific
macros used in b3a.b3a.
"""
import re, collections

OPS = [
    'ad',  'an',  'bw',  'cd',  'cl',  'dc',  'dr',  'du',
    'dv',  'dx',  'dy',  'dz',  'eq',  'ev',  'ge',  'go',
    'gt',  'h0',  'h1',  'hl',  'hp',  'jm',  'le',  'li',
    'lt',  'md',  'ml',  'ne',  'ng',  'nt',  'nx',  'or',
    'ov',  'qp',  'r0',  'r1',  'rd',  'rm',  'rp',  'rt',
    'sb',  'lb',  'sl',  'sr',  'sw',  'wm',  'wp',  'xd',
    'xr',  'yd',  'yr',  'zd',  'zp',  'zw' ]

SEEN = collections.Counter()


## lexer for b3a source code ############################

def bc(t):
    """return the byte code for a mnemonic token"""
    return OPS.index(t) + 0xA0

def hexit(c):
    """(capital) hex value of c, else 0"""
    return max('0123456789ABCDEF'.find(c),0)

def tokens(src):
    """src -> [(byte|char, ready?:bit)]"""
    src = src.replace("'#", hex(ord('#'))[2:])  # get rid of '# so # always indicates comment.
    src = re.sub('#.*','',src)                  # get rid of all comments
    for t in src.split():
        SEEN[t] += 1
        if   t[0]=="'": yield (ord(t[1]),    1)
        elif t[0]=="^": yield (ord(t[1].upper())-64, 1)
        elif t[0]==".": yield (t[1], 0)
        elif t in OPS:
            yield (bc(t), 1)
        else: yield ((hexit(t[0]) << 4) + hexit(t[1]), 1)

## built-in macros function #############################

STACK = []

def SW():
    """swap operation"""
    x = STACK.pop()
    y = STACK.pop()
    STACK.extend([x,y])

def OV():
    """over operation"""
    STACK.append(STACK[-2])

def ZD(res):
    """in b3, 'zd' writes the current write position to the stack"""
    STACK.append(len(res))

def backjump(res, op):
    last = STACK.pop()
    dist = last - len(res) # negative short int
    res.extend([bc(op), 255 & dist])

# == if/then/else/endif =================================

def i(res):
    """the IF part of IF .. THEN .. ELSE .. END"""
    # <<ctl-i>>
    # bw ^i rt go
    pass

def t(res):
    """the THEN part of IF .. THEN .. ELSE .. END"""
    # <<ctl-t>>
    # bw ^t lb h0 zw lb 00 zw zd rt go
    res.extend([bc('lb'), 0])
    ZD(res)

def e(res):
    """the ELSE part of IF .. THEN .. ELSE .. END"""
    # <<ctl-e>>
    # bw ^e .[ hp 00 .] zd sw ov ^z ev rt go
    res.extend([bc('hp'), 0])
    ZD(res); SW(); OV(); z(res)

def z(res):
    """the END part of IF .. THEN .. ELSE .. END"""
    # <<ctl-z>>
    # bw ^z du zd sw sb wb rt go
    last = STACK.pop()
    dist = (len(res) - last)
    res[last-1]=dist

# == while .. do .. od ==================================

def w(res):
    """the WHILE part of WHILE .. DO .. OD"""
    # <<ctl-w>>
    # bw ^w zd rt go
    ZD(res)

def d(res):
    """the DO part of WHILE .. DO .. OD"""
    # <<ctl-d>>
    # bw ^d ^t ev rt go
    t(res)

def o(res):
    """the OD part of WHILE .. DO .. OD"""
    # <<ctl-o>>
    # bw ^o sw .[ hp .] zd sw sb zw ^z ev rt go
    backjump(res, 'hp')
    z(res) # fwd jump when condition fails

# == for .. next ========================================

def f(res):
    """the FOR part of FOR .. NEXT"""
    # <<ctl-f>>
    # bw ^f zd .[ dr .] rt go
    ZD(res)
    res.extend([bc('dr')])

def n(res):
    """the NEXT part of FOR .. NEXT"""
    # <<ctl-f>>
    # bw ^n .[ nx .] ^b ev rt go
    res.extend([bc('nx')])
    backjump(res, 'nx')

MACROS = {
    'i':i, 't':t, 'e':e, 'z':z,
    'w':w, 'd':d, 'o':o,
    'f':f, 'n':n }

def quote(res, toks):
    """ quote handler: ( ) """
    # bw ^[ .w ^k ev du .[ FF 1C .] ad eq nt .d .[ lb .] zw .o zp rt go
    for tok,ready in toks:
        if ready: res.append(tok)
        elif tok == ']': return
        elif tok == '[': raise "cannot handle nested .[ .. .]"
        elif tok in MACROS: MACROS[tok](res)
        else: raise "cannot handle '."+tok+"' inside .[....]"


## main assembler function ##############################

def asm(src):
    res, toks = ([], tokens(src))
    while True:
        try:
            tok, ready = next(toks)
            if ready: res.append(tok)
            else: # macros
                if tok == '[': quote(res, toks)
                else: MACROS[tok](res)
        except StopIteration:
            return bytes(res)


def print_stats():
    print("opcodes that appeared in the source code:")
    for (tok,count) in SEEN.most_common():
        if tok in OPS: # or tok.startswith('.') or tok.startswith('^'):
            print (tok, '%2i|'%count, '*'*count)
    print("unused: ", ' '.join(sorted({op for op in OPS if not SEEN[op]})))


if __name__=="__main__":
    src = open('b3a.b3a').read()
    open('b3a0.out','wb').write(asm(src))
    print_stats()
