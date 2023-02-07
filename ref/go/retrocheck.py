"""
This is a start on a type checker for retro and other forth dialects.

The idea was to start with a small database of forth primitives
and their stack effects, and then use that database to derive the
stack effect for defined words.
"""
import unittest
from narrative import testcase
from collections import namedtuple

Def = namedtuple('Def', ['name', 'takes', 'gives', 'effects', 'doc'])

types = {
    "A" : "address",
    "N" : "number",
    "Q" : "quote",
    "a" : "<any.a>",
    "b" : "<any.b>",
    "c" : "<any.c>",
}


words = [

    Def('+', ['NN'], ['N'],  [], 'z+y'),
    Def('-', ['NN'], ['N'],  [], 'z-y'),
    Def('*', ['NN'], ['N'],  [], 'z*y'),
    Def('%', ['NN'], ['NN'], [], 'division'),

    Def('!', ['NA'], [],  [], 'store value'),
    Def('@', ['A'],  ['N'],   [], 'fetch value'),

    Def('!+', ['NA'], ['A'],  [], 'store value, inc addr'),
    Def('@+', ['A'],  ['AN'],   [], 'fetch value, inc addr'),

    Def('+!',  ['NA'], [],   [], 'increment variable by N'),
    Def('++',  ['A'],  [],   [], 'increment variable by 1'),
    Def('-!',  ['NA'], [],   [], 'decrement variable by N'),
    Def('--',  ['A'],  [],   [], 'decrement variable by 1'),

    Def('swap', ['ab'],  ['ba'],   [], 'swap top two items'),
    Def('over', ['ab'],  ['aba'],  [], 'copy nos over tos'),
    Def('rot',  ['abc'], ['bca'],  [], 'rotate'),
    Def('dup',  ['a'],   ['aa'],   [], 'duplicate top item'),

    Def('times',  ['NQ'], [],   [], 'run quote n times'), # TODO fx

    Def('[', [], [],    ['compiler:on'], 'begin a quote'),
    Def(']', [], ['Q'], ['compiler:off'], 'end a quote'),
]

for word in words:
    print word
