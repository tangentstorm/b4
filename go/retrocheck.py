import unittest
from narrative import testcase
from collections import namedtuple

Def = namdetuple('Def', [name, takes, gives, effects, doc])

types = {
    "A" : "address",
    "N" : "number",
    "Q" : "quote",
    "a" : "<any.a>",
    "b" : "<any.b>",
    "c" : "<any.c>",
}


words = [

    Def('+', ['II'], ['I'],  [], 'z+y'),
    Def('-', ['II'], ['I'],  [], 'z-y'),
    Def('*', ['II'], ['I'],  [], 'z*y'),
    Def('%', ['II'], ['II'], [], 'division'),

    Def('!', ['NA'], [],  [], 'store value'),
    Def('@', ['A'],  ['N'],   [], 'fetch value'),

    Def('!+', ['NA'], ['A'],  [], 'store value, inc addr'),
    Def('@+', ['A'],  ['AC'],   [], 'fetch value, inc addr'),

    Def('+!',  ['NA'], [],   [], 'increment variable by N')
    Def('++',  ['A'],  [],   [], 'increment variable by 1'),
    Def('-!',  ['NA'], [],   [], 'decrement variable by N')
    Def('--',  ['A'],  [],   [], 'decrement variable by 1'),

    Def('swap', ['ab'],  ['ba'],   [], 'swap top two items'),
    Def('over', ['ab'],  ['aba'],  [], 'copy nos over tos'),
    Def('rot',  ['abc'], ['bca'],  [], 'rotate'),
    Def('dup',  ['a'],   ['aa'],   [], 'duplicate top item'),

    Def('times',  ['NQ'], [],   [], 'run quote n times'), # TODO fx

    Def('[', [], [],    ['compiler:on'], 'begin a quote'),
    Def(']', [], ['Q'], ['compiler:off'], 'end a quote'),
]
