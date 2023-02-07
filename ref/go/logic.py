"""
Tools for logical inference.
"""
from collections import namedtuple as nt
from itertools import repeat, chain
from types import GeneratorType

Var = nt('Var',['name'])
Not = nt('Not',['x'])
Par = nt('Par',['x',])
Imp = nt('Imp',['x','y'])
Rfn = nt('Rfn',['x','y'])
And = nt('And',['x','y'])
Vel = nt('Vel',['x','y'])
Xor = nt('Xor',['x','y'])
Mux = nt('Mux',['x','y','z'])

Sym = nt('Sym',['value'])
Int = nt('Int',['value'])

# Inference rule. (entailment)
Rule = nt('Rule',['name','args','ants','cons'])

# Modus Ponens: P→Q, P ⊢ Q
MP = Rule("MP",["P","Q"],
          [Imp(Var('P'), Var('Q')), Var('P')],
          [Var('Q')])

# Modus Tollens: P→Q, ¬Q ⊢ ¬P
MT = Rule("MT",["P","Q"],
          [Imp(Var('P'),Var('Q')), Not(Var('Q'))],
          [Not(Var('P'))])

# Internal structure used to track subgoals.
# Try to prove a statement in this context
# using the rule with substitutions from ud.
# (context is a lisp-style tuple-chain of statements)
Try = nt('Try', ['stmt','ctx','ud'])

Logic = nt('Logic', ['name','rules','axioms'])


FAIL=("FAIL",)



# basic tree inspection, substitution, unification

def kind(x): return x.__class__
def kname(x): return x.__class__.__name__
def isvar(x): return kind(x)==Var
def istup(x): return isinstance(x, tuple)
def isseq(x): return istup(x) or kind(x) is list
def atomic(x): return kind(x)in[Int,Sym,str]
def kindly(seq):
     for x in seq: yield (x,kind(x))

def intree(x:any,y:nt) -> bool:
    """True when x appears somewhere in y"""
    if x == y: return True
    elif atomic(y): return False
    elif isseq(y): return any(intree(x,ey) for ey in y)
    else: return NotImplementedError('intree(x,%r)'%y)

def subs(kv:dict, x:any) -> any:
    """rewrite x according to substitution rules in kv."""
    if atomic(x): return x
    elif isvar(x): return kv.get(x.name, x)
    elif kind(x) is list: return [subs(kv,ex) for ex in x]
    elif istup(x): return kind(x)(*[subs(kv,ex) for ex in x])
    else: raise NotImplementedError('subs(kv,%r)'%k)

def unify(x:any,y:any) -> dict or FAIL:
    """attempt to perform symbolic unification."""
    if x==y: return []
    elif isvar(x) and not (isvar(y) or intree(x,y)): return {x.name:y}
    elif isvar(y) and not (isvar(x) or intree(y,x)): return {y.name:x}
    elif atomic(x) or atomic(y) or (len(x)!=len(y)): return FAIL
    elif kind(x)!=kind(y): return FAIL
    else:
        res = {}
        for ex,ey in zip(x,y):
            eu = unify(subs(res,ex),subs(res,ey))
            if eu == FAIL: return FAIL
            else: res.update(eu)
        return res


# generic pretty printer

Row = nt('Row',['args'])
Col = nt('Col',['args'])
Ind = nt('Ind',['arg']) # indent
Div = nt('Div',['arg']) # line feed at the end
Grp = nt('Grp',['arg']) # fuse into string before yielding

def sep(seq, by):
    g = zip(repeat(by), seq)
    yield next(g)[1] # drop first separator
    for p,q in g:
        yield p; yield q

def ppfmt(gen, ind=0, width:int=float('inf')) -> str:
    """low level string formatting"""
    for e in gen:
        ke=kind(e)
        if ke is str: yield e
        elif ke is Ind: yield from ppfmt(e.arg,ind+1)
        elif ke is Col:
            for arg in e.args: yield from ppfmt(arg,ind)
        elif ke is Div:
            yield (ind*'  ')
            for ee in ppfmt(e.arg,ind): yield ee
            if ee != '\n': yield '\n'
        elif ke is Row:
            for arg in e.args: yield from ppfmt(arg,ind)
        elif ke is Grp:
            yield ''.join(s for s in ppfmt(e.arg,ind))
        else: raise NotImplementedError('ppfmt(%s)'%e)

# pretty printer rules

Val = nt('Val',['name'])
Sep = nt('Sep',['seq','by'])
Unk = nt('Unk',['arg'])

rule_ss = {
    Logic: Col([Div(Row(['Logic: ', Val('name')])),
                Ind(Col([Div(Row(['Inference rules:'])),
                         Ind(Val('rules')),
                         Div(Row(['Axioms:'])),
                         Ind(Val('axioms')),
                     ])
                )]),
    Rule : Div(Row([Val('name'), ': ', Sep('ants',by=', '),
                ' ⊢ ', Sep('cons',by='; '), '.'])),
    Imp  : Row([Val('x'), ' → ', Val('y')]),
    Not  : Row(['¬',Val('x')]),
    Var  : Val('name'),
    Par  : Row(['(',Val('x'),')']),
    Try  : Row(['try ', Val('stmt'), ' in:\n', Val('ctx')]),
}

def chompg(g): return chomp(''.join(ppfmt(g)))
def ppgen(x:nt, ss:dict, fmt:nt=None) -> [str]:
    """helper for pp"""
    f = fmt or ss.get(kind(x), Unk(x)); kf=kind(f)
    #print('>>', kname(x), '.', kname(f))
    if kind(x) in (int, str): yield str(x)
    elif kf in (str, int): yield str(f)
    elif kind(x) is list:
        for ex in x: yield from ppgen(ex,ss)
    elif kind(x) is tuple:
        if len(x) == 0: h,s,t='','',''
        elif len(x) == 1: h,s,t=ppgen(x[0],ss),'',''
        else: h,s,t = chompg(ppgen(x[0],ss)), '\n', chompg(ppgen(x[1],ss))
        yield ' ['; yield from h; yield s; yield from t; yield ']\n'
    elif kf in (Ind,Div): yield kf(ppgen(x,ss,f.arg))
    elif kf in (Row,Col): yield kf([ppgen(x,ss,a) for a in f.args])
    elif kf in (Sep,):
        yield from sep([Grp(ppgen(e,ss)) for e in getattr(x,f.seq)],f.by)
    elif kf is Val:
        yield from ppgen(getattr(x,f.name),ss)
    elif kf is Unk: raise NotImplementedError('ppgen(%s,..)' % kname(x))
    else: raise NotImplementedError('ppgen(...,%s)'%(f,))


# pretty printer

def chomp(s):
    return s[:-1] if s[-1]=='\n' else s

def pp(x:nt, ss:dict=rule_ss) -> str:
    return chomp(''.join(ppfmt(ppgen(x,ss))))


# logic engine

def tactics(goal:nt, stmt:nt, ctx:(nt,())=())->[Try]:
    """
    Yields possible plans for unifying the goal with
    the given statement.
    """
    assert isinstance(stmt, tuple), str(stmt)

    # base cases of the recursion:
    ud=unify(goal,stmt); kg=kind(goal); ks = kind(stmt)
    if ud is not FAIL: yield Try(stmt, (goal,ctx), ud)

    if ks is Var: return # Vars always unify. No need to repeat.
    elif ks is Par: yield from tactics(goal, stmt.x, (stmt,ctx))
    elif ks is Rule:
        for c in stmt.cons:
            yield from tactics(goal, c, (stmt,ctx))
    elif ks is Imp:
        # either way, try to prove its consequent.
        yield from tactics(goal, stmt.y, (stmt,ctx))
    else: raise NotImplementedError('tactics(goal,%s,...)'%kname(stmt))


# language L from /A Primer for Logic and Proof/
# by Holly P. Hirst and Jeffry L. Hirst
# http://www.mathsci.appstate.edu/~jlh/primer/hirst.pdf
L = Logic('L', [MP], [

    # A1. ⊢ A → (B→A).
    Rule("A1", ["A","B"], [],
         [Imp(Var('A'),
              Par(Imp(Var('B'),Var('A'))))]),

    # A2. ⊢ ((A → (B→C)) → ((A→B) → (A→C)).
    Rule("A2", ["A","B","C"], [],
         [Imp(Par(Imp(Var('A'),
                  Par(Imp(Var('B'),Var('C'))))),
              Par(Imp(Par(Imp(Var('A'),Var('B'))),
                      Par(Imp(Var('A'),Var('C')))))) ]),

    # A3. ⊢ (¬B → ¬A) → ((¬B → A) → B)).
    Rule("A3", ["A","B"], [],
         [Imp(Par(Imp(Not(Var('B')), Not(Var('A')))),
              Par(Imp(Par(Imp(Not(Var('B')), Var('A'))),
                      Var('B')))) ]) ])

# These lemmas are either proven in the Hirst & Hirst PDF,
# or given as exercises.
LGoals = [
    # L1. ⊢ A → A.
    Rule("L1", ["A"], [], [Imp(Var("A"),Var("A"))]),
    # L2. ⊢ (¬B → B) → B.
    Rule("L2", ["B"], [], [Imp(Imp(Not(Var("B")),Var("B")), Var("B"))]),
    # L3. A → (B→C), A→B ⊢ A → C.
    Rule("L3", ["A","B","C"],
         [Imp(Var("A"),Par(Imp(Var("B"),Var("C")))),
          Imp(Var("A"),Var("B"))],
         [Imp(Var("A"),Var("C"))]),
    # L4. A → ((B→A) → C) ⊢ A → C.
    Rule("L4", ["B"], [], []),
    # L5. B ⊢ A → B.
    Rule("L5", ["B"], [], []),
    # L6. A→(B→C),B ⊢ A→C.
    Rule("L6", ["B"], [], []),
    # L7. A→(B→C),B ⊢ B→(A→C)
    Rule("L7", ["B"], [], []),
    # L8. A→B, B→C ⊢ A→C
    Rule("L8", ["B"], [], []),
    # L9. P → R ⊢ P → (Q→R)
    Rule("L9", ["P","Q","R"], [], []),
    # L10. ⊢ (¬B → ¬A) → (A→B)
    Rule("L10", ["A","B"], [], []),
    # L11. ⊢ ¬¬B → B
    Rule("L11", ["B"], [], []),
    # L12. ⊢ B→ ¬¬B
    Rule("L12", ["B"], [], []) ]


if __name__=="__main__":
    logic = L
    print(pp(L))
    for (goal,con) in [(g,c) for g in LGoals for c in g.cons]:
        print('goal:', pp(goal))
        for stmt in chain(L.rules, L.axioms):
            for t in tactics(goal,stmt):
                print(pp(t))
        break # only try first lemma for now.
