"""
Tools for logical inference.
"""
from collections import namedtuple as nt

Var = nt('Var',['name'])
Not = nt('Not',['x'])
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
# using the rule with substitutions.
Try = nt('Try', ['stmt','ctx','rule','kvs'])

Logic = nt('Logic', ['rules','axioms'])


FAIL=("FAIL",)



# basic tree inspection, substitution, unification

def kind(x): return x.__class__
def kname(x): return x.__class__.__name__
def isvar(x): return kind(x)==Var
def istup(x): return isinstance(x, tuple)
def isseq(x): return istup(x) or kind(x) is list
def atomic(x): return kind(x)in[Int,Sym,str]

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
        for ex,ey in zip(tx,ty):
            eu = unify(subs(res,ex),subs(res,ey))
            if eu == FAIL: return FAIL
            else: res.update(eu)
        return res


# pretty printer rules

Unk = nt('Unk',['arg'])
Row = nt('Row',['args'])
Col = nt('Col',['args'])
Tbl = nt('Tbl',['args'])
Val = nt('Val',['name'])
Sep = nt('Sep',['seq','by'])
Str = nt('Str',['arg'])

rule_ss = {
    Rule : Row([Val('name'), ': ', Sep(Val('ants'),by=', '),
                ' ⊢ ', Sep(Val('cons'),by='; '), '.']),
    Imp  : Row([Val('x'), '→', Val('y')]),
    Var  : Val('name'),
    Try  : Row(['try: ', Val('rule')]),
    int  : str,
    str  : str,
}

def ppgen(x:nt, ss:dict, fmt:nt=None) -> [str]:
    """helper for pp"""
    f = fmt or ss.get(kind(x), Unk(x)); kf=kind(f)
    if kind(x) in (int, str): yield str(x)
    elif kind(x) is list:
        for ex in x: yield from ppgen(ex,ss)
    elif kf in (str, int): yield str(f)
    elif kf is Unk: yield '(?: %s )' % kname(x)
    elif kf is Val: yield from ppgen(getattr(x,f.name),ss)
    elif kf is Str: yield f.arg
    elif kf is Sep: yield f.by.join(s for s in ppgen(x,ss,f.seq) if s)
    elif kf is Row:
        yield ''.join(''.join(s for s in ppgen(x,ss,ef)) for ef in f.args)
    else: raise NotImplementedError('ppgen(...,%s)'%(f,))

def pp(x:nt, ss:dict=rule_ss) -> str:
    """pretty print x according to stylesheet ss"""
    return ''.join(s for s in ppgen(x,ss) if s)


# logic engine

def tactics(stmt:nt, ctx:[Rule]=None)->[Try]:
    """
    Yields all rules from ctx with a consequent
    that unifies with stmt. These are the tactics
    we currently know that may allow us to prove
    the statement.
    """
    for rule in ctx:
        for con in rule.cons:
            kv=unify(stmt,con)
            if kv is not FAIL: yield Try(stmt,ctx,rule,kv)



# language L from /A Primer for Logic and Proof/
# by Holly P. Hirst and Jeffry L. Hirst
# http://www.mathsci.appstate.edu/~jlh/primer/hirst.pdf
L = Logic([MP], [

    # A1. ⊢ A → (B→A).
    Rule("A1", ["A","B"], [],
         [Imp(Var('A'),
              Imp(Var('B'),Var('A')))]),

    # A2. ⊢ ((A → (B→C)) → ((A→B) → (A→C)).
    Rule("A2", ["A","B","C"], [],
         [Imp(Imp(Var('A'),
                  Imp(Var('B'),Var('C'))),
              Imp(Imp(Var('A'),Var('B')),
                  Imp(Var('A'),Var('C'))))]),

    # A3. ⊢ (¬B → ¬A) → ((¬B → A) → B)).
    Rule("A3", ["A","B"], [],
         [Imp(Imp(Not(Var('B')), Not(Var('A'))),
            Imp(Imp(Not(Var('B')), Var('A')), Var('B')))]) ])

# These lemmas are either proven in the Hirst & Hirst PDF,
# or given as exercises.
LGoals = [
    # L1. ⊢ A → A.
    Rule("L1", ["A"], [], [Imp(Var("A"),Var("A"))]),
    # L2. ⊢ (¬B → B) → B.
    Rule("L2", ["B"], [], [Imp(Imp(Not(Var("B")),Var("B")), Var("B"))]),
    # L3. A → (B→C), A→B ⊢ A → C.
    Rule("L3", ["A","B","C"],
         [Imp(Var("A"),Imp(Var("B"),Var("C"))),
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
    for (goal,con) in [(g,c) for g in LGoals for c in g.cons]:
        print(pp(goal))
        for t in tactics(goal,L.rules):
            print(pp(t.rule))
        break
