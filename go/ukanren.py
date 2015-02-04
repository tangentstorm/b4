# python port of microkanren (µkanren)
from collections import namedtuple as nt

# lisp-like cons cells. most of the time, we'll
# actually use nts with the same shape but different names.
cons = Cell = nt('Cell', ['car','cdr'])
def car(cell): return cell[0]
def cdr(cell): return cell[1]
def ispair(x): return isinstance(x,tuple) and len(x)==2

# null value as a special cell
null = ()
def isnull(cell): return cell==()

# logic variables
Var = nt('Var', ['num'])
def eqvars(x:Var,y:Var)->bool: return x[0]==y[0]
def isvar(x:any)->bool: return isinstance(x,Var)

# assp: search an association list for first key to match predicate
# we could use a dict in python, but that would prevent backtracking.
# probably using a javascript-style chained dict would make more sense.
def assp(c:Cell, p:(lambda Cell:bool))->Cell or ():
    """find value in a association list"""
    if isnull(c): return null
    elif p(c[0][0]): return c[0]
    else: return assp(p,c[1])

# substitution lists
Subs = nt('Subs', ['head','tail'])
subs0 = ()
def extsub(x:Var, v:any, s:Subs)->Subs: return Subs((x,v), s)
def walk(u:any, s:Subs)->any:
    pr = isvar(u) and assp(s, lambda v:eqvars(u,v))
    # if pr finds a match, the recursion triggers so that
    # variables can be bound to other variables in a chain.
    return walk(pr[1],s) if pr else u


# states and streams
State  = nt('State', ['subs','count'])
state0 = State(subs0, 0)
Stream = nt('Stream',['head','tail'])
stream0= ()
def stream1(sc:State)->Stream: return Stream(sc, stream0)

# unification
def unify(u:any, v:any, s:Subs)-> Subs or False:
    u = walk(u,s); v=walk(v,s); vu=isvar(u); vv=isvar(v)
    if vu and vv and eqvars(u,v): return s
    elif vu: return extsub(u,v,s)
    elif vv: return extsub(v,u,s)
    elif ispair(u) and ispair(b):
        s = unify(car(u),car(v), s)
        return s and unify(cdr(u),cdr(v),s)
    else: return u==v and s


# fusion ('≡' in kanren)
Fuse = nt('Bind', ['var','val'])
def fuse(u:any,v:any)->Stream:
    def lamG(sc:State)->Goal:
        subs=unify(u,v,sc.subs)
        return stream1(State(subs, sc.count)) if subs else stream0
    return lamG

# call/fresh
Goal = (type, lambda Var:Stream)
def callf(f:Goal):
    def lamG(sc:State)->Goal:
        c=sc.count
        return f(Var(c))(State(sc.subs, c+1))
    return lamG


# monad stuff

def mplus(x:Cell,y:Cell)->Cell:
    if isnull(x): return y
    elif callable(x): return (lambda:mplus(x)(y))
    else: return cons(car(x),mplus(cdr(x),y))

def bind(w, g)->Stream:
    """monadic bind (>>= in haskell). w is the return value"""
    if isnull(w): return stream0
    elif callable(w): return lambda: bind(w(),g)
    else: return mplus(g(car(w)), bind(cdr(w), g))

def disj(g0:Goal,g1:Goal)->Goal:
    return lambda sc: mplus(g0(sc), g1(sc))

def conj(g0:Goal,g1:Goal)->Goal:
    return lambda sc: bind(g0(sc), g1)




# examples and tests
_=list(callf(lambda q: fuse(q,5))(state0))
assert _==[State(Subs((Var(0),5),()),1),()], str(s)


def fives(x): return disj(fuse(x,5), lambda sc: lambda: fives(x)(sc))
print(callf(fives)(state0))
