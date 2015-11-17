"""
A more Scheme-like variant. Call by value, multiple arguments, some
other primitive datatypes.

e = x | c | (lambda (x*) e) | (e e*)
c = number | + | - | * | =

booleans are functions, though.

Goal:
1. an explicit-stack interpreter
2. a tracing JIT compiler
3. make the compiler reduce iterative factorial, coded with the Y
   combinator, to a tight loop.

Rationale:
Multiple arguments and primitive numbers make it easier to write
and compile realistic code.

Experiment:
Let's start out doing only single arguments. I'm not convinced the
multiple args would make everything simpler on net.
"""

from peglet import Parser, Unparsable, hug
import operator

# The trampoline

loud = 1

def trampoline(state):
    k, value = state
    while k is not final_k:
        if loud: traceback((k, value))
        print 'k', k
        fn, k = k
        k, value = fn(value, k)
    return value

final_k = None

def traceback(state):
    k, value = state
    print ':', repr(value)
    while k:
        fn, k = k
        print repr(fn)


# Abstract syntax

class Const(object):
    def __init__(self, value):
        self.value = value
    def free_vars(self):
        return ()
    def eval(self, r, k):
        return k, self.value
    def __repr__(self):
        return repr(self.value)

class Var(object):
    def __init__(self, name):
        self.name = name
    def free_vars(self):
        return (self.name,)
    def eval(self, r, k):
        return k, r[self.name]
    def __repr__(self):
        return self.name

class Lam(object):
    def __init__(self, param, body):
        self.param = param
        self.body = body
        self.fvs = tuple(set(body.free_vars()) - set([param]))
    def free_vars(self):
        return self.fvs
    def eval(self, r, k):
        return k, Closure(self, tuple(r[v] for v in self.fvs))
    def run(self, arg, closure_values, k):
        return self.body.eval(make_env((self.param,) + self.fvs,
                                       (arg,) + closure_values),
                              k)
    def __repr__(self):
        return '(%s -> %r)' % (self.param, self.body)

class Closure(object):
    def __init__(self, lam, values):
        self.lam = lam
        self.values = values
    def call(self, arg, k):
        return self.lam.run(arg, self.values, k)
    def __repr__(self):
        return '<%r: %s>' % (self.lam, ' '.join(map(repr, self.values)))

class App(object):
    def __init__(self, rator, rand):
        self.rator = rator
        self.rand = rand
    def free_vars(self):
        return tuple(set(self.rator.free_vars()) | set(self.rand.free_vars()))
    def eval(self, r, k):
        return self.rator.eval(r, (RandK(self.rand, r), k))
    def __repr__(self):
        return '(%r %r)' % (self.rator, self.rand)

class RandK(object):
    def __init__(self, rand, r):
        self.rand = rand
        self.r = r
    def __call__(self, fn, k):
        return self.rand.eval(self.r, (CallK(fn), k))
    def __repr__(self):
        return 'RandK(%r,...)' % (self.rand,)

class CallK(object):
    def __init__(self, fn):
        self.fn = fn
    def __call__(self, arg, k):
        return self.fn(arg, k)
    def __repr__(self):
        return 'CallK(%r)' % (self.fn)


def make_env(names, values):
    return dict(zip(names, values))


def run(expr):
    return expr.eval(global_r, final_k)


class Primitive2(object):
    def __init__(self, fn):
        self.fn = fn
    def __call__(self, arg, k):
        return k, PartialPrimitive2(self.fn, arg)
    def __repr__(self):
        return self.fn.__name__
    
class PartialPrimitive2(object):
    def __init__(self, fn, arg1):
        self.fn = fn
        self.arg1 = arg1
    def __call__(self, arg, k):
        return k, self.fn(self.arg1, arg)
    def __repr__(self):
        return '(%s %r)' % (self.fn.__name__, self.arg1)

def yes(if_yes, if_no): return if_yes
def no(if_yes, if_no):  return if_no

def equ(x, y): return yes if x == y else no

global_r = {
    '+':   Primitive2(operator.add),
    '-':   Primitive2(operator.sub),
    '*':   Primitive2(operator.mul),
    '=':   Primitive2(equ),
    'yes': Primitive2(yes),
    'no':  Primitive2(no),
}


# Parsing

def fold_app(f, *fs): return reduce(App, fs, f)
def fold_lam(vp, e):  return foldr(Lam, e, vp)

def foldr(f, z, xs):
    for x in reversed(xs):
        z = f(x, z)
    return z

parse = Parser(r"""
start = _ e !.
e     = f fs              fold_app
fs    = f fs
      |     
f     = v                 Var
      | const             Const
      | \\ _ vp [.] _ e   fold_lam
      | [(] _ e [)] _
vp    = v vs              hug
vs    = v vs
      |
v     = ([A-Za-z_]\w*)\b _
      | ([-+*=]) _
const = ([0-9]+) _        int
_     = \s*
""",
               int=int,
               **globals())

def test(x, loud=True):
    try:
        expr, = parse(x)
        state = run(expr)
        result = trampoline(state)
    except Unparsable, exc:
        result = exc
    return result

## test(r'+ 2 3')
#. : add
#. RandK(2,...)
#. RandK(3,...)
#. k (RandK(2,...), (RandK(3,...), None))
#. : 2
#. CallK(add)
#. RandK(3,...)
#. k (CallK(add), (RandK(3,...), None))
#. : (add 2)
#. RandK(3,...)
#. k (RandK(3,...), None)
#. : 3
#. CallK((add 2))
#. k (CallK((add 2)), None)
#. 5
