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

loud = 0

def trampoline(state):
    k, value = state
    while k is not final_k:
        if loud: traceback((k, value))
        cont_step, k = k
        k, value = cont_step.step(value, k)
    return value

final_k = None

def traceback(state):
    k, value = state
    print ':', repr(value)
    while k:
        cont_step, k = k
        print repr(cont_step)


# Abstract syntax

class Const(object):
    def __init__(self, value):
        self.value = value
    def free_vars(self):
        return ()
    def eval(self, r, k):
        tracer.add('constant', self.value)
        return k, self.value
    def __repr__(self):
        return repr(self.value)

class Var(object):
    def __init__(self, name):
        self.name = name
    def free_vars(self):
        return (self.name,)
    def eval(self, (arg, closure), k):
        at = closure.lookup(self.name)
        if at == 'arg':
            tracer.add('arg')
            return k, arg
        else:
            tracer.add('closure_fetch', at)
            return k, closure.fetch(at)
    def __repr__(self):
        return self.name

def fetch_by_name((arg, closure), name):
    at = closure.lookup(name)
    return arg if at == 'arg' else closure.fetch(at)

class Lam(object):
    def __init__(self, param, body):
        self.param = param
        self.body = body
        self.fvs = tuple(set(body.free_vars()) - set([param]))
    def free_vars(self):
        return self.fvs
    def eval(self, r, k):
        tracer.add('enclose', self, *[tracer.fetch_by_name(r, name) for name in self.fvs])
        return k, Closure(self, tuple(fetch_by_name(r, v) for v in self.fvs))
    def __repr__(self):
        return '(%s -> %r)' % (self.param, self.body)
    def run(self, r, k):
        return self.body.eval(r, k)
    def lookup(self, name):
        if name == self.param:
            return 'arg'
        else:
            return self.fvs.index(name)

class Closure(object):
    def __init__(self, lam, values):
        self.lam = lam
        self.values = values
    def lookup(self, name):
        return self.lam.lookup(name)
    def fetch(self, at):
        return self.values[at]
    def call(self, arg, k):
        return self.lam.run((arg, self), k)
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
    def step(self, fn, k):
        return self.rand.eval(self.r, (CallK(fn), k))
    def __repr__(self):
        return 'RandK(%r,...)' % (self.rand,)

class CallK(object):
    def __init__(self, fn):
        self.fn = fn
    def step(self, arg, k):
        tracer.add('call')      # XXX
        return self.fn.call(arg, k)
    def __repr__(self):
        return 'CallK(%r)' % (self.fn)

class Loop(object):
    "Mark where to start trace recording."
    def __init__(self, expr):
        self.expr = expr
        self.compiled_code = None
    def free_vars(self):
        return self.expr.free_vars()
    def eval(self, r, k):
        if self.compiled_code:
            return run_compiled(self.compiled_code, r, k)
        if tracer.tracing:
            self.compiled_code = tracer.stop()
            if self.compiled_code:
                return run_compiled(self.compiled_code, r, k)
        tracer.start(k)
        return self.expr.eval(r, k)
    def __repr__(self):
        return '(LOOP %r)' % (self.expr)


# Primitive functions

class Primitive2(object):
    def __init__(self, fn):
        self.fn = fn
    def call(self, arg, k):
        tracer.add('partial2') # XXX
        return k, PartialPrimitive2(self.fn, arg)
    def __repr__(self):
        return self.fn.__name__
    
class PartialPrimitive2(object):
    def __init__(self, fn, arg1):
        self.fn = fn
        self.arg1 = arg1
    def call(self, arg, k):
        tracer.add('prim', self.fn.__name__) # XXX
        return k, self.fn(self.arg1, arg)
    def __repr__(self):
        return '(%s %r)' % (self.fn.__name__, self.arg1)

def yes(if_yes, if_no): return if_yes
def no(if_yes, if_no):  return if_no

yes_prim = Primitive2(yes)
no_prim  = Primitive2(no)

def equ(x, y): return yes_prim if x == y else no_prim

global_dict = {
    '+':   Primitive2(operator.add),
    '-':   Primitive2(operator.sub),
    '*':   Primitive2(operator.mul),
    '=':   Primitive2(equ),
    'yes': yes_prim,
    'no':  no_prim,
}

global_lam = Lam('', Var(''))
global_lam.fvs = tuple(global_dict.keys())
global_r = (None, Closure(global_lam, tuple(global_dict.values())))


# Trace recording

class Tracer(object):
    def __init__(self):
        self.tracing = False
    def start(self, k):
        self.tracing = True
        self.insns = []
        self.start_k = k
    def stop(self):
        self.tracing = False
        return None
    def add(self, *insn):
        if self.tracing:
            self.insns.append(insn)
            return len(self.insns) - 1
        return None
    def fetch_by_name(self, (arg, closure), name):
        at = closure.lookup(name)
        return self.add('arg') if at == 'arg' else self.add('closure_fetch', at)

tracer = Tracer()


# Entry point

def run(expr):
    tracer.tracing = False
    return expr.eval(global_r, final_k)


# Parsing

def fold_app(f, *fs): return reduce(App, fs, f)
def fold_lam(vp, e):  return foldr(Lam, e, vp)

def foldr(f, z, xs):
    for x in reversed(xs):
        z = f(x, z)
    return z

parse = Parser(r"""
start = _ e !.
e     = /LOOP\b/ _ e      Loop
      | f fs              fold_app
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
#. 5

## test(r'(\x.x)42')
#. 42
## test(r'((\x y.x y y) (+) 8)')
#. 16
## test(r'(\x . x) +')
#. add
## test(r'(\x y. x y y) = 5')
#. yes
## test(r'(= 5 6) 42 137')
#. 137
## test(r'(= 5 5) 42 137')
#. 42

Y    = r'\M. (\f. M (\a. f f a)) (\f. M (\a. f f a))'
fact = r"""Y (\fact n p. LOOP ((= n 0) (\_. p) (\_. fact (- n 1) (* p n)) 0)) 5 1"""
fancy_test = r'(\Y.%s)(%s)' % (fact, Y)
## fancy_test
#. '(\\Y.Y (\\fact n p. LOOP ((= n 0) (\\_. p) (\\_. fact (- n 1) (* p n)) 0)) 5 1)(\\M. (\\f. M (\\a. f f a)) (\\f. M (\\a. f f a)))'
## test(fancy_test)
#. 120
## for i, insn in enumerate(tracer.insns): print i, insn
#. 0 ('closure_fetch', 0)
#. 1 ('closure_fetch', 4)
#. 2 ('call',)
#. 3 ('partial2',)
#. 4 ('constant', 0)
#. 5 ('call',)
#. 6 ('prim', 'equ')
#. 7 ('arg',)
#. 8 ('enclose', (_ -> p), 7)
#. 9 ('call',)
#. 10 ('partial2',)
#. 11 ('arg',)
#. 12 ('closure_fetch', 1)
#. 13 ('closure_fetch', 2)
#. 14 ('closure_fetch', 3)
#. 15 ('closure_fetch', 4)
#. 16 ('enclose', (_ -> ((fact ((- n) 1)) ((* p) n))), 11, 12, 13, 14, 15)
#. 17 ('call',)
#. 18 ('prim', 'yes')
#. 19 ('constant', 0)
#. 20 ('call',)
#. 21 ('closure_fetch', 0)
