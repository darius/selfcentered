"""
Compiler producing code using flat closures. 
Needs: pip install peglet

Probably ought to be revised with an Ocaml-style calling
convention (push/enter) because of the O(N**2) pain of
repeatedly building closures for N-argument functions.

Currently includes both an interpreter and a compiler,
and it'd be simpler with one of those stripped out.
The Zero/Add1 primitives should be replaced by an I/O
interface.
"""

from collections import namedtuple
import itertools
from peglet import Parser, Unparsable, hug

class Zero(object):
    def eval(self, env): return 0
    def free_vars(self): return frozenset()
    def compile(self, senv, k): return ['zero'] + k

class Add1(object):
    def eval(self, env): return lambda n: n+1
    def free_vars(self): return frozenset()
    def compile(self, senv, k):
        return ['defer', 0, 2, 'add1', 'restore'] + k

## test5(r'0')
#. 0
## test5(r'+1 0')
#. 1

## testcomp(r'0')
#. ['zero', 'halt']
#. 
#. 0L

## testcomp(r'+1 0')
#. ['save', 7, 'zero', 'defer', 0, 2, 'add1', 'restore', 'invoke', 'halt']
#. 
#. 1L

## testcomp(r'(\x.x)(+1 0)')
#. ['save', 15, 'save', 7, 'zero', 'defer', 0, 2, 'add1', 'restore', 'invoke', 'defer', 0, 2, 'arg', 'restore', 'invoke', 'halt']
#. 
#. 1L
## testcomp(r'(\f.f)(+1) 0')
#. ['save', 15, 'zero', 'save', 11, 'defer', 0, 2, 'add1', 'restore', 'defer', 0, 2, 'arg', 'restore', 'invoke', 'invoke', 'halt']
#. 
#. 1L

## testcomp(r'(\x y. x) (+1 0) 0')
#. ['save', 24, 'zero', 'save', 20, 'save', 7, 'zero', 'defer', 0, 2, 'add1', 'restore', 'invoke', 'defer', 0, 7, 'defer', 1, 2, 'arg', 1, 'restore', 'restore', 'invoke', 'invoke', 'halt']
#. 
#. 1L
## testcomp(r'(\x y. y) (+1 0) 0')
#. ['save', 23, 'zero', 'save', 19, 'save', 7, 'zero', 'defer', 0, 2, 'add1', 'restore', 'invoke', 'defer', 0, 6, 'defer', 0, 2, 'arg', 'restore', 'restore', 'invoke', 'invoke', 'halt']
#. 
#. 0L

## testcomp(r'(\f x. x) +1 0')
#. ['save', 19, 'zero', 'save', 15, 'defer', 0, 2, 'add1', 'restore', 'defer', 0, 6, 'defer', 0, 2, 'arg', 'restore', 'restore', 'invoke', 'invoke', 'halt']
#. 
#. 0L

## testcomp(r'(\f x. f x) (+1) 0')
#. ['save', 21, 'zero', 'save', 17, 'defer', 0, 2, 'add1', 'restore', 'defer', 0, 8, 'defer', 1, 3, 'arg', 'arg', 1, 'invoke', 'restore', 'invoke', 'invoke', 'halt']
#. 
#. 1L

## one   = r'\f x. f x'
## two   = r'\f x. f (f x)'
## add   = r'\m n f x. n f (m f x)'
## add1  = r'\n f x. f (n f x)'
## mul   = r'\m n f. n (m f)'
## pow   = r'\m n. n m'
## blarg = r'(%s) ((%s) (%s) (%s)) (%s)  +1 0' % (mul, add, one, two, two)
### blarg = r'(%s) +1 0' % (one,)
## print blarg,
#. (\m n f. n (m f)) ((\m n f x. n f (m f x)) (\f x. f x) (\f x. f (f x))) (\f x. f (f x))  +1 0
## testcomp(blarg)
#. ['save', 120, 'zero', 'save', 116, 'defer', 0, 2, 'add1', 'restore', 'save', 108, 'defer', 0, 12, 'defer', 1, 7, 'arg', 'save', 3, 'arg', 1, 'invoke', 1, 'invoke', 'restore', 'save', 90, 'save', 66, 'defer', 0, 12, 'defer', 1, 7, 'arg', 'save', 3, 'arg', 1, 'invoke', 1, 'invoke', 'restore', 'save', 48, 'defer', 0, 8, 'defer', 1, 3, 'arg', 'arg', 1, 'invoke', 'restore', 'defer', 0, 33, 'defer', 1, 28, 'arg', 'defer', 2, 22, 1, 'arg', 'defer', 3, 15, 'arg', 1, 2, 'save', 7, 'arg', 'save', 3, 1, 2, 'invoke', 'invoke', 'save', 3, 1, 3, 'invoke', 'invoke', 'restore', 'restore', 'restore', 'invoke', 'invoke', 'defer', 0, 18, 'defer', 1, 13, 'arg', 'defer', 2, 7, 1, 'arg', 'save', 3, 'arg', 1, 'invoke', 2, 'invoke', 'restore', 'restore', 'invoke', 'invoke', 'invoke', 'invoke', 'halt']
#. 
#. 6L

## testcomp(r'$Y (\fact n. ($is_zero n (\_. $one) (\_. ($mul n (fact ($sub1 n))))) n) ($add $two $two) +1 0')
#. ['save', 323, 'zero', 'save', 319, 'defer', 0, 2, 'add1', 'restore', 'save', 311, 'save', 70, 'defer', 0, 12, 'defer', 1, 7, 'arg', 'save', 3, 'arg', 1, 'invoke', 1, 'invoke', 'restore', 'save', 52, 'defer', 0, 12, 'defer', 1, 7, 'arg', 'save', 3, 'arg', 1, 'invoke', 1, 'invoke', 'restore', 'defer', 0, 33, 'defer', 1, 28, 'arg', 'defer', 2, 22, 1, 'arg', 'defer', 3, 15, 'arg', 1, 2, 'save', 7, 'arg', 'save', 3, 1, 2, 'invoke', 'invoke', 'save', 3, 1, 3, 'invoke', 'invoke', 'restore', 'restore', 'restore', 'invoke', 'invoke', 'save', 236, 'defer', 0, 194, 'defer', 1, 189, 'arg', 'arg', 'save', 185, 'defer', 2, 126, 1, 'arg', 'save', 98, 'save', 94, 2, 'defer', 0, 89, 'defer', 1, 84, 'arg', 'defer', 2, 78, 1, 'arg', 'defer', 0, 6, 'defer', 0, 2, 'arg', 'restore', 'restore', 'save', 66, 'defer', 1, 11, 'arg', 1, 'save', 7, 'defer', 0, 2, 'arg', 'restore', 'arg', 'invoke', 'invoke', 'save', 48, 'defer', 1, 42, 2, 'defer', 2, 36, 'arg', 1, 'save', 28, 'save', 11, 'defer', 0, 6, 'defer', 0, 2, 'arg', 'restore', 'restore', 1, 'invoke', 'save', 12, 'defer', 0, 7, 'defer', 1, 2, 'arg', 1, 'restore', 'restore', 1, 'invoke', 'invoke', 'save', 3, 2, 'arg', 'invoke', 'invoke', 'restore', 1, 'invoke', 'invoke', 'invoke', 'restore', 'restore', 'invoke', 1, 'invoke', 'save', 23, 2, 'defer', 0, 18, 'defer', 1, 13, 'arg', 'defer', 2, 7, 1, 'arg', 'save', 3, 'arg', 1, 'invoke', 2, 'invoke', 'restore', 'restore', 'invoke', 'invoke', 'save', 51, 'defer', 0, 12, 'defer', 0, 8, 'defer', 1, 3, 'arg', 'arg', 1, 'invoke', 'restore', 'restore', 'save', 33, 'arg', 'defer', 0, 28, 'defer', 0, 7, 'defer', 1, 2, 'arg', 1, 'restore', 'restore', 'save', 15, 'defer', 0, 10, 'defer', 0, 6, 'defer', 0, 2, 'arg', 'restore', 'restore', 'restore', 'arg', 'invoke', 'invoke', 'invoke', 'invoke', 'invoke', 'invoke', 'restore', 'defer', 0, 35, 'defer', 1, 13, 'arg', 'defer', 1, 7, 'arg', 'arg', 'save', 3, 1, 1, 'invoke', 'invoke', 1, 'invoke', 'defer', 1, 13, 'arg', 'defer', 1, 7, 'arg', 'arg', 'save', 3, 1, 1, 'invoke', 'invoke', 1, 'invoke', 'invoke', 'invoke', 'invoke', 'invoke', 'invoke', 'halt']
#. 
#. 24L

def run(insns):
    pc = 0
    stack = []
    closure, arg = '*', '*'
    while True:
        insn = insns[pc]
#        xxx = ' '.join(map(str, insns[pc:pc+2]))
#        print '%-63s %2d %s' % ('a:%-3s c:%-10s %r' % (arg, closure, stack), pc, xxx)
        if 'halt' == insn:
            assert 1 == len(stack)
            return stack[-1]
        elif 'restore' == insn:
            closure, arg, pc = stack[-4:-1]; del stack[-4:-1]
        elif 'save' == insn:
            ninsns = insns[pc+1]; pc += 2
            stack += [closure, arg, pc + ninsns]
        elif 'invoke' == insn:
            closure, arg = stack.pop(), stack.pop()
            pc = closure[0]
        elif 'defer' == insn:
            nvars, ninsns = insns[pc+1:pc+3]; pc += 3
            stack.append([pc + nvars]
                         + [arg if 'arg' == insn else closure[insn]
                            for insn in insns[pc:pc+nvars]])
            pc += nvars + ninsns
        elif 'zero' == insn:
            stack.append(0L)
            pc += 1
        elif 'add1' == insn:
            assert isinstance(arg, long)
            stack.append(arg + 1)
            pc += 1
        elif 'arg' == insn:
            stack.append(arg)
            pc += 1
        else:
            stack.append(closure[insn])
            pc += 1

class Var(namedtuple('_Var', 'name')):
    def eval(self, env):
        return env[self.name]
    def free_vars(self):
        return frozenset([self.name])
    def compile(self, senv, k):
        return [senv[self.name]] + k

_Lam = namedtuple('_Lam', 'name body')
class Lam(_Lam):
    def __init__(self, name, body):
        self.fvs = body.free_vars() - frozenset([name])
    def eval(self, env):
        return lambda arg: self.body.eval(extend(env, self.name, arg))
    def free_vars(self):
        return self.fvs
    def compile(self, senv, k):
        code = self.body.compile(make_static_env(self.name, self.fvs), ['restore'])
        return (['defer', len(self.fvs), len(code)]
                + [senv[v] for v in self.fvs]
                + code + k)

class App(namedtuple('_App', 'rator rand')):
    def eval(self, env):
        return self.rator.eval(env)(self.rand.eval(env))
    def free_vars(self):
        return self.rator.free_vars() | self.rand.free_vars()
    def compile(self, senv, k):
        code = self.rand.compile(senv, self.rator.compile(senv, ['invoke']))
        if k == ['restore']: return code
        else: return ['save', len(code)] + code + k

def compile_expr(expr): # XXX what about genv?
    return expr.compile({}, ['halt'])

def make_static_env(name, fvs):
    result = {name: 'arg'}
    result.update(dict(zip(fvs, itertools.count(1))))
    return result

def extend(env, name, value):
    result = dict(env)
    result[name] = value
    return result

## compile_expr(parse(r'\x . x (\y. y x)')[0])
#. ['defer', 0, 9, 'defer', 1, 3, 'arg', 1, 'arg', 'invoke', 'arg', 'invoke', 'halt']

def fold_app(f, *fs):  return reduce(App, fs, f)
def fold_lam(vp, e):  return foldr(Lam, e, vp)

def foldr(f, z, xs):
    for x in reversed(xs):
        z = f(x, z)
    return z

genv = {}
global_ref = genv.__getitem__

parse = Parser(r"""
start = _ e !.
e     = f fs              fold_app
fs    = f fs
      |     
f     = 0\b _             Zero
      | [+]1\b _          Add1
      | [$] v             global_ref
      | v                 Var
      | \\ _ vp [.] _ e   fold_lam
      | [(] _ e [)] _
vp    = v vs              hug
vs    = v vs
      |
v     = ([A-Za-z_]\w*)\b _
_     = \s*
""",
                        **globals())

def test3(x):
    try:
        result, = parse(x)
    except Unparsable, e:
        result = e
    return result

def test4(x):
    try:
        expr, = parse(x)
        result = expr.eval(dict(z=0, s=lambda n: n+1))
    except Unparsable, exc:
        result = exc
    except KeyError, exc:
        result = exc
    return result

def test5(x):
    try:
        expr, = parse(x)
        result = expr.eval(genv)
    except Unparsable, exc:
        result = exc
    except KeyError, exc:
        result = exc
    return result

def testcomp(x):
    try:
        expr, = parse(x)
        code = compile_expr(expr)
        print code
        result = run(code)
    except Unparsable, exc:
        result = exc
    return result

for k, v in dict(I     = r'\x. x',
                 K     = r'\c x. c',
                 S     = r'\f g x. f x (g x)',
                 B     = r'\f g x. f (g x)',
                 zero  = r'\f x. x',
                 one   = r'\f x. f x',
                 two   = r'\f x. f (f x)',
                 add1  = r'\n f x. f (n f x)',
                 add   = r'\m n f x. n f (m f x)',
                 mul   = r'\m n f. n (m f)',
                 pow   = r'\m n. n m',
                 false = r'\t f. f',
                 true  = r'\t f. t',
                 and_  = r'\p q. p q p',
                 or_   = r'\p q. p p q',
                 Y     = r'\M. (\f. M (\a. f f a)) (\f. M (\a. f f a))',
                 sub1  = r'\n f x. (n (\p k. k f ((p (\a b. a)) (p (\a b. b)))) (\k. k (\s.s) x)) (\a b. b)',
                 is_zero = r'\n. n (\x t f. f) (\t f. t)',
                 sub   = r'\m n. n $sub1 m',
                 ).items():
    genv[k] = parse(v)[0]
            

## test5(r'$is_zero $zero (+1 0) 0')
#. 1
## test5(r'$is_zero $two (+1 0) 0')
#. 0

## test5(r'$Y (\fact n. ($is_zero n (\_. $one) (\_. ($mul n (fact ($sub1 n))))) n) ($add $two $two) +1 0')
#. 24

## test5(r'+1 0')
#. 1
## test5(r'$false (+1 0) 0')
#. 0
## test5(r'$true  (+1 0) 0')
#. 1
## test5(r'$zero +1 0')
#. 0
## test5(r'$one +1 0')
#. 1
## test5(r'$add1 $two +1 0')
#. 3
## test5(r'$pow $two ($add1 $two) +1 0')
#. 8
## test5(r'$add $zero $zero +1 0')
#. 0
## test5(r'$add $zero  $one +1 0')
#. 1
## test5(r'$add $one  $zero +1 0')
#. 1
## test5(r'$add $one  $one  +1 0')
#. 2
## test5(r'$add $two  $one  +1 0')
#. 3
## test5(r'$add $two  $two  +1 0')
#. 4
## test5(r'$add ($add1 $two) $two  +1 0')
#. 5
## test5(r'$mul ($add $one $two) $two  +1 0')
#. 6

## test5(r'$sub1 $two +1 0')
#. 1
## test5(r'$sub1 $one +1 0')
#. 0
## test5(r'$sub1 $zero +1 0')
#. 0
## test5(r'$sub1 ($pow $two ($add1 $two)) +1 0')
#. 7

## test4(r'x')
#. KeyError('x',)
## test4(r'\x.x')(42)
#. 42
## test4(r'(\x.x)0')
#. 0
## test4(r'0')
#. 0
## test4(r'+1 0')
#. 1
## test4(r's (+1 0)')
#. 2
## test4(r'(\add one zero . add one zero +1 0) (\m n f x. n f (m f x)) (\f x. f x) (\f x. x)')
#. 1

## test3(r'x')
#. _Var(name='x')
## test3(r'\x.x')
#. _Lam(name='x', body=_Var(name='x'))
## test3(r'(x x)')
#. _App(rator=_Var(name='x'), rand=_Var(name='x'))

## test3(r'hello')
#. _Var(name='hello')
## test3(r' x')
#. _Var(name='x')
## test3(r'\x . y  ')
#. _Lam(name='x', body=_Var(name='y'))
## test3(r'((hello world))')
#. _App(rator=_Var(name='hello'), rand=_Var(name='world'))

## test3(r'  hello ')
#. _Var(name='hello')
## test3(r'hello     there hi')
#. _App(rator=_App(rator=_Var(name='hello'), rand=_Var(name='there')), rand=_Var(name='hi'))
## test3(r'a b c d e')
#. _App(rator=_App(rator=_App(rator=_App(rator=_Var(name='a'), rand=_Var(name='b')), rand=_Var(name='c')), rand=_Var(name='d')), rand=_Var(name='e'))

## test3(r'')
#. Unparsable('start', '', '')
## test3(r'x x . y')
#. Unparsable('start', 'x x ', '. y')
## test3(r'\.x')
#. Unparsable('start', '\\', '.x')
## test3(r'(when (in the)')
#. Unparsable('start', '(when (in the)', '')
## test3(r'((when (in the)))')
#. _App(rator=_Var(name='when'), rand=_App(rator=_Var(name='in'), rand=_Var(name='the')))

## test3(r'\a.a')
#. _Lam(name='a', body=_Var(name='a'))

## test3(r'  \hello . (hello)x \t')
#. Unparsable('start', '  \\hello . (hello)x \\t', '')

## test3(r'\M . (\f . M (f f)) (\f . M (f f))')
#. _Lam(name='M', body=_App(rator=_Lam(name='f', body=_App(rator=_Var(name='M'), rand=_App(rator=_Var(name='f'), rand=_Var(name='f')))), rand=_Lam(name='f', body=_App(rator=_Var(name='M'), rand=_App(rator=_Var(name='f'), rand=_Var(name='f'))))))

## test3(r'\a b.a')
#. _Lam(name='a', body=_Lam(name='b', body=_Var(name='a')))

## test3(r'\a b c . a b')
#. _Lam(name='a', body=_Lam(name='b', body=_Lam(name='c', body=_App(rator=_Var(name='a'), rand=_Var(name='b')))))
