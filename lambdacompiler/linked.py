"""
Simple compiler using linked closures.
"""

import itertools
gen_label = itertools.count().next

def compile(e, r=[]):
    return '\n'.join(e.compile(r) + ['return;'])

class Var:
    def __init__(self, v):
        self.v = v
    def compile(self, r):
        return (['work = ep;']
                + (['work = heap[work] + 1;'] * r.index(self.v))
                + ['*++sp = heap[work];'])

class Lam:
    def __init__(self, v, e):
        self.v, self.e = v, e
    def compile(self, r):
        enter, skip = gen_label(), gen_label()
        return (['stack[++sp] = hp;',
                 'heap[hp+0] = (Word) &&enter_%d;' % enter,
                 'heap[hp+1] = ep;',
                 'hp += 2;',
                 'goto skip_%d;' % skip,
                 'enter_%d:' % enter]
                + compile_body(self.v, self.e, r)
                + ['skip_%d:' % skip])

def compile_body(v, e, r):
    return (['ep = heap[function+1];',
             'heap[hp+0] = argument;',
             'heap[hp+1] = ep;',
             'ep = hp;',
             'hp += 2;']
            + e.compile([v] + r)
            + ['work = stack[sp--];',
               'function = stack[sp--];', # Return address
               'ep = stack[sp];',
               'stack[sp] = work;',
               'goto *(void*) function;'])

class App:
    def __init__(self, e1, e2):
        self.e1, self.e2 = e1, e2
    def compile(self, r):
        return_label = gen_label()
        return (self.e1.compile(r)
                + self.e2.compile(r)
                + ['function = stack[sp];',
                   'argument = stack[sp-1];',
                   'stack[sp-1] = ep;',
                   'stack[sp] = (Word) &&R_%d;' % return_label,
                   'goto *(void*) heap[function];',
                   'R_%d:' % return_label])

x, y = Var('x'), Var('y')
r0 = ['x', 'y']

## r0.index('x')
#. 0

## print compile(x, r0)
#. work = ep;
#. *++sp = heap[work];
#. return;
#. 
## print compile(y, r0)
#. work = ep;
#. work = heap[work] + 1;
#. *++sp = heap[work];
#. return;
#. 

L = Lam('x', x)
## print compile(L)
#. stack[++sp] = hp;
#. heap[hp+0] = (Word) &&enter_0;
#. heap[hp+1] = ep;
#. hp += 2;
#. goto skip_1;
#. enter_0:
#. ep = heap[function+1];
#. heap[hp+0] = argument;
#. heap[hp+1] = ep;
#. ep = hp;
#. hp += 2;
#. work = ep;
#. *++sp = heap[work];
#. work = stack[sp--];
#. function = stack[sp--];
#. ep = stack[sp];
#. stack[sp] = work;
#. goto *(void*) function;
#. skip_1:
#. return;
#. 

## print compile(Lam('x', App(x, x)))
#. stack[++sp] = hp;
#. heap[hp+0] = (Word) &&enter_2;
#. heap[hp+1] = ep;
#. hp += 2;
#. goto skip_3;
#. enter_2:
#. ep = heap[function+1];
#. heap[hp+0] = argument;
#. heap[hp+1] = ep;
#. ep = hp;
#. hp += 2;
#. work = ep;
#. *++sp = heap[work];
#. work = ep;
#. *++sp = heap[work];
#. function = stack[sp];
#. argument = stack[sp-1];
#. stack[sp-1] = ep;
#. stack[sp] = (Word) &&R_4;
#. goto *(void*) heap[function];
#. R_4:
#. work = stack[sp--];
#. function = stack[sp--];
#. ep = stack[sp];
#. stack[sp] = work;
#. goto *(void*) function;
#. skip_3:
#. return;
#. 



f, g = map(Var, 'f g'.split())
compose = Lam('f', Lam('g', Lam('x', App(f, App(g, x)))))
## print compile(compose)
#. stack[++sp] = hp;
#. heap[hp+0] = (Word) &&enter_5;
#. heap[hp+1] = ep;
#. hp += 2;
#. goto skip_6;
#. enter_5:
#. ep = heap[function+1];
#. heap[hp+0] = argument;
#. heap[hp+1] = ep;
#. ep = hp;
#. hp += 2;
#. stack[++sp] = hp;
#. heap[hp+0] = (Word) &&enter_7;
#. heap[hp+1] = ep;
#. hp += 2;
#. goto skip_8;
#. enter_7:
#. ep = heap[function+1];
#. heap[hp+0] = argument;
#. heap[hp+1] = ep;
#. ep = hp;
#. hp += 2;
#. stack[++sp] = hp;
#. heap[hp+0] = (Word) &&enter_9;
#. heap[hp+1] = ep;
#. hp += 2;
#. goto skip_10;
#. enter_9:
#. ep = heap[function+1];
#. heap[hp+0] = argument;
#. heap[hp+1] = ep;
#. ep = hp;
#. hp += 2;
#. work = ep;
#. work = heap[work] + 1;
#. work = heap[work] + 1;
#. *++sp = heap[work];
#. work = ep;
#. work = heap[work] + 1;
#. *++sp = heap[work];
#. work = ep;
#. *++sp = heap[work];
#. function = stack[sp];
#. argument = stack[sp-1];
#. stack[sp-1] = ep;
#. stack[sp] = (Word) &&R_12;
#. goto *(void*) heap[function];
#. R_12:
#. function = stack[sp];
#. argument = stack[sp-1];
#. stack[sp-1] = ep;
#. stack[sp] = (Word) &&R_11;
#. goto *(void*) heap[function];
#. R_11:
#. work = stack[sp--];
#. function = stack[sp--];
#. ep = stack[sp];
#. stack[sp] = work;
#. goto *(void*) function;
#. skip_10:
#. work = stack[sp--];
#. function = stack[sp--];
#. ep = stack[sp];
#. stack[sp] = work;
#. goto *(void*) function;
#. skip_8:
#. work = stack[sp--];
#. function = stack[sp--];
#. ep = stack[sp];
#. stack[sp] = work;
#. goto *(void*) function;
#. skip_6:
#. return;
#. 

