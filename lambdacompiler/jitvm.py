"""
sketch of intermediate code representation for tracing jit of schemey.py
"""

(implicit slots to start:
0: arg
1: closure

maybe also:
2: no
3: yes
)

constant 42
fetch val, 3
allot lam, val, val, ..., val

prim2 name, val, val

(begin)
phi val, val
again val

insist val == expected, snapshot

so fact should turn into:

0: p
1: (..., n, ...)

2: fetch 1, (n)

(begin)
4: phi 2, 10       # n
5: phi 0, 11       # p

6: constant 0
7: prim2 '=', 4, 6   # = n 0
8: insist 4 == (no), ...
9: constant 1
10: prim2 '-', 4, 9  # - n 1
11: prim2 '*', 5, 4  # * p n
12: again 4


or how about unrolled once:

...
4: fetch 3, (n)
5: constant 0
6: prim2 '=', 4, 5
7: insist 4 == 0, ...
8: constant 1
9: prim2 '-', 4, 8
10: prim2 '*', 2, 4

11: phi 9, 15
12: phi 10, 16
13: prim2 '=', 11, 5
14: insist 13 == 0, ...
15: prim2 '-', 11, 8
16: prim2 '*', 12, 11
17: again 11
