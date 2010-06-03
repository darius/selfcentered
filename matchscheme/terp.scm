;; Scheme subset except with LETREC specified more tightly, and with
;; a procedural representation of booleans.

;; Core syntax:
;; e = v
;;   | (QUOTE constant)
;;   | (LAMBDA (v ...) e)
;;   | (LETREC ((v e) ...) e)
;;   | (e e ...)

(define (interpret e)
  (evaluate (elaborate e) the-global-env))

(define (elaborate e)
  (cond ((symbol? e) e)
        ((self-evaluating? e) (list 'quote e))
        ((string? e)
         (elaborate `(quote ,(map char->integer (string->list e)))))
        (else
         (assert (pair? e) "Bad syntax" e)
         (case (car e)
           ((quote)
            e)
           ((begin)
            (elaborate-seq (cdr e)))
           ((lambda)
            `(lambda ,(cadr e) ,(elaborate-seq (cddr e))))
           ((letrec)
            `(letrec ,(map (lambda (defn)
                             (list (car defn) (elaborate (cadr defn))))
                           (cadr e))
               ,(elaborate-seq (cddr e))))
           ((local)
            (elaborate
             `(letrec ,(map (lambda (defn)
                              ;; (define (caadr . cdadr) . cddr)
                              `(,(caadr defn)
                                (lambda ,(cdadr defn) ,@(cddr defn))))
                           (cadr e))
               ,@(cddr e))))
           ((let)
            (elaborate `((lambda ,(map car (cadr e))
                           ,@(cddr e))
                         ,@(map cadr (cadr e)))))
           ((if)
            (let ((test (cadr e)) (if-true (caddr e)) (if-false (cadddr e)))
              (elaborate `((%true? ,test) (lambda () ,if-true)
                                          (lambda () ,if-false)))))
           ((case)
            (let ((test (cadr e))
                  (clauses (cddr e))
                  (v '__subject))       ;XXX gensym
              (elaborate `(let ((,v ,test))
                            ,(expand-case-clauses v clauses)))))
           ;; ...
           (else
            (map elaborate e))))))

(define (expand-case-clauses v clauses) ;XXX incomplete
  (if (null? clauses)
      ''#f
      (let ((constant (caar clauses))
            (body (cdar clauses))
            (rest (cdr clauses)))
        `(if (eq? ,v ',constant)
             (begin ,@body)
             ,(expand-case-clauses v rest)))))

(define (self-evaluating? x)
  (or (boolean? x)
      (integer? x)))

(define (elaborate-seq es)
  (make-begin (map elaborate es)))

(define (make-begin es)
  (cond ((null? es) ''#f)
        ((null? (cdr es)) (car es))
        (else (make-begin2 (car es)
                           (make-begin (cdr es))))))

(define (make-begin2 e1 e2)
  `((lambda (v thunk) (thunk))
    ,e1
    (lambda () ,e2)))


(define (assert ok? plaint culprit)
  (if (not ok?)
      (error plaint culprit)
      'fuck-off-mzscheme))

(define (should= x expected)
  (assert (equal? x expected) "Expected" expected))

(should= (elaborate '42)
         ''42)
(should= (elaborate '#f)
         ''#f)
(should= (elaborate ''())
         ''())
(should= (elaborate '(begin (write x) (lambda (y) (+ x y))))
         '((lambda (v thunk) (thunk))
           (write x)
           (lambda () (lambda (y) (+ x y)))))
(should= (elaborate '(begin (if x y z)))
         '((%true? x) (lambda () y) (lambda () z)))
(should= (elaborate '(lambda ()
                       (local ((define (for-each f xs)
                                 (if (null? xs)
                                     '()
                                     (begin (f (car xs))
                                            (for-each f (cdr xs))))))
                         for-each)))
         '(lambda ()
            (letrec ((for-each
                      (lambda (f xs)
                        ((%true? (null? xs))
                         (lambda () '())
                         (lambda ()
                           ((lambda (v thunk) (thunk))
                            (f (car xs))
                            (lambda () (for-each f (cdr xs)))))))))
              for-each)))


(define (evaluate e r)
  (if (symbol? e)
      (env-lookup r e)
      (case (car e)
        ((quote)
         ;; (ELABORATE could do the wrap, but that'd make elaborated
         ;; expressions less readable.)
         (wrap (cadr e)))
        ((lambda)
         (make-object (lambda (r . arguments)
                        (evaluate (caddr e)
                                  (env-extend r (cadr e) arguments)))
                      r))
        ((letrec)
         ;; Specified to evaluate left-to-right with a definite
         ;; 'uninitialized' value.
         (let ((new-r (env-extend-promises r (map car (cadr e)))))
           (for-each (lambda (defn)
                       (env-resolve! new-r
                                     (car defn)
                                     (evaluate (cadr defn) new-r)))
                     (cadr e))
           (evaluate (caddr e) new-r)))
        (else
         (call (evaluate (car e) r)
               (map (lambda (operand) (evaluate operand r))
                    (cdr e)))))))

(define (env-lookup r v)
  (cond ((assq v r) => cadr)
        (else (error "Unbound variable" v))))

(define (env-extend r vs values)
  (append (map list vs values) r))

(define (env-extend-promises r vs)
  (env-extend r vs (map (lambda (_) uninitialized) vs)))

(define (env-resolve! r v value)
  (cond ((assq v r) => (lambda (pair)
                         (assert (eq? (cadr pair) uninitialized) "WTF?" pair)
                         (set-car! (cdr pair) value)))
        (else (error "Can't happen" v))))

(define (make-object script datum)
  (list 'object script datum))

(define object.script cadr)
(define object.datum caddr)

(define (call object arguments)
  (assert (eq? (car object) 'object) "Non-object" object)
  (apply (object.script object) (object.datum object) arguments))

(define (wrap x)
  ;; TODO: we also need some I/O primitives
  (cond ((boolean? x) (make-object boolean-script x))
        ((integer? x) (make-object fixnum-script x))
        ((symbol? x) (make-object symbol-script x))
        ((null? x) (make-object nil-script '()))
        ((pair? x) (make-object pair-script (cons (wrap (car x))
                                                  (wrap (cdr x)))))
        ((vector? x) (make-object vector-script (vector-map wrap x)))
        (else (error "Unknown primitive type" x))))

(define (vector-map f vec)
  (list->vector (map f (vector->list vec))))

(define (unwrap script x)
  (if (is-a? script x)
      (object.datum x)
      (error "Bad argument type" x)))

(define (is-a? script x)
  (eq? (object.script x) script))

(define (make-primitive-script)
  (lambda (me . arguments)
    (error "Can't call a primitive" me)))

(define uninitialized (make-object (make-primitive-script)
                                   '*uninitialized*))


(define (boolean-script me if-true if-false)
  (call (if me if-true if-false) '()))

(define fixnum-script (make-primitive-script))
(define symbol-script (make-primitive-script))
(define nil-script    (make-primitive-script))
(define pair-script   (make-primitive-script))
;; NB for a compiler all we badly need is a mutable box type, not full vectors
(define vector-script (make-primitive-script))

(define prim-%true?
  (make-object (lambda (me x)
                 (wrap (not (and (is-a? boolean-script x)
                                 (eq? (object.datum x) #f)))))
               '<cons>))

(define prim-cons
  (make-object (lambda (me x y) (make-object pair-script (cons x y)))
               '<cons>))

(define prim-eq?
  (make-object (lambda (me x y)
                 (wrap (and (eq? (object.script x) (object.script y))
                            (eqv? (object.datum x) (object.datum y)))))
               '<eq?>))

(define prim-+
  (make-object (lambda (me x y)
                 (wrap (+ (unwrap fixnum-script x)
                          (unwrap fixnum-script y))))
               '<+>))

(define prim--
  (make-object (lambda (me x y)
                 (wrap (- (unwrap fixnum-script x)
                          (unwrap fixnum-script y))))
               '<->))

(define prim-*
  (make-object (lambda (me x y)
                 (wrap (* (unwrap fixnum-script x)
                          (unwrap fixnum-script y))))
               '<*>))

(define prim-car
  (make-object (lambda (me x)
                 (car (unwrap pair-script x)))
               '<car>))

(define prim-cdr
  (make-object (lambda (me x)
                 (cdr (unwrap pair-script x)))
               '<cdr>))

(define prim-symbol?
  (make-object (lambda (me x)
                 (wrap (is-a? symbol-script x)))
               '<car>))

(define the-global-env
  `((%true? ,prim-%true?)
    (cons ,prim-cons)
    (eq? ,prim-eq?)
    (+ ,prim-+)
    (- ,prim--)
    (* ,prim-*)
    (car ,prim-car)
    (cdr ,prim-cdr)
    (symbol? ,prim-symbol?)
    ))


(should= (interpret '42)
         (wrap 42))
(should= (interpret 'cons)
         prim-cons)
(should= (interpret '(- 5 3))
         (wrap 2))
(should= (interpret '(car (cdr '(hello world))))
         (wrap 'world))
(should= (interpret '((lambda (x) x)
                      '55))
         (wrap 55))
(should= (interpret '(let ((x (eq? 4 (+ 2 2))))
                       x))
         (wrap #t))
(should= (interpret '(local ((define (fact n)
                               (if (eq? n 0)
                                   1
                                   (* n (fact (- n 1))))))
                       (fact 5)))
         (wrap 120))
