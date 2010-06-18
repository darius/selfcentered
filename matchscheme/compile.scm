;; 'Bytecode' compiler -- currently completely non-optimizing,
;; unless you count tail recursion and open-coding primitives.
;; Uses linked environments.

(local

 ((define (compile e)
    (evaluate (elaborate e) '() '((halt))))

  (define (evaluate e r k)
    (mcase e
      ((: v symbol?)
       (cons (env-lookup r v) k))
      (('quote datum)
       `((quote ,datum) . ,k))
      (('lambda vs body)
       (let ((code `((check-nargs ,(length vs))
                     . ,(evaluate body (env-extend r vs) '((return))))))
         `((enclose ,(length code)) . ,(append code k))))
      (('letrec defns body)
       (let ((new-r (env-extend r (map car defns))))
         `((fix ,(length defns))
           . ,(append
               (flatten (map (mlambda
                              ((v1 e1)
                               (evaluate e1 new-r
                                         (mcase (env-lookup new-r v1)
                                           (('ref '0 offset) `((fixup ,offset)))))))
                             defns))
               (evaluate body new-r k)))))
      ((operator . operands)
       (let ((rands-code
              (flatten (map (lambda (operand) (evaluate operand r '()))
                            operands))))
         (mcase (evaluate operator r `((jump ,(length operands))))
           ((('prim-ref prim) _)
            (if (not (eqv? (length operands) (lookup prim primitives)))
                (error '"Wrong # of arguments to primitive" prim))
            (append rands-code `((prim-op ,prim) . ,k)))
           (op-code
            (mcase k
              ((('return)) (append rands-code op-code))
              (_ `((push-cont ,(+ (length rands-code) (length op-code)))
                   . ,(append rands-code (append op-code k)))))))))))

  (define (env-extend r vs)
    (cons vs r))

  (define (env-lookup r v)
    (local ((define (searching r frame-index)
              (mcase r
                ('() (if (assv v primitives)
                         `(prim-ref ,v)
                         (error '"Unbound variable" v)))
                ((vs . parent)
                 (local ((define (looking vs i)
                           (mcase vs
                             ('() (searching parent (+ frame-index 1)))
                             ((v1 . rest-vs) (if (eqv? v v1)
                                                 `(ref ,frame-index ,i)
                                                 (looking rest-vs (+ i 1)))))))
                   (looking vs 0))))))
      (searching r 0)))

  (define primitives
    `((uninitialized #f)
      (%unless     3)
      (boolean?    1)
      (number?     1)
      (pair?       1)
      (symbol?     1)
      (eqv?        2)
      (+           2)
      (-           2)
      (*           2)
      (<           2)
      (cons        2)
      (car         1)
      (cdr         1)
      (make-vector 2)
      (vector-ref  2)
      (vector-set! 3)
      ;;XXX for now:
      (error       2)
      (gensym      0)
      (read        0)
      (write       1)
      (newline     0)))

  (define (elaborate e)
    (cond ((symbol? e) e)
          ((or (boolean? e) (number? e)) `',e)
          ((not (pair? e)) (error '"Bad syntax" e))
          ((lookup (car e) macros)
           => (lambda (expand) (elaborate (expand e))))
          ((lookup (car e) core-syntax)
           => (lambda (expand) (expand e)))
          (else (map elaborate e))))

  (define (lookup key a-list)
    (cond ((assv key a-list) => cadr)
          (else #f)))

  (define core-syntax
    `((quote ,(mlambda ((_ datum) `',datum)))
      (lambda ,(mlambda ((_ vars . body)
                         `(lambda ,vars ,(elaborate-seq body)))))
      (letrec ,(mlambda ((_ defns . body)
                         `(letrec ,(map (mlambda ((v e) `(,v ,(elaborate e))))
                                        defns)
                            ,(elaborate-seq body)))))
      (begin ,(mlambda ((_ . es)
                        (elaborate-seq es))))))

  (define (elaborate-seq es)
    (mcase es
      ('() ''#f)
      ((e) (elaborate e))
      ((e . es) `((lambda (,(gensym)) ,(elaborate-seq es))
                  ,(elaborate e)))))

  (define macros 
    (cons 
     ;; This awkward line to avoid a nested quasiquote in the quasiquoted main
     ;; macros table just below:
     (cons 'quasiquote (cons (mlambda ((_ q) (expand-quasiquote q)))
                             '()))
     `((local ,(mlambda
                ((_ defns . body)
                 `(letrec ,(map (mlambda
                                 ((_ (: name symbol?) e)
                                  `(,name ,e))
                                 ((_ (name . vars) . body)
                                  `(,name (lambda ,vars . ,body))))
                                defns)
                    . ,body))))
       (let ,(mlambda
              ((_ bindings . body)
               `((lambda ,(map car bindings)
                   . ,body)
                 . ,(map cadr bindings)))))
       (if ,(mlambda
             ((_ test if-true)
              `(%unless ,test (lambda () #f) (lambda () ,if-true)))
             ((_ test if-true if-false)
              `(%unless ,test (lambda () ,if-false) (lambda () ,if-true)))))
       (cond ,(mlambda
               ((_) #f)
               ((_ ('else . es)) `(begin . ,es))
               ((_ (e) . clauses) `(or ,e (cond . ,clauses)))
               ((_ (e1 '=> e2) . clauses)
                (let ((test-var (gensym)))
                  `(let ((,test-var ,e1))
                     (if ,test-var
                         (,e2 ,test-var)
                         (cond . ,clauses)))))
               ((_ (e . es) . clauses)
                `(if ,e (begin . ,es) (cond . ,clauses)))))
       (or ,(mlambda
             ((_) #f)
             ((_ e) e)
             ((_ e . es)
              `(let ((v ,e) (thunk (lambda () . ,es)))
                 (%unless v thunk (lambda () v))))))
       (mlambda ,(mlambda
                  ((_ . clauses)
                   (let ((subject (gensym)))
                     `(lambda (,subject) ,(expand-mlambda subject clauses))))))
       (mcase ,(mlambda
                ((_ subject-exp . clauses)
                 `((mlambda . ,clauses) ,subject-exp)))))))

  (define (expand-quasiquote e)
    (mcase e
      (('unquote e1) e1)
      ((qcar . qcdr) `(cons ,(expand-quasiquote qcar)
                            ,(expand-quasiquote qcdr)))
      (else `',e)))

  (define (expand-mlambda subject clauses)
    (local
     ((define (expand-clause clause else-exp)
        (let ((pattern (car clause))
              (then-exp `(begin . ,(cdr clause)))
              (fail (gensym)))
          `(let ((,fail (lambda () ,else-exp)))
             ,(expand-pattern pattern then-exp `(,fail)))))

      (define (expand-pattern pattern then-exp else-exp)
        (let ((test-constant
               (lambda (constant)
                 `(if (eqv? ,subject ',constant) ,then-exp ,else-exp))))
          (mcase pattern
            ('_ then-exp)
            (('quote constant) (test-constant constant))
            ((: v symbol?) `(let ((,pattern ,subject)) ,then-exp))
            ((': name predicate)
             `(if (,predicate ,subject)
                  (let ((,name ,subject)) ,then-exp)
                  ,else-exp))
            ((pcar . pcdr)
             `(if (pair? ,subject)
                  (mcase (car ,subject)
                    (,pcar (mcase (cdr ,subject)
                             (,pcdr ,then-exp)
                             (_ ,else-exp)))
                    (_ ,else-exp))
                  ,else-exp))
            (_ (test-constant pattern))))))

     (foldr expand-clause '(%match-error) clauses)))

  (define (caar x) (car (car x)))
  (define (cadr x) (car (cdr x)))

  (define (null? x) (eqv? x '()))
  (define (not x)   (eqv? x #f))

  (define (length xs)
    (foldr (lambda (x n) (+ n 1)) 0 xs))

  (define (flatten lists)
    (foldr append '() lists))

  (define (append xs ys)
    (foldr cons ys xs))

  (define (foldr f z xs)
    (mcase xs
      ('() z)
      ((x . xs) (f x (foldr f z xs)))))

  (define (map f xs)
    (foldr (lambda (x ys) (cons (f x) ys))
           '()
           xs))

  (define (for-each f xs)
    (cond ((not (null? xs))
           (f (car xs))
           (for-each f (cdr xs)))))

  (define (assv key pairs)
    (cond ((null? pairs) #f)
          ((eqv? key (caar pairs)) (car pairs))
          (else (assv key (cdr pairs)))))

  )

(write (compile (read)))
(newline))
