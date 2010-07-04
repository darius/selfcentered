;; terp.scm, metacircular edition

(local

 ((define (interpret e)
    (evaluate (elaborate e) (the-global-env)))

  (define (evaluate e r)
    (mcase e
      ((: v symbol?)
       (env-lookup r v))
      (('quote datum)
       datum)
      (('lambda vs body)
       (lambda (arguments)
         (evaluate body (env-extend r vs arguments))))
      (('letrec defns body)
       (let ((new-r (env-extend-promises r (map car defns))))
         (for-each (mlambda ((v1 e1)
                             (env-resolve! new-r v1 (evaluate e1 new-r))))
                   defns)
         (evaluate body new-r)))
      ((operator . operands)
       ((evaluate operator r)
        (map (lambda (operand) (evaluate operand r))
             operands)))))

  (define (env-extend r vs values)
    (if (null? vs) 
        r
        (cons (cons (car vs) (make-box (car values)))
              (env-extend r (cdr vs) (cdr values)))))

  (define (env-extend-promises r vs)
    (env-extend r vs (map (lambda (_) uninitialized) vs)))

  (define (env-lookup r v)
    (box-get (cdr (assv v r))))

  (define (env-resolve! r v value)
    (box-set! (cdr (assv v r)) value))

  (define (make-box value)     (make-vector 1 value))
  (define (box-get box)        (vector-ref box 0))
  (define (box-set! box value) (vector-set! box 0 value))

  (define (the-global-env)
    (map (mlambda ((name value) (cons name (make-box value))))
         `((uninitialized ,uninitialized)
           (%unless     ,(mlambda ((x y z) ((if x z y) '()))))
           (boolean?    ,(mlambda ((x) (boolean? x))))
           (number?     ,(mlambda ((x) (number? x))))
           (pair?       ,(mlambda ((x) (pair? x))))
           (symbol?     ,(mlambda ((x) (symbol? x))))
           (eqv?        ,(mlambda ((x y) (eqv? x y))))
           (+           ,(mlambda ((x y) (+ x y))))
           (-           ,(mlambda ((x y) (- x y))))
           (*           ,(mlambda ((x y) (* x y))))
           (<           ,(mlambda ((x y) (< x y))))
           (cons        ,(mlambda ((x y) (cons x y))))
           (car         ,(mlambda ((x) (car x))))
           (cdr         ,(mlambda ((x) (cdr x))))
           (make-vector ,(mlambda ((x y) (make-vector x y))))
           (vector-ref  ,(mlambda ((x y) (vector-ref x y))))
           (vector-set! ,(mlambda ((x y z) (vector-set! x y z))))
           ;XXX for now:
           (error       ,(mlambda ((x y) (error x y))))
           (gensym      ,(mlambda ('() (gensym))))
           (read        ,(mlambda ('() (read))))
           (write       ,(mlambda ((x) (write x))))
           (newline     ,(mlambda ('() (newline))))
           (snarf       ,(mlambda ((x) (snarf x)))))))

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
               `((lambda ,(map car bindings) . ,body)
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

(write (interpret (read)))
(newline))
