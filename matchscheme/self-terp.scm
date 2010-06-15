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
         (for-each (lambda (defn)
                     (env-resolve! new-r
                                   (car defn)
                                   (evaluate (cadr defn) new-r)))
                   defns)
         (evaluate body new-r)))
      ((operator . operands)
       ((evaluate operator r)
        (map (lambda (operand) (evaluate operand r))
             operands)))))

  (define (env-lookup r v)
    (box-get (cdr (assq v r))))

  (define (env-extend r vs values)
    (append (map2 (lambda (v value) (cons v (make-box value))) vs values)
            r))

  (define (env-extend-promises r vs)
    (env-extend r vs (map (lambda (_) uninitialized) vs)))

  (define (env-resolve! r v value)
    (let ((box (cdr (assq v r))))
      (assert (eq? (box-get box) uninitialized))
      (box-set! box value)))

  (define (make-box value)
    (make-vector 1 value))
  
  (define (box-get box)
    (vector-ref box 0))
  
  (define (box-set! box value)
    (vector-set! box 0 value))

  (define (the-global-env)
    (map (lambda (pair) (cons (car pair) (make-box (cadr pair))))
         `((uninitialized ,uninitialized)
           (%unless     ,(lambda (args) (if (car args)
                                            ((caddr args) '())
                                            ((cadr args) '()))))
           (boolean?    ,(lambda (args) (boolean? (car args))))
           (number?     ,(lambda (args) (number? (car args))))
           (pair?       ,(lambda (args) (pair? (car args))))
           (symbol?     ,(lambda (args) (symbol? (car args))))
           (eq?         ,(lambda (args) (eq? (car args) (cadr args))))
           (+           ,(lambda (args) (+ (car args) (cadr args))))
           (-           ,(lambda (args) (- (car args) (cadr args))))
           (*           ,(lambda (args) (* (car args) (cadr args))))
           (<           ,(lambda (args) (< (car args) (cadr args))))
           (cons        ,(lambda (args) (cons (car args) (cadr args))))
           (car         ,(lambda (args) (car (car args))))
           (cdr         ,(lambda (args) (cdr (car args))))
           (make-vector ,(lambda (args) (make-vector (car args) (cadr args))))
           (vector-ref  ,(lambda (args) (vector-ref (car args) (cadr args))))
           (vector-set! ,(lambda (args) (vector-set! (car args)
                                                     (cadr args)
                                                     (caddr args))))
           ;XXX for now:
           (error       ,(lambda (args) (error (car args))))
           (gensym      ,(lambda (args) (gensym)))
           (elaborate   ,(lambda (args) (elaborate (car args))))
           (read        ,(lambda (args) (read)))
           (write       ,(lambda (args) (write (car args))))
           (newline     ,(lambda (args) (newline)))
           )))

  (define (caar x) (car (car x)))
  (define (cadr x) (car (cdr x)))
  (define (cdar x) (cdr (car x)))
  (define (cadar x) (car (cdr (car x))))
  (define (caddr x) (car (cdr (cdr x))))
  (define (cdddr x) (cdr (cdr (cdr x))))
  (define (caddar x) (car (cdr (cdr (car x)))))
  (define (cadddr x) (car (cdr (cdr (cdr x)))))

  (define (map f xs)
    (foldr (lambda (x ys) (cons (f x) ys))
           '()
           xs))

  (define (foldr f z xs)
    (mcase xs
      ('() z)
      ((x . xs) (f x (foldr f z xs)))))

  (define (map2 f xs ys)
    (if (null? xs)
        '()
        (cons (f (car xs) (car ys))
              (map2 f (cdr xs) (cdr ys)))))

  (define (for-each f xs)
    (cond ((not (null? xs))
           (f (car xs))
           (for-each f (cdr xs)))))

  (define (null? x) (eq? x '()))
  (define (not x) (eq? x #f))

  (define (append xs ys)
    (foldr cons ys xs))

  (define (assq key pairs)
    (cond ((null? pairs) #f)
          ((eq? key (caar pairs)) (car pairs))
          (else (assq key (cdr pairs)))))

  (define (assert ok?)
    (if (not ok?)
        (error '"Assertion failed")))

  (define (elaborate e)
    (cond ((symbol? e) e)
          ((or (boolean? e) (number? e)) `',e)
          (else
           (if (not (pair? e)) (error '"Bad syntax" e))
           (cond ((lookup (car e) macros)
                  => (lambda (expand) (elaborate (expand e))))
                 ((lookup (car e) core-syntax)
                  => (lambda (expand) (expand e)))
                 (else 
                  (map elaborate e))))))

  (define core-syntax
    `((quote ,(lambda (e)
                (mcase e
                  ((_ _) e))))
      (lambda ,(lambda (e)
                 (mcase e
                   ((_ vars . body)
                    `(lambda ,vars ,(elaborate-seq body))))))
      (letrec ,(lambda (e)
                 (mcase e
                   ((_ defns . body)
                    `(letrec ,(map (lambda (defn)
                                     (mcase defn
                                       ((v e) `(,v ,(elaborate e)))))
                                   defns)
                       ,(elaborate-seq body))))))
      (begin ,(lambda (e)
                ;; Not actually core syntax but here's how I wrote it anyway:
                (elaborate-seq (cdr e))))))

  (define macros 
    (cons 
     ;; This awkward line to avoid a nested quasiquote in the quasiquoted main
     ;; macros table just below:
     (cons 'quasiquote (cons (lambda (e)
                               (mcase e
                                 ((_ q) (expand-quasiquote q))))
                             '()))
     `((local ,(lambda (e)
                 (mcase e
                   ((_ defns . body)
                    `(letrec ,(map (lambda (defn)
                                     (mcase defn
                                       ((_ (: name symbol?) e)
                                        `(,name ,e))
                                       ((_ (name . vars) . body)
                                        `(,name (lambda ,vars . ,body)))))
                                   defns)
                       . ,body)))))
       (let ,(lambda (e)
               (mcase e
                 ((_ bindings . body)
                  `((lambda ,(map car bindings)
                      . ,body)
                    . ,(map cadr bindings))))))
       (if ,(lambda (e)
              (mcase e
                ((_ test if-true)
                 `(%unless ,test (lambda () #f) (lambda () ,if-true)))
                ((_ test if-true if-false)
                 `(%unless ,test (lambda () ,if-false) (lambda () ,if-true))))))
       (cond ,(lambda (e)
                (mcase e
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
                   `(if ,e (begin . ,es) (cond . ,clauses))))))
       (or ,(lambda (e)
              (mcase e
                ((_) #f)
                ((_ e) e)
                ((_ e . es)
                 (let ((head (gensym)))
                   `(let ((,head ,e))
                      (if ,head ,head (or . ,es))))))))
       (mcase ,(lambda (e)
                 (mcase e
                   ((_ subject-exp . clauses)
                    (expand-mcase (gensym) subject-exp clauses))))))))

  (define (lookup key a-list)
    (cond ((assq key a-list) => cadr)
          (else #f)))

  (define (expand-quasiquote e)
    (mcase e
      (('unquote e) e)
      ((qcar . qcdr) `(cons ,(expand-quasiquote qcar)
                            ,(expand-quasiquote qcdr)))
      (else `',e)))

  (define (expand-mcase subject subject-exp clauses)
    (letrec
        ((expand-clause 
          (lambda (clause else-exp)
            (let ((pattern (car clause))
                  (then-exp `(begin . ,(cdr clause)))
                  (fail (gensym)))
              `(let ((,fail (lambda () ,else-exp)))
                 ,(expand-pattern pattern then-exp `(,fail))))))
         (expand-pattern
          (lambda (pattern then-exp else-exp)
            (let ((test-constant
                   (lambda (constant)
                     `(if (eq? ,subject ',constant) ,then-exp ,else-exp))))
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
                (_ (test-constant pattern)))))))
      `(let ((,subject ,subject-exp))
         ,(foldr expand-clause '(%match-error) clauses))))

  (define (elaborate-seq es)
    (make-begin (map elaborate es)))

  (define (make-begin es)
    (mcase es
      ('() ''#f)
      ((e) e)
      ((e . es) `((lambda (,(gensym)) ,(make-begin es))
                  ,e))))

  (define (starts-with? symbol x)
    (and (pair? x) (eq? (car x) symbol)))

  )

(write (interpret (read)))
(newline))
