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
  (define (cddr x) (cdr (cdr x)))
  (define (caadr x) (car (car (cdr x))))
  (define (cadar x) (car (cdr (car x))))
  (define (caddr x) (car (cdr (cdr x))))
  (define (cdadr x) (cdr (car (cdr x))))
  (define (cdddr x) (cdr (cdr (cdr x))))
  (define (caddar x) (car (cdr (cdr (car x)))))
  (define (cadddr x) (car (cdr (cdr (cdr x)))))

  (define (map f xs)
    (foldr (lambda (x ys) (cons (f x) ys))
           '()
           xs))

  (define (foldr f z xs)
    (if (eq? '() xs)
        z
        (f (car xs) (foldr f z (cdr xs)))))

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
          ((self-evaluating? e) `',e)
          (else
           (if (not (pair? e)) (error '"Bad syntax" e))
           (cond ((lookup (car e) macros)
                  => (lambda (expand) (elaborate (expand e))))
                 ((lookup (car e) core-syntax)
                  => (lambda (expand) (expand e)))
                 (else 
                  (map elaborate e))))))

  (define core-syntax
    `((quote ,(lambda (e) e))
      (lambda ,(lambda (e)
                 `(lambda ,(cadr e) ,(elaborate-seq (cddr e)))))
      (letrec ,(lambda (e)
                 `(letrec ,(map (lambda (defn)
                                  `(,(car defn) ,(elaborate (cadr defn))))
                                (cadr e))
                    ,(elaborate-seq (cddr e)))))
      (begin ,(lambda (e)
                ;; Not actually core syntax but here's how I wrote it anyway:
                (elaborate-seq (cdr e))))))

  (define macros 
    (cons 
     ;; This awkward line to avoid a nested quasiquote in the quasiquoted main
     ;; macros table just below:
     (cons 'quasiquote (cons (lambda (e) (expand-quasiquote (cadr e)))
                             '()))
     `((local ,(lambda (e)
                 `(letrec ,(map (lambda (defn)
                                  (parse-defn defn
                                              (lambda (name value-e)
                                                `(,name ,value-e))))
                                (cadr e))
                    . ,(cddr e))))
       (let ,(lambda (e)
               `((lambda ,(map car (cadr e))
                   . ,(cddr e))
                 . ,(map cadr (cadr e)))))
       (if ,(lambda (e)
              (let ((test (cadr e))
                    (if-true (caddr e))
                    (if-false (if (null? (cdddr e)) #f (cadddr e))))
                `(%unless ,test
                          (lambda () ,if-false)
                          (lambda () ,if-true)))))
       (cond ,(lambda (e)
                ;; Adapted from uts.scm
                (let ((rands (cdr e)))
                  (cond
                   ((null? rands) #f)
                   ((not (pair? (car rands)))
                    (error '"Invalid cond clause" (car rands)))
                   ((eq? (caar rands) 'else)
                    (if (null? (cdr rands))
                        `(begin . ,(cdar rands))
                        (error '"Else-clause is not last" rands)))
                   ((null? (cdar rands))
                    `(or ,(caar rands) (cond . ,(cdr rands))))
                   ((and (pair? (cdar rands)) (eq? (cadar rands) '=>))
                    (let ((test-var (gensym)))
                      `(let ((,test-var ,(caar rands)))
                         (if ,test-var
                             (,(caddar rands) ,test-var)
                             (cond . ,(cdr rands))))))
                   (else `(if ,(caar rands) 
                              (begin . ,(cdar rands))
                              (cond . ,(cdr rands))))))))
       (or ,(lambda (e)
              ;; Adapted from uts.scm
              (let ((rands (cdr e)))
                (cond ((null? rands) #f)
                      ((null? (cdr rands)) (car rands))
                      (else (let ((head (gensym)))
                              `(let ((,head ,(car rands)))
                                 (if ,head ,head (or . ,(cdr rands))))))))))
       (mcase ,(lambda (e)
                 (expand-mcase (gensym) (cadr e) (cddr e)))))))

  (define (lookup key a-list)
    (cond ((assq key a-list) => cadr)
          (else #f)))

  (define (expand-quasiquote e)
    (cond ((not (pair? e)) `',e)
          ((eq? (car e) 'unquote) (cadr e))
          (else `(cons ,(expand-quasiquote (car e))
                       ,(expand-quasiquote (cdr e))))))

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
              (cond ((eq? pattern '_)
                     then-exp)
                    ((starts-with? 'quote pattern)
                     (test-constant (cadr pattern)))
                    ((symbol? pattern)
                     `(let ((,pattern ,subject)) ,then-exp))
                    ((starts-with? ': pattern)
                     (let ((name (cadr pattern)) (predicate (caddr pattern)))
                       `(if (,predicate ,subject)
                            (let ((,name ,subject)) ,then-exp)
                            ,else-exp)))
                    ((pair? pattern)
                     `(if (pair? ,subject)
                          (mcase (car ,subject)
                                 (,(car pattern)
                                  (mcase (cdr ,subject)
                                         (,(cdr pattern) ,then-exp)
                                         (_ ,else-exp)))
                                 (_ ,else-exp))
                          ,else-exp))
                    (else
                     (test-constant pattern)))))))
      `(let ((,subject ,subject-exp))
         ,(foldr expand-clause '(%match-error) clauses))))

  (define (self-evaluating? x)
    (or (boolean? x)
        (number? x)))

  (define (parse-defn defn receiver)
    (if (symbol? (cadr defn))
        ;; (define cadr caddr)
        (receiver (cadr defn) (caddr defn))
        ;; (define (caadr . cdadr) . cddr)
        (receiver (caadr defn)
                  `(lambda ,(cdadr defn)
                     . ,(cddr defn)))))

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

  (define (starts-with? symbol x)
    (and (pair? x) (eq? (car x) symbol)))

  )

(write (interpret (read)))
(newline))
