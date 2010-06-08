(define (elaborate e)
  (cond ((symbol? e) e)
        ((self-evaluating? e) `',e)
        (else
         (assert (pair? e) '"Bad syntax" e)
         (case (car e)
           ((quote)
            `',(expand-literal (cadr e)))
           ((begin)
            (elaborate-seq (cdr e)))
           ((lambda)
            `(lambda ,(cadr e) ,(elaborate-seq (cddr e))))
           ((letrec)
            `(letrec ,(map (lambda (defn)
                             `(,(car defn) ,(elaborate (cadr defn))))
                           (cadr e))
               ,(elaborate-seq (cddr e))))
           ((local)
            (elaborate
             `(letrec ,(map (lambda (defn)
                              ;; (define (caadr . cdadr) . cddr)
                              `(,(caadr defn)
                                (lambda ,(cdadr defn) ,@(cddr defn))))
                           (cadr e))
                . ,(cddr e))))
           ((let)
            (elaborate `((lambda ,(map car (cadr e))
                           . ,(cddr e))
                         . ,(map cadr (cadr e)))))
           ((if)
            (let ((test (cadr e))
                  (if-true (caddr e))
                  (if-false (if (null? (cdddr e)) #f (cadddr e))))
              (elaborate `(%unless ,test
                                   (lambda () ,if-false)
                                   (lambda () ,if-true)))))
           ((cond)
            ;; Adapted from uts.scm
            (elaborate
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
                 (if (not (and (list? (car rands))
                               (= (length (car rands)) 3)))
                     (error '"Bad cond clause syntax" rands))
                 (let ((test-var (gensym)))
                   `(let ((,test-var ,(caar rands)))
                      (if ,test-var
                          (,(caddar rands) ,test-var)
                          (cond . ,(cdr rands))))))
                (else `(if ,(caar rands) 
                           (begin . ,(cdar rands))
                           (cond . ,(cdr rands))))))))
           ((or)
            ;; Adapted from uts.scm
            (elaborate
             (let ((rands (cdr e)))
               (case (length rands)
                 ((0) #f)
                 ((1) (car rands))
                 (else (let ((head (gensym)))
                         `(let ((,head ,(car rands)))
                            (if ,head ,head (or . ,(cdr rands))))))))))
           ((mcase)
            (elaborate (expand-mcase (gensym) (cadr e) (cddr e))))
           ((quasiquote)
            (elaborate (expand-quasiquote (cadr e))))
           (else
            (map elaborate e))))))

(define (expand-literal x)
  (cond ((string? x)
         (map char->integer (string->list x)))
        ((pair? x)
         (cons (expand-literal (car x))
               (expand-literal (cdr x))))
        (else x)))

(define (expand-quasiquote e)
  (if (not (pair? e))
      `',e
      (case (car e)
        ((unquote) (cadr e))
        (else `(cons ,(expand-quasiquote (car e))
                     ,(expand-quasiquote (cdr e)))))))

(define (expand-mcase subject subject-exp clauses)

  (define (expand-clause clause else-exp)
    (let ((pattern (car clause))
          (then-exp `(begin . ,(cdr clause)))
          (fail (gensym)))
      `(let ((,fail (lambda () ,else-exp)))
         ,(expand-pattern pattern then-exp `(,fail)))))

  (define (expand-pattern pattern then-exp else-exp)
    (define (test-constant constant)
      `(if (eq? ,subject ',constant) ,then-exp ,else-exp))
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
                  (,(car pattern) (mcase (cdr ,subject)
                                    (,(cdr pattern) ,then-exp)
                                    (_ ,else-exp)))
                  (_ ,else-exp))
                ,else-exp))
          (else
           (test-constant pattern))))

  `(let ((,subject ,subject-exp))
     ,(foldr expand-clause '(%match-error) clauses)))

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

(define (starts-with? symbol x)
  (and (pair? x) (eq? (car x) symbol)))

(define gensym-counter 0)

(define (gensym)
  (set! gensym-counter (+ gensym-counter 1))
  (string->symbol (string-append '":G" (number->string gensym-counter))))

(define (foldr f z xs)
  (if (eq? '() xs)
      z
      (f (car xs) (foldr f z (cdr xs)))))


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
         '(%unless x (lambda () z) (lambda () y)))
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
                        (%unless (null? xs)
                                 (lambda ()
                                   ((lambda (v thunk) (thunk))
                                    (f (car xs))
                                    (lambda () (for-each f (cdr xs)))))
                                 (lambda () '())))))
              for-each)))
