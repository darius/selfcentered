(define (elaborate e)
  (cond ((symbol? e) e)
        ((self-evaluating? e) `',e)
        (else
         (assert (pair? e) '"Bad syntax" e)
         (cond ((lookup (car e) macros)
                => (lambda (expand) (elaborate (expand e))))
               ((lookup (car e) core-syntax)
                => (lambda (expand) (expand e)))
               (else 
                (map elaborate e))))))

(define (lookup key a-list)
  (cond ((assq key a-list) => cadr)
        (else #f)))

(define core-syntax
  `((quote ,(lambda (e)
              `',(expand-literal (cadr e))))
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
                                ;; (define (caadr . cdadr) . cddr)
                                `(,(caadr defn)
                                  (lambda ,(cdadr defn) . ,(cddr defn))))
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

(define (expand-literal x)
  (cond ((string? x)
         (map char->integer (string->list x)))
        ((pair? x)
         (cons (expand-literal (car x))
               (expand-literal (cdr x))))
        (else x)))

(define (expand-quasiquote e)
  (cond ((not (pair? e)) `',e)
        ((eq? (car e) 'unquote) (cadr e))
        (else `(cons ,(expand-quasiquote (car e))
                     ,(expand-quasiquote (cdr e))))))

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
      (number? x)))

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
