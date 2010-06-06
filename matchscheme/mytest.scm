;; Adapted from Aubrey Jaffer's test.scm.

(define test
  (lambda (expect expr)
    (write expr)
    (display "  ==> ")
    ((lambda (res)
      (write res)
      (newline)
      (cond ((not (equal? expect res))
;	     (record-error (list res expect (cons fun args)))
	     (display " BUT EXPECTED ")
	     (write expect)
	     (newline)
	     #f)
	    (else #t)))
     (interpreter-to-test expr))))

(test '(quote a) '(quote 'a))

(test 12 '((if #f + *) 3 4))

(test 8 '((lambda (x) (+ x x)) 4))

(test 3 '(let ((reverse-subtract
                (lambda (x y) (- y x))))
           (reverse-subtract 7 10)))

(test 3 '(local ((define (reverse-subtract x y) (- y x)))
                (reverse-subtract 7 10)))

(test 10 '(let ((add4
                 (let ((x 4))
                   (lambda (y) (+ x y)))))
            (add4 6)))

(test 'yes '(if (< 2 3) 'yes 'no))

(test 'no '(if (< 3 2) 'yes 'no))

(test '1 '(if (< 2 3) (- 3 2) (+ 3 2)))

(test 'greater '(cond ((< 2 3) 'greater)
                      ((< 3 2) 'less)))

(test 'equal '(cond ((< 3 3) 'greater)
                    ((< 3 3) 'less)
                    (else 'equal)))

(test 2 '(local ((define (assq key pairs)
                   (cond ((null? pairs) #f)
                         ((eq? key (caar pairs)) (car pairs))
                         (else (assq key (cdr pairs)))))
                 (define (null? x) (eq? x '()))
                 (define (caar x) (car (car x)))
                 (define (cadr x) (car (cdr x))))
           (cond ((assq 'b '((a 1) (b 2))) => cadr)
                 (else #f))))

(test 'composite '(mcase (* 2 3)
                    (2 'prime)
                    (3 'prime)
                    (4 'composite)
                    (5 'prime)
                    (6 'composite)
                    (7 'prime)))

(test 'consonant '(mcase (car '(c d))
                    ('a 'vowel)
                    ('y 'semivowel)
                    (_ 'consonant)))

(test #t '(eq? 2 2))
(test #f '(eq? 2 3))

(test #t '(or (eq? 2 2) (< 1 2)))
(test #t '(or (eq? 2 2) (< 2 1)))
(test #f '(or #f #f #f))
(test #f '(or))
(test '(b c) '(or (cdr '(a b c)) (+ 3 0)))

(test 6 '(let ((x 2) (y 3)) (* x y)))
(test 35 '(let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))))
;(test 70 '(let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x))))
(test #t '(letrec ((even?
                    (lambda (n) (if (eq? 0 n) #t (odd? (- n 1)))))
                   (odd?
                    (lambda (n) (if (eq? 0 n) #f (even? (- n 1))))))
            (even? 88)))

(test '(list 3 4) '`(list ,(+ 1 2) 4))

(test '(list a (quote a)) '(let ((name 'a)) `(list ,name ',name)))

(test '(a 3 4 5 6) '(local ((define (map f xs)
                              (foldr (lambda (x ys) (cons (f x) ys))
                                     '()
                                     xs))
                            (define (foldr f z xs)
                              (if (eq? '() xs)
                                  z
                                  (f (car xs) (foldr f z (cdr xs)))))
                            (define (abs n)
                              (if (< n 0) (- 0 n) n)))
                      `(a ,(+ 1 2) . ,(map abs '(4 -5 6)))))

(test 5 '`,(+ 2 3))

(test '(list 3 4) '(quasiquote (list (unquote (+ 1 2)) 4)))
(test '`(list ,(+ 1 2) 4) ''(quasiquote (list (unquote (+ 1 2)) 4)))

(test 45 '(let ((x 5))
            (local ((define (foo y) (bar x y))
                    (define (bar a b) (+ (* a b) a)))
	      (foo (+ x 3)))))

(test #f '(eq? (cons 1 2) (cons 1 2)))
(test #f '(eq? (lambda () 1) (lambda () 2)))

(test #t '(let ((p (lambda (x) x)))
            (eq? p p)))
