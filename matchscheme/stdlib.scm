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
