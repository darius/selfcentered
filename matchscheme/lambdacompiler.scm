(define input '
(local

 ((define (compile lexp)
    (((expand lexp) 'compile) global-static-env '(halt)))

  (define expand
    (mlambda
     ((: v symbol?)   (make-var v))
     (('lambda (v) e) (make-lam v (expand e)))
     ((e1 e2)         (make-app (expand e1) (expand e2)))))

  (define (make-var v)
    (mlambda
     ('free-vars `(,v))
     ('compile (lambda (r k) (cons (r v) k)))))

  (define (make-lam v e)
    (let ((free-vars (delv v (e 'free-vars))))
      (mlambda
       ('free-vars free-vars)
       ('compile
        (lambda (r k)
          (let ((code
                 ((e 'compile) (make-static-env v free-vars) '(return))))
            `(make-closure ,(length free-vars) ,(length code)
                           . ,(append (map r free-vars) (append code k)))))))))

  (define (make-app e1 e2)
    (mlambda
     ('free-vars (union (e1 'free-vars) (e2 'free-vars)))
     ('compile
      (lambda (r k)
        (let ((code ((e2 'compile) r ((e1 'compile) r '(invoke)))))
          (mcase k
            (('return) code)
            (_ `(pushcont ,(length code) . ,(append code k)))))))))

  (define (global-static-env v)
    (error '"Unbound variable" v))

  (define (make-static-env v free-vars)
    (lambda (v1)
      (if (eqv? v1 v)
          'local
          (+ 1 (list-index v1 free-vars)))))

  (define (union set1 set2)
    (let ((adjoin (lambda (x xs)
                    (if (memv? x set2) xs (cons x xs)))))
      (foldr adjoin set2 set1)))

  (define (delv x set)
    (cond ((eqv? '() set) '())
          ((eqv? x (car set)) (cdr set))
          (else (cons (car set) (delv x (cdr set))))))

  (define (length xs)
    (foldr (lambda (x n) (+ n 1)) 0 xs))

  (define (map f xs)
    (foldr (lambda (x ys) (cons (f x) ys))
           '()
           xs))

  (define (append xs ys)    (foldr cons ys xs))

  (define (foldr f z xs)
    (if (eqv? '() xs)
        z
        (f (car xs) (foldr f z (cdr xs)))))

  (define (memv? x set)
    (cond ((eqv? '() set) #f)
          ((eqv? x (car set)) #t)
          (else (memv? x (cdr set)))))
  
  (define (list-index x xs)
    (local ((define (searching i xs)
              (if (eqv? x (car xs))
                  i
                  (searching (+ n 1) (cdr xs)))))
      (searching 0 xs))))

 (compile
;  '(lambda (x) x)
  '((lambda (x) (lambda (y) x)) (lambda (z) z))  ; XXX is output wrong?
  ))
)
