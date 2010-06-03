(define input '
(local

 ((define (compile lexp)
    (((expand lexp) 'compile) global-static-env '(halt)))

  (define (expand lexp)
    (if (symbol? lexp)
        (make-var lexp)
        (if (eq? (car lexp) 'lambda)
            (make-lam (car (car (cdr lexp)))
                      (expand (car (cdr (cdr lexp)))))
            (make-app (expand (car (cdr lexp)))
                      (expand (car (cdr (cdr lexp))))))))

  (define (make-var v)
    (lambda (m)
      (case m
        (free-vars (list1 v))
        (compile (lambda (r k) (cons (r v) k))))))

  (define (make-lam v e)
    (let ((free-vars (delq v (e 'free-vars))))
      (lambda (m)
        (case m
          (free-vars free-vars)
          (compile
           (lambda (r k)
             (let ((code
                    ((e 'compile) (make-static-env v free-vars) '(return))))
               (cons 'make-closure
                     (cons (length free-vars)
                           (cons (length code)
                                 (append3 (map r free-vars) code k)))))))))))

  (define (make-app e1 e2)
    (lambda (m)
      (case m
        (free-vars (union (e1 'free-vars) (e2 'free-vars)))
        (compile
         (lambda (r k)
           (let ((code ((e2 'compile) r ((e1 'compile) r '(invoke)))))
             (if (eq? (car k) 'return)
                 code
                 (cons 'pushcont (cons (length code) (append2 code k))))))))))

  (define (global-static-env v)
    (error "Unbound variable" v))

  (define (make-static-env v free-vars)
    (lambda (v1)
      (if (eq? v1 v)
          'local
          (+ 1 (list-index v1 free-vars)))))

  (define (union set1 set2)
    (let ((adjoin (lambda (x xs)
                    (if (memq? x set2) xs (cons x xs)))))
      (foldr adjoin set2 set1)))

  (define (delq x set)
    (if (eq? '() set)
        '()
        (if (eq? x (car set))
            (cdr set)
            (cons (car set) (delq x (cdr set))))))

  (define (length xs)
    (local ((define (counting n xs)
              (if (eq? '() xs)
                  n
                  (counting (+ n 1) (cdr xs)))))
      (counting 0 xs)))

  (define (map f xs)
    (foldr (lambda (x ys) (cons (f x) ys))
           '()
           xs))

  (define (list1 x)          (cons x '()))
  (define (append3 xs ys zs) (append2 xs (append2 ys zs)))
  (define (append2 xs ys)    (foldr cons ys xs))

  (define (foldr f z xs)
    (if (eq? '() xs)
        z
        (f (car xs) (foldr f z (cdr xs)))))

  (define (memq? x set)
    (if (eq? '() set)
        #f
        (if (eq? x (car set))
            #t
            (memq? x (cdr set)))))
  
  (define (list-index x xs)
    (local ((define (searching i xs)
              (if (eq? x (car xs))
                  i
                  (searching (+ n 1) (cdr xs)))))
      (searching 0 xs))))

 (compile '(lambda (x) x)))
)
