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
           (cons        ,(lambda (args) (cons (car args) (cadr args))))
           (pair?       ,(lambda (args) (pair? (car args))))
           (eq?         ,(lambda (args) (eq? (car args) (cadr args))))
           (+           ,(lambda (args) (+ (car args) (cadr args))))
           (-           ,(lambda (args) (- (car args) (cadr args))))
           (*           ,(lambda (args) (* (car args) (cadr args))))
           (<           ,(lambda (args) (< (car args) (cadr args))))
           (car         ,(lambda (args) (car (car args))))
           (cdr         ,(lambda (args) (cdr (car args))))
           (symbol?     ,(lambda (args) (symbol? (car args))))
           (make-vector ,(lambda (args) (make-vector (car args) (cadr args))))
           (vector-ref  ,(lambda (args) (vector-ref (car args) (cadr args))))
           (vector-set! ,(lambda (args) (vector-set! (car args)
                                                     (cadr args)
                                                     (caddr args))))
           ;XXX for now:
           (error       ,(lambda (args) (error (car args))))
           (elaborate   ,(lambda (args) (elaborate (car args))))
           (read        ,(lambda (args) (read)))
           (write       ,(lambda (args) (write (car args))))
           (newline     ,(lambda (args) (newline)))
           )))

  (define (caar x) (car (car x)))
  (define (cadr x) (car (cdr x)))
  (define (caddr x) (car (cdr (cdr x))))

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
        (error "Assertion failed")))

  )

(write (interpret (read)))
(newline))
