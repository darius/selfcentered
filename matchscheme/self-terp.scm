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
    (env-extend r vs (map (lambda (_) '*uninitialized*) vs)))

  (define (env-lookup r v)
    (box-get (cdr (assv v r))))

  (define (env-resolve! r v value)
    (box-set! (cdr (assv v r)) value))

  (define (make-box value)     (make-vector 1 value))
  (define (box-get box)        (vector-ref box 0))
  (define (box-set! box value) (vector-set! box 0 value))

  (define (the-global-env)
    (map (mlambda ((name value) (cons name (make-box value))))
         `((%unless     ,(mlambda ((x y z) (%unless x y z))))
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
           (snarf       ,(mlambda ((x) (snarf x))))
           (pp          ,(mlambda ((x) (ppnarf x)))))))

  (include "elaborate.scm")
  (include "stdlib.scm")
  )

(write (interpret (read)))
(newline))
