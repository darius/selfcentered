;; Convert to continuation-passing style.

(define (cps-convert e)
  (local
      ((define (cc e k)
         (mcase e
           ((: v symbol?) `(,k ,v))
           (('quote datum)
            (let ((x (gensym)))
              `(%load ,e ,x (,k ,x))))
           (('lambda vs body)
            (let ((k1 (gensym)))
              `(,k (lambda (,k1 . ,vs)
                     ,(cc body k1)))))
           (('letrec defns body)
            `(letrec ,(map (mlambda ((f ('lambda vs fbody))
                                     (let ((k1 (gensym)))
                                       `(,f (lambda (,k1 . ,vs)
                                              ,(cc fbody k1))))))
                           defns)
               ,(cc body k)))
           (('%prim operator . operands)
            (cc* operands (lambda (xs) `(%prim ,operator . ,xs))))
           ((operator . operands)
            (cc* (cons operator operands)
                 (mlambda ((f . xs) `(,f ,k . ,xs)))))))

       (define (cc* es c)
         (mcase es
           ('() (c '()))
           ((e1 . es)
            (let ((x1 (gensym)))
              (cc e1 `(lambda (,x1)
                        ,(cc* es (lambda (xs)
                                   (c (cons x1 xs)))))))))))

    (cc e (let ((x (gensym)))
            `(lambda (,x) (%halt ,x))))))
