(local

 ((include "elaborate.scm")
  (include "alphatize.scm")
  (include "cps.scm")
  (include "closurecvt.scm")
  (include "stdlib.scm")

  (define (compile e)
    (closure-convert (cps-convert (alphatize (elaborate (wrap e))))))

  (define (wrap e)
    `(let ((%unless     (lambda (x y z) ((%prim %unless) x y z)))
           (boolean?    (lambda (x)     ((%prim boolean?) x)))
           (number?     (lambda (x)     ((%prim number?) x)))
           (pair?       (lambda (x)     ((%prim pair?) x)))
           (symbol?     (lambda (x)     ((%prim symbol?) x)))
           (eqv?        (lambda (x y)   ((%prim eqv?) x y)))
           (+           (lambda (x y)   ((%prim +) x y)))
           (-           (lambda (x y)   ((%prim -) x y)))
           (*           (lambda (x y)   ((%prim *) x y)))
           (<           (lambda (x y)   ((%prim <) x y)))
           (cons        (lambda (x y)   ((%prim cons) x y)))
           (car         (lambda (x)     ((%prim car) x)))
           (cdr         (lambda (x)     ((%prim cdr) x)))
           (make-vector (lambda (x y)   ((%prim make-vector) x y)))
           (vector-ref  (lambda (x y)   ((%prim vector-ref) x y)))
           (vector-set! (lambda (x y z) ((%prim vector-set!) x y z)))
           ;XXX for now:
           (error       (lambda (x y)   ((%prim error) x y)))
           (gensym      (lambda ()      ((%prim gensym))))
           (read        (lambda ()      ((%prim read))))
           (write       (lambda (x)     ((%prim write) x)))
           (newline     (lambda ()      ((%prim newline))))
           (pp          (lambda ()      ((%prim pp)))))
       ,e))
  )

(pp (compile (read)))
(newline))
