(define (closure-convert e)
  (convert e (lambda (v) v)))

(define (convert e r)
  (local
      ((define (cvt e)
         (mcase e
           ((: v symbol?) (r v))
           (('quote datum) e)
           (('lambda vs body)
            (let ((fvs (free-vars e)))
              `(%enclose ,(map cvt fvs)
                         (%proc-ref 0)
                         (lambda ,vs
                           ,(convert body (make-fix-env '() fvs))))))
           (('letrec defns body)
            (let ((fvs (free-vars e)))
              (let ((r (make-fix-env (map (mlambda ((f e) f)) defns) fvs)))
                (let ((procs
                       (map (mlambda 
                             ((f ('lambda vs fbody))
                              `(lambda ,vs ,(convert fbody r))))
                            defns)))
                  `(%enclose ,(map cvt fvs)
                             ,(convert body r)
                             . ,procs)))))
           (('%prim prim . operands)
            `(%prim ,prim . ,(map cvt operands)))
           ((operator . operands)
            `(,(cvt operator) . ,(map cvt operands))))))
    (cvt e)))

(define (make-fix-env fs fvs)
  (lambda (v)
    (or (lookup v fs '%proc-ref)
        (lookup v fvs '%fv-ref)
        v)))

(define (lookup v vs operator)
  (local ((define (looking vs i)
            (mcase vs
              ('() #f)
              ((v1 . rest-vs) (if (eqv? v v1)
                                  `(,operator ,i)
                                  (looking rest-vs (+ i 1)))))))
    (looking vs 0)))

(define (free-vars e)
  (mcase e
    ((: v symbol?) `(,v))
    (('quote datum) '())
    (('lambda vs body) (set-diff (free-vars body) vs))
    (('letrec defns body)
     (set-diff (all-free-vars (cons body (map (mlambda ((f e) e)) defns)))
               (map (mlambda ((f e) f)) defns)))
    (('%prim prim . operands) (all-free-vars operands))
    ((_ . _) (all-free-vars e))))

(define (all-free-vars es)
  (foldr set-union '() (map free-vars es)))

(define (set-union xs ys) (append-set-diff xs ys ys))
(define (set-diff xs ys)  (append-set-diff xs ys '()))

(define (append-set-diff xs ys zs)
  (foldr (lambda (x set) (if (memv? x ys) set (cons x set)))
         zs
         xs))

(define (memv? x xs)
  (cond ((null? xs) #f)
        ((eqv? x (car xs)) #t)
        (else (memv? x (cdr xs)))))
