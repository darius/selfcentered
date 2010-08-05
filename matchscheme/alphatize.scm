;; Rename all variables so binding occurrences are unique.

(define (alphatize e)
  (alphatizing e '()))

(define (alphatizing e r)
  (local
      ((define (alpha e)
         (mcase e
           ((: v symbol?) (lookup v r))
           (('quote datum) e)
           (('lambda vs body)
            (let ((new-vs (map (lambda (v) (gensym)) vs)))
              `(lambda ,new-vs
                 ,(alphatizing body (append (zip vs new-vs) r)))))
           (('letrec defns body)
            (let ((fs (map car defns)))
              (let ((r (append (zip fs (map (lambda (f) (gensym)) fs)) r)))
                `(letrec ,(map (mlambda ((f e)
                                         `(,(lookup f r) ,(alphatizing e r)))
                                        defns))
                   ,(alphatizing body r)))))
           ((%prim operator . operands)
            `(%prim ,operator . ,(map alpha operands)))
           ((operator . operands)
            `(,(alpha operator) . ,(map alpha operands))))))
    (alpha e)))

(define (zip xs ys)
  (if (null? xs)
      '()
      (cons `(,(car xs) ,(car ys))
            (zip (cdr xs) (cdr ys)))))

(define (lookup x a-list)
  (cadr (assv x a-list)))
