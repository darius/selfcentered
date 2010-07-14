;; 'Bytecode' compiler -- currently completely non-optimizing,
;; unless you count tail recursion and open-coding primitives.
;; Uses linked environments.

(local

 ((define (compile e)
    (evaluate (elaborate e) '() '((halt))))

  (define (evaluate e r k)
    (mcase e
      ((: v symbol?)
       (cons (env-lookup r v) k))
      (('quote datum)
       `((quote ,datum) . ,k))
      (('lambda vs body)
       (let ((code `((check-nargs ,(length vs))
                     . ,(evaluate body (env-extend r vs) '((return))))))
         `((enclose ,(length code)) . ,(append code k))))
      (('letrec defns body)
       (let ((new-r (env-extend r (map car defns))))
         `((fix ,(length defns))
           . ,(append
               (flatten (map (mlambda
                              ((v1 e1)
                               (evaluate e1 new-r
                                         (mcase (env-lookup new-r v1)
                                           (('ref '0 offset)
                                            `((fixup ,offset)))))))
                             defns))
               (evaluate body new-r k)))))
      ((operator . operands)
       (let ((rands-code
              (flatten (map (lambda (operand) (evaluate operand r '()))
                            operands))))
         (mcase (evaluate operator r `((jump ,(length operands))))
           ((('prim-ref prim) _)
            (if (not (eqv? (length operands) (primitive-arity prim)))
                (error '"Wrong # of arguments to primitive" prim))
            (append rands-code `((prim-op ,prim) . ,k)))
           (op-code
            (mcase k
              ((('return)) (append rands-code op-code))
              (_ `((push-cont ,(+ (length rands-code) (length op-code)))
                   . ,(append rands-code (append op-code k)))))))))))

  (define (env-extend r vs)
    (cons vs r))

  (define (env-lookup r v)
    (local ((define (searching r frame-index)
              (mcase r
                ('() (if (primitive-arity v)
                         `(prim-ref ,v)
                         (error '"Unbound variable" v)))
                ((vs . parent)
                 (local ((define (looking vs i)
                           (mcase vs
                             ('() (searching parent (+ frame-index 1)))
                             ((v1 . rest-vs) (if (eqv? v v1)
                                                 `(ref ,frame-index ,i)
                                                 (looking rest-vs (+ i 1)))))))
                   (looking vs 0))))))
      (searching r 0)))

  (define (primitive-arity prim)
    (mcase prim
      ;; NB we won't need 'uninitialized' any more. But we haven't yet
      ;; eliminated it from terp, so this is a difference from there.
      ('%unless     3)
      ('boolean?    1)
      ('number?     1)
      ('pair?       1)
      ('symbol?     1)
      ('eqv?        2)
      ('+           2)
      ('-           2)
      ('*           2)
      ('<           2)
      ('cons        2)
      ('car         1)
      ('cdr         1)
      ('make-vector 2)
      ('vector-ref  2)
      ('vector-set! 3)
      ;;XXX for now:
      ('error       2)
      ('gensym      0)
      ('read        0)
      ('write       1)
      ('newline     0)
      (_ #f)))

  (include "elaborate.scm")

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

  )

(write (compile (read)))
(newline))
