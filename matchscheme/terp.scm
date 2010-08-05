;; Scheme subset with LETREC restricted to LAMBDAs only, plus a
;; primitives %UNLESS for IF to expand into.

;; Core syntax:
;; e = v
;;   | (QUOTE constant)
;;   | (LAMBDA (v ...) e)
;;   | (LETREC ((v e) ...) e)
;;   | (e e ...)

(define (interpret e)
  (evaluate (elaborate e) the-global-env))

(define (evaluate e r)
  (if (symbol? e)
      (env-lookup r e)
      (case (car e)
        ((quote)
         (cadr e))
        ((lambda)
         (lambda arguments
           (evaluate (caddr e)
                     (env-extend r (cadr e) arguments))))
        ((letrec)
         (let ((new-r (env-extend-promises r (map car (cadr e)))))
           (for-each (lambda (defn)
                       (env-resolve! new-r
                                     (car defn)
                                     (evaluate (cadr defn) new-r)))
                     (cadr e))
           (evaluate (caddr e) new-r)))
        (else
         (apply (evaluate (car e) r)
                (map (lambda (operand) (evaluate operand r))
                     (cdr e)))))))

(define (env-lookup r v)
  (cond ((assv v r) => cadr)
        (else (error '"Unbound variable" v))))

(define (env-extend r vs values)
  (append (map list vs values) r))

(define (env-extend-promises r vs)
  (env-extend r vs (map (lambda (_) '*uninitialized*) vs)))

(define (env-resolve! r v value)
  (cond ((assv v r) => (lambda (pair)
                         (assert (eqv? (cadr pair) '*uninitialized*)
                                 "Redefinition" pair)
                         (set-car! (cdr pair) value)))
        (else (error '"Can't happen" v))))

(define (%unless test if-no if-yes)
  (if test if-yes if-no))

(define the-global-env
  `((%unless     ,%unless)
    (boolean?    ,boolean?)
    (number?     ,number?)
    (pair?       ,pair?)
    (symbol?     ,symbol?)
    (eqv?        ,eqv?)
    (+           ,+)
    (-           ,-)
    (*           ,*)
    (<           ,<)
    (cons        ,cons)
    (car         ,car)
    (cdr         ,cdr)
    (make-vector ,make-vector)          ;XXX should distinguish procs
    (vector-ref  ,vector-ref)
    (vector-set! ,vector-set!)
    ;; For now:
    (error       ,error)
    (gensym      ,gensym)
    (read        ,read)
    (write       ,write)
    (newline     ,newline)
    (snarf       ,snarf)
    ))
