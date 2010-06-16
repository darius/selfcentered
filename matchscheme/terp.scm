;; Scheme subset except with LETREC specified more tightly, and with
;; a procedural representation of booleans.

;; Core syntax:
;; e = v
;;   | (QUOTE constant)
;;   | (LAMBDA (v ...) e)
;;   | (LETREC ((v e) ...) e)
;;   | (e e ...)

(define (interpret e)
  (evaluate (elaborate (expand-strings e)) the-global-env))

(define (expand-strings x)
  (cond ((string? x)
         (map char->integer (string->list x)))
        ((pair? x)
         (cons (expand-strings (car x))
               (expand-strings (cdr x))))
        (else x)))

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
         ;; Specified to evaluate left-to-right with a definite
         ;; 'uninitialized' value.
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
  (cond ((assq v r) => cadr)
        (else (error '"Unbound variable" v))))

(define (env-extend r vs values)
  (append (map list vs values) r))

(define (env-extend-promises r vs)
  (env-extend r vs (map (lambda (_) uninitialized) vs)))

(define (env-resolve! r v value)
  (cond ((assq v r) => (lambda (pair)
                         (assert (eqv? (cadr pair) uninitialized) "WTF?" pair)
                         (set-car! (cdr pair) value)))
        (else (error '"Can't happen" v))))

(define uninitialized (vector '*uninitialized*))

(define (%unless test if-no if-yes)
  (if test (if-yes) (if-no)))

(define the-global-env
  `((uninitialized ,uninitialized)
    (%unless     ,%unless)
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
    (elaborate   ,elaborate)
    (read        ,(lambda () (expand-strings (read))))
    (write       ,write)
    (newline     ,newline)
    ))


(should= (interpret '42)
         42)
(should= (interpret 'cons)
         cons)
(should= (interpret '(- 5 3))
         2)
(should= (interpret '(car (cdr '(hello world))))
         'world)
(should= (interpret '((lambda (x) x)
                      '55))
         55)
(should= (interpret '(let ((x (eqv? 4 (+ 2 2))))
                       x))
         #t)
(should= (interpret '(local ((define (fact n)
                               (if (eqv? n 0)
                                   1
                                   (* n (fact (- n 1))))))
                       (fact 5)))
         120)
