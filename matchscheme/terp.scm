;; Scheme subset except with LETREC specified more tightly, and with
;; a procedural representation of booleans.

;; Core syntax:
;; e = v
;;   | (QUOTE constant)
;;   | (LAMBDA (v ...) e)
;;   | (LETREC ((v e) ...) e)
;;   | (e e ...)

(define (interpret e)
  (evaluate (elaborate e) the-global-env))

(define (evaluate e r)
;  (pp e)
  (if (symbol? e)
      (env-lookup r e)
      (case (car e)
        ((quote)
         (cadr e))
        ((lambda)
         (make-proc (lambda (r arguments)
                      (evaluate (caddr e)
                                (env-extend r (cadr e) arguments)))
                    r))
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
         (call (evaluate (car e) r)
               (map (lambda (operand) (evaluate operand r))
                    (cdr e)))))))

(define (env-lookup r v)
  (cond ((assq v r) => cadr)
        (else (error "Unbound variable" v))))

(define (env-extend r vs values)
  (append (map list vs values) r))

(define (env-extend-promises r vs)
  (env-extend r vs (map (lambda (_) uninitialized) vs)))

(define (env-resolve! r v value)
  (cond ((assq v r) => (lambda (pair)
                         (assert (eq? (cadr pair) uninitialized) "WTF?" pair)
                         (set-car! (cdr pair) value)))
        (else (error "Can't happen" v))))

(define (make-proc script r)
  (vector proc-tag script r))

(define (proc? x)
  (and (vector? x) (eq? (vector-ref x 0) proc-tag)))

(define (call-proc proc arguments)
  ((vector-ref proc 1) (vector-ref proc 2) arguments))

(define proc-tag (list '<proc>))

(define primitive? procedure?)

(define (call object arguments)
  (cond ((boolean? object)
         (call (if object (cadr arguments) (car arguments)) '()))
        ((primitive? object)
         (apply object arguments))
        ((proc? object)
         (call-proc object arguments))
        (else (error "Non-procedure" object))))

(define uninitialized (vector '*uninitialized*))

(define (prim-%true? x)
  (not (not x)))

(define the-global-env
  `((uninitialized ,uninitialized)
    (%true?      ,prim-%true?)
    (cons        ,cons)
    (pair?       ,pair?)
    (eq?         ,eqv?)
    (+           ,+)
    (-           ,-)
    (*           ,*)
    (<           ,<)
    (car         ,car)
    (cdr         ,cdr)
    (symbol?     ,symbol?)
    (make-vector ,make-vector)          ;XXX should distinguish procs
    (vector-ref  ,vector-ref)
    (vector-set! ,vector-set!)
    ;; For now:
    (error       ,error)
    (elaborate   ,elaborate)
    (read        ,read)
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
(should= (interpret '(let ((x (eq? 4 (+ 2 2))))
                       x))
         #t)
(should= (interpret '(local ((define (fact n)
                               (if (eq? n 0)
                                   1
                                   (* n (fact (- n 1))))))
                       (fact 5)))
         120)
