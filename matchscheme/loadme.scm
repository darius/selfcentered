(load "elaborate.scm")
(load "terp.scm")


;; Simple tests of terp.scm:

(define interpreter-to-test interpret)

(load "mytest.scm")


;; The same tests on self-terp as run by terp:

(define self-terp (call-with-input-file "self-terp.scm" read))
;(define self-terp (call-with-input-file "bug.scm" read))

(define (interpreter-to-test expr)
  (with-output-to-sexpr
   (lambda ()
     (with-input-from-sexpr expr (lambda ()
                                   (interpret self-terp))))))

;; NB this stuff may be Gambit-specific:
(define (with-input-from-sexpr sexpr thunk)
  (with-input-from-string (with-output-to-string '()
                            (lambda () (write sexpr)))
    thunk))

(define (with-output-to-sexpr thunk)
  (call-with-input-string (with-output-to-string '() thunk) read))

(load "mytest.scm")
