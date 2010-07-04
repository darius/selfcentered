;; Helpers

(define (assert ok? plaint culprit)
  (if (not ok?)
      (error plaint culprit)
      'fuck-off-mzscheme))

(define (should= x expected)
  (assert (equal? x expected) "Expected" expected))

(define (snarf filename)
  (call-with-input-file filename
    (lambda (port)
      (let reading ()
        (let ((sx (read port)))
          (if (eof-object? sx)
              '()
              (cons sx (reading))))))))

(include "gambit-macros.scm")


;; The language implementation

(load "elaborate.scm")
(load "test-elaborate.scm")

(load "terp.scm")


;; Simple tests of terp.scm:

(define interpreter-to-test interpret)

(load "mytest.scm")


;; The same tests on self-terp as run by terp:

(define self-terp (call-with-input-file "self-terp.scm" read))

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
