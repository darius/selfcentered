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


;; Here's a mess: I want to use (include "foo.scm") to share code
;; between the bootstrap implementation and the self-implementation.
;; For the bootstrap, the files must also (include
;; "gambit-macros.scm") which must be a no-op when seen by the
;; self-implementation. We arrange this by starting gambit-macros.scm
;; with a special tag expression, "magic". We give it a value here, so
;; Gambit won't complain. (XXX Why not make the tag be "(quote magic)"
;; instead? Well, I did, but it broke: the elaborator then gives us a
;; match error. Sounds like a bug in ELABORATE, sigh. Look into it.)

(define magic #f)

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
