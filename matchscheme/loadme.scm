(load "elaborate.scm")
(load "terp.scm")

(define self-terp (call-with-input-file "self-terp.scm" read))
