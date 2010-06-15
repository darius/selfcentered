;; Simplify a core-Scheme expression.
;; Intended to make the expansion of mcase less overwhelmingly unreadable.

(define (shrink e)
  (if (symbol? e)
      e
      (case (car e)
        ((quote) 
         e)
        ((lambda)
         `(lambda ,(cadr e) ,(shrink (caddr e))))
        ((letrec)
         `(letrec ,(map (lambda (defn)
                          `(,(car defn)
                            ,(shrink (cadr defn))))
                        (cadr e))
            ,(shrink (caddr e))))
        (else
         (let ((rator (shrink (car e))) 
               (rands (map shrink (cdr e))))
           (if (and (starts-with? 'lambda rator)
                    (= (length (cadr rator)) (length rands)))
               (let* ((params (cadr rator)) (body (caddr rator))
                      (counts (map (lambda (v) (count v body)) params))
                      (eliminable?
                       (let checking ((params params) (rands rands))
                         (or (null? rands)
                             (and (or (atomic? (car rands))
                                      (and (effectless? (car rands))
                                           (or (< (count (car params) body) 2)
                                               (small? (car rands)))))
                                  (checking (cdr params) (cdr rands)))))))
                 (if eliminable?
                     (shrink (substitute (let ((r (map cons params rands)))
                                           (lambda (v)
                                             (cond ((assq v r) => cdr)
                                                   (else v))))
                                         body))
                     (cons rator rands)))
               (cons rator rands)))))))

(define (atomic? e)
  (or (symbol? e)
      (starts-with? 'quote e)))

(define (effectless? e)
  (or (atomic? e)
      (starts-with? 'lambda e)))

(define (small? e)
  (or (atomic? e)
      (and (starts-with? 'lambda e)
           (null? (cadr e))
           (pair? (caddr e))
           (symbol? (car (caddr e)))
           (null? (cdr (caddr e))))))

(define (all ok? xs)
  (or (null? xs)
      (and (ok? (car xs))
           (all ok? (cdr xs)))))

;; XXX must ensure we don't substitute under a rebinding of the variable (r v).
;; (This could be done most simply by renaming all variables.)
(define (substitute r e)
  (if (symbol? e)
      (r e)
      (case (car e)
        ((quote) 
         e)
        ((lambda)
         `(lambda ,(cadr e)
            ,(substitute (lambda (v)
                           (if (memq v (cadr e)) v (r v)))
                         (caddr e))))
        ((letrec)
         (let* ((vs (map car (cadr e)))
                (new-r (lambda (v)
                         (if (memq v vs) v (r v)))))
         `(letrec ,(map (lambda (defn)
                          `(,(car defn)
                            ,(substitute new-r (cadr defn))))
                        (cadr e))
            ,(substitute new-r (caddr e)))))
        (else
         (map (lambda (sube) (substitute r sube)) e)))))

(define (count v e)
  (if (symbol? e)
      (if (eq? v e) 1 0)
      (case (car e)
        ((quote) 
         0)
        ((lambda)
         (if (memq v (cadr e))
             0
             (count v (caddr e))))
        ((letrec)
         (if (memq v (map car (cadr e)))
             0
             (+ (sum (map (lambda (defn)
                            (count v (cadr defn)))
                          (cadr e)))
                (count v (caddr e)))))
        (else
         (sum (map (lambda (sube) (count v sube)) e))))))

(define (sum ns)
  (foldr + 0 ns))

(define (starts-with? symbol x)
  (and (pair? x) (eq? (car x) symbol)))

(define (foldr f z xs)
  (if (eq? '() xs)
      z
      (f (car xs) (foldr f z (cdr xs)))))
