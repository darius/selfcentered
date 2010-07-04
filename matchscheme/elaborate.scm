(include "gambit-macros.scm")

(define (elaborate e)
  (cond ((symbol? e) e)
        ((or (boolean? e) (number? e)) `',e)
        ((not (pair? e)) (error '"Bad syntax" e))
        ((lookup (car e) macros)
         => (lambda (expand) (elaborate (expand e))))
        ((lookup (car e) core-syntax)
         => (lambda (expand) (expand e)))
        (else (map elaborate e))))

(define (lookup key a-list)
  (cond ((assv key a-list) => cadr)
        (else #f)))

(define core-syntax
  `((quote ,(mlambda ((_ datum) `',datum)))
    (lambda ,(mlambda ((_ vars . body)
                       `(lambda ,vars ,(elaborate-seq body)))))
    (letrec ,(mlambda ((_ defns . body)
                       `(letrec ,(map (mlambda ((v e) `(,v ,(elaborate e))))
                                      defns)
                          ,(elaborate-seq body)))))
    (begin ,(mlambda ((_ . es)
                      (elaborate-seq es))))))

(define (elaborate-seq es)
  (mcase es
    ((e) (elaborate e))
    ((e . es) `((lambda (,(gensym)) ,(elaborate-seq es))
                ,(elaborate e)))))

(define macros 
  (cons 
   ;; This awkward line to avoid a nested quasiquote in the quasiquoted main
   ;; macros table just below:
   (cons 'quasiquote (cons (mlambda ((_ q) (expand-quasiquote q)))
                           '()))
   `((local ,(mlambda
              ((_ defns . body)
               `(letrec ,(foldr expand-defn '() defns) . ,body))))
     (let ,(mlambda
            ((_ bindings . body)
             `((lambda ,(map car bindings) . ,body)
               . ,(map cadr bindings)))))
     (if ,(mlambda
           ((_ test if-true)
            `(%unless ,test (lambda () #f) (lambda () ,if-true)))
           ((_ test if-true if-false)
            `(%unless ,test (lambda () ,if-false) (lambda () ,if-true)))))
     (cond ,(mlambda
             ((_) #f)
             ((_ ('else . es)) `(begin . ,es))
             ((_ (e) . clauses) `(or ,e (cond . ,clauses)))
             ((_ (e1 '=> e2) . clauses)
              (let ((test-var (gensym)))
                `(let ((,test-var ,e1))
                   (if ,test-var
                       (,e2 ,test-var)
                       (cond . ,clauses)))))
             ((_ (e . es) . clauses)
              `(if ,e (begin . ,es) (cond . ,clauses)))))
     (or ,(mlambda
           ((_) #f)
           ((_ e) e)
           ((_ e . es)
            `(let ((v ,e) (thunk (lambda () . ,es)))
               (%unless v thunk (lambda () v))))))
     (mlambda ,(mlambda
                ((_ . clauses)
                 (let ((subject (gensym)))
                   `(lambda (,subject) ,(expand-mlambda subject clauses))))))
     (mcase ,(mlambda
              ((_ subject-exp . clauses)
               `((mlambda . ,clauses) ,subject-exp)))))))

(define (expand-defn defn expanded-defns)
  (mcase defn
    ((_ (: name symbol?) e)
     `((,name ,e) . ,expanded-defns))
    ((_ (name . vars) . body)
     `((,name (lambda ,vars . ,body)) . ,expanded-defns))
    (('include filename)
     (mcase (snarf filename)
;       ((''magic . _) expanded-defns)
       (('magic . _) expanded-defns)
;       (('abracadabra . _) expanded-defns)
       (included-defns (foldr expand-defn expanded-defns included-defns))))))

(define (expand-quasiquote e)
  (mcase e
    (('unquote e1) e1)
    ((qcar . qcdr) `(cons ,(expand-quasiquote qcar)
                          ,(expand-quasiquote qcdr)))
    (else `',e)))

(define (expand-mlambda subject clauses)
  (local
      ((define (expand-clause clause else-exp)
         (let ((pattern (car clause))
               (then-exp `(begin . ,(cdr clause)))
               (fail (gensym)))
           `(let ((,fail (lambda () ,else-exp)))
              ,(expand-pattern pattern then-exp `(,fail)))))

       (define (expand-pattern pattern then-exp else-exp)
         (let ((test-constant
                (lambda (constant)
                  `(if (eqv? ,subject ',constant) ,then-exp ,else-exp))))
           (mcase pattern
             ('_ then-exp)
             (('quote constant) (test-constant constant))
             ((: v symbol?) `(let ((,pattern ,subject)) ,then-exp))
             ((': name predicate)
              `(if (,predicate ,subject)
                   (let ((,name ,subject)) ,then-exp)
                   ,else-exp))
             ((pcar . pcdr)
              `(if (pair? ,subject)
                   (mcase (car ,subject)
                     (,pcar (mcase (cdr ,subject)
                              (,pcdr ,then-exp)
                              (_ ,else-exp)))
                     (_ ,else-exp))
                   ,else-exp))
             (_ (test-constant pattern))))))

    (foldr expand-clause '(%match-error) clauses)))
