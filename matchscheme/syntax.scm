(define (list-of ok?)
  (lambda (xs) (all ok? xs)))

(define (let-decl? x)
  (match? '((: _ ,symbol?) _) x))

(defun let-decls-p (x)
  (and (listp x)
       (every (lambda (decl) (wok-match-p ((: _ symbolp) _) decl))
              x)))

;; XXX stuff that starts with a keyword but doesn't match a pattern
;; should be flagged as a syntax error. So this giant list of rules
;; isn't really the right organization.

(

 (('quote datum)
  `(%quote ,datum))

 (('lambda (: vars (list-of symbol?)) . exps)
  `(%lambda ,vars (begin ,@exps)))

 (('begin) #f)
 (('begin exp) exp)
 (('begin exp . exps) `((lambda (v thunk) (thunk))
                        ,exp
                        (lambda () ,@exps)))

 (('if e1 e2) `(if ,e1 ,e2 #f))
 (('if e1 e2 e3) `(,e1 (lambda () ,e2) (lambda () ,e3)))

 (('and) #t)
 (('and e) e)
 (('and e . es) `(if ,e (and ,@es) #f))

 (('or) #f)
 (('or e) e)
 (('or e . es) `((lambda (v rest) (if v v (rest)))
                 ,e
                 (lambda () (or ,@es))))

 (('let (: name symbol?) (: decls (list-of let-decl?)) . exps)
  `((letrec ((,name (lambda ,(map car decls) ,@exps)))
      ,name)
    ,@(map cadr decls)))
 (('let (: decls (list-of let-decl?)) . exps)
  `((lambda ,(map car decls) ,@exps)
    ,@(map cadr decls)))

 (('let* () . exps)
  `(begin ,@exps))
 (('let* (decl . decls) . exps)
  `(let (,decl)
     (let* ,decls
       ,@exps)))

 (('cond) #f)
 (('cond ('else . exps))
  `(begin ,@exps))
 (('cond ('else . exps) . clauses)
  (error "ELSE clause is not last in COND"))
 (('cond (exp) . clauses)
  `(or ,exp (cond ,@clauses)))
 (('cond (exp '=> receiver) . clauses)
  `((lambda (subject receiver otherwise)
      (if subject (receiver subject) (otherwise)))
    ,exp
    ,receiver
    (lambda () (cond ,@clauses))))
 (('cond (exp . exps) . clauses)
  `(if ,exp
       (begin ,@exps)
       (cond ,@clauses)))

 (('case subject . clauses)
  (let ((var (gensym)))
    `(let ((,var ,subject))
       (__case__ ,var ,@clauses))))

 (('__case__ v ('else . exps)) 
  `(begin ,@exps))
 (('__case__ v ((: values pair?) . exps) . clauses)
  `(cond ((member ,v ',values) ,@exps)
         (else ('__case__ ,v ,@clauses))))
 (('__case__ v (value . exps) . clauses)
  `(cond ((equal ,v ',value) ,@exps)
         (else (__case__ ,v ,@clauses))))

)
