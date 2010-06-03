(define mcase-expander
  (lambda (exp . cases)
    (let ((subject (gensym)))

      (define expand-cases
	(lambda (cases)
	  (cond ((null? cases)
	         `(bad-match))
	        ((and (null? (cdr cases))
	              (starts-with? 'else (car cases)))
		 `(begin ,@(cdar cases)))
		(else
		 (let ((fail (gensym)))
		   `(let ((,fail
			   (lambda () ,(expand-cases (cdr cases)))))
		      ,(expand-case (caar cases) (cdar cases) fail)))))))

      (define expand-case
	(lambda (pattern body fail)

	  (define test-constant
	    (lambda (constant)
	      `(cond ((eqv? ,subject ',constant)
		      ,@body)
		     (else (,fail)))))

	  (cond ((symbol? pattern)
		 `(let ((,pattern ,subject))
		    ,@body))
		((not (pair? pattern))
		 (test-constant pattern))
		((starts-with? 'quote pattern)
		 (test-constant (cadr pattern)))
		(else
		 `(if (pair? ,subject)
		      (match.mcase (car ,subject)
			(,(car pattern) (match.mcase (cdr ,subject)
					  (,(cdr pattern) ,@body)
					  (else (,fail))))
			(else (,fail)))
		      (,fail))))))

      `(let ((,subject ,exp))
	 ,(expand-cases cases)))))

(define (starts-with? symbol x)
  (and (pair? x) (eq? (car x) symbol)))

(define gensym-counter 0)

(define (gensym)
  (set! gensym-counter (+ gensym-counter 1))
  (string->symbol (string-append ":G" (number->string gensym-counter))))
