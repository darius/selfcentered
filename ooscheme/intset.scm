;; An example ooscheme program, from William Cook's essay on OOP vs. ADTs
;; http://www.cs.utexas.edu/~wcook/Drafts/2009/essay.pdf

(define empty
  (make _
    ('empty? ()  #t)
    ('has? (k)   #f)
    ('adjoin (k) (make-adjoin k empty))
    ('merge (s)  s)))

(define (make-adjoin n s)
  (if ('has? s n)
      s
      (make extension
        ('empty? ()  #f)
        ('has? (k)   ('= n k))
        ('adjoin (k) (make-adjoin k extension))
        ('merge (s)  (make-merge extension s)))))

(define (make-merge s1 s2)
  (make meld
    ('empty? ()  (and ('empty? s1) ('empty? s2)))
    ('has? (k)   (or ('has? s1 k) ('has? s2 k)))
    ('adjoin (k) (make-adjoin k meld))
    ('merge (s)  (make-merge meld s))))
