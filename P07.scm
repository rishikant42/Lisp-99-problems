(define nil '())

(define (my-flatten tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (my-flatten (car tree))
                      (my-flatten (cdr tree))))))
