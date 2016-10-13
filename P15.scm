(define nil '())

(define (repeat item count)
  (if (= count 0)
    nil
    (cons item (repeat item (- count 1)))))

(define (my-flatten tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (my-flatten (car tree))
                      (my-flatten (cdr tree))))))

(define (repli items n)
  (my-flatten (map (lambda (x) (repeat x n)) items)))

(define (repli2 items n)
  (if (null? items)
    nil
    (append (repeat (car items) n)
            (repli2 (cdr items) n))))
