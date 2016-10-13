(define nil '())

(define (expend items)
  (define (sub n)
    (if (= n 0)
      nil
      (cons (cadr items) (sub (- n 1)))))
  (sub (car items)))

(define (my-flatten tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (my-flatten (car tree))
                      (my-flatten (cdr tree))))))

(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items))
          (map proc (cdr items)))))

(define (decode items)
  (my-flatten (map expend items)))
