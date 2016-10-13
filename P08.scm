(define nil '())

(define (remove x items)
  (filter (lambda (y) (not (= x y))) items))

(define (compress items)
  (cond ((= (length items) 1) items)
        ((eq? (car items) (cadr items)) (compress (cdr items)))
        (else (cons (car items) (compress (cdr items))))))
