(define nil '())

(define (dupli items)
  (if (null? items) 
    nil
    (cons (car items) 
          (cons (car items) (dupli (cdr items))))))
