(define nil '())

(define (drop items n)
  (define (fn item counter)
    (cond ((null? item) nil)
          ((= counter 1) (drop (cdr item) n))
          (else (cons (car item) (fn (cdr item) (-  counter 1))))))
  (fn items n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (filter condition items)
  (cond ((null? items) nil)
        ((condition (car items)) (cons (car items) (filter condition (cdr items))))
        (else (filter condition (cdr items)))))

(define (remove x items)
  (filter (lambda (item) (not (eq? x item)))
       items))
