(define (remove x items)
  (filter (lambda (y) (not (eq? y x))) items))

(define (remove-at items n)
  (define (iter item count)
    (if (= count 1)
      (remove (car item) items)
      (iter (cdr item) (- count 1))))
  (iter items n))
