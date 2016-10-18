(define (element-at items n)
  (if (= n 1)                                ;; list index start from 1 (not 0)
    (car items)
    (element-at (cdr items) (- n 1))))

(define (remove x items)
  (filter (lambda (y) (not (eq? y x))) items))

(define (remove-at items n)
  (define (iter item count)
    (if (= count 1)
      (remove (car item) items)
      (iter (cdr item) (- count 1))))
  (iter items n))

(define (rnd-select items n)
  (define (iter item result count)
    (if (= (length item) 0)
      result
      (let ((rand-no (+ 1 (random (length item)))))
        (let ((rand-elem (element-at item rand-no)))
          (if (= count 0)
            result
            (iter (remove rand-elem item) (cons rand-elem result) (- count 1)))))))
  (iter items '() n))

(define (rnd-permu items)
  (rnd-select items (length items)))
