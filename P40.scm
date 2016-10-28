(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n divisor)
  (cond ((> (square divisor) n) n)
        ((devides? n divisor) divisor)
        (else (find-divisor n (+ divisor 1)))))

(define (devides? n divisor)
  (= (remainder n divisor) 0))

(define (prime? n)
  (if (<=  n 1)
    false
    (= (smallest-divisor n) n)))

(define (goldbach n)
  (define (helper a b)
    (if (and (prime? a) (prime? b))
      (list a b)
      (helper (+ a 1) (- b 1))))
  (helper 0 n))
