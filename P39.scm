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

(define (prime-list a b)
  (cond ((> a b) '())
        ((prime? a) (cons a (prime-list (+ a 1) b)))
        (else (prime-list (+ a 1) b))))
