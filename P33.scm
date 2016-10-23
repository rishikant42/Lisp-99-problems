(define (gcd a b)
  (define (helper x y)
    (if (= y 0)
      x
      (gcd y (remainder x y))))
  (helper (abs a) (abs b)))

(define (coprime p q)
  (= (gcd p q) 1))
