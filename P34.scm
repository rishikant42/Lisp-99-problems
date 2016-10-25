(define (gcd a b)
  (define (helper x y)
    (if (= y 0)
      x
      (gcd y (remainder x y))))
  (helper (abs a) (abs b)))

(define (coprime? p q)
  (= (gcd p q) 1))

(define (totient-phi m)
  (define (helper r)
    (cond ((= r 0) 0)
          ((coprime? m r) (+ 1 (helper (- r 1))))
          (else (helper (- r 1)))))
  (helper m))

(define (totient-phi2 m)
  (define (helper r)
    (cond ((= r m) 0)
          ((coprime? m r) (+ 1 (helper (+ r 1))))
          (else (helper (+ r 1)))))
  (if (= m 1)
    1
    (helper 1)))
