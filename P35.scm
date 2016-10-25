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

(define (prime-factors n)
  (define (helper start num res)
    (cond ((= num 1) res)
          ((and (prime? start) (devides? num start)) (helper start (/ num start) (append res (list start))))
          (else (helper (+ start 1) num res))))
  (helper 2 n '()))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Alternate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reduce op init items)
  (if (null? items)
    init
    (op (car items)
        (reduce op init (cdr items)))))

(define (reduce* * items)
  (reduce * 1 items))

(define (prime-factors2 n)
  (define (helper start num res)
    (cond ((= n (reduce* * res)) res)
          ((and (prime? start) (devides? num start)) (helper start (/ num start) (append res (list start))))
          (else (helper (+ start 1) num res))))
  (helper 2 n '()))
