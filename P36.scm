;;;;;;;;;;;;;;;;;;;; P35 ;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;; P10 ;;;;;;;;;;;;;;;;;

(define (pack items)
  (define (helper remain sub-list res)
    (cond ((null? remain) (reverse (cons sub-list res)))
          ((equal? (car remain) (car sub-list)) (helper (cdr remain) (cons (car remain) sub-list) res))
          (else (helper (cdr remain) (list (car remain)) (cons sub-list res)))))
  (helper (cdr items) (list (car items)) '()))

(define (encode items)
  (let ((pack-items (pack items)))
    (map (lambda (item)
           (list (car item) (length item)))
         pack-items)))

;;;;;;;;;;;;;;;;;;;; P36 ;;;;;;;;;;;;;;;;;;

(define (prime-factors-mult n)
  (let ((factors (prime-factors n)))
    (encode factors)))
