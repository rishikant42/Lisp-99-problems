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

(define (goldbach-list a b)
  (define (helper count)
    (cond ((> count b) '())
          ((even? count) (cons (list count (goldbach count)) (helper (+ count 1))))
          (else (helper (+ count 1)))))
  (helper a))
 
;; Test
 
;; 1 ]=> (goldbach-list 9 20)
;; 
;; ;Value 15: ((10 (3 7)) (12 (5 7)) (14 (3 11)) (16 (3 13)) (18 (5 13)) (20 (3 17)))


;;;;;;;;;;;;;;;;;;;;;;;;;; Second part of ques ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (goldbach-list-modify a b val)
  (define (helper count)
    (cond ((> count b) '())
          ((even? count) (let ((res (goldbach count)))
                           (if (and (> (car res) val) (> (cadr res) val))
                             (cons (list count res) (helper (+ count 1)))
                             (helper (+ count 1)))))
          (else (helper (+ count 1)))))
  (helper a))

;; Test

;; 1 ]=> (goldbach-list-modify 900 2000 50)

;; ;Value 18: ((992 (73 919)) (1382 (61 1321)) (1856 (67 1789)) (1928 (61 1867)))
