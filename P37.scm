;; Given in ques: 
;; 
;; Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given number 
;; 
;; then , phi(m) = (p1 - 1) * p1 ** (m1 - 1) + (p2 - 1) * p2 ** (m2 - 1) + (p3 - 1) * p3 ** (m3 - 1) + ...
;; 
;; Above formula seems wrong.
;; 
;; More appporiate formula:
;; 
;; phi(m) = m (1 - 1/m1) (1 - 1/m2) (1 - 1/m3) .......
;;
;; refrence: https://en.wikipedia.org/wiki/Euler%27s_totient_function



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


;;;;;;;;;;;;;;;;;;;;;;;;;;; P37 ;;;;;;;;;;;;;;;;;;;;;;;;;

;; wrong soln

(define (phi m)
  (define (helper items result)
    (if (null? items)
      result
      (helper (cdr items) (* (- 1 (/ 1 (car items))) result))))
  (helper (prime-factors m) m))


;;;;;;;;;;;;;;;;;;;;; Test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1 ]=> (phi 315)

;; ;Value: 96

;; wrong answer, correct is 144

;; Explanation:

;; (phi 315)
;; 
;; (helper (3 3 5 7) 315)
;; (helper (3 5 7) (* (/ 2 3) 315))
;; (helper '(5 7) (* (/ 2 3) 210))
;; (helper ('7) (* (/ 4 5) 140))
;; (helper '() (* (/ 6 7) 112))
;; 96

;; why it is so? 
;; 1 ]=> (prime-factors 315)

;; ;Value 12: (3 3 5 7)

;; By formula, we don't want repeated prime-factor at the time multiplication, here 3 is repeated

;;;;;;;;;;;;;;;;; correct solution ;;;;;;;;;;;;;;;;;;;;;;

(define (compress items)
  (cond ((= (length items) 1) items)
        ((eq? (car items) (cadr items)) (compress (cdr items)))
        (else (cons (car items) (compress (cdr items))))))

(define (modify-phi m)
  (define (helper items result)
    (if (null? items)
      result
      (helper (cdr items) (* (- 1 (/ 1 (car items))) result))))
  (helper (compress (prime-factors m)) m))
