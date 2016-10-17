;;;;;;;;;;;;;;;;;;;; PROBLEM-22 ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (range start end)
  (if (<= start end)
    (iter1 start end '())
    (iter2 start end '())))

(define (iter1 s e result)
  (if (> s e)
    result
    (iter1 (+ s 1) e (append result (list s)))))

(define (iter2 s e result)
  (if (< s e)
    result
    (iter2 (- s 1) e (append result (list s)))))

;;;;;;;;;;;;;;;;;; PROBLEM-23 ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(define (element-at items n)
  (if (= n 1)   
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lotto-select a b)
  (let ((items (range a b)))
    (let ((len (random (length items))))      ;; len ==> length of random list  (select random no for genrated list)
      (rnd-select items len))))
