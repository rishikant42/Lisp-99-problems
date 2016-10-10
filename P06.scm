(define (reverse items)
  (define (iter item result)
    (if (null? item)
      result
      (iter (cdr item) (cons (car item) result))))
  (iter items '()))

(define (check list1 list2)
  (cond ((null? list1) true)
        ((= (car list1) (car list2)) (check (cdr list1) (cdr list2)))
        (else false)))

(define (palindrome? items)
  (check items (reverse items)))

;; Items of list should be integer
