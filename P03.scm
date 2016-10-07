(define (element-at items n)
  (if (= n 1)
    (car items)
    (element-at (cdr items) (- n 1))))

;; Note: Give condition in question
;; The first element in the list is number 1. (not 0)
;; Generally first element in list is consider as 0
