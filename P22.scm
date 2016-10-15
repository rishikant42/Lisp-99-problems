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

;;;;;;;;;;;;;;;;;;;; Alternate ;;;;;;;;;;;;;;;;;;;;
;; 
;; (define (range start end)
;;   (if (<= start end)
;;     (iter1 start end '())
;;     (reverse (iter1 end start '()))))
