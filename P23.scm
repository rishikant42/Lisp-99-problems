(define (element-at items n)
  (if (= n 1)                                ;; list index start from 1 (not 0)
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


;; primitive procedure "(random n)" generate a random no b/w 0 to (n - 1)
;; we need to generate random no b/w 1 to n so that we can get proper element from "(element-at items n)" defined procedure
;; "(element-at items n)" doesn't take 0 as argument, bcoz we are considering first element of list have index 1
;; we can get rid of adding 1 in random no if we consider list first item have index 0 
;; for consider list first item have index 0, replace "(if (= n 1) " with "(if (= n 0) " in "(element-at items n)" procedure

(define (rnd-select items n)
  (define (iter result count)
    (let ((rand-no (+ 1 (random (length items)))))
      (if (= count 0)
        result
        (iter (cons (element-at items rand-no)
                    result)
              (- count 1)))))
  (iter '() n))
 
;; 1 ]=> (rnd-select '(a b c d e f) 3)
;; 
;; ;Value 20: (b b c)
;;

;; By above procedure, element of resultant list can be repeated

;;;;;;;;;;;;;;;;;;;Alternate (All element of resultant list will be distinct) ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rnd-select2 items n)
  (define (iter item result count)
    (let ((rand-no (+ 1 (random (length item)))))
      (let ((rand-elem (element-at item rand-no)))
        (if (= count 0)
          result
          (iter (remove rand-elem item) (cons rand-elem result) (- count 1))))))
  (iter items '() n))
