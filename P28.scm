(define (insert-by-length x items)
  (cond ((null? items) (list x))
        ((<= (length x) (length (car items))) (cons x items))
        (else (cons (car items) (insert-by-length x (cdr items))))))


(define (lsort items)
  (if (null? items)
    '()
    (insert-by-length (car items) (lsort (cdr items)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (maximum items)
  (define (helper item res)
    (cond ((null? item) res)
          ((> (car item) res) (helper (cdr item) (car item)))
          (else (helper (cdr item) res))))
  (helper items (car items)))

(define (minimum items)
  (define (helper item res)
    (cond ((null? item) res)
          ((< (car item) res) (helper (cdr item) (car item)))
          (else (helper (cdr item) res))))
  (helper items (car items)))

(define (symbol-frequency symbol items)
  (define (helper item res)
    (cond ((null? item) (list symbol res))
          ((equal? (car item) symbol) (helper (cdr item) (+ res 1)))
          (else (helper (cdr item) res))))
  (helper items 0))

;; symbol-frequency procedure return the frequency (total no of given symbol in list) of symbol in items

;; 1 ]=> (symbol-frequency 'a '(b a c a d a))

;; ;Value: (a 3)              
;;

(define (len-frq-pair start end items)
  (define (helper count res)
    (if (> count end) 
      res
      (helper (+ count 1) (cons (symbol-frequency count items) res))))
  (helper start '()))

(define (frequency x) (cadr x))
(define (symbol x) (car z))

(define (insert-by-frq x items)
  (cond ((null? items) (list x))
        ((<= (frequency x) (frequency (car items))) (cons x items))
        (else (cons (car items) (insert-by-frq x (cdr items))))))

(define (sort-by-frq items)
  (if (null? items)
    '()
    (insert-by-frq (car items) (sort-by-frq (cdr items)))))

(define (len-item len items)
  (filter (lambda (y) (= (length y) len)) items))

(define (len-set lfpair)
  (if (null? lfpair)
    '()
    (cons (caar lfpair) (len-set (cdr lfpair)))))

(define (modify-flatten tree)
  (cond ((null? tree) '())
        ((not (pair? (car tree))) (list tree))
        (else (append (modify-flatten (car tree))
                      (modify-flatten (cdr tree))))))

(define (extract lfpair items)
  (let ((length-set (len-set lfpair)))
    (modify-flatten (map (lambda (x) (len-item x items)) length-set))))


(define (lfsort items)
  (let ((len-set (map length items)))
    (let ((max-len (maximum len-set))
          (min-len (minimum len-set)))
      (let ((pair (len-frq-pair min-len max-len len-set)))
        (let ((lfpair (sort-by-frq pair)))
          (extract lfpair items))))))
