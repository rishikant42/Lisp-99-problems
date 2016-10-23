;; Generate the all combinations of K objects chosen from the N elements of a list ;;

(define (split items n)
  (define (fn first rest count)
    (if (= count 0)
      (list first rest)
      (fn (append first (list (car rest))) 
          (cdr rest) 
          (- count 1))))
  (fn '() items n))

(define (single-combine n items)
  (define (iter first rest res)
    (if (null? rest)
      res
      (iter first
            (cdr rest)
            (cons (append first (list (car rest))) res))))
  (iter (car (split items (- n 1)))
        (cadr (split items (- n 1)))
        '()))

(define (remove x items) 
  (filter (lambda (y) (not (eq? x y))) items))

(define (modify-flatten tree)
  (cond ((null? tree) '())
        ((not (pair? (car tree))) (list tree))
        (else (append (modify-flatten (car tree))
                      (modify-flatten (cdr tree))))))

(define (combination n items)
  (modify-flatten (map (lambda (x) 
                         (single-combine n (cons x (remove x items) ))) 
                       items)))

;; 1 ]=> (combination 3 '(a b c d e))

;;;Value 11: ((a b e) (a b d) (a b c) (b a e) (b a d) (b a c) (c a e) (c a d) (c a b) (d a e) (d a c) (d a b) (e a d) (e a c) (e a b))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;P26 solution;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Generate the combinations of K distinct objects chosen from the N elements of a list;;;;;

(define (combination1 k xs) 
  (cond ((null? xs) '()) 
        ((= k 1) (map list xs)) 
        (else (append (map (lambda (x) (cons (car xs) x)) 
                           (combination1 (- k 1) (cdr xs))) 
                      (combination1 k (cdr xs))))))

;; 1 ]=> (combination1 3 '(a b c d e))

;; ;Value 12: ((a b c) (a b d) (a b e) (a c d) (a c e) (a d e) (b c d) (b c e) (b d e) (c d e))
