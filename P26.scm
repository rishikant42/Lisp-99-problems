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
