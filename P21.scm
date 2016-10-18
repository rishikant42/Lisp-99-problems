(define (insert-at element items position)
  (define (iter start end count)
    (if (= count 1)
      (append start (list element) end)
      (iter (cons (car end) start)
            (cdr end)
            (- count 1))))
  (iter '() items position))