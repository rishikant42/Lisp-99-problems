(define (my-last items)
  (if (null? (cdr items))
    items
    (my-last (cdr items))))
