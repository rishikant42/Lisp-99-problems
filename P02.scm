(define (my-but-last items)
  (if (null? (cddr items))
    items
    (my-but-last (cdr items))))
