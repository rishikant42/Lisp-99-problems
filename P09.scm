(define pack 
  (lambda (items) 
    (if (null? items) '() 
      (let loop ((rest (cdr items)) 
                 (pkg (list (car items)))) 
        (cond ((null? rest) (list pkg)) 
              ((eq? (car pkg) (car rest)) (loop (cdr rest) (cons (car rest) pkg))) 
              (else (cons pkg (loop (cdr rest) (list (car rest))))))))))
