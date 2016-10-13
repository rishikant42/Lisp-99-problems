 (define encode-direct 
   (lambda (xs) 
     (if (null? xs) '() 
       (let ((pkg (lambda (N E) 
                    (if (= N 1) E (list N E))))) 
         (let loop ((rest (cdr xs)) 
                    (E (car xs)) 
                    (N 1)) 
           (cond ((null? rest) 
                  (list (pkg N E))) 
                 ((eq? E (car rest)) 
                  (loop (cdr rest) E (+ N 1))) 
                 (else 
                   (cons (pkg N E) (loop (cdr rest) (car rest) 1))))))))) 
