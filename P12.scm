(define nil '())

(define (my-flatten tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (my-flatten (car tree))
                      (my-flatten (cdr tree))))))

(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items))
          (map proc (cdr items)))))

(define (expend items)
  (define (sub n)
    (if (= n 0)
      nil
      (cons (cadr items) (sub (- n 1)))))
  (sub (car items)))

;;;;;;;;;;;;;;;; Decode version of problem P10 ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (decode items)
  (my-flatten (map expend items)))

;;;;;;;;;;;;;;;; Decode version of problem P11 ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (decode-modified items)
  (my-flatten (map  (lambda (x) (if (not (pair? x)) (list x) (expend x)))
                    items)))

;; 1 ]=> (load "P10.scm")
;; 1 ]=> (load "P11.scm")
;; 1 ]=> (load "P12.scm")
;;
;; 1 ]=> (define e (encode '(a a a b b a c c c d d e f f)))
;; 
;; ;Value: e
;; 
;; 1 ]=> e
;; 
;; ;Value 11: ((3 a) (2 b) (1 a) (3 c) (2 d) (1 e) (2 f))
;; 
;; 1 ]=> (decode e)
;; 
;; ;Value 12: (a a a b b a c c c d d e f f)
;; 
;; 1 ]=> (decode-modified e)
;; 
;; ;Value 13: (a a a b b a c c c d d e f f)
;; 
;; 1 ]=> (define ee (encode-modified '(a a a b b a c c c d d e f f)))
;; 
;; ;Value: ee
;;
;; 1 ]=> ee

;; ;Value 14: ((3 a) (2 b) a (3 c) (2 d) e (2 f))
;; 
;; 1 ]=> (decode-modified ee)
;; 
;; ;Value 15: (a a a b b a c c c d d e f f)
