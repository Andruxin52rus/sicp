#lang racket

(define (cont-frac n d k)
    (define (cont-frac-iter i result)  ; iterative version
        (if (> i 0)
            (cont-frac-iter (- i 1) (/ (n i) (+ (d i) result)))
            result))
    (cont-frac-iter k 0))

(define (d i)
    (if (= (remainder (- i 2) 3) 0)
        (/ (+ (* 2 i) 2) 3)
        1))
        
(cont-frac (lambda (i) 1.0) d 2)
(cont-frac (lambda (i) 1.0) d 4)
(cont-frac (lambda (i) 1.0) d 6)
(cont-frac (lambda (i) 1.0) d 8)
(cont-frac (lambda (i) 1.0) d 10)
(cont-frac (lambda (i) 1.0) d 12)

