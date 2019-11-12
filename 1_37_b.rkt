#lang racket

(define (cont-frac n d k)
    (define (cont-frac-iter i result)  ; iterative version
        (if (> i 0)
            (cont-frac-iter (- i 1) (/ (n i) (+ (d i) result)))
            result))
    (cont-frac-iter k 0))

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 2)
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 4)
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 6)
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 8)
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 12)