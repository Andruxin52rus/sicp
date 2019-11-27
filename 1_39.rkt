#lang racket

(define (cont-frac x n d k)
    (define (cont-frac-iter i result)  ; iterative version
        (if (> i 0)
            (cont-frac-iter (- i 1) (/ (n x i) (+ (d i) result)))
            result))
    (cont-frac-iter k 0))

(define (n x i)
    (if (> i 1)
        (- (* x x))
        x))
        
(define (tan-cf x k)
    (cont-frac x n (lambda (i) (- (* 2 i) 1)) k))

(define (deg-to-rad deg)
    (* (/ deg 180) pi))
    
(tan-cf (deg-to-rad 0) 32)
(tan-cf (deg-to-rad 30) 32)
(tan-cf (deg-to-rad 45) 32)
(tan-cf (deg-to-rad 60) 32)
(tan-cf (deg-to-rad 90) 32)