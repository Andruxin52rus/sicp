#lang racket

(define (fixed-point func starting-point tolerance)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try-next func starting-point)
        (let ((next-point (func starting-point)))
            (if (close-enough? starting-point next-point)
                starting-point
                (try-next func next-point))))
    (try-next func starting-point)
)

(define tolerance 0.00001)
(fixed-point cos 1.0 tolerance)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0 tolerance)
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0 tolerance)  ; calc golden ratio