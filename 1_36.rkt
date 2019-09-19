#lang racket

(define (fixed-point func starting-point tolerance)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try-next func starting-point counter)
        (display starting-point)
        (newline)
        (let ((next-point (func starting-point)))
            (if (close-enough? starting-point next-point)
                (values starting-point counter)
                (try-next func next-point (+ counter 1))))
     )
    (newline)
    (try-next func starting-point 0)
)

(define tolerance 0.00001)
(define log_1000 (log 1000))
(define (average v1 v2)
    (/ (+ v1 v2) 2))


(let-values (((fixed-point-res count) (fixed-point (lambda (x) (/ log_1000 (log x))) 10 tolerance)))
    (display "Iterations number using fixed-point: ")
    (display count))

(newline)
(let-values (((fixed-point-res count) (fixed-point (lambda (x) (average x (/ log_1000 (log x)))) 10 tolerance)))
    (display "Iterations number using fixed-point w/ average dumping: ")
    (display count))
