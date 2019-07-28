#lang racket
(define (square a)
    (* a a))
    
(define (sum-of-squares a b)
    (+ (square a) (square b)))

(define (func a b c)
    (if (= a (min a b c))
        (sum-of-squares b c)
        (if (= b (min a b c))
            (sum-of-squares a c)
            (sum-of-squares a b))))

(func 2 1 3)
(func 3 1 2)
(func 3 2 1)