#lang racket

(define (apply-op op term-func first-item next-item-func last-item gt-func)
    (define (iter item)
        (if (gt-func item last-item)
        0
        (op (term-func item)
            (iter (next-item-func item)))))
  (iter first-item))

; This may be not the most popular way to define complex simpson integral. In my solution n approximate integrals are calculated, as opposite to, for instance, Simpson method explained in Wiki with its n/2 calculations
; This doesn't relate to complexity, the numbers n and n/2 just reflect conventions on how much calculations we should for particular n set
(define (simpson-integral func a b n)
    (define step (/ (- b a) n))
    (define (square-approx a)
        (+ (func a) (func (+ a step)) (* 4 (func (/ (+ a (+ a step)) 2)))))
    (define (next-bucket a)
        (+ a step))
    (define (gt first second)
        (> first second))
    (/ (* step (apply-op + square-approx a next-bucket (- b step) gt)) 6))

(define (cube x)
    (* x x x))

(simpson-integral cube 0 1 1)
(simpson-integral cube 0 1 2)
(simpson-integral cube 0 1 1000)

(simpson-integral cube 0 2 1)
(simpson-integral cube 0 2 2)
(simpson-integral cube 0 2 1000)