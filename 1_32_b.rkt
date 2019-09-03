#lang racket

(define (accumulate op-func null-term term-func first-item next-item-func last-item gt-func)
    (define (iter item result)
        (if (gt-func item last-item)
        result
        (iter (next-item-func item) (op-func result (term-func item)))))
  (iter first-item null-term))

(define (factorial n)
    (define (identity x)
        x)
    (define (next x)
        (+ x 1))
    (define (gt first second)
        (> first second))
    (accumulate * 1 identity 1 next n gt))

(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)
(factorial 6)

(define (pi-approx n)
    (define (square x)
        (* x x))
    (define (term x)
        (/ (* x (+ x 2))(square (+ x 1))))
    (define (next x)
        (+ x 2))
    (define (gt first second)
        (> first second))
    (* 4 (accumulate * 1 term 2 next (+ 3 (* 2 (- n 1))) gt)))

(pi-approx 1)
(pi-approx 2)
(pi-approx 3)
(pi-approx 4)
(pi-approx 5)
(pi-approx 6)
(pi-approx 60)
(pi-approx 600)

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
    (/ (* step (accumulate + 0 square-approx a next-bucket (- b step) gt)) 6))

(define (cube x)
    (* x x x))

(simpson-integral cube 0 1 1)
(simpson-integral cube 0 1 2)
(simpson-integral cube 0 1 1000)

(simpson-integral cube 0 2 1)
(simpson-integral cube 0 2 2)
(simpson-integral cube 0 2 1000)