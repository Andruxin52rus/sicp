#lang racket

(define (square x)
  (* x x))
(define (even? x)
  (= (/ x 2) 0))
(define (fast-expt-iter a b n)
  (if (= n 0)
      a
      (if (even? n)
          (fast-expt-iter a (square b) (/ n 2))
          (fast-expt-iter (* a b) b (- n 1))
          )
      ))

; calc exponent for logarithmic time
(define (fast-expt b n)
  (fast-expt-iter 1 b n))

(fast-expt 2 6)
(fast-expt 4 3)
(fast-expt 5 3)
(fast-expt 2 0)
(fast-expt -1 3)