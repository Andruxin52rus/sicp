#lang racket

(define (double x)
  (+ x x))
(define (halve x)
  (/ x 2))
(define (even? x)
  (= (/ x 2) 0))
(define (fast-mul-iter a b n)
  (if (= n 0)
      a
      (if (even? n)
          (fast-mul-iter a (double b) (halve n))
          (fast-mul-iter (+ a b) b (- n 1))
          )
      ))

; calc product for logarithmic time
(define (fast-mul b n)
  (if (< n 0)
      (- (fast-mul-iter 0 b (- n)))
      (fast-mul-iter 0 b n)))

(fast-mul 2 6)
(fast-mul 4 3)
(fast-mul 5 3)
(fast-mul 2 0)
(fast-mul -1 3)
(fast-mul -1 -3)