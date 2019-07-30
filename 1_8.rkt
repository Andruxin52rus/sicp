#lang racket

(define (square x)
  (* x x)
 )

; Newton's method for cube root calc
(define (improve guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3)
  )

; cubic root
(define (cbrt x)
  (define guess 1)
  (define eps 0.001)
  (define (good-enough? old-guess new-guess)
    (< (abs (- new-guess old-guess)) (* eps old-guess))    
    )
  (define (sqrt-iter old-guess new-guess x)
    (if (good-enough? old-guess new-guess)
        new-guess
        (sqrt-iter new-guess (improve new-guess x) x))
    )

  (sqrt-iter guess (+ guess 1) x)
  )

; tests
(cbrt 27)
(cbrt 1000)
(cbrt 0.001)