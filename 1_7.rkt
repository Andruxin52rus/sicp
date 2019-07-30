#lang racket
(define (average x y)
  (/ (+ x y) 2)
  )

(define (square x)
  (* x x)
 )

; Newton's method for square root calc
(define (improve guess x)
    (average guess (/ x guess))
  )

; bad accuracy on small numbers (<< 1)
(define (sqrt1 x)
  (define guess 1)
  (define eps 0.001)
  (define (good-enough? guess x)
    (< (abs (- x (square guess))) eps)    
    )
  (define (sqrt-iter guess x)
    (define new-guess (average guess (/ x guess)))
    (if (good-enough? new-guess x)
        new-guess
        (sqrt-iter (improve new-guess x) x))
    )

  (sqrt-iter guess x)
  )

; sqrt2 is dramatically more accurate on small numbers (<< 1) or at least not worse than sqrt1 on any number
(define (sqrt2 x)
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
(sqrt1 1522756) ; 1234
(sqrt2 1522756)

(sqrt1 9) ; 3
(sqrt2 9)

(sqrt1 0.04) ; 0.2
(sqrt2 0.04)

(sqrt1 0.0001) ; 0.01
(sqrt2 0.0001)