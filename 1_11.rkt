#lang racket

; pure recurcive procedure
(define (fib3-recursive x)
    (if (< x 3)
        x
        (+ (fib3-recursive (- x 1)) (fib3-recursive (- x 2)) (fib3-recursive (- x 3)))))

; recurcive procedure that will be executed as linear iterative process due to tail recursion
(define (fib3-iterative x)
    (fib3-iterative-impl 0 1 2 x))    
(define (fib3-iterative-impl num1 num2 num3 x)
    (if (= x 0)
        num1
        (fib3-iterative-impl num2 num3 (+ num1 num2 num3) (- x 1))))

(fib3-recursive 0)
(fib3-recursive 1)
(fib3-recursive 2)
(fib3-recursive 3)
(fib3-recursive 4)
(fib3-recursive 5)
(fib3-recursive 10)

(fib3-iterative 0)
(fib3-iterative 1)
(fib3-iterative 2)
(fib3-iterative 3)
(fib3-iterative 4)
(fib3-iterative 5)
(fib3-iterative 10)