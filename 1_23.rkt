#lang racket

(define (divide? x y)
    (= (remainder x y) 0))

(define (next test)
    (if (<= test 2)
        (+ test 1)
        (+ test 2)))

(define (smallest-divisor-iter n test)
    (cond ((> (* test test) n) n)
          ((divide? n test) test)
          (else (smallest-divisor-iter n (next test)))))

(define (smallest-divisor n)
    (smallest-divisor-iter n 2))
    
(define (prime? n)
    (= (smallest-divisor n) n))

(define (start-prime-test n start-time)
    (cond ((prime? n)
           (newline)
           (display n)
           (display " *** ")
           (display (- (current-milliseconds) start-time))
           #t)
          (else #f)))

(define (timed-prime n)
    (start-prime-test n (current-milliseconds)))

(define (n-primes-after-i n i)
    (cond ((= n 0) (newline))
          ((timed-prime i) (n-primes-after-i (- n 1) (+ i 1)))
          (else (n-primes-after-i n (+ i 1)))))

; square root of 2 faster than exercise 1.22
(n-primes-after-i 3 100000000000)
(n-primes-after-i 3 10000000000000)
(n-primes-after-i 3 1000000000000000)
