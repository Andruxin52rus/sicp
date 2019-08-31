#lang racket

(define (divide? x y)
    (= (remainder x y) 0))

(define (smallest-divisor-iter n test)
    (cond ((> (* test test) n) n)
          ((divide? n test) test)
          (else (smallest-divisor-iter n (+ test 1)))))

(define (smallest-divisor n)
    (smallest-divisor-iter n 2))
    
(define (prime? n)
    (= (smallest-divisor n) n))

(define (expmod base exp m)
    (define (square x)
        (* x x))
    (cond ((= exp 0) 1) 
            ((even? exp) 
            (remainder (square (expmod base (/ exp 2) m)) 
                        m)) 
            (else 
            (remainder (* base (expmod base (- exp 1) m)) 
                        m))))

(define (fermat-test n) 
    (define (try-it a) 
        (= (expmod a n n) a)) 
    (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times) 
    (cond ((= times 0) true) 
            ((fermat-test n) (fast-prime? n (- times 1))) 
            (else false)))

(define (start-prime-test n start-time)
    (cond ((fast-prime? n)
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

; square root complexity => expect about 10x time difference between following tests (10 is square root of 100)
(n-primes-after-i 3 100000000000)
(n-primes-after-i 3 10000000000000)
(n-primes-after-i 3 1000000000000000)
