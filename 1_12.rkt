#lang racket
(define (pascal-triangle-elem row column)
    ; boundaries check
    (if (and (<= 1 column) (<= column row))
        (pascal-triangle-elem-impl row column)
        -1))

(define (pascal-triangle-elem-impl row column)
    (if (or (= column 1) (= column row))
        1
        (+ (pascal-triangle-elem (- row 1) (- column 1)) (pascal-triangle-elem (- row 1) column))))

; correct indices
(pascal-triangle-elem 1 1)
(pascal-triangle-elem 2 2)
(pascal-triangle-elem 3 3)
(pascal-triangle-elem 5 3)
(pascal-triangle-elem 6 3)

; incorrect indices -> non-existing items
(pascal-triangle-elem 3 0)
(pascal-triangle-elem 5 7)
(pascal-triangle-elem -1 -1)