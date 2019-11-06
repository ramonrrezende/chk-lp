#lang racket

(provide print-tabs index-string print-tree print-list)

(define (index-string s c)
    (when (< c (string-length s))
        (display (string-ref s c))
        (display " ")
        (display c)
        (newline)
        (index-string s (+ c 1))
    )
)

(define (print-list lst)
    (when (> (length lst) 0)
        (display (first lst))
        (newline)
        (print-list (rest lst))
    )
)

(define (print-tree tr deep)
    (when (> (length tr) 0)
        (print-tabs deep)
        (if (list? (first tr))
            (print-tree (first tr) (+ deep 1))
            (display (first tr))
        )
        (newline)
        (print-tree (rest tr) deep)
    )
)

(define (print-tabs deep)
    (when (> deep 0)
        (display "  ")
        (print-tabs (- deep 1))
    )
)