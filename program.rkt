#lang racket

(require "graph.rkt")
(require "node.rkt")

(provide parse-program is-simbol lex)

(define (parse-program program instructions cursor)
    (if(< cursor (- (string-length program) 1))
        (parse-program program (append instructions (list (string-ref program cursor))) (+ cursor 1))
        (append instructions)
    )
)

(define (is-simbol s)
    (cond
        [(equal? s ";") #t]
        [(equal? s "U") #t]
        [(equal? s "*") #t]
        [(equal? s "(") #t]
        [(equal? s ")") #t]
        [(equal? s "[") #t]
        [(equal? s "]") #t]
        [(equal? s "?") #t]
        [else #f]
    )
)
