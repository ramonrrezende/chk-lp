#lang racket

(require "graph.rkt")
(require "node.rkt")

(provide parse-program is-simbol lex)

(define (parse-program program tokens cursor)
    (if (< cursor (string-length program))
        (parse-program program (append tokens (list (lex (string-ref program cursor)))) (+ cursor 1))
        (append tokens)
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

(define (lex s)
    (cond
        [(equal? s #\;) "END"]
        [(equal? s #\U) "UNION"]
        [(equal? s #\*) "AST"]
        [(equal? s #\() "OPEN"]
        [(equal? s #\)) "CLOSE"]
        [(equal? s #\[) "OPEN"]
        [(equal? s #\]) "CLOSE"]
        [(equal? s #\?) "QST"]
        [else s]
    )
)