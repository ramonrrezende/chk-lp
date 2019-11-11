#lang racket

(require "graph.rkt")
(require "node.rkt")

(provide pre-order split-pv next build-tree parse-program is-simbol lex)

(define (pre-order tree)
    (when (> (length tree) 0)
        (if (list? (first tree))
            (pre-order (first tree))
            (show-node tree)
        )
        (pre-order (rest tree))
    )
)

(define (show-node tree)
    (display (first tree))
    (newline)
)

(define (next program cursor deep)
    (if(and (< cursor (string-length program)) (not (= 0 deep)))
        (cond
            [(equal? (lex (string-ref program cursor)) "OPEN") (next program (+ cursor 1) (+ deep 1))]
            [(equal? (lex (string-ref program cursor)) "CLOSE") (next program (+ cursor 1) (- deep 1))]
            [else (next program (+ cursor 1) deep)]
        )
        (- cursor 1)
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
        [(equal? s #\U) "UNION"];letra U
        [(equal? s #\*) "AST"]
        [(equal? s #\() "OPEN"]
        [(equal? s #\)) "CLOSE"]
        [(equal? s #\[) "OPEN"]
        [(equal? s #\]) "CLOSE"]
        [(equal? s #\?) "QST"]
        [else s]
    )
)