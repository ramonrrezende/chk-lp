#lang racket

(require "graph.rkt")
(require "node.rkt")

(provide next index-string print-list build-tree parse-program is-simbol lex)

(define (parse-program program tokens cursor)
    (if (< cursor (string-length program))
        (parse-program program (append tokens (list (lex (string-ref program cursor)))) (+ cursor 1))
        (append tokens)
    )
)

(define (build-tree program tree cursor)
    (if(< cursor (string-length program))
        (if (equal? (lex (string-ref program cursor)) "OPEN")
            (build-tree
                (substring program (+ (next program (+ cursor 1) 1) 1) (string-length program))
                (append tree (list (substring program 0 cursor) (substring program (+ cursor 1) (next program (+ cursor 1) 1))))
                0
            )
            (build-tree program tree (+ cursor 1))
        )
        (append tree (list (substring program 0 (string-length program))))
    )
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