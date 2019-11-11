#lang racket

(require "graph.rkt")
(require "node.rkt")

(provide clean get-type is-atomic split-op pre-order next parse-program is-simbol lex)

(define (clean program)
    (if (and (equal? (lex (string-ref program 0)) "OPEN") (equal? (lex (string-ref program (- (string-length program) 1))) "CLOSE"))
        (if (= (next program 1 1) (- (string-length program) 1))
            (substring program 1 (- (string-length program) 1))
            (string-append program)
        )
        (string-append program)
    )
)

(define (split-op program lst op cursor)
    (if (< cursor (string-length program))
        (cond
            [(equal? (lex (string-ref program cursor)) "OPEN") (split-op program lst op (+ (next program (+ cursor 1) 1) 1))]
            [(equal? (lex (string-ref program cursor)) (lex (string-ref op 0))) (split-op (substring program (+ cursor 1) (string-length program)) (append lst (list (substring program 0 cursor))) op 0)]
            [else (split-op program lst op (+ cursor 1))]
        )
        (if (> (string-length program) 0)
            (append lst (list program))
            (append lst)
        )
    )
)

(define (get-type program cursor)
    (if (not (is-atomic program))
        (if (< cursor (string-length program))
            (cond
                [(equal? (lex (string-ref program cursor)) "OPEN") (get-type program (+ (next program (+ cursor 1) 1) 1))]
                [(equal? (lex (string-ref program cursor)) "END") "END"]
                [(equal? (lex (string-ref program cursor)) "UNION") "UNION"]
                [(equal? (lex (string-ref program cursor)) "AST") "AST"]
                [else (get-type program (+ cursor 1))]
            )
            (string-append "ERRO")
        )
        (string-append "ATOMIC")
    )
)

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