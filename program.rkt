#lang racket

(require "tree.rkt")

(provide xel execute execute-atomic execute-end clean get-type is-atomic split-op pre-order next is-simbol lex)
(provide step get-dst node node-id node-childrens edge edge-dst edge-program)

(define (execute program tree context)
    ;(display program)
    ;(newline)
    (define pr (clean program))
    (define type (get-type pr 0))
    (display type)
    (newline)
    (define subprogs (split-op pr null (xel type) 0))
    (display subprogs)
    (newline)
    (cond
        [(equal? type "END") (execute-end subprogs tree context)]
        [(equal? type "UNION") (execute-union subprogs tree context null)]
;        [(equal? type "AST") ]
        [(equal? type "ATOMIC") (execute-atomic program tree context)]
    )
)

(define (execute-end subprogs tree context)
    (define new-context (execute (clean (first subprogs)) tree context))
    (if (not (void? new-context))
        (if (> (length (rest subprogs)) 0)
            (execute-end (rest subprogs) tree new-context)
            new-context
        )
        -1
    )
)

(define (execute-union subprogs tree context lcontext)
    (if(> (length subprogs) 0)
        (execute-union (rest subprogs) tree context (execute (first subprogs) tree context))
        lcontext
    )
)

(define (execute-atomic program tree context)
    (define result (step tree context program))
    (if (= result -1)
        (display "INVALID\n")
        result
    )
)

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

(define (is-atomic program)
    (if (and (= (string-length program) 1) (not (is-simbol program)))
        #t
        #f
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

(define (xel s)
    (cond
        [(equal? s "END") ";"]
        [(equal? s "UNION") "U"];letra U
        [(equal? s "AST") "*"]
        [(equal? s "OPEN") "("]
        [(equal? s "CLOSE") ")"]
        [(equal? s "OPEN") "["]
        [(equal? s "CLOSE") "]"]
        [(equal? s "QST") "?"]
        [else s]
    )
)