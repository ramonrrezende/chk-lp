#lang racket

(provide step get-dst node node-id node-childrens edge edge-dst edge-program)

(struct node(id childrens))

(struct edge(dst program))

(define (get-dst edges program index)
    (define p (edge-program (list-ref edges index)))
    (if (equal? p program)
        (edge-dst (list-ref edges index))
        (if (< (+ index 1) (length edges))
            (get-dst edges program (+ index 1))
            -1
        )
    )
)

(define (step tree context program)
    (define nd (list-ref tree context))
    (if (> (length (node-childrens nd)) 0)
        (get-dst (node-childrens nd) program 0)
        -1
    )
)
