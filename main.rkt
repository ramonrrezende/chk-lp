#lang racket

(struct node(id marked childrens))

(define n1 (#f (list 2 3)))

(define n2 (#f (list 3)))

(define n3 (#f (list 1)))

(define g (make-hash))

(define (create_graph g nodes)
    (hash-set! g (first nodes)
    (create_graph g (rest nodes))
    ))

(define (create_graph g nodes)
    (hash-set! g
        ((node-id (first nodes)) 
        (node-childrens (first nodes))) )
    (if (> (length (rest nodes)) 0) (create_graph g (rest nodes))) (display "Done!\n") )

