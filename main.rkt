#lang racket
(require "node.rkt" "graph.rkt")

(define n1 (node 1 #f (list 4 2 3)))

(define n2 (node 2 #f (list 3)))

(define n3 (node 3 #f (list 1)))

(define n4 (node 4 #f (list 4)))

(define g (make-hash))

(define nodes (list n1 n2 n3 n4))

(create-graph g nodes)

(newline)
(run g 1)