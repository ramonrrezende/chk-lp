#lang racket
(require "node.rkt" "graph.rkt" "node.rkt" )

(define e11 (edge 4 "a"))

(define e12 (edge 2 "a"))

(define e13 (edge 3 "b"))

(define e21 (edge 3 "c"))

(define e31 (edge 1 "b"))

(define e41 (edge 4 "d"))

(define n1 (node 1 #f (list e11 e12 e13)))

(define n2 (node 2 #f (list e21)))

(define n3 (node 3 #f (list e31)))

(define n4 (node 4 #f (list e41)))

(define g (make-hash))

(define nodes (list n1 n2 n3 n4))

(define formula "a ; b ; ( a U b)")

(create-graph g nodes)

(newline)
(run g 2)

(display formula)
(newline)