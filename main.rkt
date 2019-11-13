#lang racket
(require "program.rkt")

(define e01 (edge 1 "a"))

(define e02 (edge 2 "b"))

(define e13 (edge 3 "c"))

(define e14 (edge 4 "a"))

(define e25 (edge 5 "c"))

(define n0 (node 0 (list e01 e02)))

(define n1 (node 1 (list e13 e14)))

(define n2 (node 2 (list e25)))

(define n3 (node 3 (list)))

(define n4 (node 4 (list)))

(define n5 (node 5 (list)))

(define tree (list n0 n1 n2))

(define formula "(((a;c)Ub)U(cUa));c")

(execute formula tree 0)
;(step tree 0 formula)