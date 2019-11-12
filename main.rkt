#lang racket
(require "program.rkt")

(define e01 (edge 1 "a"))

(define e02 (edge 2 "b"))

(define e12 (edge 2 "b"))

(define n0 (node 0 (list e01 e02)))

(define n1 (node 1 (list e12)))

(define n2 (node 2 (list)))

(define tree (list n0 n1 n2))

(define formula "a;b")

(execute formula tree 0)
;(step tree 0 formula)