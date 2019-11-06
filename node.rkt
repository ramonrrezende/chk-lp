#lang racket

(provide node node-id node-marked node-childrens edge edge-dst edge-program)

;define estrutura para um nó
(struct node(id marked childrens))

(struct edge(dst program))