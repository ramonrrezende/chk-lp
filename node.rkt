#lang racket

(provide node node-id node-marked node-childrens)

;define estrutura para um nó
(struct node(id marked childrens))