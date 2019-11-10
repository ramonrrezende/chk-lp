#lang racket

;(struct bst-node (id tag childs) #:transparent #:mutable)
(struct programs (commands))

(define run_1(lambda(p) p))


;'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

;x y a
;y z b
;z w a
;z r b
;a b (a U b)


(struct bst-node (id tag childs) #:transparent #:mutable)

(define (bst-add tree node-a node-b)
  (if (null? tree) (bst-node 1 #t list)
      (set-bst-node-childs! tree (list node-a node-b))
      ))
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

(struct node(id marked childs))

(struct tree(root))

(define n1 (node 1 #f (list 4 3)))
(define n2 (node 2 #f (list 5)))
(define n3 (node 3 #f (list 6)))
(define n4 (node 4 #f (list 2)))
(define n5 (node 5 #f (list 5)))
(define n6 (node 6 #f (list 6)))

(define t1 (tree n1))

(define g (make-hash))
(define nodes (list n1 n2 n3 n4))

;Cria um grafo a partir de uma hash table vazia e uma lista de nós
(define (create_graph g nodes)
    (hash-set! g (node-id (first nodes)) (first nodes) );associa o nó a uma chave da hash table(a chave é o id do nó)
    (if (> (length (rest nodes)) 0) (create_graph g (rest nodes)) (display "Done!\n"));chama a função recursivamente para adicionar todos os nós da lista
)

(define (explore graph ch)
   (when (not (node-marked (hash-ref graph (first ch))))
        (hash-set! graph (first ch) (struct-copy node (hash-ref graph (first ch)) [marked #t]));marca o nó como visitado e atualiza a hash table
        (display (first ch));adicionar operação relevante aqui
        (newline)
        (when (> (length (rest ch)) 0) (explore graph (rest ch)))
    )
)

;Função que percorre o grafo, partindo de um nó inicial, marcando os nós visitados
(define (run graph bgn)
    (hash-set! graph bgn (struct-copy node (hash-ref graph bgn) [marked #t]));cria uma cópia da estrutura com o atributo "marcado" como verdade e associa à chave da hash table
    (display  bgn);Mostra o id do nó visitado(substituir por operação relevante posteriormente)
    (newline)
    (if (> (length (node-childs (hash-ref graph bgn))) 0)
        (explore graph (node-childs (hash-ref graph bgn)))
        (display "END!\n")
    )
)

(create_graph g nodes)

(newline)
(run g 1)



(provide factorial)

(define (factorial n)
  (if (< n 0)
      (raise-argument-error 'factorial "positive integer" n)
      (factorial-iter n n 1)))

(define (factorial-iter n i fat)
  (if (= i 0)
      fat
      (factorial-iter n (sub1 i) (* fat i))))



