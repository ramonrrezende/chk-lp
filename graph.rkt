#lang racket

(require "node.rkt")

(provide create-graph run explore)

;Cria um grafo a partir de uma hash table vazia e uma lista de nós
(define (create-graph g nodes)
    (hash-set! g (node-id (first nodes)) (first nodes) );associa o nó a uma chave da hash table(a chave é o id do nó)
    (if (> (length (rest nodes)) 0) (create-graph g (rest nodes)) (display "Done!\n"));chama a função recursivamente para adicionar todos os nós da lista
)

(define (explore graph ch)
   (when (not (node-marked (hash-ref graph (edge-dst (first ch)))))
        (hash-set! graph (first ch) (struct-copy node (hash-ref graph (edge-dst (first ch))) [marked #t]));marca o nó como visitado e atualiza a hash table
        (display (edge-dst (first ch)));adicionar operação relevante aqui
        (display " ")
        (display (edge-program (first ch)))
        (newline)
        (when (> (length (rest ch)) 0) (explore graph (rest ch)))
    )
)

;Função que percorre o grafo, partindo de um nó inicial, marcando os nós visitados
(define (run graph bgn)
    (hash-set! graph bgn (struct-copy node (hash-ref graph bgn) [marked #t]));cria uma cópia da estrutura com o atributo "marcado" como verdade e associa à chave da hash table
    (display  bgn);Mostra o id do nó visitado(substituir por operação relevante posteriormente)
    (display " ")
    (newline)
    (if (> (length (node-childrens (hash-ref graph bgn))) 0)
        (explore graph (node-childrens (hash-ref graph bgn)))
        (display "END!\n")
    )
)
