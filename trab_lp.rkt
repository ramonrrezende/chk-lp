#lang racket

(provide node)
(provide goThrough getDstId)

(struct node (id tag childs))

; Lista de triplas
; Primeira solução já é válida

(define n1 (node 0 #f '((1 "a"))))
(define n2 (node 1 #f '((2 "b"))))
(define n3 (node 2 #f '((3 "a") (4 "b"))))
(define n4 (node 3 #f '()))
(define n5 (node 4 #f '()))

; [n1: (0 [1 "a"]), n2: (0 [1 "a"]), n3: (0 [1 "a"]), n4: (0 [1 "a"]), n5: (0 [1 "a"])]
(define root (list n1 n2 n3 n4 n5))

; return True if node already visited
(define (getTag root pos) 
    (node-tag (list-ref root pos))
)

; returns child list from root
(define (getChilds root pos) 
    (node-childs (list-ref root pos))
)

; VOCE VAI USAR ESSA AQUI
(define (getDstId root pos command)
    (define childs (getChilds root pos))
    ; (display command)
    (define filteredChilds (map 
        (lambda (arg)
            (when (string=? (list-ref arg 1) command) (list-ref arg 0))
        )
    childs))
    filteredChilds
)

; return rest of the commands
(define (shiftCommand program)
    (take-right program (- (length program) 1))
)

; main
(define (goThrough root pos program)
    (define isMarked (getTag root pos))
    (define childs (getChilds root pos))
    (define command (list-ref program 0))
    (printf "\nI am in ~a\n" pos)
    (display program)
    (display "\n")
    (define lenChilds (length childs))

    ; Na hora de percorrer o feixo transitivo, retornar o id's dos nós finais da execução do mesmo
    ; 
    ; 
    ; (take-right '(1 2 3 4) (- (length '(1 2 3 4)) 1))
    ; (split-at '(1 2 3) (floor (/ (length '(1 2 3)) 2)))
    (define filteredChilds (filter 
        (lambda (arg)
            (if (not (null? childs)) (string=? (list-ref arg 1) command) null)
        ) 
    childs))
    
    (printf "\nIDS ~a\n" (getDstId root pos command))
    
    (cond
        [(and (not isMarked) (>= lenChilds 1)) 
            (for-each (lambda (arg)
            ; (printf "ARG ~a\n" (list-ref arg 0)))       
            (printf "\nlenChilds ~a\n" lenChilds) (goThrough root (list-ref arg 0) (shiftCommand program)
            )
        )
        childs)]
    )
        ; ((printf "\nlenChilds ~a\n" lenChilds) (goThrough root (list-ref (argmin car filteredChilds) 0) (shiftCommand program)))
)

    ; (list-copy node (hash-ref graph bgn) [marked #t])

    ; (if (and (not isMarked) (= (length filteredChilds) 1))
    ;  (goThrough root (list-ref (argmin car filteredChilds) 0) (shiftCommand program))
    ;  null
    ; )


; (goThrough root 0 '("a" "b" "a" "b"))

; (for-each (lambda (arg)
;     (printf "got ~a\n" (node-childs arg))) 
;     root)

