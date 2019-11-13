#lang racket

(provide node root)
(provide goThrough step)

(require "program.rkt")

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
(define (step root pos command)
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

(define (goThrough root pos program)
    (define isMarked (getTag root pos))
    (define childs (getChilds root pos))
    (define command  0);(list-ref program 0))
    (printf "\nI am in ~a\n" pos)
    (define lenChilds (length childs))

    (display program)
    (display "\n")
    (define pr 
        (if (string? program)
            (clean program)
            (clean (list-ref program 0))
        )
    )
    (printf "PR: ~a\n" pr)
    (define type (get-type pr 0))
    (printf "Type: ~a\n" type)
    (define subprogs (split-op pr null (xel type) 0))
    (printf "Subprogs: ~a\n" subprogs)

    (cond 
        [(string=? type "END") (goThrough root 0 (rest subprogs))]
        [(string=? type "UNION") (display type)]
        [(string=? type "ASC") (display type)]
        [(string=? type "ATOMIC") (display type)]
    )



    ; (for-each (lambda (arg)
    ;     ()
    ; ) 
    ; childs)

    
    ; (define filteredChilds (filter 
    ;     (lambda (arg)
    ;         (if (not (null? childs)) (string=? (list-ref arg 1) (list-ref subprogs 0)) null)
    ;     ) 
    ; childs))
    
    ; (cond
    ;     [(and (not isMarked) (>= lenChilds 1)) 
    ;         (for-each (lambda (arg)      
    ;             (printf "\nlenChilds ~a\n" lenChilds) (goThrough root (list-ref arg 0) (rest subprogs)
    ;         )
    ;     )
    ;     childs)]
    ; )
)

    ; (list-copy node (hash-ref graph bgn) [marked #t])

    ; (if (and (not isMarked) (= (length filteredChilds) 1))
    ;  (goThrough root (list-ref (argmin car filteredChilds) 0) (shiftCommand program))
    ;  null
    ; )


(goThrough root 0 "(aUb);(cUd)")

; (for-each (lambda (arg)
;     (printf "got ~a\n" (node-childs arg))) 
;     root)

