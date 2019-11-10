#lang racket

(struct node (id tag childs) #:transparent)

(define n1 (node 0 #f '((1 "a"))))
(define n2 (node 1 #f '((2 "b"))))
(define n3 (node 2 #f '((3 "a") (4 "b"))))
(define n4 (node 3 #f '()))
(define n5 (node 4 #f '()))

(define root (list n1 n2 n3 n4 n5))

; return True if node already visited
(define (getTag root pos) 
    (node-tag (list-ref root pos))
)

; returns child list from root
(define (getChilds root pos) 
    (node-childs (list-ref root pos))
)

; return rest of the commands
(define (shiftCommand program)
    (take-right program (- (length program) 1))
)

; (define (parseAtomic command)
;     ()
; )

; (define (getDstNode childs)
;     (filter (lambda (arg)
;         (= arg-a arg-b)    
;     ) childs)
; )

; main
(define (goThrough root pos program)
    (define isMarked (getTag root pos))
    (define childs (getChilds root pos))
    (define command (list-ref program 0))
    (display program)
    (display childs)
    (display "\n")
    (display command)
    (display "\n")

    ; (take-right '(1 2 3 4) (- (length '(1 2 3 4)) 1))
    ; (split-at '(1 2 3) (floor (/ (length '(1 2 3)) 2)))
    (define filteredChilds (filter 
        (lambda (arg)
            (if (not (null? childs)) (string=? (list-ref arg 1) command) null)
        ) 
    childs))

    (display filteredChilds)
    (printf "\nI am in ~a\n" pos) ; Fazer a magia
    
    (if (and (not isMarked) (= (length filteredChilds) 1))
        (goThrough root (list-ref (argmin car filteredChilds) 0) (shiftCommand program))
        (if () 
            ()
            ()
        )
    )

    ; (list-copy node (hash-ref graph bgn) [marked #t])

    ; (if (and (not isMarked) (= (length filteredChilds) 1))
    ;  (goThrough root (list-ref (argmin car filteredChilds) 0) (shiftCommand program))
    ;  null
    ; )
)

(goThrough root 0 '("a" "b" "a|b"))

; (for-each (lambda (arg)
;     (printf "got ~a\n" (node-childs arg))) 
;     root)

