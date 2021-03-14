#lang racket

;Author: Amit Moyal, ID: 206086365, mmn 11, programming languages 

(require "utils.scm")

;;; question 1
(define (my_flatten lst)
                          (cond ((null? lst) lst)
                                ((pair? lst) (append (my_flatten (car lst)) (my_flatten (cdr lst))))
                                (else (list lst)))
                           )

;;;; question 1 unit-tests
(equal?? (my_flatten '()) '())
(equal?? (my_flatten '(1)) '(1))
(equal?? (my_flatten '((1))) '(1))
(equal?? (my_flatten '(1 2 3)) '(1 2 3))
(equal?? (my_flatten '((1) (1 2 3) (1 1 (((3)))))) '(1 1 2 3 1 1 3))


;;;; question 2a
(define (my_count prdct lst) 
  (cond ((null? lst) 0)
        ((pair? lst) (if (prdct (car lst)) (+ 1 (my_count prdct (cdr lst))) (my_count prdct (cdr lst))))
  )) 



;;;; question 2a unit-tests
(equal?? (my_count positive? '()) 0)
(equal?? (my_count positive? '(4 4 4)) 3)
(equal?? (my_count positive? '(-2 -3 1)) 1)
(equal?? (my_count positive? '(0 0)) 0)
(equal?? (my_count null? '((1 2 3) (1))) 0)
(equal?? (my_count number? '(1 2 3 2)) 4)
(equal?? (my_count null? '(() () (2))) 2)


;;;; question 2b
(define (my_count_foldr prdct lst) 
  (foldr (λ (var num) (if (prdct var) (+ num 1) num)) 0 lst))

;;;; question 2b unit-tests
(equal?? (my_count_foldr positive? '()) 0)
(equal?? (my_count_foldr positive? '(4 4 4)) 3)
(equal?? (my_count_foldr positive? '(-2 -3 1)) 1)
(equal?? (my_count_foldr positive? '(0 0)) 0)
(equal?? (my_count_foldr null? '((1 2 3) (1))) 0)
(equal?? (my_count_foldr number? '(1 2 3 2)) 4)
(equal?? (my_count_foldr null? '(() () (2))) 2)


;;;; question 3
(define (my_partition prdct lst) 
  (let ((true-list '())
        (false-list '())) 
            (for-each (λ (x) (if (prdct x) (set! true-list (append true-list (list x))) (set! false-list (append false-list (list x))))) lst)
            (list true-list false-list)
   )
)

;;;; question 3 unit-tests
(equal?? (my_partition even? '(1 2 3 4 5 6)) '((2 4 6) (1 3 5)))
(equal?? (my_partition even? '()) '(() ()))
(equal?? (my_partition positive? '(1 -2 3 -4)) '((1 3) (-2 -4)))
(equal?? (my_partition null? '(() (1 2) () (2 3))) '((() ()) ((1 2) (2 3))))

;;;; question 4
(define (my_append_map func lst) (my_flatten (map func lst))) 

;;;; question 4 unit-tests
(equal?? (my_append_map cdr '((1 2 3)(4 5 6)(7 8 9))) '(2 3 5 6 8 9))
(equal?? (my_append_map cdr '()) '())
(equal?? (my_append_map cdr '((1 2 3))) '(2 3))
(equal?? (my_append_map cdr '((1 2) (3) (4 5 6))) '(2 5 6))

;;;; question 5
(define (my_permutations lst)
  (cond [(empty? lst) empty]
        [(empty? (rest lst)) (list lst)]
        [else
         (let recloop [(l '()) (fst (first lst)) (rst (rest lst))]
           (append
            (map (lambda (x) (cons fst x))
                 (my_permutations (append l rst)))
            (if (empty? rst)
                empty
                (recloop (cons fst l) (car rst) (cdr rst)))))]))

;;;; question 5 unit-tests
(equal?? (my_permutations '(1)) '((1)))
(equal?? (my_permutations '()) '())
(equal?? (my_permutations '(1 2 3)) '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 2 1) (3 1 2)))