#lang racket

(struct int (n) #:transparent)
(struct true () #:transparent)
(struct false () #:transparent)
(struct add (x y) #:transparent)

(define (fri expr)
  (match expr
    [(int n) expr]
    [(true) expr]
    [(false) expr]
    [(add x y)
     (match (list (fri x) (fri y))
       [(list (true) _) (true)]
       [(list _ (true)) (true)]
       [(list (int a) (int b)) (int (+ a b))]
       [else (error "Error add expression")])]
    [else (error "Expression not found")]))

(fri (add (add (int 3) (int 2)) (int 5)))
