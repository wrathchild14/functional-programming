#lang racket

(define naravna (letrec ([f (lambda (x) (mcons x (delay (f (+ x 1)))))])
                  (f 1)))

; doesnt work, it doesnt append new stream
(define (razvij tok n)
  (if (zero? n)
      tok
      (begin
        (set-mcdr! tok (force (mcdr tok)))
        (razvij (force (mcdr tok)) (- n 1)))))

(let ([a 5]
      [b 6]
      [c 7])
  (begin
    (let ([c a])
      (set! a b)
      (set! b c))
    (let ([c b])
      (set! b c)
      (set! c c)))
  (list a b c))


(define-syntax-rule (swap x y)
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

(let ([tmp 5]
      [other 6])
  (swap tmp other)
  (displayln (list tmp other)))


(let ([tmp 5]
      [other 6])
  (let ([tmp tmp])
    (set! tmp other)
    (set! other tmp))
  (displayln (list tmp other)))


(define (add-or-concat a b)
  (if (and (number? a) (number? b))
      (+ a b)
      (if (and (string? a) (string? b))
          (string-append a b)
          (error "Invalid arguments"))))


(define-struct tree (id left value right))

(define example
  (make-tree "koren"
             (make-tree "l" 
                        (make-tree "ll" null 2 (make-tree "lld" null 3 null))
                        5
                        (make-tree "ld" null 7 null))
             10
             (make-tree "d" null
                        15
                        (make-tree "dd" null
                                   17
                                   (make-tree "ddd" null 19 null)))))

(define memo (make-hash))

(define (poisci tree elements)
  (map (lambda (element) (search tree element)) elements))

(define (search tree element)
  (cond
    [(hash-ref memo element #f) => (lambda (x) (string-append x "*"))]
    [(null? tree) (hash-set! memo element "null") "null"]
    [(= element (tree-value tree)) (hash-set! memo element (tree-id tree)) (tree-id tree)]
    [(< element (tree-value tree)) (search (tree-left tree) element)]
    [else (search (tree-right tree) element)]))

(poisci example (list 6 15 18 17 7 2))

(define (potenca #:osnova [osnova 2] #:exp [exp 10])
  (if (zero? exp)
      1
      (* osnova (potenca #:osnova osnova #:exp (sub1 exp)))))
