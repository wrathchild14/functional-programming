#lang racket

(struct true () #:transparent)
(struct false () #:transparent)
(struct int (n) #:transparent)
(struct exception (exn) #:transparent)

(define-syntax-rule (.. e1 e2)
  (cons e1 e2))
(define empty '())

(define (fri expr env)
  (cond
    [(true? expr) #t]
    [(false? expr) #f]
    [(int? expr) expr]
    [(exception? expr) (raise (exception-exn expr))]
    [else (error "Expression not found")]
    )
  )