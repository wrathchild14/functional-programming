#lang racket

(provide false true int .. empty exception
         trigger triggered handle
         ;  if-then-else
         ;  ?int ?bool ?.. ?seq ?empty ?exception
         ;  add mul ?leq ?= head tail ~ ?all ?any
         ;  vars valof fun proc closure call
         ;  greater rev binary filtering folding mapping
         fri)

(struct true () #:transparent)
(struct false () #:transparent)
(struct int (n) #:transparent)
(struct exception (exn) #:transparent)
(struct .. (e1 e2) #:transparent)

(struct trigger (exn) #:transparent)
(struct triggered (exn) #:transparent)

(struct handle (e1 e2 e3) #:transparent)

(define empty '())

(define (fri expr env)
  (cond
    [(true? expr) #t]
    [(false? expr) #f]
    [(int? expr) expr]
    [(..? expr)
     (let ([e1 (..-e1 expr)]
           [e2 (..-e2 expr)])
       (let ([v1 (fri e1 env)])
         (let ([v2 (fri e2 env)])
           (cons v1 v2))))]
    [(exception? expr) (raise (exception-exn expr))]
    [(trigger? expr)
     (let ([v (fri (trigger-exn expr) env)])
       (cond
         [(triggered? v) (triggered v)]
         [(exception? v) (triggered (exception-exn v))]
         [else (triggered? (exception "trigger: wrong argument type"))]))]
    [(handle? expr)
     (let ([e1 (handle-e1 expr)]
           [e2 (handle-e2 expr)]
           [e3 (handle-e3 expr)])
       (let ([v (fri e1 env)])
         (cond
           [(exception? v) v]
           [(triggered? v)
            (let ([v2 (fri e2 env)])
              (if (triggered? v2)
                  (if (equal? v2 v) (fri e3 env) v2)
                  (fri e3 env)))]
           [else (triggered (exception "handle: wrong argument type"))])))]
    [else (error "Expression not found")]
    )
  )
