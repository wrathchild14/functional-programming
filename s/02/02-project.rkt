#lang racket

(provide false true int .. empty exception
         trigger triggered handle
         if-then-else
         ?int ?bool ?.. ?seq ?empty ?exception
         ;  add mul ?leq ?= head tail ~ ?all ?any
         ;  vars valof fun proc closure call
         ;  greater rev binary filtering folding mapping
         fri)

(struct true () #:transparent)
(struct false () #:transparent)
(struct int (n) #:transparent)
(struct exception (exn) #:transparent)

(struct .. (e1 e2) #:transparent)
(struct empty () #:transparent)

(struct trigger (exn) #:transparent)
(struct triggered (exn) #:transparent)
(struct handle (e1 e2 e3) #:transparent)
(struct if-then-else (cond e1 e2) #:transparent)

(struct ?int (x) #:transparent)
(struct ?bool (x) #:transparent)
(struct ?.. (x) #:transparent)
(struct ?seq (x) #:transparent)
(struct ?empty (x) #:transparent)
(struct ?exception (x) #:transparent)

(define (fri expr env)
  (cond
    [(true? expr) expr]
    [(false? expr) expr]
    [(int? expr) expr]
    [(..? expr)
     (let ([e1 (..-e1 expr)]
           [e2 (..-e2 expr)])
       (let ([v1 (fri e1 env)])
         (let ([v2 (fri e2 env)])
           (cons v1 v2))))]
    [(exception? expr) (triggered expr)]
    [(trigger? expr)
     (let ([v (fri (trigger-exn expr) env)])
       (if (triggered? v) v
           (triggered (exception "trigger: wrong argument type"))))]
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
    [(if-then-else? expr)
     (let ([cond (if-then-else-cond expr)]
           [e1 (if-then-else-e1 expr)]
           [e2 (if-then-else-e2 expr)])
       (match (fri cond env)
         [#f (fri e2 env)]
         [_ (fri e1 env)]))]
    [(?int? expr)
     (let ([n (fri (?int-x expr) env)])
       (if (triggered? n) n (if (int? n) (true) (false))))]
    [(?bool? expr)
     (let ([b (fri (?bool-x expr) env)])
       (if (triggered? b) b (if (or (eq? b (true)) (eq? b (false))) (true) (false))))]
    [(?..? expr)
     (let ([dots (fri (?..-x expr) env)])
       (if (triggered? dots) dots (if (pair? dots) (true) (false))))]
    [(?seq? expr)
     (let ([s (fri (?seq-x expr) env)])
       (cond
         [(triggered? s) s]
         [(empty? s) (true)]
         [(..? s)
          (let ([v1 (..-e1 s)]
                [v2 (..-e2 s)])
            (if (empty? v2)
                (true)
                (let ([result (fri (?seq v2) env)])
                  (if (triggered? result) result (false)))))]
         [else (false)]))]
    [(?empty? expr)
     (let ([e (fri (?empty-x expr) env)])
       (if (triggered? e) e (if (empty? e) (true) (false))))]
    [(?exception? expr)
     (let ([ex (fri (?exception-x expr) env)])
       (if (triggered? ex) ex (if (exception? ex) (true) (false))))]
    [else (error "Expression not found")]
    )
  )
