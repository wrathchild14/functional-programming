#lang racket

(provide false true int .. empty exception
         trigger triggered handle
         if-then-else
         ?int ?bool ?.. ?seq ?empty ?exception
         add mul ?leq ?= head tail ~ ?all ?any
         ;  vars valof fun proc closure call
         ;  greater rev binary filtering folding mapping
         fri)

(struct true () #:transparent)
(struct false () #:transparent)
(struct int (e) #:transparent)
(struct exception (exn) #:transparent)

(struct .. (e1 e2) #:transparent)
(struct empty () #:transparent)

(struct trigger (exn) #:transparent)
(struct triggered (exn) #:transparent)
(struct handle (e1 e2 e3) #:transparent)
(struct if-then-else (cond e1 e2) #:transparent)

(struct ?int (e) #:transparent)
(struct ?bool (e) #:transparent)
(struct ?.. (e) #:transparent)
(struct ?seq (e) #:transparent)
(struct ?empty (e) #:transparent)
(struct ?exception (e) #:transparent)

(struct add (e1 e2) #:transparent)
(struct mul (e1 e2) #:transparent)
(struct ?leq (e1 e2) #:transparent)
(struct ?= (e1 e2) #:transparent)

(struct head (e) #:transparent)
(struct tail (e) #:transparent)

(struct ~ (e) #:transparent)
(struct ?all (e) #:transparent)
(struct ?any (e) #:transparent)

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
           (cond
             [(triggered? v1) v1]
             [(triggered? v2) v2]
             [(and (empty? v1) (empty? v2)) v1]
             [else (.. v1 v2)]))))]
    [(exception? expr) expr]
    [(trigger? expr)
     (let ([v (fri (trigger-exn expr) env)])
       (cond [(triggered? v) v]
             [(exception? v) (triggered v)]
             [else (triggered (exception "trigger: wrong argument type"))]))]
    [(handle? expr) (let ([e1 (fri (handle-e1 expr) env)]
                          [e2 (fri (handle-e2 expr) env)]
                          [e3 (fri (handle-e3 expr) env)])
                      (if (triggered? e1)
                          e1
                          (if (exception? e1)
                              (if (and (triggered? e2) (equal? (exception-exn e1) (exception-exn (triggered-exn e2))))
                                  e3
                                  e2)
                              (triggered (exception "handle: wrong argument type")))))]
    [(if-then-else? expr)
     (let ([cond (if-then-else-cond expr)]
           [e1 (if-then-else-e1 expr)]
           [e2 (if-then-else-e2 expr)])
       (match (fri cond env)
         [#f (fri e2 env)]
         [_ (fri e1 env)]))]
    [(?int? expr)
     (let ([n (fri (?int-e expr) env)])
       (if (triggered? n) n (if (int? n) (true) (false))))]
    [(?bool? expr)
     (let ([b (fri (?bool-e expr) env)])
       (if (triggered? b) b (if (or (eq? b (true)) (eq? b (false))) (true) (false))))]
    [(?..? expr)
     (let ([v (fri (?..-e expr) env)])
       (cond
         [(triggered? v) v]
         [(pair? v) (true)]
         [else (false)]))]
    [(?seq? expr)
     (let ([s (fri (?seq-e expr) env)])
       (cond
         [(triggered? s) s]
         [(empty? s) (true)]
         [(..? s)
          (let (
                [v2 (..-e2 s)])
            (if (empty? v2)
                (true)
                (fri (?seq v2) env)))]
         [else (false)]))]
    [(empty? expr) expr]
    [(?empty? expr)
     (let ([e (fri (?empty-e expr) env)])
       (if (triggered? e) e (if (empty? e) (true) (false))))]
    [(?exception? expr)
     (let ([ex (fri (?exception-e expr) env)])
       (if (triggered? ex) ex (if (exception? ex) (true) (false))))]
    [(add? expr)
     (let ([e1 (add-e1 expr)]
           [e2 (add-e2 expr)])
       (let ([v1 (fri e1 env)]
             [v2 (fri e2 env)])
         (cond
           [(triggered? v1) v1]
           [(triggered? v2) v2]
           [(and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
            (if (or (true? v1) (true? v2)) (true) (false))]
           [(and (int? v1) (int? v2)) (int (+ (int-e v1) (int-e v2)))]
           ; from v1, rec insert first till not empty
           [(and (..? v1) (..? v2))
            (let loop ([lst v1])
              (if (empty? lst)
                  v2
                  (let ([first (..-e1 lst)]
                        [rest (..-e2 lst)])
                    (.. first (loop rest)))))]
           [(and (empty? v1) (empty? v2)) v1]
           [else (triggered (exception "add: wrong argument type"))])))]
    [(mul? expr)
     (let ([e1 (mul-e1 expr)]
           [e2 (mul-e2 expr)])
       (let ([v1 (fri e1 env)]
             [v2 (fri e2 env)])
         (cond
           [(and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
            (if (and (true? v1) (true? v2)) (true) (false))]
           [(and (int? v1) (int? v2)) (int (* (int-e v1) (int-e v2)))]
           [else (triggered (exception "mul: wrong argument type"))])))]
    [(?leq? expr)
     (let ([e1 (?leq-e1 expr)]
           [e2 (?leq-e2 expr)])
       (let ([v1 (fri e1 env)]
             [v2 (fri e2 env)])
         (cond
           [(or (eq? v1 (true)) (eq? v2 (true))) (true)]
           [(and (int? v1) (int? v2)) (<= v1 v2)]
           [(?seq? v1) (= (length v1) (length v2))]
           [else (triggered (exception "?leq: wrong argument type"))])))]
    ; test with add fails, not sure if head or add (with seqs) is wrong
    [(head? expr)
     (let ([e (fri (head-e expr) env)])
       (cond
         [(triggered? e) e]
         [(empty? e) e]
         [(..? e)
          (let ([e1 (..-e1 e)])
            (if (empty? e1)
                (triggered (exception "head: empty sequence"))
                (fri e1 env)))]
         [else (triggered (exception "head: wrong argument type"))]
         ))]
    [(tail? expr)
     (let ([e (fri (tail-e expr) env)])
       (cond
         [(triggered? e) e]
         [(empty? e) (triggered (exception "tail: empty sequence"))]
         [(..? e) (let ([e2 (..-e2 e)])
                    (if (empty? e2)
                        e2
                        (fri e2 env)))]
         [else (triggered (exception "tail: wrong argument type"))]
         ))]
    [(~? expr)
     (cond
       [(int? expr) (int (- (int-e expr)))]
       [(true? expr) (int 1)]
       [(false? expr) (int 0)]
       [else (triggered (exception "~: wrong argument type"))])]
    [(?all? expr)
     (cond
       [(?seq? expr)
        (let ([res (fri expr env)])
          (if (triggered? res)
              res
              (not (memv (false) res))))]  ; if false exists
       [else (triggered (exception "?all: wrong argument type"))])]
    [(?any? expr)
     (cond
       [(?seq? expr)
        (let ([res (fri expr env)])
          (if (triggered? res)
              res
              (not (null? (memv #f res)))))]  ; if non false exists
       [else (triggered (exception "?any: wrong argument type"))])]
    [(?= expr)
     (let ([e1 (?=-e1 expr)]
           [e2 (?=-e2 expr)])
       (let ([v1 (fri e1 env)]
             [v2 (fri e2 env)])
         (if (equal? v1 v2) (true) (false))))]
    [else (error "Expression not found")]
    )
  )
