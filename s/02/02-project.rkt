#lang racket

(provide false true int .. empty exception
         trigger triggered handle
         if-then-else
         ?int ?bool ?.. ?seq ?empty ?exception
         add mul ?leq ?= head tail ~ ?all ?any
         vars valof fun proc closure call
         greater rev binary filtering folding mapping
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

(struct vars (s e1 e2) #:transparent)
(struct valof (s) #:transparent)
(struct fun (name fargs body) #:transparent)
(struct proc (name body) #:transparent)
(struct closure (env f) #:transparent)
(struct call (e args) #:transparent)

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
             [else (.. v1 v2)]))))]
    [(empty? expr) (empty)]
    [(exception? expr) expr]
    [(trigger? expr)
     (let ([v (fri (trigger-exn expr) env)])
       (cond [(triggered? v) v]
             [(exception? v) (triggered v)]
             [else (triggered (exception "trigger: wrong argument type"))]))]
    [(handle? expr)
     (let ([v1 (fri (handle-e1 expr) env)]
           [v2 (fri (handle-e2 expr) env)]
           [v3 (fri (handle-e3 expr) env)])
       (if (triggered? v1)
           v1
           (if (not (exception? v1))
               (triggered (exception "handle: wrong argument type"))
               (if (not (triggered? v2))
                   v2
                   (if (eq? (exception-exn v1) (exception-exn (triggered-exn v2))) v3 v2)))))]
    [(if-then-else? expr)
     (let ([cond-value (fri (if-then-else-cond expr) env)])
       (cond
         [(triggered? cond-value) cond-value]
         [(false? cond-value) (fri (if-then-else-e2 expr) env)]
         [else (fri (if-then-else-e1 expr) env)]))]
    [(?int? expr)
     (let ([n (fri (?int-e expr) env)]) (if (triggered? n) n (if (int? n) (true) (false))))]
    [(?bool? expr)
     (let ([b (fri (?bool-e expr) env)])
       (if (triggered? b) b (if (or (true? b) (false? b)) (true) (false))))]
    [(?..? expr)
     (let ([v (fri (?..-e expr) env)])
       (cond
         [(triggered? v) v]
         [(..? v) (true)]
         [else (false)]))]
    [(?seq? expr)
     (let ([s (fri (?seq-e expr) env)])
       (cond
         [(triggered? s) s]
         [(empty? s) (true)]
         [(..? s) (let ([v2 (..-e2 s)]) (if (empty? v2) (true) (fri (?seq v2) env)))]
         [else (false)]))]
    [(?empty? expr)
     (let ([v (fri (?empty-e expr) env)]) (if (triggered? v) v (if (empty? v) (true) (false))))]
    [(?exception? expr)
     (let ([ex (fri (?exception-e expr) env)])
       (if (triggered? ex) ex (if (exception? ex) (true) (false))))]
    [(add? expr)
     (let ([e1 (add-e1 expr)] [e2 (add-e2 expr)])
       (let ([v1 (fri e1 env)] [v2 (fri e2 env)])
         (cond
           [(triggered? v1) v1]
           [(triggered? v2) v2]
           [(and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
            (if (or (true? v1) (true? v2)) (true) (false))]
           [(and (int? v1) (int? v2)) (int (+ (int-e v1) (int-e v2)))]
           ; from v1, rec insert first till not empty
           [(and (true? (fri (?seq v1) env)) (true) (fri (?seq v2) env))
            (let loop ([lst v1])
              (if (empty? lst)
                  v2
                  (let ([first (..-e1 lst)]
                        [rest (..-e2 lst)])
                    (.. first (loop rest)))))]
           [(and (empty? v1) (empty? v2)) v1]
           [(and (..? v1) (empty? v2)) v1]
           [(and (empty? v1) (..? v2)) v2]
           [else (triggered (exception "add: wrong argument type"))])))]
    [(mul? expr)
     (let ([e1 (mul-e1 expr)] [e2 (mul-e2 expr)])
       (let ([v1 (fri e1 env)] [v2 (fri e2 env)])
         (cond
           [(triggered? v1) v1]
           [(triggered? v2) v2]
           [(and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
            (if (and (true? v1) (true? v2)) (true) (false))]
           [(and (int? v1) (int? v2)) (int (* (int-e v1) (int-e v2)))]
           [else (triggered (exception "mul: wrong argument type"))])))]
    [(?leq? expr)
     (let ([e1 (?leq-e1 expr)] [e2 (?leq-e2 expr)])
       (let ([v1 (fri e1 env)] [v2 (fri e2 env)])
         (cond
           [(triggered? v1) v1]
           [(triggered? v2) v2]
           [(and (true? v1) (true? v2)) (true)]
           [(and (false? v1) (false? v2)) (true)]
           [(and (false? v1) (true? v2)) (true)]
           [(and (true? v1) (false? v2)) (false)]
           [(and (int? v1) (int? v2)) (if (<= (int-e v1) (int-e v2)) (true) (false))]
           ;  [(and (int? v1) (int? v2)) (<= (int-e v1) (int-e v2))] ; ??
           [(and (empty? v1) (empty? v2)) (true)]
           [(empty? v2) (false)]
           [(empty? v1) (true)]
           [(and (..? v1) (..? v2))
            (cond
              [(and (empty? v1) (not (empty? v2))) (true)]
              [(and (not (empty? v1)) (empty? v2)) (false)]
              [(and (empty? v1) (empty? v2)) (true)]
              [else (fri (?leq (..-e2 v1) (..-e2 v2)))])]
           [else (triggered (exception "?leq: wrong argument type"))])))]
    [(?=? expr)
     (let ([e1 (?=-e1 expr)] [e2 (?=-e2 expr)])
       (let ([v1 (fri e1 env)] [v2 (fri e2 env)])
         (cond
           [(triggered? v1) v1]
           [(triggered? v2) v2]
           [(and (empty? v1) (empty? v2)) (true)]
           [(and (false? v1) (false? v2)) (true)]
           [(and (true? v1) (true? v2)) (true)]
           [(and (true? v1) (false? v2)) (false)]
           [(and (false? v1) (true? v2)) (false)]
           [(and (int? v1) (int? v2)) (if (eq? (int-e v1) (int-e v2)) (true) (false))]
           [(and (..? v1) (..? v2))
            (cond
              [(true? (fri (?= (..-e1 v1) (..-e1 v2)) env)) (fri (?= (..-e2 v1) (..-e2 v2)) env)]
              [else (false)]
              )]
           [else (false)])))]
    [(head? expr)
     (let ([e (fri (head-e expr) env)])
       (cond
         [(triggered? e) e]
         [(empty? e) (triggered (exception "head: empty sequence"))]
         [(..? e) (..-e1 e)]
         [else (triggered (exception "head: wrong argument type"))]))]
    [(tail? expr)
     (let ([e (fri (tail-e expr) env)])
       (cond
         [(triggered? e) e]
         [(empty? e) (triggered (exception "tail: empty sequence"))]
         [(..? e) (..-e2 e)]
         [else (triggered (exception "tail: wrong argument type"))]))]
    [(~? expr)
     (let ([v (fri (~-e expr) env)])
       (cond
         [(triggered? v) v]
         [(int? v) (int (- (int-e v)))]
         [(true? v) (false)]
         [(false? v) (true)]
         [else (triggered (exception "~: wrong argument type"))]))]
    [(?all? expr)
     (let ([v (fri (?all-e expr) env)])
       (cond
         [(triggered? v) v]
         [(true? (fri (?seq v) env))
          (cond
            [(empty? v) (true)]
            [(false? (..-e1 v)) (false)]
            [else (fri (?all (..-e2 v)) env)])]
         [else (triggered (exception "?all: wrong argument type"))]))]
    [(?any? expr)
     (let ([v (fri (?any-e expr) env)])
       (cond
         [(triggered? v) v]
         [(true? (fri (?seq v) env))
          (cond
            [(empty? v) (false)]
            [(true? (..-e1 v)) (true)]
            [else (fri (?all (..-e2 v)) env)])]
         [else (triggered (exception "?any: wrong argument type"))]))]
    [(vars? expr)
     (let ([s (vars-s expr)]
           [e1 (vars-e1 expr)]
           [e2 (vars-e2 expr)])
       (if (and (list? s) (list? e1))
           ; map new variables and check for duplicate ids
           (let ([new-vars (map (lambda (var val) (cons var (fri val env))) s e1)])
             (if (equal? (length new-vars) (length (remove-duplicates (map car new-vars)))) ; names
                 (fri e2 (append new-vars env))
                 (triggered (exception "vars: duplicate identifier"))))
           (let ([v1 (fri e1 env)])
             (if (triggered? v1)
                 v1
                 (fri e2 (cons (cons s v1) env))))))]
    [(valof? expr)
     (let ([s (valof-s expr)])
       (let ([v (lookup s env)])
         (if (triggered? v)
             v
             (cond
               ; return funs as is
               [(closure? v) v]
               [(fun? v) v]
               [(proc? v) v]
               [else (fri v env)])
             )))]
    [(proc? expr) (closure env expr)]
    [(fun? expr)
     ; check fun definition (unique argument ids)
     (let ([fargs (fun-fargs expr)])
       (let ([unique-args-bool (equal? (length fargs) (length (remove-duplicates fargs)))])
         (if (not unique-args-bool)
             (triggered (exception "fun: duplicate argument identifier"))
             (closure env expr))))]
    [(call? expr)
     (let ([e (fri (call-e expr) env)]
           [args (map (lambda (val) (fri val env)) (call-args expr))]) ; eval passed args
       (cond [(triggered? e) e]
             [(triggered? args) args]
             [(closure? e)
              (let ([fun (closure-f e)] [fun-env (closure-env e)])
                (cond
                  [(triggered? fun) fun]
                  [(fun? fun)
                   (let ([name (fun-name fun)]
                         [arg-names (fun-fargs fun)]
                         [body (fun-body fun)])
                     (let ([arg-values (map (lambda (arg) (fri arg env)) args)]) ; eval arg values
                       (if (= (length arg-names) (length arg-values)) ; match arg count with fun def
                           (let ([new-env
                                  (append (list (cons name e))
                                          (append (map (lambda (i j) (cons i j)) arg-names arg-values)
                                                  fun-env))])
                             (if (triggered? new-env) new-env
                                 (let ([call (fri body new-env)])
                                   (if (triggered? call) ; handle undefined vars in closure
                                       (if (equal? (exception "valof: undefined variable") (triggered-exn call))
                                           (triggered (exception "closure: undefined variable"))
                                           call)
                                       call))))
                           (triggered (exception "call: arity mismatch")))))]
                  [(proc? fun)
                   (let ([name (proc-name fun)]
                         [body (proc-body fun)])
                     (if (= (length args) 0)
                         (fri body (cons (cons name e) env))
                         (triggered (exception "call: arity mismatch"))))]
                  [else (triggered (exception "call: wrong argument type"))])) ; non fun/proc types
              ]
             [else (triggered (exception "call: wrong argument type"))]
             )
       )] ; non closure types
    [else (error "Expression not found")]))

(define (lookup var env)
  (cond
    [(null? env) (triggered (exception "valof: undefined variable"))]
    [(equal? (caar env) var) (cdar env)]
    [else (lookup var (cdr env))]))

(define (greater e1 e2) (if (true? (fri (?= e1 e2) null)) (false) (?leq e2 e1)))

(define (rev e)
  (letrec ([rev-h (lambda (seq)
                    (cond
                      [(empty? seq) (empty)]
                      [(..? seq) (add (rev-h (..-e2 seq)) (.. (..-e1 seq) (empty)))]
                      [else (triggered (exception "rev: wrong argument type"))]))])
    (rev-h (fri e null))))

(define (binary e1)
  (let ([value (fri e1 null)])
    (cond
      [(and (int? value) (>= (int-e value) 0)) (rev (to-binary (int-e value)))]
      [else (triggered (exception "binary: wrong argument type"))])))

(define (to-binary n)
  (cond
    [(zero? n) (empty)]
    [else (.. (int (remainder n 2)) (to-binary (quotient n 2)))]))

(define (mapping f seq)
  (letrec ([map-h (lambda (arg)
                    (cond
                      [(empty? arg) (empty)]
                      [(..? arg) (.. (fri (call f (list (..-e1 arg))) null) (map-h (..-e2 arg)))]
                      [else (triggered (exception "mapping: wrong argument type"))]
                      ))])
    (map-h seq)))

(define (filtering f seq)
  (letrec ([filter-h (lambda (arg)
                       (cond
                         [(empty? arg) (empty)]
                         [(true? (fri (call f (list (..-e1 arg))) null))
                          (.. (..-e1 arg) (filter-h (..-e2 arg)))]
                         [else (filter-h (..-e2 arg))]))])
    (filter-h seq)))

(define (folding f init seq)
  (letrec ([fold-h (lambda (arg acc)
                     (cond
                       [(empty? arg) acc]
                       [(..? arg) (fold-h (..-e2 arg) (fri (call f (list (..-e1 arg) acc)) null))]
                       [else (triggered (exception "folding: wrong argument type"))]
                       ))])
    (fold-h seq init)))
