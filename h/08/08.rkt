#lang racket

(struct int (n) #:transparent)
(struct true () #:transparent)
(struct false () #:transparent)
(struct add (x y) #:transparent)
(struct mul (x y) #:transparent)
(struct ?leq (x y) #:transparent)
(struct ~ (x) #:transparent)
(struct ?int (x) #:transparent)
(struct if-then-else (cond e1 e2) #:transparent)

(define (fri expr)
  (match expr
    [(int _) expr]
    [(true) expr]
    [(false) expr]
    [(add x y)
     (match (list (fri x) (fri y))
       [(list (true) _) (true)]
       [(list _ (true)) (true)]
       [(list (int a) (int b)) (int (+ a b))]
       [_ expr]
       )
     ]
    [(mul x y)
     (match (list (fri x) (fri y))
       [(list (true) (true)) (true)]
       [(list _ (true)) (false)]
       [(list (int a) (int b)) (int (* a b))]
       [_ expr]
       )
     ]
    [(?leq x y)
     (match (list (fri x) (fri y))
       [(list (true) e2) e2]
       [(list _ (true)) (true)]
       [(list (int a) (int b)) (if (<= a b) (true) (false))]
       [_ expr]
       )
     ]
    [(~ x)
     (match (fri x)
       [(int a) (int (- a))]
       [(true) (false)]
       [(false) (true)]
       [_ expr]
       )
     ]
    [(?int x)
     (match (fri x)
       [(int _) (true)]
       [_ (false)]
       )
     ]
    [(if-then-else cond e1 e2)
     (match (fri cond)
       [(true) (fri e1)]
       [(false) (fri e2)]
       [_ (error "Bad condition")]
       )
     ]
    )
  )

(fri (?int (int 5)))
(fri (if-then-else (true) (int 5) (add (int 2) (int "a"))))