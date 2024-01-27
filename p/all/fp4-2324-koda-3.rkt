#lang racket

; ***************************************************************
; ************************** 1. UVOD ****************************
; ***************************************************************

; To je komentar

#|
To je večvrstični komentar
|#

; #lang racket ; prva direktiva na vrhu datoteke


; definicija spremenljivke
(define x "Hello world")   

; operacije
(define q 3)
(define w (+ q 2))
(define e (+ q 2 1 w))


;DEFINICIJA FUNKCIJ
;uporaba ključne besede LAMBDA
(define sestej1
  (lambda (a b)
    (+ a b)))
; ALI: ekvivalentno (sintaktična olepšava)
(define (sestej2 a b)
  (+ a b))


;POGOJNI STAVEK (IF)
;(if pogoj ce_res ce_nires)
; > (if (< 3 2) "a" 100)
; 100
; > (if (< 3 12) "a" #t)
; "a"



; CURRYING
; izračun potence
(define (pot x n)
  (if (= n 0) 
      1
      (* x (pot x (- n 1)))))
; ovojna funkcija, ki izvaja currying argumentov
(define potenca2
  (lambda (x) 
    (lambda (n)
      (pot x n))))

; oklepaji se uporabljajo za klicanje funkcije
; > (potenca2 2)
; > ((potenca2 2) 3) 

(define stiri_na (potenca2 4))
; > (stiri_na 3)




; ***************************************************************
; ******************* 2. PARI IN SEZNAMI ************************
; ***************************************************************

(define p1 (cons "a" 1))
(define p2 (cons "a" (cons 2 (cons #f 3.14))))
(define l1 (cons "a" (cons 2 (cons #f null))))
(define l2 (cons "a" (cons 2 (cons #f (cons 3.14 null)))))
(define sez (list "a" 2 #f 3.14))
;
#|
> (car sez)
"a"
> (cdr sez)
'(2 #f 3.14)
> (car (cdr (cdr sez)))
#f
> (car (cdr (cdr (cdr l3))))
3.14
> (null? (cdr (cdr (cdr (cdr l3)))))
|#



; ******************** FUNKCIJE NAD SEZNAMI *********************

; vsota seznama
(define (vsota_sez sez)
  (if (null? sez)
      0
      (+ (car sez) (vsota_sez (cdr sez)))))

; filter
(define (mojfilter f sez)
  (if (null? sez)
      null
      (if (f (car sez))
          (cons (car sez) (mojfilter f (cdr sez)))
          (mojfilter f (cdr sez)))))

; vgnezdeno štetje
(define a (list 1 2 5 "a"))
(define b (list (list 1 2 (list #f) "lala") (list 1 2 3) 5))

; vgnezdeno štetje - s stavkom IF
(define (prestej sez)
  (if (null? sez)
      0
      (if (list? (car sez))
          (+ (prestej (car sez)) (prestej (cdr sez)))
          (+ 1 (prestej (cdr sez))))))
; vgnezdeno štetje - s stavkom COND
(define (prestej1 sez)
  (cond [(null? sez) 0]
        [(list? (car sez)) (+ (prestej (car sez)) (prestej (cdr sez)))]
        [#t (+ 1 (prestej (cdr sez)))]))




; ***************************************************************
; ******************** 3. LOKALNO OKOLJE ************************
; ***************************************************************

#|
let  ; izrazi se evalvirajo v okolju PRED izrazom let
let* ; izrazi se evalvirajo kot rezultat predhodnih deklaracij (tako dela SML)
letrec ; izrazi se evalvirajo v okolju, ki vključuje vse podane deklaracije
define ; semantika ekvivalentna kot pri letrec, drugačna sintaksa
|#

; delovanje let
(define (test-let a)
  (let ([a 3]
        [b (+ a 2)])
    (+ a b)))    
; > (test-let 10)
; 3 + (10+2) = 15


; delovanje let*
(define (test-let* a)
  (let* ([a 3]
         [b (+ a 2)])
    (+ a b)))    
; > (test-let* 10)
; 3 + (3+2) = 8


; delovanje letrec
(define (test-letrec a)
  (letrec ([b 3]
           [c (lambda (x) (+ a b d x))]
           [d (+ a 1)])
    (c a)))    
; > (test-letrec 50)
; 154   ; a=50, b=3, c=..., d= 51
; (c 50) = 50 + 3 + 51 + 50 = 154


; nedelovanje: deklaracije se izvajajo zaporedno
(define (test-letrec2 a)
  (letrec ([b 3]
           [c (+ d 1)]
           [d (+ a 1)])
    (+ a d)))    
; d: undefined;
; cannot use before initialization


; delovanje define (enakovredno letrec)
(define (test-define a)
  (define b 3)
  (define c (lambda (x) (+ a b d x)))
  (define d (+ a 1))
  (c a))   
; > (test-define 50)
; 154

; ***************************************************************
; ***************** 4. ZAKASNITVENE FUNKCIJE ********************
; ***************************************************************

#|
(define (potenca x n)
  (if (= n 0) 
      1
      (* x (potenca x (- n 1)))))
|#

; primer, ki ne deluje - neskoncna rekurzija
(define (moj-if pogoj res nires)
  (if pogoj res nires))

(define (potenca-moj x n)
  (moj-if (= n 0) 
          1
          (* x (potenca-moj x (- n 1)))))


; uporaba zakasnjene evalvacije - thunking
(define (moj-if-super pogoj res nires)
  (if pogoj (res) (nires)))

(define (potenca-super x n)
  (moj-if-super (= n 0) 
          (lambda () 1)
          (lambda () (* x (potenca-super x (- n 1))))))



; ***************************************************************
; ******************** 5. ZAKASNJENA EVALVACIJA *****************
; ***************************************************************

; funkcija za testiranje - vrne število x z zakasnitvijo (simulacija dolgega izračuna)
(define (dolga_operacija x)
  (begin
    (printf "Dolga operacija~n")
    (sleep 1)    ; počaka 1 sekundo
    x))


; ***************************************************************
; 1. PRIMER: osnovna verzija potence, eksponent zakasnjen *******

; izračuna x^n; n dobimo zakasnjeno (thunk) s klicem (klic_n)
(define (potenca x klic_n)
  (cond [(= x 0) 0]
        [(= x 1) 1]
        [(= (klic_n) 1) x]
        [#t (* x (potenca x (lambda () (- (klic_n) 1))))]))

; (potenca 0 (lambda () (dolga_operacija 2)))    ; 0x evalvacija eksponenta  :)
; (potenca 1 (lambda () (dolga_operacija 20)))   ; 0x evalvacija eksponenta  :)
; (potenca 200 (lambda () (dolga_operacija 1)))  ; 1x evalvacija eksponenta  :|
; (potenca 2 (lambda () (dolga_operacija 4)))    ; 4x evalvacija eksponenta  :(


; ***************************************************************
; 2. PRIMER: uporabimo lokalno spremenljivko za eksponent *******

;(potenca 0 (let ([rez (dolga_operacija 2)]) (lambda () rez)))   ; 1x evalvacija eksponenta  :(
;(potenca 1 (let ([rez (dolga_operacija 2)]) (lambda () rez)))   ; 1x evalvacija eksponenta  :(
;(potenca 200 (let ([rez (dolga_operacija 1)]) (lambda () rez))) ; 1x evalvacija eksponenta  :|
;(potenca 2 (let ([rez (dolga_operacija 4)]) (lambda () rez)))   ; 1x evalvacija eksponenta  :)



; ***************************************************************
; 3. PRIMER: uporabimo zakasnitev in sprožitev ******************

; ORODJE: delo s spremenljivimi seznami (MCONS)
#|
(define msez (mcons 1 (mcons 2 3)))
msez
(mcar msez)
(mcdr msez)
(mcar (mcdr msez))
(set-mcar! msez 4)
msez
(set-mcdr! msez (mcons 5 6))
(set-mcar! (mcdr msez) 7)
|#      

; zakasnitev
(define (my-delay thunk) 
  (mcons #f thunk)) 

; sprožitev
(define (my-force prom)
  (if (mcar prom)
      (mcdr prom)
      (begin (set-mcar! prom #t)
             (set-mcdr! prom ((mcdr prom)))
             (mcdr prom))))

; primer delovanja zakasnitve in sprožitve
#|   
> (define md (my-delay (lambda () (+ 3 2))))
> md
> (mcdr md)
> ((mcdr md))
> (my-force md)
> md
> (my-force md)
> (my-force md)
> md
|#

#|
(potenca 0 (let* ([rez (my-delay (lambda () (dolga_operacija 2)))]) 
             (lambda () (my-force rez))))                            ; 0x evalvacija eksponenta  :)
(potenca 1 (let* ([rez (my-delay (lambda () (dolga_operacija 2)))]) 
             (lambda () (my-force rez))))                            ; 0x evalvacija eksponenta  :)
(potenca 200 (let* ([rez (my-delay (lambda () (dolga_operacija 1)))]) 
               (lambda () (my-force rez))))                          ; 1x evalvacija eksponenta  :|
(potenca 200 (let* ([rez (my-delay (lambda () (dolga_operacija 3)))]) 
               (lambda () (my-force rez))))                          ; 1x evalvacija eksponenta  :)
|#




; ***************************************************************
; *************************** 6. TOKOVI *************************
; ***************************************************************

(define enke (cons 1 (lambda () enke)))

;(car enke)                    ; prvi element
;(car ((cdr enke)))            ; drugi element
;(car ((cdr ((cdr enke)))))    ; tretji element


(define naravna (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
                  (f 1)))

(define plusminus (letrec ([f (lambda (x) (cons x (lambda () (if (= x 1) (f -1) (f 1)))))])
                  (f 1)))

(define potence (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
                  (f 2)))

; funkcije nad tokovi

; izpiši prvih n
(define (izpisi n tok)
  (if (> n 1) 
      (begin
        (displayln (car tok))
        (izpisi (- n 1) ((cdr tok))))
      (displayln (car tok))))

; izpisi dokler velja POGOJ
(define (izppog tok pogoj)
  (cond [(pogoj (car tok)) (begin
                             (displayln (car tok))
                             (izppog ((cdr tok)) pogoj))]
        [#t #t]))
                           



; ***************************************************************
; ********************** 7. MEMOIZACIJA *************************
; ***************************************************************

; rekurzivna rešitev
(define (fib1 x)
  (cond [(= x 1) 1]
        [(= x 2) 1]
        [#t (+ (fib1 (- x 1))
               (fib1 (- x 2)))]))

; rekurzivna rešitev z akumulatorjem
(define (fib2 x)
  (letrec ([pomozna (lambda (f1 f2 n)   ; n-to fib število se izračuna kot f1 + f2
                      (cond [(= n x) (+ f1 f2)]
                            [#t (pomozna f2 (+ f1 f2) (+ n 1))]))])
    (cond [(= x 1) 1]
          [(= x 2) 1]
          [#t (pomozna 1 1 3)])))      


; assoc
; (define resitve (list (cons 1 "a") (cons 2 "b") (cons 3 "c") (cons 4 "d")))
; (assoc 2 resitve)
; (assoc 4 resitve)
; (assoc 7 resitve)
; (assoc "b" resitve)


;(set! spremenljivka vrednost) ; spremeni vrednost x
;(define x 15)
;(set! x 9)                    ; mutacija

;(define resitve (list (cons 1 "a") (cons 2 "b") (cons 3 "c") (cons 4 "d")))
;(set! resitve 4)              ; spremeni podatkovni tip

;(car (car resitve))

;(set! (car (car resitve)) 5)  ; ne moremo spreminjati delov seznama

;(define resitve null)

(define fib3
  (letrec ([resitve null]  
           [pomozna (lambda (x)
                      (let ([ans (assoc x resitve)])              ; poiscemo resitev
                        (if ans 
                            (cdr ans)                             ; vrnemo obstojeco resitev
                            (let ([nova (cond [(= x 1) 1]         ; resitve ni
                                              [(= x 2) 1]
                                              [#t (+ (pomozna (- x 1))      ; izracun resitve
                                                     (pomozna (- x 2)))])])
                              (begin
                                ;(displayln resitve)
                                (set! resitve (cons (cons x nova) resitve)) ; shranimo resitev
                                nova)))))])                                 ; vrnemo resitev
    pomozna))



; ***************************************************************
; ************************* 8. MAKRI ****************************
; ***************************************************************


; PRIMERI MAKROV ***********************************************


; moj-if, ki uporablja besedi then in else
(define-syntax mojif             ; ime makra
  (syntax-rules (then else)       ; druge ključne besede
    [(mojif e1 then e2 else e3)  ; sintaksa makra
     (if e1 e2 e3)]))             ; razširitev makra

; trojni if z 2 pogojema in 3 izidi
(define-syntax if-trojni 
  (syntax-rules (then elsif else)
    [(if-trojni e1 then e2 elsif e3 then e4 else e5)
     (if e1 e2 (if e3 e4 e5))]))
; (if-trojni #t then 1 elsif #t then 2 else 3)
; (if-trojni #f then 1 elsif #t then 2 else 3)
; (if-trojni #f then 1 elsif #f then 2 else 3)

; prvi element toka
(define-syntax prvi 
  (syntax-rules ()
    [(prvi e)
     (car e)]))
; drugi element toka
(define-syntax drugi
  (syntax-rules ()
    [(drugi e)
     (car ((cdr e)))]))
; tretji element toka
(define-syntax tretji
  (syntax-rules ()
    [(drugi e)
     (car ((cdr ((cdr e)))))]))

;(define naravna
;  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
;    (f 1)))


; (prvi naravna)
; (drugi naravna)
; (tretji naravna)

;anotacija spremenljivk
(define-syntax anotiraj
  (syntax-rules ()
    [(anotiraj e s)
     e]))

; primer anotacije spremenljivk

(define fib4
  (letrec ([resitve (anotiraj null "zacetna resitev je prazna")]  
           [pomozna (lambda (x)
                      (let ([ans (assoc x resitve)])         
                        (if (anotiraj ans "odgovor ze obstaja") 
                            (anotiraj (cdr ans) "vrnemo obstojeco resitev")
                            (let ([nova (cond [(= x 1) 1]         ; resitve ni
                                              [(= x 2) 1]
                                              [#t (+ (pomozna (- x 1))      ; izracun resitve
                                                     (pomozna (- x 2)))])])
                              (begin 
                                (set! resitve (cons (cons x nova) resitve)) ; shranimo resitev
                                nova)))))])                                 ; vrnemo resitev
    pomozna))



; 1. SMOTRNOST UPORABE *****************************************

; primernost uporabe my-delay in my-force

; DEFINICIJA ZAKASNITVE IN SPROŽITVE
; zakasnitev 
(define (my-delay-fun thunk) 
  (mcons #f thunk)) 
; sprožitev
(define (my-force-fun prom)
  (if (mcar prom)
      (mcdr prom)
      (begin (set-mcar! prom #t)
             (set-mcdr! prom ((mcdr prom)))
             (mcdr prom))))

; uporaba makra
(define-syntax my-delaym
  (syntax-rules ()
    [(my-delaym e)
     (mcons #f (lambda() e))]))

(define md (my-delay (lambda () (+ 3 2))))
(define mdm (my-delaym (+ 3 2)))

;> (my-force md)
;> md
;> (my-force mdm)
;> mdm



; 3. NAČIN EVALVACIJE ******************************************

; ekvivalentno
(define (dvakrat1 x) (+ x x))
(define (dvakrat2 x) (* 2 x))

; ni ekvivalentno
(define-syntax dvakrat3 
  (syntax-rules()
    [(dvakrat3 x)(+ x x)]))
(define-syntax dvakrat4 
  (syntax-rules()
    [(dvakrat4 x)(* 2 x)]))

; testiranje zgornjega s pomožno funkcijo
; samo izpiše in vrne vrednost
(define (vrni x)
  (begin
    (displayln x)
    x))
; (dvakrat3 (vrni 5))
; (dvakrat4 (vrni 3))

; rešitev: uporaba lokalnih spremenljivk
(define-syntax dvakrat5
  (syntax-rules()
    [(dvakrat5 x)(let ([mojx x]) (+ mojx mojx))]))
; (dvakrat5 (vrni 3))



; 4. DOSEG MAKRO DEFINICIJ *************************************

; 1. primer: upoštevanje leksikalnega dosega
(define-syntax formula   ; prišteje 5
  (syntax-rules ()
    [(formula x)
     (let ([y 5])
       (+ x y))]))

; naivna razširitev
(define (f1 x y)
  (+ x y (let ([y 5])
           (+ y y))))
; (f1 1 2)

; upoštevanje leksikalnega dosega
(define (f2 x y)
  (+ x y (formula y)))
; (f2 1 2)



; 2. primer: zamenjava elementov
(define-syntax swap
  (syntax-rules ()
    ((swap x y)
     (let ([tmp x])
       (set! x y)
       (set! y tmp)))))

#|  naivna razširitev
(let ([tmp 5]
      [other 6])
  (let ([tmp tmp])
    (set! tmp other)
    (set! other tmp))
  (list tmp other))
|#

#|  upoštevanje leksikalnega dosega
(let ([tmp 5]
      [other 6])
  (swap tmp other)
  (list tmp other))
|#