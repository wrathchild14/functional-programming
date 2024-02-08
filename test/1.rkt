#lang racket

(define (fun1 x) (+ x (car x)))

; 
(define (foldX/stream func acc stop stream)
    (if (= stop 0)
    acc
    (foldX/stream func (func (car stream) acc) (- stop 1) ((cdr stream)))))

(define naravna
    (letrec ([get (lambda (x) (cons x (lambda () (get (+ x 1)))))])
    (get 1)))

(foldX/stream + 0 5 naravna)

; 
(define (izpisi n tok)
  (if (> n 1) 
      (begin
        (displayln (car tok))
        (izpisi (- n 1) ((cdr tok))))
      (displayln (car tok))))

(define (prozi f n)
  (define (helper i)
    (cons (if (= i n)
              (cons #t (force (delay (f i))))
              (cons #f (delay (f i))))
          (lambda () (helper (+ i 1)))))
  (helper 1))

(izpisi 5 (prozi (lambda (x) (* x 3)) 2))
(displayln "-----")
(izpisi 5 (prozi (lambda (x) (* x 3)) 4))

(define ones (cons 1 (lambda () ones)))


(define (make-stream prev-fn current next-fn)
  (list prev-fn current next-fn))

(define (prev stream)
  ((first stream) stream))

(define (current stream)
  (second stream))

(define (next stream)
  ((third stream) stream))

(define potence
  (letrec ([potence (make-stream
                      (lambda (s) (make-stream prev (/ (current s) 2) next))
                      2
                      (lambda (s) (make-stream prev (* (current s) 2) next)))])
    potence))

         
; cons-stream - works on streams - creates a pair and delays the evaluation of the second pair
; (define (unpack f)
;     (stream-cons
;      f
;      (if (procedure? f)
;              (unpack (f))
;              (let ((result (f)))
;                  (stream-cons result (unpack result))))))
; (define f1
;  (lambda ()
;  (lambda ()
;  (lambda ()
;  (lambda ()
;  (+ 3 2))))))
; (izpisi 10 (unpack f1))

; (define (funkcijski f initial)
;   (stream-cons initial (funkcijski f (f initial))))

; (define (rotate sez) (append (cdr sez) (list (car sez))))
; (define x (funkcijski rotate (list 1 3 5 7 9 11)))
; (izpisi 8 x)

(define izpisi_primitave
  (lambda sez
    (displayln sez)))

(define zmnozi
  (lambda stevila
    (apply * stevila)))


; Primer na funcija ki ni sprejeta v trdnih sistemov:
; To je LP primer fun, i guess zaradi if true then 0 del
; f x = if true then 0 else 4 div "hello"
(define (f x)
  (if #t
      0
      (/ 4 "hello")))
