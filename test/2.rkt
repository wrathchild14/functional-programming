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

(define-syntax swap
    (syntax-rules ()
        ((swap x y)
            (let ([tmp x])
              (set! x y)
              (set! y tmp)))))

(let* ([tmp 5]
       [other 6])
    (let ([tmp tmp])
        (set! tmp other)
        (set! other tmp))
    (list tmp other))

 (let ([tmp 5]
       [other 6])
    (swap tmp other)
    (list tmp other))
