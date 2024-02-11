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
