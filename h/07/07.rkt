#lang racket

(define ones
  (cons 1 (lambda () ones)))

(define naturals
  (letrec ([generate (lambda (x) (cons x (lambda () (generate (+ x 1)))))])
    (generate 1)))

(define fibs
  (letrec ([generate (lambda (x y) (cons x (lambda () (generate y (+ x y)))))])
    (generate 1 1)))

(define (first n str)
  (if (= n 0)
      null
      (cons (car str) (first (- n 1) ((cdr str))))))

(define (squares str)
  (letrec ([generate (lambda (x) (cons (* (car x) (car x)) (lambda () (generate ((cdr x))))))])
    (generate str)))

(define-syntax sml
  (syntax-rules
    (nil null :: hd tl)
    ((sml nil) '())
    ((sml null xs) (null? xs))
    ((sml head :: tail) (cons head tail))
    ((sml hd xs) (car xs))
    ((sml tl xs) (cdr xs))))

(define (my-delay a)
  '())

(define (my-force a)
  '())

(define (partitions a b)
  '())
