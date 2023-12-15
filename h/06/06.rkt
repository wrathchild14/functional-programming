#lang racket
(provide (all-defined-out))

(define (power x n)
  (if (= n 0)
      1
      (* x (power x (- n 1)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (modulo a b))))

(define (fib x)
  (cond [(= x 1) 1]
        [(= x 2) 1]
        [#t (+ (fib (- x 1)) (fib (- x 2)))]))

(define (reverse arr)
  (if (null? arr)
      '()
      (append (reverse (cdr arr)) (list (car arr)))))

(define (remove x arr)
  (if (null? arr)
      '()
      (if (= x (car arr))
          (remove x (cdr arr))
          (cons (car arr) (remove x (cdr arr))))))

(define (map func lst)
  (if (null? lst)
      '()
      (cons (func (car lst)) (map func (cdr lst)))))

(define (filter func lst)
  (if (null? lst)
      '()
      (if (func (car lst))
          (cons (car lst) (filter func (cdr lst)))
          (filter func (cdr lst)))))

(define (zip lst1 lst2)
  '())

(define (range start end step)
  '())

(define (is-palindrome lst)
  '())
