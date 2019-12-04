#lang racket

; definitions from book
(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))
; exercise 1.3
(define (sum-of-largest-squares a b c)
  (if (> a b)
      (if (b > c)
          (sum-of-squares a b)
          (sum-of-squares a c))
      (sum-of-squares b c)))

; exercise 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

; definitions from book
(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter-new guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-new (improve guess x) x)))

; exercise 1.8
(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (improve-cube guess x) x)))

(define (cube-root x)
  (cube-root-iter 1.0 x))

; nesting definitions as local functions, lexical scoping
(define (sqrt-localdefs x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
    (sqrt-iter 1.0))