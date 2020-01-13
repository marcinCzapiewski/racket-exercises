#lang racket

(define (nwd a b)
    (cond [(= b 0) a]
          [else (nwd b (modulo a b))]))

(define (nww a b)
  (/ (* a b) (nwd a b)))

(define (new.< x y) (< x y))

(define (new.> x y) (and (< y x) (or (< y x) (< x y))))

(define (new.= x y)
  (nor (< y x) (< x y)))

(define (new.<= x y)
  (or (new.= x y) (new.< x y)))

(define (new.>= x y)
  (or (new.= x y) (new.> x y)))

(define (new.<> x y)
  (not (new.= x y)))

(define (same-values? p1 p2 x y)
  (equal? (p1 x y) (p2 x y)))

(define (even? n)
  (cond [(zero? n) #t]
        [else (odd? (- n 1))]))

(define (odd? n)
  (cond [(zero? n) #f]
        [else (even? (- n 1))]))

(define (fib n)
  (if (<= n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (fib2 n) 
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0) b
      (fib-iter b (+ a b) (- count 1))))

(define (exp b e)
  (cond [(= e 0) 1]
        [(even? e) (let ((x ( exp b (/ e 2)))) (* x x))]
        [else (* b (exp b (- e 1)))]))

(define (exp2 b e)
  (exp-iter e b 1))

 (define (exp-iter e b acc)
   (if (= e 0) acc
       (exp-iter (- e 1) b (* acc b))))

(define (factorial n)
 (cond ((< n 0) #f)
         ((<= n 1) 1)
         (else (* n (factorial (- n 1))))))

(define (max-two a b c)
  (if (>= a  b)
      ;; then
      (if (>= b c)
          (cons a b)
          (cons a c))
      ;; else
   (if (>= a c)
      (cons b a)
      (cons b c))))
(define (sum-of-squares x y)
  (+ (* x x) (* y y)))
(define (sum-of-squares-of-maxima a b c)
  (sum-of-squares (car (max-two a b c)) (cdr (max-two a b c))))

(define (call f x)
  (f x))

(define (new-if warunek alternatywa1 alternatywa2)
   (cond (warunek alternatywa1)
         (else    alternatywa2)))

(define (newfib n)
  (new-if (<= n 2)
      1
      (+ (newfib (- n 1)) (newfib (- n 2)))))

(define m 1)
(define (p m)
  (pp 5))
(define (pp x)
  (+ x m))

(define n 1)
(define (q n)
  (define (qq x)
    (+ x n))
  (qq 5))

(define (product term a next b) 
     (if (> a b) 1 
        (* (term a) (product term (next a) next b))))

(define (comb f g)
   (lambda (x) (f (g x))))

(define (square x) (* x x))

(define (double x) (+ x x))

(define (iter f n)
  (if (= n 1)
      f
      (lambda (x) 
        (f ((iter f (- n 1)) x)))))

(define (f g) (g 2))

(define (append lis1 lis2)
  (cond ((null? lis1)
         lis2)
        (else
         (cons (car lis1)
               (append (cdr lis1) lis2)))))

(define (reverse lis)
   (if (null? lis)
       '()
       (append (reverse (cdr lis))
               (list (car lis)))))

(define (last lis)
  (cond ((null? (cdr lis)) (car lis))
        (else (last (cdr lis)))))

(define (delete x ls)
  (if (null? ls)
      '()
      (if (eqv? x (car ls))
          (delete x (cdr ls))
          (cons (car ls)
                (delete x (cdr ls))))))

(define (split xs y) 
  (define (less x)    (<= x y))
  (define (greater x) (>  x y))
  (list (filter less xs)
        (filter greater xs)))

(define (square-list xs)
  (map square xs))

(define (mapf function list)
    (if (null? list)
        '()
        (cons (function (car list))
              (mapf function (cdr list)))))

(define (my-filter pred lst)
  (cond ((null? lst) null)
        ((pred (first lst))
         (cons (first lst) (my-filter pred (rest lst))))
        (else (my-filter pred (rest lst)))))

