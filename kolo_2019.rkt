#lang racket
;1a Napisać funkcję (delete x l), która usuwa wszystkie wystąpienia elementu x w liście l
(define (delete x ls)
  (if (null? ls)'()
      (if (eq? x (car ls))
          (delete x (cdr ls))
          (cons (car ls) (delete x (cdr ls))))))

;1b: Napisać funkcję (map f l), która na wszystkich elementach listy l wywołuje funkcję f
;Np. l=(1,2,3,4,5),(map square l)
;spowoduje powstanie takiej listy: (1,4,9,16,25)

(define (mapf function list)
    (if (null? list)
        '()
        (cons (function (car list))
              (mapf function (cdr list)))))

;2a: Napisać funkcję (fib n) obliczającą n-ty wyraz ciągu Fibonacciego
(define (fib n)
  (cond ((= 0) 0)
        ((= 1 ) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (union A B)
  (set-union A B))

;; wlasna implementacja
(define (element-of-set x lst1)
  (cond ((null? lst1) #f)
        ((equal? x (car lst1)) #t)
        (else (element-of-set x (cdr lst1)))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set (car set1) set2)
         (union-set (cdr set1) set2))
        (else
         (cons (car set1) (union-set (cdr set1) set2)))))