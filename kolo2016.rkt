#lang racket

; zad1a)
(define (member x l)
  (cond
    ((null? l) #f)
    ((eq? (car l) x) #t)
    (else (member x (cdr l)))))

; zad 2 a)
(define (numbers n)
  (append (range (+ n 1))))

; zad 2 b)
(define (my-flatten lst)
  (cond ((null? lst) '())
        ((pair? lst)
         (append (my-flatten (car lst)) (my-flatten (cdr lst))))
        (else (list lst))))


