#lang racket

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))
(list-ref squares 3)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))
(length odds)

; (append odds squares)
; (append squares odds)

;(define (append list1 list2)
;  (if (null? list1)
;      list2
;      (cons (car list1) (append (cdr list1) list2)))) 
;(append odds squares)


; Exercise 2.17
(define (last-pair items)
  (if (= 2 (length items))
      (cdr items)
      (last-pair (cdr items))))

(last-pair (list 23 72 149 34))

; Exercise 2.18
(define (reverse items)
  (if (= 1 (length items))
      items
      (append (reverse (cdr items)) (list (car items)))))

(reverse odds)

(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17))

;; 2.2.2 Hierarchical Structures
(define x (cons (list 1 2) (list 3 4)))
(list x x)

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(pair? (list 1 2))
(pair? (cons 1 2))
(null? '())
(null? (cdr '(1)))

(count-leaves x)
(count-leaves (list x x))
(count-leaves (cons x x))

(list 1 (list 2 (list 3 4)))

;; ex 2.26
(define a (list 1 2 3))
(define b (list 4 5 6))

(append a b)
(cons a b)
(list a b)

;; ex 2.27
(define x28 (list (list 1 2) (list 3 4)))
x28
(reverse x28)

(define (deep-reverse l)
  (cond ((null? l) null)
        ((pair? (car l))
         (append
                           (deep-reverse (cdr l))

          (list (deep-reverse (car l)))
                 ))
        (else
         (append (list (car l))
                 (deep-reverse (cdr l))))))

      
(deep-reverse x28)