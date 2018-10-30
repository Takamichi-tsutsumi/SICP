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