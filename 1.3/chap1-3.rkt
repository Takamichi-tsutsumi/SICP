#lang racket
(require rackunit)

; 1.3.1

; sum of the integers from a through b
;(define (sum-integers a b)
;  (if (> a b)
;      0
;      (+ a (sum-integers (+ a 1) b))))

;(sum-integers 2 5)


(define (cube x)
  (* x x x))

; sum of the cubes of integers in the given range
;(define (sum-cubes a b)
;  (if (> a b)
;      0
;      (+ (cube a)
;         (sum-cubes (+ a 1) b))))

;(sum-cubes 1 3)


; sum of a sequence of terms in the series
; 1/1*3 + 1/5*7 + 1/9*11 ...
;(define (pi-sum a b)
;  (if (> a b)
;      0
;      (+ (/ 1.0 (* a (+ a 2)))
;         (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))
(sum-cubes 1 3)

(define (identity n) n)
(define (sum-integers a b)
  (sum identity a inc b))
(sum-integers 2 5)

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))
(* 8 (pi-sum 1 1000))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
(integral cube 0 1 0.01)


; Exercise 1.29

