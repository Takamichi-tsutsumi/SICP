#lang racket
(define (average x y) (/ (+ x y) 2))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))
(sqrt 4.0)

; Exercise 1.35
; x^2 = x + 1
; x = (x + 1) / x = 1 + 1/x which is fixed-point
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

; Exercise 1.36
; x^x = 1000
; x = log(1000) / log(x)
(define (fixed-point-print f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point-print (lambda (x) (/ (log 1000) (log x))) 5.0)

; Exercise 1.37
