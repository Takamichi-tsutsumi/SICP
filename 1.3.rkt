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

; (fixed-point-print (lambda (x) (/ (log 1000) (log x))) 5.0)

; Exercise 1.37
(define (cont-frac n d k)
  (define (iter-frac i)
    (display (d i))
    (newline)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (iter-frac (+ i 1))))))
  (iter-frac 1))

; a. k is 11
;(define phi (/ 1.0 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)))
;phi

; b. write iterative process version of cont-frac

(define (cont-frac-iterative n d k)
  (define (iter-frac i acc)
    (if (= i 1)
        (/ (n i) (+ (d i) acc))
        (iter-frac (- i 1)
                   (/ (n i) (+ (d i) acc)))))
  (iter-frac k 0))

(/ 1 (cont-frac-iterative (lambda (i) 1.0) (lambda (i) 1.0) 11))

; Exercise 1.38
(+ 2 (cont-frac
 (lambda (i) 1.0)
 (lambda (i)
   (if (= (remainder i 3) 2)
       (* 2 (+ 1 (quotient i 3)))
       1))
 10))
