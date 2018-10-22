#lang racket
; 1.3.4 Procedures as Returned Values
(define tolerance 0.00001)

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

((average-damp square) 10)

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

(define dx 0.00001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (cube x) (* x x x))

((deriv cube) 3)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; Abstractions and first-class procedures
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;(define (sqrt x)
;  (fixed-point-of-transform
;   (lambda (y) (/ x y)) average-damp 1.0))

; Exercise 1.40
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

; Exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))

(define (inc x)
  (+ 1 x))

(((double (double double)) inc) 5) ; returns 21

; Exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)

; Exercise 1.43
(define (repeated f n)
  (cond (> 0 n) (error "n must be positive number"))
  (define (iter f n result)
    (if (= n 0)
        result
        (iter f (- n 1) (compose f result))))
  (iter f (- n 1) f))

((repeated inc 10) 5)

; Exercise 1.44
(define (smooth f)
  (lambda (x) (/ (+ (f x) (f (- x dx)) (f (+ x dx))) 3)))

(define (n-fold f n)
        ((repeated smooth n) f))

; Exercise 1.46
(define (iterative-improve check improve)
  (define (iterate guess)
    (let ((next (improve guess)))
      (if (check guess next)
          next
          (iterate next))))
  (lambda (initial-guess) (iterate initial-guess)))

(define (fixed-point-iter f first-guess)
  ((iterative-improve
   (lambda (x y) (< (abs (- x y)) 0.00001))
   f) first-guess))

(fixed-point-iter (lambda (x) (sin x)) 0.1)
(fixed-point (lambda (x) (sin x)) 0.1)