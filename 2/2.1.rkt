#lang racket
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d))
        (positive (> (* n d) 0)))
    (cons (if positive
              (abs (/ n g))
              (- (abs (/ n g))))
          (abs (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (average x y)
  (/ (+ x y) 2))

; suppose we have procedures
; (make-rat n d) returns a rational numbers whose numerator n and denominator d
; (numer x) returns numerator of rational number x
; (denom x) returns denominator of rational number x
(define (add-rat x y)
  (make-rat
   (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat
   (- (* (numer x) (denom y)) (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat
   (* (numer x) (numer y))
   (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat
   (* (numer x) (denom y))
   (* (numer y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


(define one-half (make-rat 1 2))
(print-rat one-half)
(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

(define minus-one-half (make-rat -1 2))
(print-rat minus-one-half)

; Exercise 2.2
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (make-point
   (average (x-point (start-segment segment)) (x-point (end-segment segment)))
   (average (y-point (start-segment segment)) (y-point (end-segment segment)))))

(define (print-segment segment)
  (newline)
  (display "[")
  (print-point (start-segment segment))
  (display ",")
  (print-point (end-segment segment))
  (newline)
  (display "]"))

(define p1 (make-point 2 4))
(define p2 (make-point 12 17))
(define line-p (make-segment p1 p2))

; (midpoint-segment line-p)


; Exercise 2.3
(define (make-rect x y z)
  (list x y z))
(define (first-point rect)
  (car rect))
(define (second-point rect)
  (car (cdr rect)))
(define (third-point rect)
  (car (cdr (cdr rect))))

(define p3 (make-point 0 0))

(define rect-a (make-rect p1 p2 p3))
(first-point rect-a)
(second-point rect-a)
(third-point rect-a)
;; skip perimeter and area

; Exercise 2.4
(define (_cons x y)
  (lambda (m) (m x y)))

(define (_car z)
  (z (lambda (p q) p)))

(define (_cdr z)
  (z (lambda (p q) q)))

(_car (_cons 1 2))
(_cdr (_cons 1 2))

; Exercise 2.5
(define (times-div x y)
  (define (iter a res)
    (if (= (remainder a y) 0)
        (iter (/ a y) (add1 res))
        res))
  (iter x 0))
  
(define (__cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (__car z)
  (times-div z 2))

(define (__cdr z)
  (times-div z 3))

; Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
