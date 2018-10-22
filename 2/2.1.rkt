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
