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
