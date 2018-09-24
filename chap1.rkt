#lang racket
; Chapter 1.1.1
486
(+ 137 349)
(- 1000 334)
(* 5 99)
(/ 10 5)
(+ 2.7 10)

(+ 21 35 12 7)
(* 25 4 12)
(+ (* 3 5) (- 10 6))
(+
 (* 3
    (+ (* 2 4)
       (+ 3 5)))
 (+ (- 10 7)
    6))


; Chapter 1.1.2
(define size 2)
(* 5 size)
(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
(define circumference (* 2 pi radius))

; Chapter 1.1.4
(define (square x) (* x x))
(square 4)
(square (* 2 5))


; Chapter 1.1.6
(define (absold x)
  (cond ((> x 0) x)
        ((= x 0 ) 0)
        ((< x 0) (- x))))
(absold 5)

(define (abs x)
  (cond ((> x 0) x)
        (else (- x))))
(abs -5)

(define (abs-if x)
  (if (< x 0) (- x) x))
(abs-if 0)

; Excercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

; Excercise 1.3
(define (sum-of-squares-of-biggest-two x y z)
  (cond ((and (< x y) (< x z)) (+ (* y y) (* z z)))
        ((and (< y x) (< y z)) (+ (* x x) (* z z)))
        (else (+ (* x x) (* y y)))))
(sum-of-squares-of-biggest-two 4 5 3)

; Excercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(a-plus-abs-b 3 -1)
(a-plus-abs-b 4 1)

; Excersise 1.5
; (define (p) (p))
; (define (test x y)
;   (if (= x 0) 0 y))
; (test 0 (p))
;
; normal order evaluation model
; expand all the expressions until all the expressions are primitive operator and operands.
; So, in above case, (test 0 (p)) has two operands and 0 itself is primitive number, and (p) is next evaluated.
; but then (p) has no value without defining p's value. so the interpreter might throw an error.
; on the other hand, in applicative order evaluation model,
; test is first evaluted with 0 and (p). in test function, if first operator is equal to 0, then returns 0 without
; evaluatin the second argument.

