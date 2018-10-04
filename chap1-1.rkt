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

; Chapter 1.1.7
; Newton's method to calculate square root
(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (printf "guess = ~v, x = ~v\n" guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 3.0)

; Exercise 1.6
;(define (new-if predicate then-clause else-clause)
;  (cond (predicate then-clause)
;        (else else-clause)))
;
;(define (sqrt-iter-with-new-if guess x)
;  (new-if (good-enough? guess x)
;          guess
;          (sqrt-iter-with-new-if (improve guess x) x)))
;
;(sqrt-iter-with-new-if 1.0 4.0)
;
; When using function, all the arguments passed to the function are evaluated first. (applicative order)
; The third argument (sqrt-iter-with-new-if) are called recursively and cause infinite loop


; Exercise 1.7
; For small numbers, the square of guess soon lead to predefined tolerance.
; For very large numbers, square of guess has less precise than 0.001. square of guess hit the same value
; whose diff with x is greator than tolerance. 

; (sqrt 10000000000000)
; 3162277.6601683795
(define (sqr x)
  (* x x))
(sqr 3162277.6601683795)

(define (calc-error guess x)
  (abs (- (square guess) x)))

(define (good-enough-new? guess x)
  (< (abs (- (calc-error guess x) (calc-error (improve guess x) x))) 0.00001))

(define (sqrt-improved guess x)
  (if (good-enough-new? guess x)
      guess
      (sqrt-improved (improve guess x) x)))

(sqrt-improved 1.0 0.001)

; Excercise 1.8
(define (cube x)
  (* x x x))

(define (improve-cube guess x)
  (/ (+ (/ x (sqr guess)) (* 2 guess)) 3))

(define (calc-error-cube guess x)
  (abs (- (cube guess) x)))

(define (good-enough-cube? guess x)
  ;(printf "error = ~v, next-error = ~v" (calc-error-cube guess x) (calc-error-cube (improve-cube guess x) x))
  (< (abs (-
           (calc-error-cube guess x)
           (calc-error-cube (improve-cube guess x) x)))
     0.0001))


(define (cube-root-iter guess x)
  ;(printf "guess = ~v, x = ~v" guess x)
  ;(printf "good-enough-cube? = ~v" (good-enough-cube? guess x))
  (if (good-enough-cube? guess x)
      guess
      (cube-root-iter (improve-cube guess x) x)))

(define (cube-root x)
  (cube-root-iter 1.0 x))

(cube-root 27)
(cube-root 285)
