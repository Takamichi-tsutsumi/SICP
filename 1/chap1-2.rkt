#lang racket
(require rackunit)

; Excercise 1.11
(define (f-recursive n)
  (cond ((< n 3) n)
        (else (+ (f-recursive (- n 1))
                 (* 2 (f-recursive (- n 2)))
                 (* 3 (f-recursive (- n 3)))))))

(define (f-iter a b c n)
  (if (< n 3)
      a
      (f-iter (+ a (* 2 b) (* 3 c)) a b (- n 1))))

(define (f-iterative n)
  (if (< n 3)
      n
      (f-iter 2 1 0 n)))

(check-equal? (f-recursive 1) 1)
(check-equal? (f-recursive 3) 4)
(check-equal? (f-recursive 4) 11)
(check-equal? (f-recursive 5) (f-iterative 5))

; Exercise 1.12
(define (pascal row column)
  (cond ((= row column) 1)
        ((= column 0) 1)
        (else (+ (pascal (- row 1) (- column 1)) (pascal (- row 1) column))))
  )

(pascal 3 2)