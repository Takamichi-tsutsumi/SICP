#lang racket

;; Ex3.1
(define (make-accumulator i)
  (let ((sum i))
    (lambda (x) (begin (set! sum (+ sum x)) sum))))

;; Ex3.2
(define (make-monitored procedure)
  (let ((ms 0))
    (lambda (x)
      (if (eq? x 'how-many-calls?)
          ms
          (begin (set! ms (+ ms 1))
                 (procedure x))))))