#lang racket

;; Ex3.1
(define (make-accumulator i)
  (let ((sum i))
    (lambda (x) (begin (set! sum (+ sum x)) sum))))