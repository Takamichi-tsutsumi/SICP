#lang racket

;; Ex3.8
(define (g)
  (let ((initial #t))
    (lambda (x)
      (if initial
          (begin (set! initial #f) 0)
          (if (eq? x 0)
              0
              1)))))

(define f (g))

; (+ (f 0) (f 1))
; returns 0 if evaluated from left to right
; returns 1 if evaluated from right to left