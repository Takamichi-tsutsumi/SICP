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

;; Ex3.3
(define (make-account balance secret-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (eq? p secret-password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT" m)))
        (error "Incorrect password")))
  dispatch)