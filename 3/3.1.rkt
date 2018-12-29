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

;; Ex3.3 Ex3.4
(define (make-account balance secret-password)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((fail-count 0))
    (define (dispatch p m)
      (if (eq? p secret-password)
          (begin (set! fail-count 0)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       (else (error "Unknown request: MAKE-ACCOUNT" m))))
          (begin (set! fail-count (+ fail-count 1))
                 (if (>= fail-count 3)
                     (error "call the police")
                     (error "Incorrect password")))))
  dispatch))