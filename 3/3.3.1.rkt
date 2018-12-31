;; ex3.12
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))


(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;; ex3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;; ex3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))