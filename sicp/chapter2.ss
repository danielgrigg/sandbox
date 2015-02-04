(define (make-rat n d)
  (cond ((>= d 0) (cons n d))
        ((> n 0) (cons (- n) (abs d)))
         (else (cons (- n) (- d)))))
         

(define (numer x) 
  (car x))
(define (denom x)
  (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))

(define (div-rat x y) (make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))

(define (equal-rat? x y) 
  (= (* (numer x) (denom y)) (* (numer y) (denom x))))

(cdr (cons 1 (cons 2 3)))

(define (make-point x y)
  (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p) 
  (newline)
  (display "(")
  (display (car p))
  (display ",")
  (display (cdr p))
  (display ")"))
(define (mid-point p q)
  (make-point (/ (+ (x-point p) (x-point q)) 2.0) (/ (+ (y-point p) (y-point q)) 2)))
(define (make-segment a b)
  (cons a b))

(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define (midpoint-segment s)
  (mid-point (start-segment s) (end-segment s)))

(mid-point (make-point 2 5) (make-point 3 -1))
(midpoint-segment (make-segment (make-point 2 5) (make-point 3 -1)))

(lambda (m) (m x y))
((lambda (m) (m x y) (lambda (p q) p)))
((lambda (p q) p) x y)
x

(define (cdr z)
  (z (lambda (p q) q)))

(lambda (f) (lambda (x) (f ((lambda (y) y) x))))
(lambda (f) (lambda (x) (f x)))

(define (last-pair xs)
  (if (null? (cdr xs))
      (car xs)
      (last-pair (cdr xs))))
(last-pair (list 23 72 149 34))
