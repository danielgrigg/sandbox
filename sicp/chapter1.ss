(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b 
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

(/ (+ 5 4 (- 2 3 (+ 6 (/ 4 5))))
   (* 3 (- 6 2) (- 2 7)))

(define (sum-of-squares x y) (+ (* x x) (* y y)))
(define (sum-squares-largest-two x y z)
  (cond ((or (> x y z) (> y x z)) (sum-of-squares x y))
        ((or (> z x y) (> x z y)) (sum-of-squares x z))
        ((or (> z y x) (> y z x)) (sum-of-squares y z))))

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
(test 0 (p))

(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x) x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess next-guess x)
  (< (abs (- (/ guess next-guess) 1.0)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt 0.00000003)



(define (cube-root x)
  (cube-root-iter 1.0 x))

(define (cube-root-improve-guess guess x)
  (/ (+ (* 2 guess) (/ x (* guess guess))) 3))

(define (cube-root-iter guess x)
  (if (good-enough? guess (cube-root-improve-guess guess x) x)
      guess
      (cube-root-iter (cube-root-improve-guess guess x) x)))
