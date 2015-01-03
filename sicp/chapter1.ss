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

(define (A x y)
  (cond ((= y 0) 0)
        (( = x 0) (* 2 y))
        (( = y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(f 19)

(define (f2 n)
  (if (< n 3)
      n
      (f-iter 2 1 0 (- n 2))))

(define (f-iter n1 n2 n3 i)
  (if (> i 0)
      (f-iter (+ n1 (* 2 n2) (* 3 n3)) n1 n2 (- i 1))
      n1))
(f2 19)

(define (pascal row col)
  (cond ((= row 0) 1)
        ((or (= col 0) (= col row)) 1)
        (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))))
(pascal 4 3)

(define (cube x) (* x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))
(sine 12.15)
(p (sine 4.05))
(p (p (sine 1.35)))
(p (p (p (sine .45))))
(p (p (p (p (sine .15)))))
(p (p (p (p (p .05)))))

(define (fast-expt b n)
  (cond ((= n 0)
         1)
         ((even? n)
          (square (fast-expt b (/ n 2))))
          (else (* b (fast-expt b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))

(fast-expt 2 8)

(define (fast-expt-iter b n a)
  (cond ((= n 0) 1)
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt b (- n 1) a))))
   
    

