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
; (test 0 (p))

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


(define (fast-expt-iter b n a)
  (cond ((= n 0) 1)
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt b (- n 1) a))))
   
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n ) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(smallest-divisor 19999999)

(define (search-for-primes a b)
  (cond ((<= a b) 
         (if (prime? a) (timed-prime-test a))
         (search-for-primes (+ a 1) b))))
(fast-prime? 100019 100)
(search-for-primes 10000000000 10000001000)
        
(define (inc n) (+ n 1))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))
(define (even? x) 
  (= (remainder x 2) 0))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x) (+ x 4))
  (sum pi-term a pi-next b))

(define (cube x) (* x x x))

(define (integral f a b dx) 
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

    
(define (simpson-it f y n)
  (define (s-term k)
    (cond ((= k 0) (y k))
          ((= k n) (y k))
          ((even? k) (* 2 (y k)))
          (else (* 4 (y k)))))
  (sum s-term 0 inc n))

(define (simpson f a b n)
  (define (h) 
    (/ (- b a) n))
  (define (y k) 
    (f (+ a (* k (h)))))
  (/ (* (h) (simpson-it f y n)) 3))

(define (identity x) x)
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (approx-pi n)
  (define (pi-term a)
    (/ (* a (+ a 2.0)) (square (+ a 1.0))))
  (define (pi-next x) 
    (+ x 2))
  (* 4.0 (product pi-term 2 pi-next n)))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (product-acc term a next b)
  (accumulate * 1 term a next b))
(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum-acc-iter term a next b)
  (accumulate-iter + 0 term a next b))

(sum-acc-iter identity 1 inc 9)
(+ 1 2 3 4 5 6 7 8 9)

(define (filtered-accumulate combiner null-value term a next b pred)
  (cond ((> a b) null-value)
        ((pred a) 
         (combiner (term a) 
                   (filtered-accumulate combiner null-value term (next a) next b pred)))
        (else (filtered-accumulate combiner null-value term (next a) next b pred))))

(filtered-accumulate + 0 square 1 inc 10 prime?)
(+ 1 (square 2) (square 3) (square 5) (square 7))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(average 3 7)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2) 
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
    
(fixed-point (lambda (x) (+ 1.0 (/ 1.0 x))) 1)

(fixed-point (lambda (x) (/ (log 1000.0) (log x))) 2)
(fixed-point (lambda (x) (/ (+ x (/ (log 1000.0) (log x))) 2)) 2)

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))
(sqrt 16.0)

(define (double f)
  (lambda (x) (f (f x))))

(((double (double double)) inc) 5)

((double inc) 1)

(define (compose f g)
  (lambda (x) (f (g x))))
((compose square inc) 6)

(define (repeated f n)
  (if (> n 1)
      (compose f (repeated f (- n 1)))
      f))
((repeated square 2) 5)

(define (deriv f)
  (let ((dx 0.00001))
    (lambda (x) (/ (- (f (+ x dx)) (f x)) dx))))
((deriv (lambda (x) (* x x x))) 4)

(average 2 3)

(define (smooth f)
  (let ((dx 0.0001))
    (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3.0))))

