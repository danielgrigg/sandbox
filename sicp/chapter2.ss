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

(define (reverse xs)
  (if (null? xs)
      xs
      (append (reverse (cdr xs)) (list (car xs)))))
(reverse (list 1 4 9 16 25))

(define (same-parity x . xs)
  (define (build-nums ys)
    (if (null? ys)
        (list)
        (if (= 0 (remainder (- (car ys) x) 2))
            (cons (car ys) (build-nums (cdr ys)))
            (build-nums (cdr ys)))))
  (cons x (build-nums xs)))
  
(same-parity 2 3 4 5 6 7)
(same-parity 1 2 3 4 5 6 7)

(define (square-list items)
  (if (null? items)
      (list)
      (cons (* (car items) (car items)) (square-list (cdr items)))))
(define (square-list items)
  (map (lambda (x) (* x x)) items))
(square-list (list 1 2 3 4))

(define (my-for-each f xs)
  (cond ((null? xs) '())
        (else 
         (f (car xs))
         (my-for-each f (cdr xs)))))
(cons (list 1 2) (list 3 4))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x)) (count-leaves (cdr x))))))

(count-leaves (list 1 (list 2 (list 3 4))))

(car (cdr (cdr '(1 3 (5 7) 9))))
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y)
(cons x y)
(list x y)

(reverse '(1 2 3 4 5))

(define (deep-reverse xs)
  (cond ((null? xs) '())
        ((not (pair? xs)) xs)
        (else (append (deep-reverse (cdr xs)) (list (deep-reverse (car xs)))))))

(define x '((1 2) (3 4)))
(deep-reverse '(1 2))
(deep-reverse x)

(define (fringe xs)
  (cond ((null? xs) '())
        ((not (pair? xs)) (list xs))
        (else (append (fringe (car xs)) (fringe (cdr xs))))))

(fringe '(1 2))
(fringe x)
(fringe (list x x))

(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

(define m1 (make-mobile (make-branch 1 2) (make-branch 3 4)))
(left-branch m1)
(right-branch m1)
(branch-length (make-branch 1 2))
(branch-structure (make-branch 3 4))

(define (weight? branch)
  (not (pair? (branch-structure branch))))
(weight? (make-branch 1 2))

(define (total-branch-weight branch)
  (cond ((weight? branch) (branch-structure branch))
        (else (total-branch-weight (branch-structure branch)))))
(total-branch-weight (make-branch 1 2))
(total-branch-weight (make-branch 1 ()

(define (total-weight mobile)
  (let ((l (left-branch mobile))
        (r (right-branch mobile))
        (sl (branch-structure (left-branch mobile)))
        (sr (branch-structure (right-branch mobile))))
    (cond ((and (weight? l) (weight? r)) (+ sl sr))
          ((and (weight? l) (not (weight? r)) (+ sl (total-weight sr))))
          ((and (not (weight? l)) (weight? r)) (+ (total-weight sl) sr))
          ((and (not (weight? l)) (not (weight? r))) (+ (total-weight sl) (total-weight sr))))))

(total-weight m1)

(total-weight (make-mobile (make-branch 3 m1) (make-branch 5 m1)))

(define (branch-torque branch)
  (* (branch-length branch)
     (if (weight? branch) 
         (branch-structure branch) 
         (total-weight (branch-structure branch)))))

(define (balanced? mobile)
  (= (branch-torque (left-branch mobile))
     (branch-torque (right-branch mobile))))

(balanced? m1)
(branch-torque (make-branch 2 m1))
(balanced? (make-mobile (make-branch 3 4) (make-branch 2 6)))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))
(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))
(define (square-tree2 tree) (tree-map square tree))
(square-tree2 (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op  initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(map square '(1 2 3 4 5))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append '(1 2 3) '(5 6 7))
(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))
(length '(1 2 3))

(define (horner x coeffs)
  (accumulate (lambda (this-coeff higher-terms) 
                (+ this-coeff (* x higher-terms))) 0 coeffs))

(horner 2 (list 1 3 0 5 0 1))

