#lang sicp

;1.3 Formulating Abstractions with Higher-Order Procedures

(define (cube x) (* x x x))

(define (sum-old term a next b)
  (if (> a b) 0)
  (+ (term a) sum term (next a) (next b)))

;1.31 Procedures as Arguments

; sums all integers in range [a,b]
(define (sum-integers-old a b)
  (if (> a b) 0
  (+ a (sum-integers (+ a 1) b))))

; sums the cubes of all integers in range [a,b]
(define (sum-cubes-old a b)
  (if (> a b) 0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))

; sums 1/(x*(x+2)) for range [a,b]
; slowly convreges to pi/8
(define (pi-sum-old a b)
  (if (> a b) 0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))


; The three previous functions all perform an operation on
; every integer in a range and sum the resulting values.
; This tells us there's a common abstraction: map.
(define (sum-linear term a next b)
  (if (> a b) 0
      (+ (term a)
      (sum term (next a) next b))))
; increments given value by one
; helper function
(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

; returns the passed value
; helper function, enables a null-operation in maps
(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))


; Excercise 1.29 Simpson's rule of integration
; y = f(a + kh)
; even terms have a coefficient of 2
; odd terms have a coeeficient of 4
; y_a and y_b have a coefficient of 1
(define (simpsons f a b n)
  (define h (/ (- b a) n))
  (define (next-y x) (+ x h h))
  (* (+ (f a)
        (* 2 (sum f a next-y b))
        (* 4 (sum f (+ a h) next-y b))
        (f b))
     (/ h 3)))
                

; Exercise 1.30 iterative sum function
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
    (iter (next a) (+ result (term a)))))
  (iter a 0))

; Exercise 1.31 product abstraction
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
    (iter (next a) (* (term a) result))))
  (iter a 1))
  
; we can use product to implement factorial
(define (factorial n)
  (product identity 1 inc n))

; pi/4 formula
; wikipedia says this can be rewritten as the product of the series ((2n/(2n-1)) * (2n/(2n+1))

(define (wallis n)
  (define (next n) (+ n 1))
  (define (numer k)
    (if (even? k) (+ k 2) (+ k 1)))
  (define (denom k)
    (if (even? k) (+ k 1) (+ k 2)))
  (define (term i) (/ (numer i) (denom i)))
  (product term 1.0 next n))

; Exercise 1.33 accumulate abstraction
(define (accmulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))


(define (acc-rec combiner null-value term a next b)
  (if (> a b) null-value
      (combiner (term a) (acc-rec combiner null-value term (next a) next b))))


;============================  1.3.3 Procedures as General Methods ======================================

; finds average of two values; helper function
(define (average x y) (/ (+ x y) 2))

; defines error tolerance for later approximations
(define (close-enough? x y) (< (abs (- x y)) 0.001))

; search a continuous function for an approximate zero value
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

; find approximate zero using half-interval method
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

; approximate fixed point of a function: f(x) = x
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
 (try first-guess))

; sqrt x = y
; y^2 = x : rewrite
; y = x/y : divide both sides by y
; 2y = y + x/y : add y to both sides
; y = (1/2)(y + x/y) divide both sides by two, or the average of y and x/y 
(define (sqrt x)
  (fixed-point-print (lambda (y) (average y (/ x y)))
               1.0))

; 1.35 golden ratio: (a+b)/a  = a/b
; f_1 = a/b
; f_2 = a+b / a
; f_1 = f_2
; fixed point
(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 3.0))

; 1.36 Modify fixed-point to print sequences and compare
; the number of steps with and without average dampening
(define (fixed-point-print  f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess) (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
 (try first-guess))

; without dampening
(define (without-dampening) (fixed-point-print (lambda (x) (/(log 1000) (log x)))2))
; with dampening
(define (with-dampening) (fixed-point-print (lambda (x) (average x (/(log 1000) (log x))))2))

; 1.37 continued fraction
; 1/phi = 0.61803398875
; takes 11 iterations to get 4 decimals
(define (cont-frac n d k)
  (define (iter i acc)
    (if (= 0 i)
        acc
        (iter (- i 1) (/ (n i) (+ acc (d i))))))
  (iter (- k 1) (/ (n k) (d k))))


; 1.38 Approximate e with a continued fraction for e-2
(define (euler-expansion k)
  (define (d-exp i)
    (if (= (modulo i 3) 2)
        (* 2 (/ (+ i 1) 3))
        1))
  (cont-frac (lambda (i) 1.0) d-exp k))

; 1.39 approximate tan with continued-fractions
(define (tan-cf x k)
  (define (d i)
    (- (* i 2) 1))
  (define (numerator i)
    (if (= i 1)
        x
        (* x x)))
  (cont-frac numerator d k))

(define dx 0.00001)

(define (deriv g)
(lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
(lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
(fixed-point (newton-transform g) guess))

; 1.40 use newtowns method to find zeros of cubic polynomial

(define (cubic a b c)
  (define (poly x)
    (+ (* x x x) (* a x x) (* b x) c))
  (newtons-method poly 1))

; 1.41 double, which applies a function twice
;(((double (double double)) inc) 5) = 21
(define (double function)
  (lambda (x)
    (function (function x))))
(define (square x)
  (* x x))
; 1.42 compose for single argument functions
(define (compose f g)
  (lambda (x)
    (f (g x))))

; 1.43
(define (repeated procedure n)
  (if (= n 1) procedure
      (compose procedure (repeated procedure (- n 1)))))

; 1.44 n-fold smoothing function
(define (smooth f)
  (lambda (x) (/ (+ (f(- x dx)) (f x) (f (+ x dx)) ) 3)))
(define (n-fold-smoothed f n)
  (repeated smooth n) f)

; 1.45
(define (average-damp f)
(lambda (x) (average x (f x))))

(define (nth-root-damped x nth damp)
  (fixed-point ((repeated average-damp damp)
                (lambda (y)
                  (/ x (power y (- nth 1)))))
               1.0))
(define (power x n)
  (if (= n 1)
      x
      (* x (power x (- n 1)))))

(define (nth-root x nth)
  (fixed-point 
    ((repeated average-damp (floor (log nth 2))) 
    (lambda (y) 
      (/ x (power y (- nth 1)))))
   1.0))



; 1.46 iterative improvement function
(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess) guess
        (iter (improve guess))))
  (lambda (guess) (iter guess)))


(define (sqrt-improve num)
  (define (close-enough? num guess)
    (< (abs(- num (* guess guess))) 0.001))
  (define (improve guess)
    (average guess (/ num guess)))
  ((iterative-improve close-enough? improve) 1.0))