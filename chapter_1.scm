#lang sicp
;#lang racket/base
;(require racket/trace)

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y) ))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (exercise_1_2)
  ( / (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
      (* 3 ( - 6 2 ) ( - 2 7))
      ))


;exercise 1.3
;given three numbers, find the sum of the squares of the two larger numberes

(define (greater-sum-of-squares x y z)
  (cond ((> x y) (cond ((> y z) (sum-of-squares x y))
                       (else (sum-of-squares x z))))
         ; x <= y
         ((> x z) (sum-of-squares x y))
         (else (sum-of-squares y z))))
                  
;exercise 1.4
(define ( a-plus-abs-b a b)
  (( if (> b 0) + -) a b))

;exercise 1.7 version; sqrt by change in guess
(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
  guess
  (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))
  
(define (good-enough? previous current)
  (< (abs (/ (- current previous) current)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;exercise 1.8, Newton's cube root
(define (cube x)
  (* x x x))
  
(define (cube_root x)
  (cube-iter 1.0 x))

(define (cube-iter guess x)
  (if (good-enough? guess (cube-improve guess x))
      guess
      (cube-iter (cube-improve guess x) x)))

(define (cube-improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess))
   3))

;exercise 1.9, substitution method for (+ 4 5)
; the trees have been omitted due to their size
; these are  iterative by scheme standards
; a new process will be made for each until a is zero
; then the increased value of b will be returned up the stack

;exercise 1.10, Ackerman numbers
; turns out running this eats memory with a quickness
; (A 2 16) takes over 4 gbs for me
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))
;(define (f n) (A 0 n)) => 2*n
;(define (g n) (A 1 n)) => 2^n
;(define (h n) (A 2 n)) => 2^(h(n-1))

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

;exercise 1.11
(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
         (* 2 (f-rec (- n 2)))
         (* 3 (f-rec ( - n 3))))))

(define (f-iter n)
  (define (f-iter-step a b c count)
    (cond ((= 0 count) a)
          (else (f-iter-step b
                             c
                             (+ c
                                (* 2 b)
                                (* 3 a))
                             (- count 1)))))
  (f-iter-step 0 1 2 n))

;exercise 1.12
(define (pascal-number row col)
  (if (or (= col 1) (= row col))
      1
      (+ (pascal-number (- row 1) (- col 1))
         (pascal-number (- row 1) (col)))))

;exercise 1.15

(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))
;a. 4 times
;b. the sine function g
;> (/ (/ (/(/ 12.15 4.0) 4) 4) 4)
;0.0474609375

; exercise 1.16

; rescursive implementation
;(define (fast-expt b n)
;(cond ((= n 0) 1)
;((even? n) (square (fast-expt b (/ n 2))))
;(else (* b (fast-expt b (- n 1))))))



(define (fast-exp base exp)
  (define (fast-exp-iter product exp)
  (cond
    ((= exp 0) 1) 
    ((= exp 1) product)
    ((= exp 2) (* product product))
    (else (if (even? exp)
                  (fast-exp-iter (* product product) (/ exp 2))
                  (fast-exp-iter (* product base) (- exp 1))))))
  (fast-exp-iter base exp))

;exercise 1.17/
;design a multiplication procedure analogous to fast-expt that uses a
;logarithmic number of steps

;reference function
;(define (* a b)
;(if (= b 0)
;0
;(+ a (* a (- b 1)))))

(define (mult-rec a b)
  (cond
    ((= b 0) 0)
    ((even? b) (mult-rec (+ a a) (/ b 2)))
    (else (+ a (mult-rec a (- b 1))))))

;excercise 1.18 iterative multiplication in logarithmic time
(define (mult-iter a b)
  (define (inner-mult a b acc)
    (cond
      ((= b 0) acc)
      ((even? b) (inner-mult (+ a a) (/ b 2) acc))
      (else (inner-mult a (- b 1) (+ acc a)))))
  (inner-mult a b 0))
                 

;exercise 1.19
;Russian peasant Fibonacci numbers
;T_pq(a,b) = (aq + ap + bq) , (bp + aq)
;p' = p^2 + q^2
;q' = 2pq + q^2
