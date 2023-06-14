#lang sicp
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
; the processes are recursive
; a new process will be made for each until a is zero
; then the increased value of b will be returned up the stack

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


