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
;the proof isn't difficult per se, but it has a lot of moving parts
;p_0, q_0 = 0, 1
(define (fast-fib n)
  (define (fast-fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count) (fast-fib-iter a
                                    b
                                    (+ (square p) (square q))
                                    (+ (* 2 p q) (square q))
                                    (/ count 2)))
          (else (fast-fib-iter (+ (* a q) (* a p) (* b q))
                               (+ (* b p) (* a q))
                               p
                               q
                               (- count 1)))))
  (fast-fib-iter 1 0 0 1 n))

;exercise 1.20 applicative vs normal order for Euclidean gcd
; no deseire to type this out, but its about 18 times to 4 times for remainder


;Exercise 1.21: Use the smallest-divisor procedure to find
;the smallest divisor of each of the following numbers: 199,
;1999, 19999.
(define (slow-smallest-divisor n) (slow-find-divisor n 2))
(define (slow-find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (slow-find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
(= n (smallest-divisor n)))

;fermat's little theorem:
; if n is a prime number, and a is any positive integer less than n,
; then a raised to the nth power is congruent to a modulo n
; a^n % n == a & n

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square(expmod base (/ exp 2) m))
                    m))
        (else (remainder (* base ( expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;Fermat's primality test is probablistic, some non-primes will pass.
;These rare numbers are called "Carmichael numbers".

;Exercise 1.21: Use the smallest-divisor procedure to find
;the smallest divisor of each of the following numbers: 199,
;1999, 19999.
;> (smallest-divisor 199)
;199
;> (smallest-divisor 1999)
;1999
;> (smallest-divisor 19999)
;7
;>

;Exercise 1.22: Most Lisp implementations include a primitive called runtime that returns an integer that specifies
;the amount of time the system has been running (measured, for example, in microseconds). e following timedprime-test procedure, when called with an integern, prints
;n and checks to see if n is prime. If n is prime, the procedure
;prints three asterisks followed by the amount of time used
;in performing the test


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


(define (search-for-primes lower upper)
  (cond ((even? lower)
         (search-for-primes (+ lower 1) upper))
        ((not (> lower upper))
         (timed-prime-test lower)
         (search-for-primes (+ lower 2) upper))))

;Exercise 1.23: e smallest-divisor procedure shown at
;the start of this section does lots of needless testing: Aer it
;checks to see if the number is divisible by 2 there is no point
;in checking to see if it is divisible by any larger even numbers. is suggests that the values used for test-divisor
;should not be 2, 3, 4, 5, 6, . . ., but rather 2, 3, 5, 7, 9, . . ..
;To implement this change, define a procedure next that returns 3 if its input is equal to 2 and otherwise returns its input plus 2. Modify the smallest-divisor procedure to use
;(next test-divisor) instead of (+ test-divisor 1).
;With timed-prime-test incorporating this modified version of smallest-divisor, run the test for each of the 12
;primes found in Exercise 1.22. Since this modification halves
;the number of test steps, you should expect it to run about
;twice as fast. Is this expectation confirmed? If not, what is
;the observed ratio of the speeds of the two algorithms, and
;how do you explain the fact that it is different from 2

(define (rand-int n)
  (+ 1 (random (- n 1))))


(define (next n)
  (cond ((even? n) (+ n 1))
        (else (+ n 2))))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (mb-test n)
  (= (remainder(fast-exp (rand-int n) (- n 1)) n) 1))