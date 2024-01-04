#lang sicp

; 2.1 redefine make-rat to allow for positve and negative arguments
; if a numerator is positive and the denominator is negative, only
; the numerator should be negative in the final rational
(define (make-rat n d)
    (let ((g (gcd n d))
        (sign (if (> d 0) 1 (- 1))))
(cons (* sign (/ n g)) (* sign (/ d g)))))

(define (gcd a b)
(if (= b 0)
a
(gcd b (remainder a b))))


(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
(newline)
(display (numer x))
(display "/")
(display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
(= (* (numer x) (denom y))
(* (numer y) (denom x))))

(define (average a b)
  (/ (+ a b) 2))

; 2.2 implement make-segement and use it to implement midpoint-segment
(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
  
(define (make-point x y)
  (cons x y))
(define (x-coord point)
  (car point))
(define (y-coord point)
  (cdr point))

(define (midpoint-segment segment)
  (make-segment (average
                 (x-coord (start-segment segment))
                 (x-coord (end-segment segment)))
                (average
                 (y-coord (start-segment segment))
                 (y-coord (end-segment segment)))))
                
; 2.3 rectangle
; im not writing this a second time, but here is how I could
; replace the diagnol segment with a horizontal and a veertical
; adjust width and height accordingly
; perimeter and area can remain unchanged
(define (make-rectangle diagnol)
  diagnol)

(define (first-point rectangle)
  (car rectangle))

(define (second-point rectangle)
  (cdr rectangle))

(define (height rectangle)
  (abs (- (y-coord (first-point rectangle))
          (y-coord (second-point rectangle)))))

(define (width rectangle)
  (abs (- (x-coord (first-point rectangle))
          (x-coord (second-point rectangle)))))

(define (perimeter rectangle)
  (+ (* 2 (height rectangle))
     (* 2 (width rectangle))))

(define (area rectangle)
  (* (width rectangle)
     (height rectangle)))

; not writing lambda calculus proofs, fight me

; 2.7 Alyssa's interval implementation
; given
(define (make-interval a b)
  (if (< a b)
      (cons a b)
      (cons b a)))

(define (add-interval x y)
(make-interval (+ (lower-bound x) (lower-bound y))
(+ (upper-bound x) (upper-bound y))))

(define (old-mul-interval x y)
(let ((p1 (* (lower-bound x) (lower-bound y)))
      (p2 (* (lower-bound x) (upper-bound y)))
      (p3 (* (upper-bound x) (lower-bound y)))
      (p4 (* (upper-bound x) (upper-bound y))))
  (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))

(define (case interval)
  (if (positive? (lower-bound interval))
      1
      (if (positive? (upper-bound interval))
          2
          (+ 0 3))))
          
                 
; 2.11 mul-interval by case
(define (mul-interval x y)
  (let ((a (case x))
        (b (case y)))
    
  (cond ((= a 1)
         (cond ((= b 1) (make-interval (* (lower-bound x) (lower-bound y))
                                       (* (upper-bound x) (upper-bound y))))
               ((= b 2) (make-interval (* (upper-bound x) (lower-bound y))
                                       (* (upper-bound x) (upper-bound y))))
               (else (make-interval    (* (upper-bound x) (lower-bound y))
                                       (* (lower-bound x) (upper-bound y))))
               ))
        ((= a 2)
         (cond ((= b 1) (make-interval (* (lower-bound x) (upper-bound y))
                                       (* (upper-bound x) (upper-bound y))))
               
               ((= b 2) (make-interval (min (* (lower-bound x) (upper-bound y))
                                            (* (upper-bound x) (lower-bound y)))
                                       (max (* (lower-bound x) (lower-bound y))
                                            (* (upper-bound x) (upper-bound y)))
                                            ))
               (else (make-interval    (* (upper-bound x) (lower-bound y))
                                       (* (lower-bound x) (lower-bound y))))
               ))
        (else
         (cond ((= b 1) (make-interval (* (lower-bound x) (upper-bound y))
                                       (* (upper-bound x) (lower-bound y))))
               ((= b 2) (make-interval (* (lower-bound x) (upper-bound y))
                                       (* (lower-bound x) (lower-bound y))))
               (else (make-interval    (* (upper-bound x) (upper-bound y))
                                       (* (lower-bound x) (lower-bound y))))
        ))
        )))

(define one (make-interval 2 3))
(define two (make-interval -2 3))
(define three (make-interval -2 -3))   
                                       
; test all 9 casese against old mul-interval
(define (test-mul-interval)
  (define one (make-interval 2 3))
  (define two (make-interval -2 3))
  (define three (make-interval -2 -3))
  (if (equal? (old-mul-interval one one) (mul-interval one one))
      (display "1 1 test case passed\n")
      (display "1 1 test cass failed\n"))
  (if (equal? (old-mul-interval one two ) (mul-interval one two))
      (display "1 2 test case passed\n")
      (display "1 2 test case failed\n"))
  (if (equal? (old-mul-interval one three) (mul-interval one three ))
      (display "1 3 test case passed\n")
      (display "1 3 test case failed\n"))
  (if (equal? (old-mul-interval two one) (mul-interval two one ))
      (display "2 1 test case passed\n")
      (display "2 1 test case failed\n"))
  (if (equal? (old-mul-interval two two) (mul-interval two two ))
      (display "2 2 test case passed\n")
      (display "2 2 test case failed\n"))
  (if (equal? (old-mul-interval two three) (mul-interval two three ))
      (display "2 3 test case passed\n")
      (display "2 3 test case failed\n"))
  (if (equal? (old-mul-interval three one) (mul-interval three one))
      (display "3 1 test case passed\n")
      (display "3 1 test case failed\n"))
  (if (equal? (old-mul-interval three two) (mul-interval three two))
      (display "3 2 test case passed\n")
      (display "3 2 test case failed\n"))
  (if (equal? (old-mul-interval three three) (mul-interval three three))
      (display "3 3 test case passed\n")
      (display "3 3 test case failed\n"))
  )
  

      
(define (div-interval x y)
  (if (and (or (< 0 (lower-bound y)) (= 0 (lower-bound y)))
           (or (> 0 (upper-bound y)) (= 0 (upper-bound)))
           ) (error "Cannot divide by an interval spanning zero" y))
(mul-interval
x
(make-interval (/ 1.0 (upper-bound y))
(/ 1.0 (lower-bound y)))))

; work

(define (upper-bound interval)
  (cdr interval))
(define (lower-bound interval)
  (car interval))

; 2.8 subtraction
(define (sub-interval a b)
  (make-interval (- (lower-bound a) (upper-bound b))
                 (- (upper-bound a) (lower-bound b))))

;2.12 alternative constructor
(define (make-center-percent center percent)
  (cons (* center (- 1 percent)) (* center (+ 1 percent))))
(define (center interval)
  (average (upper-bound interval) (lower-bound interval)))
(define (percentage interval)
  (- (/ (upper-bound interval) (center interval)) 1.0))

; dont want to be an electrical engineer, skipping to 2.2