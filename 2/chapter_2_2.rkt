#lang sicp

; 2.17 implement last-pair
(define (last-pair items)
  (define (iter items)
      (if (null? (cddr items))
          (cons (car items) (cdr items))
          (iter (cdr items))))
  (if (or (null? items) (null? (cdr items)))
      nil
      (iter items)))

; 2.18 implement reverse
(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

; 2.19 reimplement count change with list operations
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))
(define (first-denomination coins)
  (car coins))
(define (except-first-denomination coins)
  (cdr coins))
(define (no-more? coins)
  (null? coins))

; 2.20 implement same parity
(define (same-parity first . rest)
  (define (iter parity items acc)
    (if (null? items)
        acc
        (if (equal? parity (even? (car items)))
            (iter parity (cdr items) (append acc (list (car items))))
            (iter parity (cdr items) acc))))
  (iter (even? first) rest (list first)))
  