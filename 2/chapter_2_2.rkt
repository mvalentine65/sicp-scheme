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

; 2.21 square-list implementations
(define (square x)
  (* x x))

(define (square-list-car items)
  (if (null? items)
    nil
    (cons (square (car items))
          (square-list (cdr items)))))

(define (square-list items)
  (map square items))


(define test-square-list (square-list '(1 2 3 4 5)))

; 2.23 for-each implementation
(define (for-each procedure items)
   (cond ((not (null? items)) (procedure (car items)) 
          (for-each procedure (cdr items))))) 

(define (printer x)
  (newline)
  (display x))
  
(define (test-for-each)
  (for-each printer '(1 2 3 4)))

; 2.24 (1(2 (3 4))) -- (2 (3 4)) -- (3 4) -- 3 
;                   `- 1         `- 2     `- 4


; 2.25
(define (two-point-twenty-five)
  (define one '(1 3 (5 7) 9))
  (define two '((7)))
  (define three '(1 (2 (3 (4 (5 (6 7)))))))
  (newline)
  (display (car (cdr (car (cdr (cdr one))))))
  (newline)
  (display (car (car two)))
  (newline)
  (display (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr three)))))))))))))
  )

; 2.26
(define (two-point-twenty-six)
  (define x (list 1 2 3))
  (define y (list 4 5 6))
  (newline)
  ;(1 2 3 4 5 6)
  (display (append x y))
  (newline)
  ;((1 2 3) 4 5 6)
  (display (cons x y))
  (newline)
  ; ((1 2 3) (4 5 6))
  (display (list x y))
  )

; 2.27 deep-reverse
; I dont want to find an iterative approach anymore
; if a single item, just return the item
; if pair, return the reversed list of the reverse of each element
(define (deep-reverse items)
  (if (pair? items)
      (reverse (map deep-reverse items))
      items))

; 2.28 fringe
; recurisvely check tree
; if null, return empty list
; if a single leaf, return leaf in a list
; if a pair, return left list appended to right list
(define (fringe tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree)) (fringe (cdr tree))))))

; 2.29 binary mobile
(define (make-mobile left right)
(list left right))
(define (make-branch length structure)
(list length structure))

; a. write left-branch and right-branch selectors
(define left-branch car)
(define right-branch cadr)
(define branch-length car)
(define branch-structure cadr)
; b. implement total-weight
 (define (weigh-branch branch)
   (if (not (list? (branch-structure branch)))
       (branch-structure branch)
       (total-weight (branch-structure branch))))
(define (total-weight mobile)
  (+ (weigh-branch (left-branch mobile)) (weigh-branch (right-branch mobile))))
        
; c. balanced mobile test
(define (balanced? mobile)
  (define (torque branch)
    (* (branch-length branch) (weigh-branch branch)))
  (if (not (pair? (branch-structure mobile))) true
        (and (equal? (torque (left-branch mobile)) (torque (right-branch mobile)))
             (balanced? (left-branch mobile))
             (balanced? (right-branch mobile)))))
    

(define mob (make-mobile (make-branch 4 5) (make-branch 5 6)))
(define mobb (make-mobile (make-branch 2 2) (make-branch 3 mob)))

(define x (list (list 1 2) (list 3 4)))

; 2.30 square tree
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not(pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))
(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (square sub-tree)))
       tree))

; 2.31 tree-map
(define (tree-map procedure tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map procedure sub-tree)
             (procedure sub-tree)))
  tree))
; 2.32 subsets
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

; 2.33 map, append, length
; commented out to avoid "map: undefined" errors
;(define (map p sequence)
 ; (accumulate (lambda (x y) (cons (p x) y) nil sequence)))
;(define (append seq1 seq2)
 ; (accumulate cons seq2 seq1))
;(define (length sequence)
;  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))


; 2.34  horner evaluation of polynomials
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

; 2.35 redefine count-leaves from 2.2.2
(define (count-leaves t)
  (accumulate (lambda (x y) (+ y 1)) 0
                (map identity (enumerate-tree t))))

; 2.36 accumulate-n
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init  (map car seqs))
      (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define test-m (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose m)
  (accumulate-n cons '() m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))

; 2.39
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
(define fold-right accumulate)

(define (flatmap proc seq)
(accumulate append nil (map proc seq)))
; 2.40 unique pairs
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))


(define (permutations s)
  (if (null? s) ; empty set?
    (list nil) ; sequence containing empty set
    (flatmap (lambda (x)
               (map (lambda (p) (cons x p))
                    (permutations (remove x s))))
             s)))