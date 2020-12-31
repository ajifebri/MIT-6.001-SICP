(define (sum-odd-squares tree)
  (if (leaf-node? tree)
      (if (odd? tree)
          (square tree)
          0)
      (+ (sum-odd-squares
           (left-branch tree))
         (sum-odd-squares
           (right-branch tree)))))

(define (odd-fibs n)
  (define (next k)
    (if (> k n)
        '()
        (let ((f (fib k)))
          (if (odd? f)
              (cons f (next (1+ k)))
              (next (1+ k))))))
  (next 1))

(cons-stream x y)
(head s)
(tail s)
(empty-stream? s)

;(head (cons-stream x y)) --> x
;(tail (cons-stream x y)) --> y

(define (map-stream proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream
        (proc (head s))
        (map-stream proc (tail s)))))

(define (filter pred s)
  (cond
    ((empty-stream? s) the-empty-stream)
    ((pred (head s))
     (cons-stream (head s)
                  (filter pred (tail s))))
    (else (filter pred (tail s)))))

(define (accumulate combiner init-val s)
  (if (empty-stream? s)
      init-val
      (combiner (head s)
                (accumulate combiner
                            init-val
                            (tail s)))))

(define (enumerate-tree tree)
  (if (leaf-node? tree)
      (cons-stream tree the-empty-stream)
      (append-streams
        (enumerate-tree (left-branch tree))
        (enumerate-tree (right-branch tree)))))

(define (append-streams s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream 
        (head s1)
        (append-streams (tail s1) s2))))

(define (enum-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
        low
        (enum-interval (1+ low) (high)))))

(define (sum-odd-squares tree)
  (accumulate
    +
    0
    (map
      square
      (filter odd
              (enumerate-tree tree)))))

(define (odd-fibs n)
  (accumulate
    cons
    '()
    (filter 
      odd
      (map fib (enum-interval 1 n)))))

(define (flatten st-of-st)
  (accumulate append-streams
              the-empty-stream
              st-of-st))

(define (flatmap f s)
  (flatten (map f s)))

(define (prime-sum-pairs n)
  (map
    (lambda (p)
      (list (car p)
            (cadr p)
            (+ (car p) (cadr p))))
    (filter
      (lambda (p)
        (prime? (+ (car p) (cadr p))))
      (flatmap
        (lambda (i)
          (map
            (lambda (j) (list i j))
            (enum-interval 1 (-1+ i))))
        (enum-interval 1 n)))))

(define (prime-sum-pairs n)
  (collect
    (list i j (+ i j))
    ((i (enum-interval 1 n))
     (j (enum-interval 1 (-1+ i))))
    (prime? (+ i j))))

(define (prime-sum-pairs n)
  (collect
    (list i j (+ i j))
    ((i (enum-interval 1 n))
     (j (enum-interval 1 (-1+ i))))
    (prime? (+ i j))))

;;; 8-Queens Problem

(safe? <row> <column> <rest-of-positions>)

(define (queens size)
  (define (fill-cols k)
    (if 
      (= k 0)
      (singleton empty-board)
      (collect 
        (adjoin-position try-row
                         k
                         rest-queens)
        ((rest-queens (fill-cols (-1+ k)))
         (try-row (enum-interval 1 size)))
        (safe? try-row k rest-queens))))
  (fill-cols size))

;;; Find the second prime between 10.000 and 1.000.000
(head
  (tail (filter
          prime?
          (enum-interval 10000 1000000))))

(cons-stream x y)
;; abbreviation for
(cons x (delay y))

(head s)
;; abbreviation for
(car s)

(tail s)
;; abbreviation for
(force (cdr s))

(delay <exp>)
;; abbreviation for
(lambda () <exp>)
;; more efficient
(memo-proc (lambda () <exp>))

(force p)
;; abbreviation for
(p)

(define (memo-proc proc)
  (let ((already-run? nil) (result nil))
    (lambda ()
      (if (not already-run?)
          (sequence
            (set! result (proc))
            (set! already-run? (not nil))
            result)
          result))))

(define (nth-stream n s)
  (if (= n 0)
      (head s)
      (nth-stream (-1+ n) (tail s))))

(define (print-stream s)
  (cond ((empty-stream? s) 'done)
        (else (print (head s))
              (print-stream (tail s)))))

(define (integers-from n)
  (cons-stream 
    n
    (integers-from (+ n 1))))

(define integers (integers-from 1))
(nth-stream 20 integers)

(define (no-seven x)
  (not (= (remainder x 7)
          0)))

(define ns (filter no-seven
                   integers))
(nth-stream 100 ns)
(print-stream ns)

(define (sieve s)
  (cons-stream
    (head s)
    (sieve (filter
             (lambda (x)
               (not
                 (divisible? x (head s))))
             (tail s)))))
(define primes
  (sieve (integers-from 2)))

(print-stream primes)

(define (add-streams s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else
          (cons-stream
            (+ (head s1) (head s2))
            (add-streams (tail s1)
                         (tail s2))))))

(define (scale-stream c s)
  (map-stream (lambda (x) (* x c))
              s))

(define ones (cons-stream 1 ones))
;; abbreviation
(define ones (cons 1 (delay ones)))

(define integers
  (cons-stream 1
     (add-streams integers ones)))

(define (integral s initial-value dt)
  (define int
    (cons-stream
      initial-value
      (add-streams (scale-stream dt s)
                   int)))
  int)

(define fibs
  (cons-stream 0
    (cons-stream 1
      (add-streams fibs (tail fibs)))))

(define (integral delayed-s
                  initial-value
                  dt)
  (define int
    (cons-stream
      initial-value
      (let ((s (force delayed-s)))
        (add-streams (scale-stream dt s)
                     int))))
  int)

(define y
  (integral 
    (delay dy) 1 0.001))

(define dy
  (map square y))

(define (fact-iter n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(define x 0)

(define (id n)
  (set! x n)
  n)

(define (inc a) (1+ a))

(define y (inc (id 3)))

(define (make-deposit-account
          balance deposit-stream)
  (cons-stream
    balance
    (make-deposit-account
      (+ balance (head deposit-stream))
      (tail deposit-stream))))

