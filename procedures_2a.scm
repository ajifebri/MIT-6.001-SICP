(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term
              (next a)
              next
              b))))

(define (sum-int a b)
  (define (identity a) a)
  (sum identity a 1+ b))

(define (sum-sq a b)
  (sum square a 1+ b))

(define (pi-sum a b)
  (sum (lambda (i) (/ 1 (* i (+ i 2))))
       a
       (lambda (i) (+ i 4))
       b))

;;; iterative
(define (sum term a next)
  (define (iter j ans)
    (if (> j b)
        ans
        (iter (next j)
              (+ (term j) (ans)))))
  (iter a 0))

;;; Heron's square root method
(define (sqrt x)
  (define tolerance 0.00001)
  (define (good-enuf? y)
    (< (abs (- (* y y) x)) tolerance))
  (define (improve y)
    (average (/ x y) y))
  (define (try y)
    (if (good-enuf? y)
        y
        (try (improve y))))
  (try 1))

(define (sqrt x)
  (fixed-point 
    (lambda (y) (average (/ x y) y))
    1))

(define (fixed-point f start)
  (define tolerance 0.00001)
  (define (close-enuf? u v)
    (< (abs (- u v)) tolerance))
  (define (iter old new)
    (if (close-enuf? old new)
        new
        (iter new (f new))))
  (iter start (f start)))

(define (sqrt x)
  (fixed-point
    (average-damp (lambda (y) (/ x y)))
    1))

(define average-damp
  (lambda (f)
    (lambda (x)
      (average (f x) x))))

;; without lambda notation
(define (average-damp f)
  (define (foo x)
    (average (f x) x))
  foo)

(define (sqrt x)
  (newton (lambda (y) (- x (square y)))
          1))

(define (newton f guess)
  (define df (deriv f))
  (fixed-point
    (lambda (x) (- x (/ (f x) (df x))))
    guess))

(define deriv
  (lambda (f)
    (lambda (x)
      (/ (- (f (+ x dx))
            (f x))
         dx))))

(define dx 0.00001)
