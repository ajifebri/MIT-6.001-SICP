;;; Functional programs
;;; Factorial
(define (fact n)
  (cond ((= n 1) 1)
        (else (* n (fact (- n 1))))))

(define (+ n m)
  (cond ((= n 0) m)
        (else (1+ (+ (-1+ n) m)))))

(define (+ n m)
  (cond ((= n 0) m)
        (else (+ (-1+ n) (1+ m)))))

;;; Assignment
(define count 1)

(define (demo x)
  (set! count (1+ count))
  (+ x count))

(define (fact n)
  (define (iter m i)
    (cond ((> i n) m)
          (else (iter (* i m)(+ i 1)))))
  (iter 1 1))

(define (fact n)
  (let ((i 1) (m 1))
    (define (loop)
      (cond ((> i n) m)
            (else
              (set! m (* i m))
              (set! i (+ i 1))
              (loop))))
    (loop)))

;;; Let example
(let ((var1 e1) (var2 e2))
  e3)
;;;
((lambda (var1 var2)
   e3)
 e1
 e2)

;;; Bound Variables
(lambda (y) ((lambda (x) (* x y)) 3))

(lambda (y) ((lambda (z) (* z y)) 3))

;;; Free Variables
; y is a free variable
(lambda (x) (* x y))

; * is a free variable
(lambda (y) ((lambda (x) (* x y)) 3))

;;; Examples
(define make-counter
  (lambda (n)
    (lambda ()
      (set! n (1+ n))
      n)))

(define c1 (make-counter 0))
(define c2 (make-counter 10))

;;; Cesaro's method for estimating Pi:
;;;     Prob(gcd(n1,n2)=1) = 6/(Pi*Pi)
(define (estimate-pi n)
  (sqrt (/ 6 (monte-carlo n cesaro))))

(define (cesaro)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter remaining passed)
    (cond ((= remaining 0)
           (/ passed trials))
          ((experiment)
           (iter (-1+ remaining)
                 (1+ passed)))
          (else
            (iter (-1+ remaining)
                  passed))))
  (iter trials 0))

(define (estimate-pi n)
  (sqrt (/ 6 (random-gcd-test n))))

(define (random-gcd-test trials)
  (define (iter remaining passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= remaining 0)
               (/ passed trials))
              ((= (gcd x1 x2) 1)
               (iter (-1+ remaining)
                     (1+ passed)
                     x2))
              (else
                (iter (-1+ remaining)
                      passed
                      x2))))))
  (iter trials 0 random-seed))
