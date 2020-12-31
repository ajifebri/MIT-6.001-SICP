(define (sum-of-squares x y)
  (+ (* x x) (* y y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(f 5)

(f 10)
