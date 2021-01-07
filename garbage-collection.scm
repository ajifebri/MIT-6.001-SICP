((1 2) 3 4)

(assign a (car (fetch b)))
;;;;
(assign a (vector-ref (fetch the-cars)
                      (fetch b)))

(assign a (cdr (fetch b)))
;;;;
(assign a (vector-ref (fetch the-cdrs)
                      (fetch b)))

(perform (set-car! (fetch a) (fetch b)))
;;;;
(perform (vector-set! (fetch the-cars)
                      (fetch a)
                      (fetch b)))

(perform (set-cdr! (fetch a) (fetch b)))
;;;;
(perform (vector-set! (fetch the-cdrs)
                      (fetch a)
                      (fetch b)))

;With freelist method of allocation
(assign a (cons (fetch b) (fetch c)))
;;;;
(assign a (fetch free))
(assign free
        (vector-ref (fetch the-cdrs)
                    (fetch free)))
(perform (vector-set! (fetch the-cars)
                      (fetch a)
                      (fetch b)))
(perform (vector-set! (fetch the-cdrs)
                      (fetch a)
                      (fetch c)))

;A source of garbage
(define (rev-loop x y)
  (if (null? x)
      y
      (rev-loop (cdr x)
                (cons (car x) y))))

(define (append u v)
  (rev-loop (rev-loop u '()) v))

;; Halting problem
(define diag1
  (lambda (p)
    (if (safe? p p)
        (inf)
        3)))

(define inf
  (lambda ()
    ((lambda (x) (x x))
      (lambda (x) (x x)))))

(diag1 diag1)

(define diag2
  (lambda (p)
    (if (safe? p p)
        (other-than (p p))
        false)))

(define other-than
  (lambda (x)
    (if (eq? x 'a)
        'b
        'a)))

(diag2 diag2)

