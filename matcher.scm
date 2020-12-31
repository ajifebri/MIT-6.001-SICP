
(define (match pat exps dict)
  (cond ((eq?) dict 'failed) 'failed)
        ; Atomic pattern
        ((atom? pat)
         (if (atom? exps)
             (if (eq? pat exps)
                 dict
                 'failed)
             'failed))
        ; Pattern variable clauses
        ((arbitrary-constant? pat)
         (if (constant? exps)
             (extend-dict pat exps dict)
             'failed))
        ((arbitrary-variable? pat)
         (if (variable? exps)
             (extend-dict pat exps dict)
             'failed))
        ((arbitrary-expression? pat)
         (extend-dict pat exps dict))

        ((atom? exps) 'failed)
        (else
          (match (cdr pat)
                 (cdr exps)
                 (match (car pat)
                        (car exps)
                        dict))))

(define (instantiate skeleton dict)
  (define (loop s)
    (cond ((atom? s) s)
          ((skeleton-evaluation? s)
           (evaluate (eval-exp s) dict))
          (else (cons (loop (car s))
                      (loop (cdr s))))))
  (loop skeleton))

(define (simplifier the-rules)
  (define (simplify-exp exps)
    ***)
  (define (simplify-parts exps)
    ***)
  (define (try-rules exps)
    ***)
  simplify-exp)


(define (simplify-exp exps)
  (try-rules (if (compound? exps)
                 (simplify-parts exps)
                 exps)))

(define (simplify-exp exps)
  (try-rules 
    (if (compound? exps)
        (map simplify-exp exps)
        exps)))

(define (simplify-parts exps)
  (if (null? exps)
      '()
      (cons (simplify-exp (car exps))
            (simplify-parts (cdr exps)))))

(define (simplify-exp exps)
  (try-rules
    (if (compound? exps)
        (map simplify-exp exps)
        exps)))

(define (try-rules exps)
  (define (scan rules)
    ***)
  (scan the-rules))

(define (scan rules)
  (if (null? rules)
      exps
      (let ((dict
              (match (pattern (car rules))
                     exp
                     (empty-dictionary))))
        (if (eq? dict 'failed)
            (scan (cdr rules))
            (simplify-exp
              (instantiate
                (skeleton (car rules))
                dict))))))

(define deriv-rules
  ( (dd (?c c) (? v))   0)
  ( (dd (?v v) (? v))   1)
  ( (dd (?v u) (? v))   0)

  ( (dd (+ (? x1) (? x2)) (? v))
    (+ (dd (: x1) (: v))
       (dd (: x2) (: v)))  )
  )

(define dsimp
  (simplifier deriv-rules))

(dsimp '(dd (+ x y) x))
--> (+ 1 0)

(define algebra-rules
  ( ((? op) (?c e1) (?c e2))
   (: (op e1 e2)))

  ( ((? op) (?e1) (?c e2))
   ((: op) (: e2) (: e1)))

  ( (+ 0 (? e))  (: e))
  ( (* 1 (? e))  (: e))
  ( (* 0 (? e))  0)
  )
