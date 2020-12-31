(define (fact n)
  (cond ((= n 0) 1)
        (else (* n (fact (- n 1))))))

(define eval
  (lambda (exp env)
    (cond ((number? exp) exp)
          ((symbol? exp) (lookup exp env))
          ((eq? (car exp) 'quote) (cdr exp))
          ((eq? (car exp) 'lambda)
           (list 'closure (cdr exp) env))
          ((eq? (car exp) 'cond)
           (evcond (cdr exp) env))
          (else (apply (eval (car exp) env)
                       (evlist (cdr exp) env))))))

(define apply
  (lambda (proc args)
    (cond ((primitive? proc)
           (apply_primop proc args))a
          ((eq? (car proc) 'closure)
           (eval (cadadr proc)
                 (bind (caadr proc) 
                       args
                       (caddr proc))))
          (else 'error))))

(define evlist
  (lambda (l env)
    (cond ((eq? l '()) '())
          (else
            (cons (eval (car l) env)
                  (evlist (cdr l) env))))))

(define evcond
  (lambda (clauses env)
    (cond ((eq? clauses '()) '())
          ((eq? (caar clauses) 'else)
           (eval (cadar clauses) env))
          ((false? (eval (caar clauses) env))
           (evcond (cdr clauses) env))
          (else
            (eval (cadar clauses) env)))))

(define bind
  (lambda (vars vals env)
    (cons (pair-up vars vals)
          env)))

(define pair-up
  (lambda (vars vals)
    (cond
      ((eq? vars '())
       (cond ((eq? vals '()) '())
             (else (error TMA))))
      ((eq? vals '()) (error TFA))
      (else
        (cons (cons (car vars)
                    (car vals))
              (pair-up (cdr vars)
                       (cdr vals)))))))

(define lookup
  (lambda (sym env)
    (cond ((eq? env '()) (error UBV))
          (else
            ((lambda (vcell)
               (cond ((eq? vcell '())
                      (lookup sym
                              (cdr env)))
                     (else (cdr vcell))))
             (assq sym (car env)))))))

(define assq
  (lambda (sym alist)
    (cond ((eq? alist '()) '())
          ((eq? sym (caar alist))
           (car alist))
          (else
            (assq sym (cdr alist))))))

(eval '(((lambda (x) (lambda (y) (+ x y))) 3) 4) <e0>)
(apply (eval '((lambda (x) (lambda (y) (+ x y))) 3) <e0>)
       (evlist '(4) <e0>))
(apply (eval '((lambda (x) (lambda (y) (+ x y))) 3) <e0>)
       (cons (eval '4 <e0>)
             (evlist '() <e0>)))
(apply (eval '((lambda (x) (lambda (y) (+ x y))) 3) <e0>)
       (cons 4 '()))
(apply (eval '((lambda (x) (lambda (y) (+ x y))) 3) <e0>)
       '(4))
(apply (apply (eval '(lambda (x) (lambda (y) (+ x y))) <e0>)
              '(3))
       '(4))
(apply (apply '(closure((x)(lambda (y) (+ x y))) <e0>)
              '(3))
       '(4))
(apply (eval '(lambda (y) (+ x y)) <e1>)
       '(4))
(apply '(closure((y) (+ x y)) <e1>)
       '(4))
(eval '(+ x y) <e2>)
(apply (eval '+ <e2>)
       (evlist '(x y) <e2>))
(apply '<addition> '(3 4))
7

(define expt
  (lambda (x n)
    (cond ((= n 0) 1)
          (else
            (* x (expt x (- n 1)))))))

F = (lambda (g)
        (lambda (x n)
            (cond ((= n 0) 1)
                  (else
                    (* x
                       (g x (- n 1)))))))

;;; expt is a fixed point of f

E0 = error

E1 = (lambda (x n)
        (cond ((= n 0) 1)
              (else
                (* x (E0 x (- n 1))))))

E2 = (lambda (x n)
        (cond ((= n 0) 1)
              (else
                (* x (E1 x (- n 1))))))

E3 = (lambda (x n)
        (cond ((= n 0) 1)
              (else
                (* x (E2 x (- n 1))))))

;;; infinite loop
F = ((lambda (x) (x x)) (lambda (x) (x x)))

Y = (lambda (f) 
        ((lambda (x) (f (x x)))
         (lambda (x) (f (x x)))))

(Y F) = 
((lambda (x) (F (x x)))
 (lambda (x) (F (x x))))

(F ((lambda (x) (F (x x))) (lambda (x) (F (x x)))))

(Y F) = (F (Y F))

(lambda (x . y)
  (map (lambda (u) (* x u))
       y))

(define pair-up
  (lambda (vars vals)
    (cond
      ((eq? vars '())
       (cond ((eq? vals '()) '())
             (else (error TMA))))
      ((symbol? vars)
       (cons (cons vars vals) '()))
      ((eq? vals '()) (error TFA))
      (else
        (cons (cons (car vars)
                    (car vals))
              (pair-up (cdr vars)
                       (cdr vals)))))))

(define sum
  (lambda (term a next b)
    (cond ((> a b) 0)
          (else
            (+ (term a)
               (sum term 
                    (next a)
                    next
                    b))))))

(define sum-powers
  (lambda (a b n)
    (sum (lambda (x) (expt x n))
         a
         1+
         b)))

(define product-powers
  (lambda (a b n)
    (product (lambda (x) (expt x n))
             a
             1+
             b)))

;;; Dynamic binding (Interpretation of free variables)
(define sum-powers
  (lambda (a b n)
    (sum nthpower a 1+ b)))

(define product-powers
  (lambda (a b n)
    (product nthpower a 1+ b)))

(define nthpower
  (lambda (x)
    (expt x n)))
;;;

(define pgen
  (lambda (n)
    (lambda (x) (expt x n))))

(define sum-powers
  (lambda (a b n)
    (sum (pgen n) a 1+ b)))

(define product-powers
  (lambda (a b n)
    (product (pgen n) a 1+ b)))

(define (unless p c a)
  (cond ((not p) c)
        (else a)))

(define eval
  (lambda (exp env)
    (cond ((number? exp) exp)
          ((symbol? exp) (lookup exp env))
          ((eq? (car exp) 'quote) (cdr exp))
          ((eq? (car exp) 'lambda)
           (list 'closure (cdr exp) env))
          ((eq? (car exp) 'cond)
           (evcond (cdr exp) env))
          (else 
            (apply (undelay (eval (car exp) 
                                  env))
                   (cdr exp)
                   env)))))

(define apply
  (lambda (proc ops env)
    (cond ((primitive? proc)
           (apply_primop proc
                         (evlist ops env)))
          ((eq? (car proc) 'closure)
           (eval (cadadr proc)
                 (bind (vnames (caadr proc))
                       (gevlist (caadr proc)
                                ops
                                env)
                       (caddr proc))))
          (else error-unknown-procedure))))

(define evlist
  (lambda (l env)
    (cond
      ((eq? l '()) '())
      (else
        (cons (undelay (eval (car l) env))
              (evlist (cdr l) env))))))
