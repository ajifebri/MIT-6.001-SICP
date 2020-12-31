(define (merge x y)
  (cond
    ((null? x) y)
    ((null? y) x)
    (else
      (let ((a (car x)) (b (car y)))
        (if (< a b)
            (cons a
                  (merge (cdr x) y))
            (cons b
                  (merge x (cdr y))))))))

;;;

(let ((a (car x)) (b (car y)))
  (if (< a b)
      (cons a (merge (cdr x) y))
;;;

if 
  (cdr-x and y merge-to-form z)
and
  a < (car y)
then
  ((cons a cdr-x) and y merge-to-form (cons a z))
;;;

(let ((a (car x)) (b (car y)))
  (if (< b a)
      (cons b
            (merge x (cdr y)))))
;;;
if 
  (x and cdr-y merge-to-form z)
and
  b < (car x)
then
  (x and (cons b cdr-y) merge-to-form (cons b z))
;;;

;;;
for all x, (x and () merge-to-form x)
for all y, (() and y merge-to-form y)

(job (Bitdiddle Ben) (computer wizard))
(salary (Bitdiddle Ben) 40000)
(supervisor (Bitdiddle Ben) (Warbucks Oliver))
(address (Bitdiddle Ben) 
         (Slumerville (Ridge Road) 10))

(address (Hacker Alyssa P)
         (Cambridge (Mass Ave) 78))
(job (Hacker Alyssa P)
     (computer programmer))
(salary (Hacker Alyssa P) 35000)
(supervisor (Hacker Alyssa P)
            (Bitdiddle Ben))

;; primitives
;; primitive queries

;; means of combination
;; and, not, or, lisp-value

;; means of abstraction
;; rules

(and
  (job ?x (computer . ?y))
  (not (and (supervisor ?x ?z)
            (job ?z (computer . ?w)))))

(rule
  (bigshot ?x ?dept)
  (and
    (job ?x (?dept . ?y))
    (not (and (supervisor ?x ?z)
              (job ?z (?dept . ?w))))))

(rule (merge-to-form () ?y ?y))

(rule (merge-to-form ?y () ?y))

(rule
  (merge-to-form
    (?a . ?x) (?b . ?y) (?b . ?z))
  (and (merge-to-form (?a . ?x) ?y ?z)
       (lisp-value > ?a ?b)))

(rule
  (merge-to-form
    (?a . ?x) (?b . ?y) (?a . ?z))
  (and (merge-to-form ?x (?b . ?y) ?z)
       (lisp-value > ?b ?a)))

(match pat data dictionary)

(job ?x (?d . ?y))

;; database
(job (hacker ..) (...))
(job ... ...)

(and (job ?x (?d . ?y))
     (supervisor ?x ?y))

(rule (boss ?z ?d)
      (and (job ?x (?d . ?y))
           (supervisor ?x ?z)))

(boss (bitdiddle ben) computer)

(boss ?who computer)

;; unify
(?x ?x)
;; with
((a ?y c) (a b ?z))

;?x : (a b c)
;?y : b
;?z : c

;; unify
(?x ?x)
;; with
((?y a ?w) (b ?v ?z))

;?y : b
;?v : a
;?w : ?z
;?x : (b a ?w)

;; unify
(?x ?x)
;; with
(?y (a . ?y))

;?x : ?y
;?y : ( a a a ...)

(cons 'a y) = y
(f y) = y

