;;; Primitives and Means of Combination
(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(or-gate a b d)
(and-gate a b c)
(inverter c e)
(and-gate d e s)

;;; Means of Abstraction
(define (half-adder a b s c)
  (let ((d (make wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)))

(define (inverter in out)
  (define (invert-in)
    (let ((new
            (logical-not (get-signal in))))
      (after-delay inverter-delay
            (lambda ()
              (set-signal! out new)))))
  (add-action! in invert-in))

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else
          (error "invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1)
                         (get-signal a2))))
      (after-delay and-gate-delay
            (lambda ()
              (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure))

(define (make-wire)
  (let ((signal 0) (action-procs '()))
    (define (set-my-signal! new)
      (cond ((= signal new) 'done)
            (else
              (set! signal new)
              (call-each action-procs))))
    (define (accept-action-proc proc)
      (set! action-procs
        (cons proc action-procs))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal)
            ((eq? m 'set-signal!)
             set-my-signal!)
            ((eq? m 'add-action!)
             accept-action-proc)
            (else
              (error "Bad message" m))))
    dispatch))

(define (call-each procedures)
  (cond ((null? procedures) 'done)
        (else
          ((car procedures))
          (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-proc)
  ((wire 'add-action!) action-proc))

(define (after-delay delay action)
  (add-to-agenda!
    (+ delay (current-time the-agenda))
    action
    the-agenda))

(define (propagate)
  (cond ((empty-agenda? the-agenda)
         'done)
        (else
          ((first-item the-agenda))
          (remove-first-item! the-agenda)
          (propagate))))

(define (probe name wire)
  (add-action! wire
    (lambda () 
      (newline)
      (display name)
      (display " ")
      (display (current-time the-agenda))
      (display " New-value = ")
      (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

;;; Agenda (Priority Queue)
(make-agenda)
(current-time agenda)
(empty-agenda? agenda)
(add-to-agenda! time action agenda)
(first-item agenda)
(remove-first-item agenda!)

;;; Queue
(make-queue)
(insert-queue! queue item)
(delete-queue! queue)
(front-queue queue)
(empty-queue? queue)

;;; Car and Cdr
(set-car! <pair> <value>)
(set-cdr! <pair> <value>)

(define a (cons 1 2))
(define b (cons a a))
(set-car! (car b) 3)

(define (cons x y)
  (lambda (m) (m x y)))

(define (car x)
  (x (lambda (a d) a)))

(define (cdr x)
  (x (lambda (a d) d)))

;;; "Lambda Calculus" Mutable Data
(define (cons x y)
  (lambda (m)
    (m x
       y
       (lambda (n) (set! x n))
       (lambda (n) (set! y n)))))

(define (car x)
  (x (lambda (a d sa sd) a)))

(define (cdr x)
  (x (lambda (a d sa sd) d)))

(define (set-car! x y)
  (x (lambda (a d sa sd) (sa y))))

(define (set-cdr! x y)
  (x (lambda (a d sa sd) (sd y))))

