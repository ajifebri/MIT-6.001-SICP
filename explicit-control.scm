(define (fact-rec n)
  (if (= n 0)
      1
      (* n (fact-rec (- n 1)))))

;; coded by myself
(define (fact-iter n)
  (define (iter i ans)
    (if (> i n) 
        ans
        (iter (1+ i) (* i ans))))
  (iter 1 1))

(define (fact-iter n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;; Sample evaluator-machine operations
(assign val (fetch exp))

(branch 
  (conditional? (fetch exp))
  ev-cond)

(assign exp (first-clause (fetch exp)))

(assign val
        (lookup-variable-value (fetch exp)
                               (fetch env)))

eval-dispatch
  (branch (self-evaluating? (fetch exp))
          ev-self-eval)
  (branch (variable? (fetch exp))
          ev-variable)
  
  <... more special forms ...>
  
  (branch (application? (fetch exp))
          ev-application)
  (goto unknown-expression-error)

ev-self-eval
  (assign val (fetch exp))
  (goto (fetch continue))

ev-variable
  (assign
    val
    (lookup-variable-value (fetch exp)))
  (goto (fetch continue))

ev-application
  (assign unev (operands (fetch exp)))
  (assign exp (operator (fetch exp)))
  (save continue)
  (save env)
  (save unev)
  (assign continue eval-args)
  (goto eval-dispatch)

eval-args
  (restore unev)
  (restore env)
  (assign fun (fetch val))
  (save fun)
  (assign argl '())
  (goto eval-arg-loop)

eval-arg-loop
  (save argl)
  (assign
    exp
    (first-operand (fetch unev)))
  (branch (last-operand? (fetch unev))
          eval-last-arg)
  (save env)
  (save unev)
  (assign continue accumulate-arg)
  (goto eval-dispatch)

accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign
    argl
    (cons (fetch val) (fetch argl)))
  (assign
    unev
    (rest-operands (fetch unev)))
  (goto eval-arg-loop)


