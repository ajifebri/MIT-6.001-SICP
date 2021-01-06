(f x)

(if p a b)

<code for p -- put result in val>
branch if val is true, to label1
<code for b>
goto next thing
label1: <code for a>
goto next thing


;; Eliminating unnecessary stack operations
(assign fun
        (lookup-var-val 'f (fetch env)))
(assign val
        (lookup-var-val 'x (fetch env)))
(assign argl (cons (fetch val) '()))

;; Computation proceeds at apply-dispatch

(op a1 a2)

{compile op; result in fun} ; preserving env
{compile a1; result in val}
(assign argl (cons (fetch val) '())) ; preserving env
{compile a2; result in val} ; preserving argl
(assign argl (cons (fetch val) (fetch argl))) ; preserving fun
(goto apply-dispatch)

;; Compiler
;; append seq1 and seq2 preserving reg
if seq2 needs reg and seq1 modifies reg
  (save reg)
  <seq1> ;; compiled code of seq1
  (restore reg)
  <seq2> ;; compiled code of seq2
else
  <seq1>
  <seq2>

;; compiled code sequence for compiler
<sequence of instructions; set of regs. modified; set of regs. needed>
;; example
<(assign r1 (fetch r2)); {r1}; {r2}>

<seq1;m1;n1> and <seq2;m2;n2>
;; combines to
seq and seq2; m1 union m2; n1 union (n2-m1)
