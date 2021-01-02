(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define-machice gcd
  (registers a b t)
  (controller
    loop (branch (zero? (fetch b)) done)
         (assign t (remainder (fetch a) (fetch b)))
         (assign a (fetch b))
         (assign b (fetch t))
         (goto loop)
    done))

(define-machice gcd
  (registers a b t)
  (controller
    main (assign a (read))
         (assign b (read))
    loop (branch (zero? (fetch b)) done)
         (assign t (remainder (fetch a) (fetch b)))
         (assign a (fetch b))
         (assign b (fetch t))
         (goto loop)
    done (perform (print (fetch a)))
         (goto main)))

(define (remainder n d)
  (if (< n d)
      n
      (remainder (- n d) d)))

(define (fact n)
  (if (= n 1)
      1
      (* n (fact (- n 1)))))

     (assign continue done)
loop (branch (= 1 (fetch n)) base)
     (save continue)
     (save n)
     (assign n (-1+ (fetch n)))
     (assign continue aft)
     (goto loop)
aft  (restore n)
     (restore continue)
     (assign val (* (fetch n) (fetch val)))
     (goto (fetch continue))
base
     (assign val (fetch n))
     (goto (fetch continue))
done

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

        (assign continue done)
fib-loop  ; n contains arg, continue is recipient
        (branch (< (fetch n) 2) immediate-ans)
        (save continue)
        (assign continue after-fib-n-1)
        (save n)
        (assign n (- (fetch n) 1))
        (goto fib-loop)
after-fib-n-1
        (restore n)
        (restore continue) ; not needed
        (assign n (- (fetch n) 2))
        (save continue) ; not needed
        (assign continue after-fib-n-2)
        (save val)
        (goto fib-loop)
after-fib-n-2
        (assign n (fetch val)) ; val = fib(n-2)
        (restore val)
        (restore continue)
        (assign val (+ (fetch val) (fetch n)))
        (goto (fetch continue))
immediate-ans
        (assign val (fetch n))
        (goto (fetch continue))
fib-done

