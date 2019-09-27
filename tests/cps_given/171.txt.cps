(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$f (void))
 (set-then!
  g$f
  (lambda (a b k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15)
       ((lambda (k16)
          ((cps py-print)
           a
           (lambda (rv17) (set-then! a 3 ((cps py-print) a k16)))))
        k15))
     k14))
  (g$f 1 2 $halt)))
