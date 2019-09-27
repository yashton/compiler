(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$f (void))
 (set-then!
  g$f
  (lambda (x k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15) (return x k15))
     k14))
  (g$f 3 (lambda (rv16) ((cps py-print) rv16 $halt)))))
