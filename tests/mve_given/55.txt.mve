(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$Fibonacci (void))
 (set-then!
  g$Fibonacci
  (lambda (n k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15)
       ((lambda (k16)
          ((cps equal?)
           n
           0
           (lambda (rv18)
             ((lambda (t14 k17) (if t14 (k17 t14) ((cps equal?) n 1 k17)))
              rv18
              (lambda (rv19)
                (if rv19
                  ((lambda (k20) (return n k20)) k16)
                  ((lambda (k21)
                     ((cps -)
                      n
                      1
                      (lambda (rv22)
                        (g$Fibonacci
                         rv22
                         (lambda (rv23)
                           ((cps -)
                            n
                            2
                            (lambda (rv24)
                              (g$Fibonacci
                               rv24
                               (lambda (rv25)
                                 ((cps +)
                                  rv23
                                  rv25
                                  (lambda (rv26) (return rv26 k21))))))))))))
                   k16)))))))
        k15))
     k14))
  ($halt (void))))
