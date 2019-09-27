(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$fact (void))
 (set-then!
  g$fact
  (lambda (n k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15)
       ((lambda (k16)
          ((cps <)
           n
           0
           (lambda (rv17)
             (if rv17
               ((lambda (k18) (return #f k18)) k16)
               ((cps equal?)
                n
                0
                (lambda (rv19)
                  (if rv19
                    ((lambda (k20) (return 1 k20)) k16)
                    ((lambda (k21)
                       ((cps -)
                        n
                        1
                        (lambda (rv22)
                          (g$fact
                           rv22
                           (lambda (rv23)
                             ((cps *)
                              n
                              rv23
                              (lambda (rv24) (return rv24 k21))))))))
                     k16))))))))
        k15))
     k14))
  (g$fact 5 (lambda (rv25) ((cps py-print) rv25 $halt)))))
