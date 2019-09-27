(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$fact (void))
 (define g$s (void))
 (set-then!
  g$fact
  (lambda (x k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15)
       ((lambda (k16)
          ((cps -)
           1
           (lambda (rv17)
             ((cps equal?)
              x
              rv17
              (lambda (rv18)
                (if rv18
                  ((lambda (k19) (return 0+1.0i k19)) k16)
                  ((cps equal?)
                   x
                   0
                   (lambda (rv20)
                     (if rv20
                       ((lambda (k21) (return 1 k21)) k16)
                       ((lambda (k22)
                          ((cps -)
                           x
                           1
                           (lambda (rv23)
                             (g$fact
                              rv23
                              (lambda (rv24)
                                ((cps *)
                                 x
                                 rv24
                                 (lambda (rv25) (return rv25 k22))))))))
                        k16))))))))))
        k15))
     k14))
  (set-then! g$s "foo\\ \n'\"" (g$fact 20 $halt))))
