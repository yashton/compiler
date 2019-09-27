(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$sum (void))
 (set-then!
  g$sum
  (lambda (n k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15)
       ((lambda (result i k16)
          (set-then!
           result
           0
           (set-then!
            i
            1
            ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
             (lambda (break k17)
               ((lambda (loop k18)
                  (set-then!
                   loop
                   (lambda (k19)
                     ((cps <=)
                      i
                      n
                      (lambda (rv20)
                        (if rv20
                          ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                           (lambda (continue k21)
                             ((lambda (k22)
                                ((cps +)
                                 result
                                 i
                                 (lambda (rv23)
                                   (set-then!
                                    result
                                    rv23
                                    ((cps +)
                                     i
                                     1
                                     (lambda (rv24)
                                       (set-then! i rv24 (k22 (void)))))))))
                              k21))
                           (lambda (rv25) (loop k19)))
                          (k19 (void))))))
                   (loop (lambda (rv26) (k18 (void))))))
                (void)
                k17))
             (lambda (rv27) (return result k16))))))
        (void)
        (void)
        k15))
     k14))
  ($halt (void))))
