(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$gamma (void))
 (set-then!
  g$gamma
  (lambda (k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15)
       ((lambda (result i k16)
          (set-then!
           result
           0.0
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
                      500000
                      (lambda (rv20)
                        (if rv20
                          ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                           (lambda (continue k21)
                             ((lambda (k22)
                                ((cps /)
                                 1.0
                                 i
                                 (lambda (rv23)
                                   (get-field
                                    g$math
                                    log
                                    (lambda (rv24)
                                      ((cps +)
                                       i
                                       1.0
                                       (lambda (rv25)
                                         ((cps /)
                                          rv25
                                          i
                                          (lambda (rv26)
                                            (rv24
                                             rv26
                                             (lambda (rv27)
                                               ((cps -)
                                                rv23
                                                rv27
                                                (lambda (rv28)
                                                  ((cps +)
                                                   result
                                                   rv28
                                                   (lambda (rv29)
                                                     (set-then!
                                                      result
                                                      rv29
                                                      ((cps +)
                                                       i
                                                       1
                                                       (lambda (rv30)
                                                         (set-then!
                                                          i
                                                          rv30
                                                          (k22
                                                           (void)))))))))))))))))))))
                              k21))
                           (lambda (rv31) (loop k19)))
                          (k19 (void))))))
                   (loop (lambda (rv32) (k18 (void))))))
                (void)
                k17))
             (lambda (rv33) (return result k16))))))
        (void)
        (void)
        k15))
     k14))
  ($halt (void))))
