(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$geometricSeriesSum (void))
 (set-then!
  g$geometricSeriesSum
  (lambda (x n k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15)
       ((lambda (sum i prod j k16)
          (set-then!
           sum
           0
           (set-then!
            i
            0
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
                                (set-then!
                                 prod
                                 1
                                 (set-then!
                                  j
                                  0
                                  ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                                   (lambda (break k23)
                                     ((lambda (loop k24)
                                        (set-then!
                                         loop
                                         (lambda (k25)
                                           ((cps <)
                                            j
                                            i
                                            (lambda (rv26)
                                              (if rv26
                                                ((lambda (f cc)
                                                   (f
                                                    (lambda (x k) (cc x))
                                                    cc))
                                                 (lambda (continue k27)
                                                   ((lambda (k28)
                                                      ((cps *)
                                                       prod
                                                       x
                                                       (lambda (rv29)
                                                         (set-then!
                                                          prod
                                                          rv29
                                                          ((cps +)
                                                           j
                                                           1
                                                           (lambda (rv30)
                                                             (set-then!
                                                              j
                                                              rv30
                                                              (k28
                                                               (void)))))))))
                                                    k27))
                                                 (lambda (rv31) (loop k25)))
                                                (k25 (void))))))
                                         (loop (lambda (rv32) (k24 (void))))))
                                      (void)
                                      k23))
                                   (lambda (rv33)
                                     ((cps +)
                                      sum
                                      prod
                                      (lambda (rv34)
                                        (set-then!
                                         sum
                                         rv34
                                         ((cps +)
                                          i
                                          1
                                          (lambda (rv35)
                                            (set-then!
                                             i
                                             rv35
                                             (k22 (void)))))))))))))
                              k21))
                           (lambda (rv36) (loop k19)))
                          (k19 (void))))))
                   (loop (lambda (rv37) (k18 (void))))))
                (void)
                k17))
             (lambda (rv38) (return sum k16))))))
        (void)
        (void)
        (void)
        (void)
        k15))
     k14))
  ($halt (void))))
