(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 ((cps <)
  2
  3
  (lambda (rv14)
    ((cps <)
     3
     2
     (lambda (rv15)
       ((cps py-print)
        rv14
        rv15
        (lambda (rv16)
          ((lambda (t14 k17) (if t14 (k17 t14) (k17 3)))
           2
           (lambda (rv18)
             ((lambda (t15 k19) (if t15 (k19 t15) (k19 2)))
              3
              (lambda (rv20)
                ((cps py-print)
                 rv18
                 rv20
                 (lambda (rv21)
                   ((lambda (t16 k22) (if t16 (k22 t16) (k22 3)))
                    (py-list*)
                    (lambda (rv23)
                      ((cps py-print)
                       rv23
                       (lambda (rv24)
                         ((lambda (t17 k25) (if t17 (k25 t17) (k25 (dict))))
                          (py-list*)
                          (lambda (rv26)
                            ((cps py-print)
                             rv26
                             (lambda (rv27)
                               ((lambda (k28) (if 2 (k28 3) (k28 #f)))
                                (lambda (rv29)
                                  ((lambda (k30) (if 3 (k30 2) (k30 #f)))
                                   (lambda (rv31)
                                     ((cps py-print)
                                      rv29
                                      rv31
                                      (lambda (rv32)
                                        ((lambda (k33)
                                           (if (py-list*)
                                             (k33 (dict))
                                             (k33 #f)))
                                         (lambda (rv34)
                                           ((cps py-print)
                                            rv34
                                            (lambda (rv35)
                                              ((lambda (k36)
                                                 (if 3
                                                   (k36 (py-list*))
                                                   (k36 #f)))
                                               (lambda (rv37)
                                                 ((cps py-print)
                                                  rv37
                                                  $halt))))))))))))))))))))))))))))))))))
