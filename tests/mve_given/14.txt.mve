(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 ((lambda (k14)
    ((lambda (cv14 k17)
       ((cps <)
        1900
        cv14
        (lambda (rv18) (if rv18 ((cps <) cv14 2100 k17) (k17 #f)))))
     g$year
     (lambda (rv19)
       (if rv19
         ((lambda (cv15 k20)
            ((cps <=)
             1
             cv15
             (lambda (rv21) (if rv21 ((cps <=) cv15 12 k20) (k20 #f)))))
          g$month
          (lambda (rv22)
            (if rv22
              ((lambda (cv16 k23)
                 ((cps <=)
                  1
                  cv16
                  (lambda (rv24) (if rv24 ((cps <=) cv16 31 k23) (k23 #f)))))
               g$day
               (lambda (rv25)
                 (if rv25
                   ((lambda (cv17 k26)
                      ((cps <=)
                       0
                       cv17
                       (lambda (rv27)
                         (if rv27 ((cps <) cv17 24 k26) (k26 #f)))))
                    g$hour
                    (lambda (rv28)
                      (if rv28
                        ((lambda (cv18 k29)
                           ((cps <=)
                            0
                            cv18
                            (lambda (rv30)
                              (if rv30 ((cps <) cv18 60 k29) (k29 #f)))))
                         g$minute
                         (lambda (rv31)
                           (if rv31
                             ((lambda (cv19 k32)
                                ((cps <=)
                                 0
                                 cv19
                                 (lambda (rv33)
                                   (if rv33 ((cps <) cv19 60 k32) (k32 #f)))))
                              g$second
                              k14)
                             (k14 #f))))
                        (k14 #f))))
                   (k14 #f))))
              (k14 #f))))
         (k14 #f)))))
  (lambda (rv15)
    (if rv15 ((lambda (k16) (return 1 k16)) $halt) ($halt (void))))))
