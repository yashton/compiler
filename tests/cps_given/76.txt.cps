(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$two_e (void))
 (set-then!
  g$two_e
  (lambda (str k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15)
       ((lambda (count ch k16)
          (set-then!
           count
           0
           ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
            (lambda (break k17)
              ((lambda ($seq14 $loop15 k18)
                 ((lambda (k19)
                    ((cps set?)
                     $seq14
                     (lambda (rv21)
                       (if rv21
                         (for-set-k $seq14 $loop15 k19)
                         ((cps tuple?)
                          $seq14
                          (lambda (rv22)
                            (if rv22
                              (for-tuple-k $seq14 $loop15 k19)
                              ((cps py-list?)
                               $seq14
                               (lambda (rv23)
                                 (if rv23
                                   (for-py-list-k $seq14 $loop15 k19)
                                   ((cps dict?)
                                    $seq14
                                    (lambda (rv24)
                                      (if rv24
                                        (for-dict-k $seq14 $loop15 k19)
                                        (k19 (void)))))))))))))))
                  (lambda (rv20) (k18 (void)))))
               str
               (lambda (i14 k25)
                 ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                  (lambda (continue k26)
                    (set-then!
                     ch
                     i14
                     ((lambda (k27)
                        ((cps equal?)
                         ch
                         "e"
                         (lambda (rv28)
                           (if rv28
                             ((lambda (k29)
                                ((cps +)
                                 count
                                 1
                                 (lambda (rv30)
                                   (set-then! count rv30 (k29 (void))))))
                              k27)
                             (k27 (void))))))
                      k26)))
                  k25))
               k17))
            (lambda (rv31)
              ((cps equal?)
               count
               2
               (lambda (rv32)
                 (if rv32
                   ((lambda (k33) (return #t k33)) k16)
                   ((lambda (k34) (return #f k34)) k16))))))))
        (void)
        (void)
        k15))
     k14))
  ($halt (void))))
