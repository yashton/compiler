(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$index (void))
 (define g$myList (void))
 (set-then!
  g$myList
  (py-list* 1 2 3 4)
  ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
   (lambda (break k14)
     (g$len
      g$myList
      (lambda (rv22)
        (g$range
         rv22
         (lambda (rv23)
           ((lambda ($seq14 $loop15 k15)
              ((lambda (k16)
                 ((cps set?)
                  $seq14
                  (lambda (rv18)
                    (if rv18
                      (for-set-k $seq14 $loop15 k16)
                      ((cps tuple?)
                       $seq14
                       (lambda (rv19)
                         (if rv19
                           (for-tuple-k $seq14 $loop15 k16)
                           ((cps py-list?)
                            $seq14
                            (lambda (rv20)
                              (if rv20
                                (for-py-list-k $seq14 $loop15 k16)
                                ((cps dict?)
                                 $seq14
                                 (lambda (rv21)
                                   (if rv21
                                     (for-dict-k $seq14 $loop15 k16)
                                     (k16 (void)))))))))))))))
               (lambda (rv17) (k15 (void)))))
            rv23
            (lambda (i14 k24)
              ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
               (lambda (continue k25)
                 (set-then!
                  g$index
                  i14
                  ((lambda (k26)
                     ((lambda (b16 k27)
                        ((lambda (i15 k28)
                           ((lambda (e19 k36)
                              ((lambda (i18 k37)
                                 ((cps py-list?)
                                  e19
                                  (lambda (rv38)
                                    (if rv38
                                      ((cps py-list-ref) e19 i18 k37)
                                      ((cps tuple?)
                                       e19
                                       (lambda (rv39)
                                         (if rv39
                                           ((cps tuple-ref) e19 i18 k37)
                                           ((cps dict?)
                                            e19
                                            (lambda (rv40)
                                              (if rv40
                                                ((cps dict-ref) e19 i18 k37)
                                                (error
                                                 "cannot index object"
                                                 k37)))))))))))
                               i15
                               k36))
                            b16
                            (lambda (rv41)
                              ((lambda (v17 k29)
                                 ((cps tuple?)
                                  b16
                                  (lambda (rv30)
                                    (if rv30
                                      ((cps +)
                                       v17
                                       1
                                       (lambda (rv31)
                                         ((cps tuple-set!) b16 i15 rv31 k29)))
                                      ((cps py-list?)
                                       b16
                                       (lambda (rv32)
                                         (if rv32
                                           ((cps +)
                                            v17
                                            1
                                            (lambda (rv33)
                                              ((cps py-list-set!)
                                               b16
                                               i15
                                               rv33
                                               k29)))
                                           ((cps dict?)
                                            b16
                                            (lambda (rv34)
                                              (if rv34
                                                ((cps +)
                                                 v17
                                                 1
                                                 (lambda (rv35)
                                                   ((cps dict-set!)
                                                    b16
                                                    i15
                                                    rv35
                                                    k29)))
                                                (k29 (void))))))))))))
                               rv41
                               k28))))
                         g$index
                         k27))
                      g$myList
                      k26))
                   k25)))
               k24))
            k14))))))
   (lambda (rv42) ((cps py-print) g$myList $halt)))))
