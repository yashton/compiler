(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$pair_13 (void))
 (set-then!
  g$pair_13
  (lambda (nums k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15)
       ((lambda (i k16)
          ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
           (lambda (break k17)
             (g$len
              nums
              (lambda (rv25)
                ((cps -)
                 rv25
                 1
                 (lambda (rv26)
                   (g$range
                    rv26
                    (lambda (rv27)
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
                       rv27
                       (lambda (i14 k28)
                         ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                          (lambda (continue k29)
                            (set-then!
                             i
                             i14
                             ((lambda (k30)
                                ((lambda (k31)
                                   ((lambda (e16 k34)
                                      ((lambda (i15 k35)
                                         ((cps py-list?)
                                          e16
                                          (lambda (rv36)
                                            (if rv36
                                              ((cps py-list-ref) e16 i15 k35)
                                              ((cps tuple?)
                                               e16
                                               (lambda (rv37)
                                                 (if rv37
                                                   ((cps tuple-ref)
                                                    e16
                                                    i15
                                                    k35)
                                                   ((cps dict?)
                                                    e16
                                                    (lambda (rv38)
                                                      (if rv38
                                                        ((cps dict-ref)
                                                         e16
                                                         i15
                                                         k35)
                                                        (error
                                                         "cannot index object"
                                                         k35)))))))))))
                                       i
                                       k34))
                                    nums
                                    (lambda (rv39)
                                      ((cps equal?)
                                       rv39
                                       13
                                       (lambda (rv40)
                                         (if rv40
                                           ((lambda (e18 k41)
                                              ((cps +)
                                               i
                                               1
                                               (lambda (rv46)
                                                 ((lambda (i17 k42)
                                                    ((cps py-list?)
                                                     e18
                                                     (lambda (rv43)
                                                       (if rv43
                                                         ((cps py-list-ref)
                                                          e18
                                                          i17
                                                          k42)
                                                         ((cps tuple?)
                                                          e18
                                                          (lambda (rv44)
                                                            (if rv44
                                                              ((cps tuple-ref)
                                                               e18
                                                               i17
                                                               k42)
                                                              ((cps dict?)
                                                               e18
                                                               (lambda (rv45)
                                                                 (if rv45
                                                                   ((cps
                                                                     dict-ref)
                                                                    e18
                                                                    i17
                                                                    k42)
                                                                   (error
                                                                    "cannot index object"
                                                                    k42)))))))))))
                                                  rv46
                                                  k41))))
                                            nums
                                            (lambda (rv47)
                                              ((cps equal?) rv47 13 k31)))
                                           (k31 #f)))))))
                                 (lambda (rv32)
                                   (if rv32
                                     ((lambda (k33) (return #t k33)) k30)
                                     (k30 (void))))))
                              k29)))
                          k28))
                       k17))))))))
           (lambda (rv48) (return #f k16))))
        (void)
        k15))
     k14))
  ($halt (void))))
