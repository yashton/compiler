(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$fred (void))
 (define g$test (void))
 (define g$t (void))
 (set-then!
  g$fred
  (dict ("mike" 456) ("bill" 399) ("sarah" 521))
  (set-then!
   g$test
   (py-list* "zingo" "sarah" "bill" "wilma")
   ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
    (lambda (break k14)
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
       g$test
       (lambda (i14 k22)
         ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
          (lambda (continue k23)
            (set-then!
             g$t
             i14
             ((lambda (k24)
                ((cps py-print)
                 g$t
                 (lambda (rv25)
                   (get-field
                    g$fred
                    has_key
                    (lambda (rv26)
                      (rv26
                       g$t
                       (lambda (rv27)
                         (if rv27
                           ((lambda (k28)
                              ((lambda (e16 k29)
                                 ((lambda (i15 k30)
                                    ((cps py-list?)
                                     e16
                                     (lambda (rv31)
                                       (if rv31
                                         ((cps py-list-ref) e16 i15 k30)
                                         ((cps tuple?)
                                          e16
                                          (lambda (rv32)
                                            (if rv32
                                              ((cps tuple-ref) e16 i15 k30)
                                              ((cps dict?)
                                               e16
                                               (lambda (rv33)
                                                 (if rv33
                                                   ((cps dict-ref) e16 i15 k30)
                                                   (error
                                                    "cannot index object"
                                                    k30)))))))))))
                                  g$t
                                  k29))
                               g$fred
                               (lambda (rv34) ((cps py-print) "=>" rv34 k28))))
                            k24)
                           ((lambda (k35)
                              ((cps py-print) "is not present." k35))
                            k24)))))))))
              k23)))
          k22))
       k14))
    $halt))))
