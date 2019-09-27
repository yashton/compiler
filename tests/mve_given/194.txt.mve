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
   (py-list* "mike" "sarah" "bill")
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
                   ((lambda (e16 k26)
                      ((lambda (i15 k27)
                         ((cps py-list?)
                          e16
                          (lambda (rv28)
                            (if rv28
                              ((cps py-list-ref) e16 i15 k27)
                              ((cps tuple?)
                               e16
                               (lambda (rv29)
                                 (if rv29
                                   ((cps tuple-ref) e16 i15 k27)
                                   ((cps dict?)
                                    e16
                                    (lambda (rv30)
                                      (if rv30
                                        ((cps dict-ref) e16 i15 k27)
                                        (error
                                         "cannot index object"
                                         k27)))))))))))
                       g$t
                       k26))
                    g$fred
                    (lambda (rv31) ((cps py-print) rv31 k24))))))
              k23)))
          k22))
       k14))
    $halt))))
